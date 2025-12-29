library(readr)
library(rgdal)
library(ggplot2)
library(lubridate)
library(devtools)
library(remotes)
library(dplyr)
library(rSOILWAT2)
library(zoo)
library(Surrogate)
library(daymetr)
library(stringr)

clay = read.csv("D:/Thesis/Chapter2_simulations/soils/allsites_clay.csv")
sand = read.csv("D:/Thesis/Chapter2_simulations/soils/allsites_sand.csv")
gravel = read.csv("D:/Thesis/Chapter2_simulations/soils/allsites_gravel.csv")
veggie = read.csv("D:/Thesis/Chapter2_simulations/vegetation/allsites_veg.csv")
plots = read.csv("D:/Thesis/Chapter2_simulations/locations/points_States.csv")

sw_args <- function(dir, files.in, echo, quiet) {
  input <- "SOILWAT2"
  
  if (dir != "")
    input <- c(input, "-d", dir)
  if (files.in != "")
    input <- c(input, "-f", files.in)
  if (echo)
    input <- c(input, "-e")
  if (quiet)
    input <- c(input, "-q")
  
  input
}

path_demo <- system.file("extdata", "example1", package = "rSOILWAT2")

sw_in <- rSOILWAT2::sw_exampleData

create.Veg = function(wdata,relAbundanc_L1,relAbundanc_L2,relAbundanc_Grass,slope,yearStart,yearEnd) {
  
  #Create a shell based on the inputs folder in the repository
  sw_in3_low_U <- sw_inputDataFromFiles(dir = path_demo, files.in = "files.in")
  sw_in3_low_U@site@IntrinsicSiteParams['Slope'] = slope
  sw_in3_low_U@years@StartYear = yearStart
  sw_in3_low_U@years@EndYear = yearEnd
  # Note: `estimate_PotNatVeg_composition()` does not estimate fractional cover
  # of trees, bare-ground, etc. and sets those to zero or any other user
  # defined value
  
  clim <- rSOILWAT2::calc_SiteClimate(weatherList = wdata, do_C4vars = TRUE)
  
  #Generate data off daymet data
  veg_cover_low_U <-  rSOILWAT2::estimate_PotNatVeg_composition(
    MAP_mm = 10 * clim[["MAP_cm"]],
    MAT_C = clim[["MAT_C"]],
    mean_monthly_ppt_mm = 10 * clim[["meanMonthlyPPTcm"]],
    mean_monthly_Temp_C = clim[["meanMonthlyTempC"]],
    dailyC4vars = clim[["dailyC4vars"]]
  )
  
  # Ensure the sum of all components in veg_cover is equal to 1
  #Here I'm manually changing calculated starting veg values for later calculations.  3 different veg_cover outputs, were low shrub cover (10%), moderate shrub cover (40%), and high shrub cover (60%). the values of Rel_Abundance_L0 and Rel_Abundance_L1 need to add up to 1. low shrub cover( grass 43%, forbs 23%), moderate shrub cover (grass 26%, forbs 16%), and high shrub cover (grass 15%, forbs 12%) (Pennington et al., 2019).  
  veg_cover_low_U[["Rel_Abundance_L1"]][["SW_TREES"]]<-relAbundanc_L1[1]
  veg_cover_low_U[["Rel_Abundance_L1"]][["SW_SHRUB"]]<-relAbundanc_L1[2]
  veg_cover_low_U[["Rel_Abundance_L1"]][["SW_FORBS"]]<-relAbundanc_L1[3]
  veg_cover_low_U[["Rel_Abundance_L1"]][["SW_GRASS"]]<-relAbundanc_L1[4]
  veg_cover_low_U[["Rel_Abundance_L1"]][["SW_BAREGROUND"]]<-relAbundanc_L1[5]
  
  
  veg_cover_low_U[["Rel_Abundance_L0"]][["Grasses_C3"]]<-relAbundanc_L2[3]
  veg_cover_low_U[["Rel_Abundance_L0"]][["Grasses_C4"]]<-relAbundanc_L2[4]
  veg_cover_low_U[["Rel_Abundance_L0"]][["Grasses_Annuals"]]<-relAbundanc_L2[5]
  veg_cover_low_U[["Rel_Abundance_L0"]][["Shrubs"]]<-relAbundanc_L2[6]
  veg_cover_low_U[["Rel_Abundance_L0"]][["Forbs"]]<-relAbundanc_L2[2]
  veg_cover_low_U[["Rel_Abundance_L0"]][["Trees"]]<-relAbundanc_L2[7]
  veg_cover_low_U[["Rel_Abundance_L0"]][["BareGround"]]<-relAbundanc_L2[8]
  veg_cover_low_U[["Rel_Abundance_L0"]][["Succulents"]]<-relAbundanc_L2[1]
  
  veg_cover_low_U[["Grasses"]][["Grasses_C3"]]<-relAbundanc_Grass[1]
  veg_cover_low_U[["Grasses"]][["Grasses_C4"]]<-relAbundanc_Grass[2]
  veg_cover_low_U[["Grasses"]][["Grasses_Annuals"]]<-relAbundanc_Grass[3]
  
  ids <- sapply(
    X = names(rSOILWAT2::swProd_Composition(sw_in3_low_U)),
    FUN = function(x) {
      grep(
        pattern = substr(x, 1, 4),
        x = names(veg_cover_low_U[["Rel_Abundance_L1"]]),
        ignore.case = TRUE
      )
    }
  )
  # Assign fractional cover values to rSOILWAT2 input object
  rSOILWAT2::swProd_Composition(sw_in3_low_U) <- veg_cover_low_U[["Rel_Abundance_L1"]][ids]
  ### Biomass amount and phenology (for shrubs and grasses)
  # Reference biomass values from Bradford et al. 2014 are used
  # Mean monthly reference temperature corresponding to default phenology values
  # for the median across 898 big sagebrush sites are used
  veg_biom_low_U <- rSOILWAT2::estimate_PotNatVeg_biomass(
    target_temp = clim[["meanMonthlyTempC"]],
    target_MAP_mm = 10 * clim[["MAP_cm"]],
    do_adjust_phenology = TRUE,
    do_adjust_biomass = TRUE,
    fgrass_c3c4ann = veg_cover_low_U[["Grasses"]],
  )
  
  
  # Assign monthly biomass values to rSOILWAT2 input object
  # Note: monthly biomass values of forbs, trees, etc. need to be estimated
  v1 <- c("Litter", "Biomass", "Perc.Live")
  v2 <- c("Litter", "Biomass", "Live_pct")
  rSOILWAT2::swProd_MonProd_grass(sw_in3_low_U)[, v2] <- veg_biom_low_U[["grass"]][, v1]
  rSOILWAT2::swProd_MonProd_shrub(sw_in3_low_U)[, v2] <- veg_biom_low_U[["shrub"]][, v1]
  
  ### Rooting profiles of vegetation types
  
  # Select rooting profile types
  # Set those to "FILL" where cover == 0 (because of transpiration regions)
  trco_type_by_veg <- list(
    grass_C3 = if (veg_cover_low_U[["Rel_Abundance_L0"]][["Grasses_C3"]] > 0) {
      "SchenkJackson2003_PCdry_grasses"
    } else {
      "FILL"
    },
    grass_C4 = if (veg_cover_low_U[["Rel_Abundance_L0"]][["Grasses_C4"]] > 0) {
      "SchenkJackson2003_PCdry_grasses"
    } else {
      "FILL"
    },
    grass_annuals = if (
      veg_cover_low_U[["Rel_Abundance_L0"]][["Grasses_Annuals"]] > 0
    ) {
      "Jacksonetal1996_crops"
    } else {
      "FILL"
    },
    shrub = if (veg_cover_low_U[["Rel_Abundance_L0"]][["Shrubs"]] > 0) {
      "SchenkJackson2003_PCdry_shrubs"
    } else {
      "FILL"
    },
    forb = if (veg_cover_low_U[["Rel_Abundance_L0"]][["Forbs"]] > 0) {
      "SchenkJackson2003_PCdry_forbs"
    } else {
      "FILL"
    },
    tree = if (veg_cover_low_U[["Rel_Abundance_L0"]][["Trees"]] > 0) {
      "Bradfordetal2014_LodgepolePine"
    } else {
      "FILL"
    }
  )
  return(list(sw_in = sw_in3_low_U,trco = trco_type_by_veg,
              veg_cover = veg_cover_low_U))
}

create.Soil = function(gravel,sand,clay,sw_in3_low_U) {
  #Here I'm creating new soil values to be inserted into the sW_in3_low file
  
  soil_fixed <- data.frame(
    depth_cm = c(5, 10, 20, 30, 40,60,80,100),
    bulkDensity_g.cm.3 = c(1.3,1.3,1.3,1.3,1.3,1.3,1.3,1.3),
    gravel_content = gravel,
    EvapBareSoil_frac = c(0.9182,0.0818,0.000,0.000,0.000,0.000,0.000,0.000),
    transpGrass_frac = c(0.1978543,0.1651257,0.2528258,0.3841942,0.3841942,0.2528258,0.3841942,0.3841942),
    transpShrub_frac = c(0.1585857,0.1414536,0.2387138,0.4612469,0.4612469,0.2387138,0.4612469,0.4612469),
    transpTree_frac =  c(0.1,0.1,0.2,0.6,0.4,0.2,0.6,0.4),
    transpForb_frac = c(0.1978543,0.1651257,0.2528258,0.3841942,0.3841942,0.2528258,0.3841942,0.3841942),
    sand_frac = sand,
    clay_frac = clay,
    impermeability_frac = c(0,0,0,0,0,0,0,0),
    soilTemp_c = c(-0.9803060,-0.9606121,-0.9212242,-0.8030605, -0.8030605,-0.9212242,-0.8030605, -0.8030605)
  )
  soil_new <- data.frame(rSOILWAT2::swSoils_Layers(sw_in3_low_U)[0, ])
  
  soil_new[seq_len(nrow(soil_fixed)), ] <- soil_fixed
  
  all_soils = data.frame(soil_new)
  
  # Create empty dataframes for storing results, I care about VWC and AET so I am making empty files based on that
  output_all_soils_VWC <- data.frame(
    Year = numeric(),
    Day = numeric(),
    Lyr_1 = numeric(),
    Lyr_2 = numeric(),
    Lyr_3 = numeric(),
    Lyr_4 = numeric(),
    Lyr_5 = numeric(),
    Lyr_6 = numeric(),
    Lyr_7 = numeric(),
    Lyr_8 = numeric()
  )
  output_all_soils_AET <- data.frame(
    Year = numeric(),
    Day = numeric(),
    Lyr_1 = numeric(),
    Lyr_2 = numeric(),
    Lyr_3 = numeric(),
    Lyr_4 = numeric(),
    Lyr_5 = numeric(),
    Lyr_6 = numeric(),
    Lyr_7 = numeric(),
    Lyr_8 = numeric()
  )
  output_all_soils_SWP = data.frame(
    Year = numeric(),
    Day = numeric(),
    Lyr_1 = numeric(),
    Lyr_2 = numeric(),
    Lyr_3 = numeric(),
    Lyr_4 = numeric(),
    Lyr_5 = numeric(),
    Lyr_6 = numeric(),
    Lyr_7 = numeric(),
    Lyr_8 = numeric()
  )
  
  return(list(VWC = output_all_soils_VWC,AET = output_all_soils_AET,
              SWP = output_all_soils_SWP,all_soils = all_soils))
}

run_SOILWAT2 = function(startYear,all_soils, trco_type_by_veg, veg_cover_low_U,
                        sw_in3_low_U, wdata,output_all_soils_VWC,
                        output_all_soils_AET,output_all_soils_SWP) {
  soil_new2 <- data.frame(
    depth_cm = all_soils$depth_cm[1:8],
    bulkDensity_g.cm.3 = all_soils$bulkDensity_g.cm.3[1:8],
    gravel_content = all_soils$gravel_content[1:8],
    EvapBareSoil_frac = all_soils$EvapBareSoil_frac[1:8],
    transpGrass_frac = all_soils$transpGrass_frac[1:8],
    transpShrub_frac = all_soils$transpShrub_frac[1:8],
    transpTree_frac = all_soils$transpTree_frac[1:8],
    transpForb_frac = all_soils$transpForb_frac[1:8],
    sand_frac = all_soils$sand_frac[1:8],
    clay_frac = all_soils$clay_frac[1:8],
    impermeability_frac = all_soils$impermeability_frac[1:8],
    soilTemp_c = all_soils$soilTemp_c[1:8]
  )
  
  veg_roots <- rSOILWAT2::estimate_PotNatVeg_roots(
    layers_depth = soil_new2[, "depth_cm"],
    trco_type_by_veg = trco_type_by_veg,
    fgrass_c3c4ann = veg_cover_low_U[["Grasses"]],
  )
  # Add rooting profile to soil
  v1 <- c("Grass", "Shrub", "Tree", "Forb")
  v2 <- paste0("transp", v1, "_frac")
  soil_new2[, v2] <- veg_roots[, v1]
  # Create a new sw_in object with soil_new
  rSOILWAT2::swSoils_Layers(sw_in3_low_U) <- data.matrix(soil_new2)
  # Run sw_in through rSOILWAT2::sw_exec() to create sw_out
  sw_out11 <- rSOILWAT2::sw_exec(inputData = sw_in3_low_U, weatherList = wdata)
  # Add the results to the respective output dataframe based on the current bucket
  
  output_VWC2 <- as.data.frame(sw_out11@VWCBULK@Day)
  output_all_soils_VWC  <- rbind(output_all_soils_VWC , output_VWC2)
  output_all_soils_VWC$Year = output_all_soils_VWC$Year# - (1980-startYear)
  
  output_AET2 <- as.data.frame(sw_out11@AET@Day)
  output_all_soils_AET <- rbind(output_all_soils_AET , output_AET2)
  output_all_soils_AET$Year = output_all_soils_AET$Year# - (1980-startYear)
  
  output_SWP = as.data.frame(sw_out11@SWPMATRIC@Day)
  output_all_soils_SWP = rbind(output_all_soils_SWP,output_SWP)
  output_all_soils_SWP$Year = output_all_soils_SWP$Year
  
  return(list(VWC = output_all_soils_VWC, AET = output_all_soils_AET, 
              SWP = output_all_soils_SWP))
}

create.Climate.daymet = function(site,lat,long,yearStart,yearEnd) {
  if (
    requireNamespace("daymetr") &&
    requireNamespace("curl") &&
    curl::has_internet()
  ) {
    
    # Download from `DayMet`
    dm_site <- try(daymetr::download_daymet(
      site = site,
      lat = lat,
      lon = long,
      start = yearStart,
      end = yearEnd,
      internal = TRUE,
      simplify = FALSE
    ))
    
    if (!inherits(dm_site, "try-error")) {
      # Convert data to a `rSOILWAT2`-formatted weather object
      vars <- c("year", "yday", "tmax..deg.c.", "tmin..deg.c.", "prcp..mm.day.")
      xdf <- dm_site[["data"]][, vars]
      xdf[, "prcp..mm.day."] <- xdf[, "prcp..mm.day."] / 10 # convert mm -> cm
      colnames(xdf) <- c("Year", "DOY", "Tmax_C", "Tmin_C", "PPT_cm")
      
      #xdf$Year <- xdf$Year - (1992 - 1980)
      #need to figure out what the above line does#########################
      
      wdata_dm <- rSOILWAT2::dbW_dataframe_to_weatherData(weatherDF = xdf)
      
      # Convert `DayMet`'s `noleap` calendar to proleptic Gregorian calendar
      xdf2 <- rSOILWAT2::dbW_convert_to_GregorianYears(weatherData = wdata_dm)
      
      wdata <- rSOILWAT2::dbW_generateWeather(
        weatherData = rSOILWAT2::dbW_dataframe_to_weatherData(weatherDF = xdf2),
        seed = 123
      )
      
      # Check that weather data is well-formed
      stopifnot(rSOILWAT2::dbW_check_weatherData(wdata))
    }
  }
  return(wdata)
}

for(k in 1:898){
      sit = plots[k,1]
      site_clay = clay %>% filter(site==sit)
      site_sand = sand %>% filter(site==sit)
      site_gravel = gravel %>% filter(site==sit)
      
      if (is.na(site_clay[3])==T){
        clay_in = c(site_clay[[2]],site_clay[[2]],site_clay[[2]],site_clay[[2]],site_clay[[2]],site_clay[[2]],
                    site_clay[[2]],site_clay[[2]])
        sand_in = c(site_sand[[2]],site_sand[[2]],site_sand[[2]],site_sand[[2]],site_sand[[2]],site_sand[[2]],
                    site_sand[[2]],site_sand[[2]])
        gravel_in = c(site_gravel[[2]],site_gravel[[2]],site_gravel[[2]],site_gravel[[2]],site_gravel[[2]],site_gravel[[2]],
                      site_gravel[[2]],site_gravel[[2]])
      } else if(is.na(site_clay[4])==T){
        clay_in = c(site_clay[[2]],site_clay[[3]],site_clay[[3]],site_clay[[3]],site_clay[[3]],site_clay[[3]],
                    site_clay[[3]],site_clay[[3]])
        sand_in = c(site_sand[[2]],site_sand[[3]],site_sand[[3]],site_sand[[3]],site_sand[[3]],site_sand[[3]],
                    site_sand[[3]],site_sand[[3]])
        gravel_in = c(site_gravel[[2]],site_gravel[[3]],site_gravel[[3]],site_gravel[[3]],site_gravel[[3]],site_gravel[[3]],
                      site_gravel[[3]],site_gravel[[3]])
      }else if(is.na(site_clay[5])==T){
        clay_in = c(site_clay[[2]],site_clay[[3]],site_clay[[4]],site_clay[[4]],site_clay[[4]],site_clay[[4]],
                    site_clay[[4]],site_clay[[4]])
        sand_in = c(site_sand[[2]],site_sand[[3]],site_sand[[4]],site_sand[[4]],site_sand[[4]],site_sand[[4]],
                    site_sand[[4]],site_sand[[4]])
        gravel_in = c(site_gravel[[2]],site_gravel[[3]],site_gravel[[4]],site_gravel[[4]],site_gravel[[4]],site_gravel[[4]],
                      site_gravel[[4]],site_gravel[[4]])
      }else if(is.na(site_clay[6])==T){
        clay_in = c(site_clay[[2]],site_clay[[3]],site_clay[[4]],site_clay[[5]],site_clay[[5]],site_clay[[5]],
                    site_clay[[5]],site_clay[[5]])
        sand_in = c(site_sand[[2]],site_sand[[3]],site_sand[[4]],site_sand[[5]],site_sand[[5]],site_sand[[5]],
                    site_sand[[5]],site_sand[[5]])
        gravel_in = c(site_gravel[[2]],site_gravel[[3]],site_gravel[[4]],site_gravel[[5]],site_gravel[[5]],site_gravel[[5]],
                      site_gravel[[5]],site_gravel[[5]])
      }else if(is.na(site_clay[6])==T){
        clay_in = c(site_clay[[2]],site_clay[[3]],site_clay[[4]],site_clay[[5]],site_clay[[5]],site_clay[[5]],
                    site_clay[[5]],site_clay[[5]])
        sand_in = c(site_sand[[2]],site_sand[[3]],site_sand[[4]],site_sand[[5]],site_sand[[5]],site_sand[[5]],
                    site_sand[[5]],site_sand[[5]])
        gravel_in = c(site_gravel[[2]],site_gravel[[3]],site_gravel[[4]],site_gravel[[5]],site_gravel[[5]],site_gravel[[5]],
                      site_gravel[[5]],site_gravel[[5]])
      }else if(is.na(site_clay[7])==T){
        clay_in = c(site_clay[[2]],site_clay[[3]],site_clay[[4]],site_clay[[5]],site_clay[[6]],site_clay[[6]],
                    site_clay[[6]],site_clay[[6]])
        sand_in = c(site_sand[[2]],site_sand[[3]],site_sand[[4]],site_sand[[5]],site_sand[[6]],site_sand[[6]],
                    site_sand[[6]],site_sand[[6]])
        gravel_in = c(site_gravel[[2]],site_gravel[[3]],site_gravel[[4]],site_gravel[[5]],site_gravel[[6]],site_gravel[[6]],
                      site_gravel[[6]],site_gravel[[6]])
      }else if(is.na(site_clay[8])==T){
        clay_in = c(site_clay[[2]],site_clay[[3]],site_clay[[4]],site_clay[[5]],site_clay[[6]],site_clay[[7]],
                    site_clay[[7]],site_clay[[7]])
        sand_in = c(site_sand[[2]],site_sand[[3]],site_sand[[4]],site_sand[[5]],site_sand[[6]],site_sand[[7]],
                    site_sand[[7]],site_sand[[7]])
        gravel_in = c(site_gravel[[2]],site_gravel[[3]],site_gravel[[4]],site_gravel[[5]],site_gravel[[6]],site_gravel[[7]],
                      site_gravel[[7]],site_gravel[[7]])
      }else if(is.na(site_clay[9])==T){
        clay_in = c(site_clay[[2]],site_clay[[3]],site_clay[[4]],site_clay[[5]],site_clay[[6]],site_clay[[7]],
                    site_clay[[8]],site_clay[[8]])
        sand_in = c(site_sand[[2]],site_sand[[3]],site_sand[[4]],site_sand[[5]],site_sand[[6]],site_sand[[7]],
                    site_sand[[8]],site_sand[[8]])
        gravel_in = c(site_gravel[[2]],site_gravel[[3]],site_gravel[[4]],site_gravel[[5]],site_gravel[[6]],site_gravel[[7]],
                      site_gravel[[8]],site_gravel[[8]])
      }else {
        clay_in = c(site_clay[[2]],site_clay[[3]],site_clay[[4]],site_clay[[5]],site_clay[[6]],site_clay[[7]],
                    site_clay[[8]],site_clay[[9]])
        sand_in = c(site_sand[[2]],site_sand[[3]],site_sand[[4]],site_sand[[5]],site_sand[[6]],site_sand[[7]],
                    site_sand[[8]],site_sand[[9]])
        gravel_in = c(site_gravel[[2]],site_gravel[[3]],site_gravel[[4]],site_gravel[[5]],site_gravel[[6]],site_gravel[[7]],
                      site_gravel[[8]],site_gravel[[9]])
      }
      
      soils = data.frame(gravel_in,sand_in,clay_in)
      
      if (k==372){
        gravel_in=sw_in@soils@Layers[,3]
        sand_in = sw_in@soils@Layers[,9]
        clay_in = sw_in@soils@Layers[,10]}
      if (k==689){
        gravel_in=sw_in@soils@Layers[,3]
        sand_in = sw_in@soils@Layers[,9]
        clay_in = sw_in@soils@Layers[,10]}
      
      site_veg = veggie %>% filter(Label==sit)     
      
      L1 = unlist(lapply(site_veg$L1, function(L1) {
        # Remove parentheses and split the values by commas
        values <- strsplit(gsub("[()]", "", L1), ",")[[1]]
        
        # Convert the character values to numeric
        values_numeric <- as.numeric(values)
        
        # Return the list
        return(values_numeric)
      }))
      
      L2 = unlist(lapply(site_veg$L0, function(L0) {
        # Remove parentheses and split the values by commas
        values <- strsplit(gsub("[()]", "", L0), ",")[[1]]
        
        # Convert the character values to numeric
        values_numeric <- as.numeric(values)
        
        # Return the list
        return(values_numeric)
      }))
      grass = unlist(lapply(site_veg$Grass, function(Grass) {
        # Remove parentheses and split the values by commas
        values <- strsplit(gsub("[()]", "", Grass), ",")[[1]]
        
        # Convert the character values to numeric
        values_numeric <- as.numeric(values)
        
        # Return the list
        return(values_numeric)
      }))
      
      
      slopes = plots$SLOPE
      
      slope = slopes[k]
      
      #wdata = create.Climate.rcp(sit,2040,2060)
      #site,lat,long,yearStart,yearEnd
      wdata = create.Climate.daymet(sit,plots[k,7],plots[k,6],1980,2022)
      
      veg = create.Veg(wdata,L1,L2,grass,slope,as.integer(1980),as.integer(2022))
      soil_outputs = create.Soil(gravel_in,sand_in,clay_in,sw_in)
      #co2 = read.csv('input_files/CO2_historic.csv')
      #co2 = co2 %>% rename(CO2ppm = CO2,Year = year) %>% select(Year,CO2ppm)
      #co2 = co2 %>% filter(Year != 1914)
      #if(j=='45'){co2=data.frame(Year=2040:2060,CO2ppm=500:520)}
      #lse if(j=='85'){co2=data.frame(Year=2040:2060,CO2ppm=600:620)}
      co2 = rbind(data.frame(veg$sw_in@carbon@CO2ppm),
            data.frame(Year=c(2011,2012,2013,2014,2015,2016,2017,2018,2019,
                                                  2020,2021,2022),CO2ppm=c(391.85,394.06,396.74,398.81,401.01,
                                                                           404.41,406.76,408.72,
                                                                           411.65,414.21,416.41,
                                                                           418.53)))
      veg$sw_in@carbon@CO2ppm = as.matrix(co2)
      historic_run = run_SOILWAT2(1980,soil_outputs$all_soils,veg$trco,veg$veg_cover,
                                  veg$sw_in,wdata,soil_outputs$VWC,
                                  soil_outputs$AET,soil_outputs$SWP)
      matrix = as.data.frame(rSOILWAT2::dbW_weatherData_to_dataframe(wdata))
      size = nrow(matrix)
      simulated = data.frame(date=1:size,month=1:size,avgtemp=1:size)
      simulated$date = as.Date(paste(matrix$Year, matrix$DOY), format = "%Y %j")
      simulated$month <- as.numeric(format(simulated$date, "%m"))
      simulated$avgtemp = (matrix$Tmin_C + matrix$Tmax_C)/2
      swp = historic_run$SWP
      aet = historic_run$AET
      ddd.variables = cbind(simulated,swp,aet)
      keeps = c('Year','Day','avgtemp','esnow_cm','Lyr_1','Lyr_2','Lyr_3','Lyr_4',
                'Lyr_5','Lyr_6','Lyr_7','Lyr_8')
      ddd.variables = ddd.variables[keeps]
      drydd.days = ddd.variables %>% subset(esnow_cm==0) %>% 
        subset(Lyr_1>=15&Lyr_2>=15&Lyr_3>=15&Lyr_4>=15&Lyr_5>=15&Lyr_6>=15&
                 Lyr_7>=15&Lyr_8>=15) %>%
        subset(avgtemp >= 5)
      data = ddd.variables %>% aggregate(avgtemp ~ Year,FUN=mean)
      dat = (drydd.days %>% count(Year))
      data = left_join(data,dat,join_by(Year==Year))
      data$plot = sit
      #write.csv(data,paste0('output_files/future/',i,"_",j,'_drydaysTEMP_midCentury.csv'))
      #if(k==1){out = data}
      #else{out = rbind(out,data)}
    write.csv(data,paste0("D:/Thesis/Chapter2_simulations/recent_output/",sit,'_recent.csv'))}
