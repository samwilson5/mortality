# read in plot names csv
plots = read.csv("D:/Thesis/Chapter2_simulations/locations/points_States.csv")

# create an empty dataframe to store median values
median_by_plot = data.frame(plot = plots$Label,median_ztemp = 1:898,median_zn = 1:898)
# calculates median z-score, not z-score of the median!
for(i in 1:898){
  # get plot name
  plot = median_by_plot[i,1]
  # read corresponding csv
  data = read.csv(paste0("D:/Thesis/Chapter2_simulations/recent_output/",plot,'_recent.csv'))
  # calculate the zscore of each year for that plot's temperature data
  ztemp =  (data$avgtemp - mean(data$avgtemp,na.rm=T))/sd(data$avgtemp,na.rm=T)
  # calculate the zscore of each year for that plot's n data
  zn = (data$n - mean(data$n,na.rm=T))/sd(data$n,na.rm=T)
  # calculate the median zscore temperature for that plot and store it in median_by_plot
  median_by_plot[i,2] = median(ztemp,na.rm = T)
  # calculate the median zscore n for that plot and store it in median_by_plot
  median_by_plot[i,3] = median(zn,na.rm = T)
}

# create an empty dataframe to store mean and sd of each plot
mean_by_plot = data.frame(plot = plots$Label,mean_temp = 1:898,mean_n = 1:898,
                          sd_temp = 1:898, sd_n = 1:898)
# calculates mean of each plot!
for(i in 1:898){
  # get plot name
  plot = mean_by_plot[i,1]
  # read corresponding csv
  data = read.csv(paste0("D:/Thesis/Chapter2_simulations/recent_output/",plot,'_recent.csv'))
  # calculate and store mean temp for that plot 
  mean_by_plot[i,2] = mean(data$avgtemp,na.rm = T)
  # calculate and store mean n for that plot
  mean_by_plot[i,3] = mean(data$n,na.rm = T)
  # calculate and store sd temp for that plot
  mean_by_plot[i,4] = sd(data$avgtemp,na.rm = T)
  # calculate and store sd n for that plot
  mean_by_plot[i,5] = sd(data$n,na.rm = T)
}

library(dplyr)

# get a list of all csv's for RCP 45
files_45 = list.files("D:/Thesis/Chapter2_simulations/future_output/",
                      pattern="45+", full.names=TRUE)
# get a list of all csv's for RCP 85
files_85 = list.files("D:/Thesis/Chapter2_simulations/future_output/",
                      pattern="85+", full.names=TRUE)
################## all RCP 45!
library(stringr)
# bind all the 45 files into a single dataframe
for (i in files_45){
  gcm = str_split(str_split(i,"_")[[1]][3],'/')[[1]][2]
  df = read.csv(i)
  df['gcm'] = gcm
  if(i == files_45[1]){x=df}
  else{x = rbind(x,df)}
}

# create empty columns, I do this to easily set the column names
x$ztemp = 1
x$zn = 1

for( i in 1:898){
  site_id = mean_by_plot[i,1]
  limit = x %>% filter(plot==site_id)
  for(k in 1:nrow(limit)){
    limit[k,7] = (limit[k,3] - mean_by_plot[i,2])/mean_by_plot[i,4]
    limit[k,8] = (limit[k,4] - mean_by_plot[i,3])/mean_by_plot[i,5]
  }
  if(i==1){final=limit}
  else{final=rbind(final,limit)}
}



values = read.csv("E:/SoilWat/recent_visited_plots_zscore.csv")

mean.n = mean(values$max_zn)
mean.temp = mean(values$max_ztemp)
mean.n = median(values$max_zn)
mean.temp = median(values$max_ztemp)

y = final %>% aggregate(zn ~ plot + Year,mean)
final$mortality = ifelse(final$ztemp >= mean.temp,ifelse(final$zn >= mean.n,1,0),0)


gcm.mort = final %>% aggregate(mortality ~ gcm+plot,max)

prop.mort = gcm.mort %>% aggregate(mortality ~ plot, sum)

write.csv(prop.mort,"C:/Users/Sam/Desktop/mortality_uncertain_prob_45.csv")

################## all RCP 85! ################################################
library(stringr)
# bind all the 45 files into a single dataframe
for (i in files_85){
  gcm = str_split(str_split(i,"_")[[1]][3],'/')[[1]][2]
  df = read.csv(i)
  df['gcm'] = gcm
  if(i == files_85[1]){x=df}
  else{x = rbind(x,df)}
}

# create empty columns, I do this to easily set the column names
x$ztemp = 1
x$zn = 1

for( i in 1:898){
  site_id = mean_by_plot[i,1]
  limit = x %>% filter(plot==site_id)
  for(k in 1:nrow(limit)){
    limit[k,7] = (limit[k,3] - mean_by_plot[i,2])/mean_by_plot[i,4]
    limit[k,8] = (limit[k,4] - mean_by_plot[i,3])/mean_by_plot[i,5]
  }
  if(i==1){final=limit}
  else{final=rbind(final,limit)}
}

values = read.csv("E:/SoilWat/recent_visited_plots_zscore.csv")

mean.n = mean(values$max_zn)
mean.temp = mean(values$max_ztemp)

mean.n = median(values$max_zn)
mean.temp = median(values$max_ztemp)

final$mortality = ifelse(final$ztemp >= mean.temp,ifelse(final$zn >= mean.n,1,0),0)


gcm.mort = final %>% aggregate(mortality ~ gcm+plot,max)

prop.mort = gcm.mort %>% aggregate(mortality ~ plot, sum)


write.csv(prop.mort,"C:/Users/Sam/Desktop/mortality_uncertain_prob_85.csv")
