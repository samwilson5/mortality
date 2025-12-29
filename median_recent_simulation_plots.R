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

# aggregate the 45 temperature data by Year and plot
mean.temp = x %>% aggregate(avgtemp ~ Year+plot,mean)
# aggregate the 45 n data by Year and plot
mean.n = x %>% aggregate(n ~ Year+plot,mean)

# join the aggregated data, we now have a dataframe with an average value for
# temp and n for each year for each plot for the RCP 45 data
mean_future = mean.temp %>% left_join(mean.n,join_by(plot==plot,Year==Year))
# now we need to find z-score of each yearXplot compared to orinigal average
# then find distance from the median z-score to each year, then look at maxes!

# create empty columns, I do this to easily set the column names
mean_future$ztemp = 1
mean_future$zn = 1

# assign a modified euclidean distance formula that only return non-zero if both 
# values in the second point are larger than those of the first
CED = function(vect1,vect2) {
  if (vect2[1]>=vect1[1] & vect2[2]>=vect1[2]){
    
    return(sqrt(sum((vect1 - vect2)^2)))}
  #return(sum((vect2 - vect1)))
  else{return(0)}
}



for( i in 1:898){
  site_id = mean_by_plot[i,1]
  limit = mean_future %>% filter(plot==site_id)
  for(k in 1:nrow(limit)){
    limit[k,5] = (limit[k,3] - mean_by_plot[i,2])/mean_by_plot[i,4]
    limit[k,6] = (limit[k,4] - mean_by_plot[i,3])/mean_by_plot[i,5]
  }
  if(i==1){final=limit}
  else{final=rbind(final,limit)}
}
mean_future = final
mean_future$dist = 1

for( i in 1:898){
  site_id = median_by_plot[i,1]
  limit = mean_future %>% filter(plot==site_id)
  for (k in 1:nrow(limit)){
    limit[k,7] = CED(c(median_by_plot[i,2],median_by_plot[i,3]),c(limit[k,5],limit[k,6]))
  }
  if(i==1){final=limit}
  else{final=rbind(final,limit)}
}
dist_future = final

write.csv(dist_future,"D:/Thesis/Chapter2_simulations/future_45_distances.csv")
#########################################################


################## all RCP 85!
# bind all the 85 files into a single dataframe
for (i in files_85){
  if(i == files_85[1]){x=read.csv(i)}
  else{x = rbind(x,read.csv(i))}
}

# aggregate the 85 temperature data by Year and plot
mean.temp = x %>% aggregate(avgtemp ~ Year+plot,mean)
# aggregate the 85 n data by Year and plot
mean.n = x %>% aggregate(n ~ Year+plot,mean)

# join the aggregated data, we now have a dataframe with an average value for
# temp and n for each year for each plot for the RCP 85 data
mean_future = mean.temp %>% left_join(mean.n,join_by(plot==plot,Year==Year))
# now we need to find z-score of each yearXplot compared to orinigal average
# then find distance from the median z-score to each year, then look at maxes!

# create empty columns, I do this to easily set the column names
mean_future$ztemp = 1
mean_future$zn = 1



for( i in 1:898){
  site_id = mean_by_plot[i,1]
  limit = mean_future %>% filter(plot==site_id)
  for(k in 1:nrow(limit)){
    limit[k,5] = (limit[k,3] - mean_by_plot[i,2])/mean_by_plot[i,4]
    limit[k,6] = (limit[k,4] - mean_by_plot[i,3])/mean_by_plot[i,5]
  }
  if(i==1){final=limit}
  else{final=rbind(final,limit)}
}
mean_future = final
mean_future$dist = 1

for( i in 1:898){
  site_id = median_by_plot[i,1]
  limit = mean_future %>% filter(plot==site_id)
  for (k in 1:nrow(limit)){
    limit[k,7] = CED(c(median_by_plot[i,2],median_by_plot[i,3]),c(limit[k,5],limit[k,6]))
  }
  if(i==1){final=limit}
  else{final=rbind(final,limit)}
}
dist_future = final

write.csv(dist_future,"D:/Thesis/Chapter2_simulations/future_85_distances.csv")



