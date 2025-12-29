files = list.files("E:/SoilWat/dryness/dry_output/recent/",
                   full.names=TRUE)
library(stringr)

for (i in files){
  breaks = unlist(strsplit(i,split='/'))
  name = breaks[6]
  if (substr(name,1,7) == 'Lowland'){
    name = substr(name,1,9)
    x = read.csv(i)
    assign(name,x) 
  }else{
    name = substr(name,1,8)
    x = read.csv(i)
    assign(name,x)}
}

library(dplyr)


CED = function(vect1,vect2) {
  if (vect2[1]>=vect1[1] & vect2[2]>=vect1[2]){
    return(sqrt(sum((vect2 - vect1)^2)))}
  #return(sum((vect2 - vect1)))
  else{return(0)}
}

means = read.csv("E:/SoilWat/recent_visited_plots_zscore_UPDATE.csv")


CO_data = rbind(CO_01_01,CO_01_02,CO_01_03,
                CO_01_04,CO_01_05,CO_01_06,
                CO_01_07,CO_01_08)
CO_data = CO_data %>%
  mutate(across(everything(), ~ ifelse(is.na(.), 0, .)))
num = CO_data %>% aggregate(n ~ Year,FUN=mean)
temp = CO_data %>% aggregate(avgtemp ~ Year,FUN=mean)
CO_past = left_join(num,temp,join_by(Year==Year))
#CO_current = read.csv("E:/SoilWat/dryness/dry_output/nonSimulated/CO_01_drydaysTemp.csv")
# calculate z-score for all temperatures
CO_means = means %>% filter(site =='CO')
CO_past$ztemp = (CO_past$avgtemp - CO_means$mean_temp)/CO_means$sd_temp
# calculate z-score for all n
CO_past$zn = (CO_past$n - CO_means$mean_n)/CO_means$sd_n
median_values = c(CO_means$median_ztemp,CO_means$median_zn)
CO_past$dist = 0
for (i in 1:1001){
  CO_past[i,6] = CED(median_values,c(CO_past[i,4],CO_past[i,5]))
  #CO_current[i,8] = CED(median_values,c(CO_current[i,6],CO_current[i,7]))
}

#CO_prob = nrow(CO_past %>% filter(dist >= CO_means$max_dist))/1001
CO_prob = nrow(CO_past %>% filter(round(ztemp,6) >= round(CO_means$max_ztemp,6)) %>% filter(round(zn,6) >= round(CO_means$max_zn,6)))/1001

WY_data = rbind(Lowland01,Lowland08,Lowland25,Lowland31,Upland16,Upland34,Upland42,Upland48)
WY_data = WY_data %>%
  mutate(across(everything(), ~ ifelse(is.na(.), 0, .)))
num = WY_data %>% aggregate(n ~ Year,FUN=mean)
temp = WY_data %>% aggregate(avgtemp ~ Year,FUN=mean)
WY_past = left_join(num,temp,join_by(Year==Year))
#CO_current = read.csv("E:/SoilWat/dryness/dry_output/nonSimulated/CO_01_drydaysTemp.csv")
# calculate z-score for all temperatures
WY_means = means %>% filter(site =='WY')
WY_past$ztemp = (WY_past$avgtemp - WY_means$mean_temp)/WY_means$sd_temp
# calculate z-score for all n
WY_past$zn = (WY_past$n - WY_means$mean_n)/WY_means$sd_n
median_values = c(WY_means$median_ztemp,WY_means$median_zn)
WY_past$dist = 0
for (i in 1:1001){
  WY_past[i,6] = CED(median_values,c(WY_past[i,4],WY_past[i,5]))
  #CO_current[i,8] = CED(median_values,c(CO_current[i,6],CO_current[i,7]))
}

#WY_prob = nrow(WY_past %>% filter(dist >= WY_means$max_dist))/1001
WY_prob = nrow(WY_past %>% filter(round(ztemp,6) >= round(WY_means$max_ztemp,6)) %>% filter(round(zn,6) >= round(WY_means$max_zn,6)))/1001



NV1_data = rbind(NV_01_01,NV_01_02,NV_01_03,
                 NV_01_04,NV_01_05,NV_01_06,
                 NV_01_07,NV_01_08)
NV1_data = NV1_data %>%
  mutate(across(everything(), ~ ifelse(is.na(.), 0, .)))
num = NV1_data %>% aggregate(n ~ Year,FUN=mean)
temp = NV1_data %>% aggregate(avgtemp ~ Year,FUN=mean)
NV1_past = left_join(num,temp,join_by(Year==Year))
#CO_current = read.csv("E:/SoilWat/dryness/dry_output/nonSimulated/CO_01_drydaysTemp.csv")
# calculate z-score for all temperatures
NV1_means = means %>% filter(site =='NV1')
NV1_past$ztemp = (NV1_past$avgtemp - NV1_means$mean_temp)/NV1_means$sd_temp
# calculate z-score for all n
NV1_past$zn = (NV1_past$n - NV1_means$mean_n)/NV1_means$sd_n
median_values = c(NV1_means$median_ztemp,NV1_means$median_zn)
NV1_past$dist = 0
for (i in 1:1001){
  NV1_past[i,6] = CED(median_values,c(NV1_past[i,4],NV1_past[i,5]))
  #CO_current[i,8] = CED(median_values,c(CO_current[i,6],CO_current[i,7]))
}

#NV1_prob = nrow(NV1_past %>% filter(dist >= NV1_means$max_dist))/1001
NV1_prob = nrow(NV1_past %>% filter(round(ztemp,6) >= round(NV1_means$max_ztemp,6)) %>% filter(round(zn,6) >= round(NV1_means$max_zn,6)))/1001


NV2_data = rbind(NV_02_01,NV_02_02,NV_02_03,
                 NV_02_04,NV_02_05,NV_02_06,
                 NV_02_07,NV_02_08)
NV2_data = NV2_data %>%
  mutate(across(everything(), ~ ifelse(is.na(.), 0, .)))
num = NV2_data %>% aggregate(n ~ Year,FUN=mean)
temp = NV2_data %>% aggregate(avgtemp ~ Year,FUN=mean)
NV2_past = left_join(num,temp,join_by(Year==Year))
#CO_current = read.csv("E:/SoilWat/dryness/dry_output/nonSimulated/CO_01_drydaysTemp.csv")
# calculate z-score for all temperatures
NV2_means = means %>% filter(site =='NV2')
NV2_past$ztemp = (NV2_past$avgtemp - NV2_means$mean_temp)/NV2_means$sd_temp
# calculate z-score for all n
NV2_past$zn = (NV2_past$n - NV2_means$mean_n)/NV2_means$sd_n
median_values = c(NV2_means$median_ztemp,NV2_means$median_zn)
NV2_past$dist = 0
for (i in 1:1001){
  NV2_past[i,6] = CED(median_values,c(NV2_past[i,4],NV2_past[i,5]))
  #CO_current[i,8] = CED(median_values,c(CO_current[i,6],CO_current[i,7]))
}

#NV2_prob =nrow(NV2_past %>% filter(dist >= NV2_means$max_dist))/1001
NV2_prob = nrow(NV2_past %>% filter(round(ztemp,6) >= round(NV2_means$max_ztemp,6)) %>% filter(round(zn,6) >= round(NV2_means$max_zn,6)))/1001


UT_data = rbind(UT_01_01,UT_01_02,UT_01_03,
                UT_01_04,UT_01_05,UT_01_06,
                UT_01_07,UT_01_08)
UT_data = UT_data %>%
  mutate(across(everything(), ~ ifelse(is.na(.), 0, .)))
num = UT_data %>% aggregate(n ~ Year,FUN=mean)
temp = UT_data %>% aggregate(avgtemp ~ Year,FUN=mean)
UT_past = left_join(num,temp,join_by(Year==Year))
#CO_current = read.csv("E:/SoilWat/dryness/dry_output/nonSimulated/CO_01_drydaysTemp.csv")
# calculate z-score for all temperatures
UT_means = means %>% filter(site =='UT')
UT_past$ztemp = (UT_past$avgtemp - UT_means$mean_temp)/UT_means$sd_temp
# calculate z-score for all n
UT_past$zn = (UT_past$n - UT_means$mean_n)/UT_means$sd_n
median_values = c(UT_means$median_ztemp,UT_means$median_zn)
UT_past$dist = 0
for (i in 1:1001){
  UT_past[i,6] = CED(median_values,c(UT_past[i,4],UT_past[i,5]))
  #CO_current[i,8] = CED(median_values,c(CO_current[i,6],CO_current[i,7]))
}

#UT_prob = nrow(UT_past %>% filter(dist >= UT_means$max_dist))/1001
UT_prob = nrow(UT_past %>% filter(round(ztemp,6) >= round(UT_means$max_ztemp,6)) %>% filter(round(zn,6) < round(UT_means$max_zn,6)))/1001

df = data.frame(site = c('CO','WY','NV1','NV2','UT'),
                historic_probability = c(CO_prob,WY_prob,NV1_prob,NV2_prob,UT_prob),
                historic_occurrences = c(0,0,1,0,2))
write.csv(df,"E:/SoilWat/recent_visited_simulated_probabilities_UPDATE.csv")

