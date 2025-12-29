library(dplyr)


CED = function(vect1,vect2) {
  if (vect2[1]>=vect1[1] & vect2[2]>=vect1[2]){
  return(sqrt(sum((vect2 - vect1)^2)))}
  #return(sum((vect2 - vect1)))
  else{return(0)}
}
#CED = function(vect1,vect2) {
#    sqrt(sum((vect2 - vect1)^2))}
  #return(sum((vect2 - vect1)))
  
#################################
# everything needs to be redone with the file path used in CO_data below!!!
## not the files above, then redo calculations for past,present, future

CO_data = read.csv("E:/SoilWat/dryness/dry_output/nonSimulated/CO_01_drydaysTemp.csv")
num = CO_data %>% aggregate(n ~ Year,FUN=median)
temp = CO_data %>% aggregate(avgtemp ~ Year,FUN=median)
CO_current = left_join(num,temp,join_by(Year==Year))
#CO_current = read.csv("E:/SoilWat/dryness/dry_output/nonSimulated/CO_01_drydaysTemp.csv")
# calculate z-score for all temperatures
CO_current$ztemp = (CO_current$avgtemp - mean(CO_current$avgtemp))/sd(CO_current$avgtemp)
# calculate z-score for all n
CO_current$zn = (CO_current$n - mean(CO_current$n))/sd(CO_current$n)
median_values = c(median(CO_current$ztemp),median(CO_current$zn))
CO_current$dist = 0
for (i in 1:43){
  CO_current[i,6] = CED(median_values,c(CO_current[i,4],CO_current[i,5]))
  #CO_current[i,8] = CED(median_values,c(CO_current[i,6],CO_current[i,7]))
}



NV1_data = read.csv("E:/SoilWat/dryness/dry_output/nonSimulated/NV_01_drydaysTemp.csv")
num = NV1_data %>% aggregate(n ~ Year,FUN=median)
temp = NV1_data %>% aggregate(avgtemp ~ Year,FUN=median)
NV1_current = left_join(num,temp,join_by(Year==Year))
#NV1_current = read.csv("E:/SoilWat/dryness/dry_output/nonSimulated/NV_01_drydaysTemp.csv")
NV1_current$ztemp = (NV1_current$avgtemp - mean(NV1_current$avgtemp))/sd(NV1_current$avgtemp)
NV1_current$zn = (NV1_current$n - mean(NV1_current$n))/sd(NV1_current$n)
median_values = c(median(NV1_current$ztemp),median(NV1_current$zn))
NV1_current$dist = 0
for (i in 1:43){
  #NV1_current[i,8] = CED(median_values,c(NV1_current[i,6],NV1_current[i,7]))
  NV1_current[i,6] = CED(median_values,c(NV1_current[i,4],NV1_current[i,5]))
}


NV2_data = read.csv("E:/SoilWat/dryness/dry_output/nonSimulated/NV_02_drydaysTemp.csv")
num = NV2_data %>% aggregate(n ~ Year,FUN=median)
temp = NV2_data %>% aggregate(avgtemp ~ Year,FUN=median)
NV2_current = left_join(num,temp,join_by(Year==Year))
#NV2_current = read.csv("E:/SoilWat/dryness/dry_output/nonSimulated/NV_02_drydaysTemp.csv")
NV2_current$ztemp = (NV2_current$avgtemp - mean(NV2_current$avgtemp))/sd(NV2_current$avgtemp)
NV2_current$zn = (NV2_current$n - mean(NV2_current$n))/sd(NV2_current$n)
median_values = c(median(NV2_current$ztemp),median(NV2_current$zn))
NV2_current$dist = 0
for (i in 1:43){
  #NV2_current[i,8] = CED(median_values,c(NV2_current[i,6],NV2_current[i,7]))
  NV2_current[i,6] = CED(median_values,c(NV2_current[i,4],NV2_current[i,5]))
}


UT_data = read.csv("E:/SoilWat/dryness/dry_output/nonSimulated/UT_01_drydaysTemp.csv")
num = UT_data %>% aggregate(n ~ Year,FUN=median)
temp = UT_data %>% aggregate(avgtemp ~ Year,FUN=median)
UT_current = left_join(num,temp,join_by(Year==Year))
#UT_current = read.csv("E:/SoilWat/dryness/dry_output/nonSimulated/UT_01_drydaysTemp.csv")
UT_current$ztemp = (UT_current$avgtemp - mean(UT_current$avgtemp))/sd(UT_current$avgtemp)
UT_current$zn = (UT_current$n - mean(UT_current$n))/sd(UT_current$n)
median_values = c(median(UT_current$ztemp),median(UT_current$zn))
UT_current$dist = 0
for (i in 1:43){
  #UT_current[i,8] = CED(median_values,c(UT_current[i,6],UT_current[i,7]))
  UT_current[i,6] = CED(median_values,c(UT_current[i,4],UT_current[i,5]))
}


WY_data = read.csv("E:/SoilWat/dryness/dry_output/nonSimulated/UGRB_drydaysTemp.csv")
num = WY_data %>% aggregate(n ~ Year,FUN=median)
temp = WY_data %>% aggregate(avgtemp ~ Year,FUN=median)
WY_current = left_join(num,temp,join_by(Year==Year))
#WY_current = read.csv("E:/SoilWat/dryness/dry_output/nonSimulated/UGRB_drydaysTemp.csv")
WY_current$ztemp = (WY_current$avgtemp - mean(WY_current$avgtemp))/sd(WY_current$avgtemp)
WY_current$zn = (WY_current$n - mean(WY_current$n))/sd(WY_current$n)
median_values = c(median(WY_current$ztemp),median(WY_current$zn))
WY_current$dist = 0
for (i in 1:43){
  #WY_current[i,8] = CED(median_values,c(WY_current[i,6],WY_current[i,7]))
  WY_current[i,6] = CED(median_values,c(WY_current[i,4],WY_current[i,5]))
}

mean(c(max(CO_current$dist),max(NV1_current$dist),max(NV2_current$dist),max(WY_current$dist),max(UT_current$dist)))
CO_max = CO_current %>% filter(dist== max(CO_current$dist))
WY_max = WY_current %>% filter(dist== max(WY_current$dist))
NV1_max = NV1_current %>% filter(dist== max(NV1_current$dist))
UT_max = UT_current %>% filter(dist== max(UT_current$dist))
NV2_max = NV2_current %>% filter(dist== max(NV2_current$dist))

CO_max = CO_current %>% filter(Year== 2018)
WY_max = WY_current %>% filter(Year==2012)
NV1_max = NV1_current %>% filter(Year==2021)
UT_max = UT_current %>% filter(Year==2020)
NV2_max = NV2_current %>% filter(Year==2012)

current_means = data.frame(site = c('CO','WY','NV1','UT','NV2'),
                           mean_temp = c(mean(CO_current$avgtemp),mean(WY_current$avgtemp),mean(NV1_current$avgtemp),mean(UT_current$avgtemp),mean(NV2_current$avgtemp)),
                           mean_n = c(mean(CO_current$n),mean(WY_current$n),mean(NV1_current$n),mean(UT_current$n),mean(NV2_current$n)),
                           sd_temp = c(sd(CO_current$avgtemp),sd(WY_current$avgtemp),sd(NV1_current$avgtemp),sd(UT_current$avgtemp),sd(NV2_current$avgtemp)),
                           sd_n = c(sd(CO_current$n),sd(WY_current$n),sd(NV1_current$n),sd(UT_current$n),sd(NV2_current$n)),
                           median_ztemp = c(median(CO_current$ztemp),median(WY_current$ztemp),median(NV1_current$ztemp),median(UT_current$ztemp),median(NV2_current$ztemp)),
                           median_zn = c(median(CO_current$zn),median(WY_current$zn),median(NV1_current$zn),median(UT_current$zn),median(NV2_current$zn)),
                           max_dist = c(CO_max$dist,WY_max$dist,NV1_max$dist,UT_max$dist,NV2_max$dist),
                           max_ztemp = c(CO_max$ztemp,WY_max$ztemp,NV1_max$ztemp,UT_max$ztemp,NV2_max$ztemp),
                           max_zn = c(CO_max$zn,WY_max$zn,NV1_max$zn,UT_max$zn,NV2_max$zn))

write.csv(current_means,"E:/SoilWat/recent_visited_plots_zscore.csv")

library(ggplot2)
library(ggpubr)
### need to standardize axes
# how to layout for 5??
## add mahalanobis distance
centroid <- c(median(NV2_current$ztemp),median(NV2_current$zn))
# Create a grid for contour interpolation
grid <- expand.grid(
  x = seq(min(NV2_current$ztemp), max(NV2_current$ztemp), length.out = 100),
  y = seq(min(NV2_current$zn), max(NV2_current$zn), length.out = 100)
)

# Interpolate distances over the grid
grid$distance <- with(grid,sqrt((x-centroid[1])^2+(y-centroid[2])^2))

p1= ggplot(NV2_current, aes(x = ztemp, y = zn)) +
  geom_text(aes(label = Year), size = 4) +
  geom_text(data = subset(NV2_current, Year == 2012), aes(label = "2012"), color = "red", size = 4) +
  #xlim(-3,3)+
  #ylim(-1,2)+
  geom_contour(data=grid%>%filter(distance<3),aes(x=x,y=y,z=distance),colour='black',
               binwidth=1,linetype=2)+
  labs(title = "NV 02",y = 'Number of Dry Days Z-score',x = 'Annual Average Temperature Z-score')+
  theme_classic()+
  theme(
    text = element_text(size = 13, family = "sans"),
    plot.title = element_text(size = 20, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12))
###################################################
centroid <- c(median(NV1_current$ztemp),median(NV1_current$zn))
# Create a grid for contour interpolation
grid <- expand.grid(
  x = seq(min(NV1_current$ztemp), max(NV1_current$ztemp), length.out = 100),
  y = seq(min(NV1_current$zn), max(NV1_current$zn), length.out = 100)
)
grid$distance <- with(grid,sqrt((x-centroid[1])^2+(y-centroid[2])^2))


p2= ggplot(NV1_current, aes(x = ztemp, y = zn)) +
  geom_text(aes(label = Year), size = 4) +
  geom_text(data = subset(NV1_current, Year == 2021), aes(label = "2021"), color = "red", size = 4) +
  #xlim(-3,3)+
  #ylim(-1,2)+
  geom_contour(data=grid%>%filter(distance<3),aes(x=x,y=y,z=distance),colour='black',
               binwidth=1,linetype=2)+
  labs(title = "NV 01",y = 'Number of Dry Days Z-score',x = 'Annual Average Temperature Z-score')+
  theme_classic()+
  theme(
    text = element_text(size = 13, family = "sans"),
    plot.title = element_text(size = 20, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12))
###################################################
centroid <- c(median(UT_current$ztemp),median(UT_current$zn))
# Create a grid for contour interpolation
grid <- expand.grid(
  x = seq(min(UT_current$ztemp), max(UT_current$ztemp), length.out = 100),
  y = seq(min(UT_current$zn), max(UT_current$zn), length.out = 100)
)
grid$distance <- with(grid,sqrt((x-centroid[1])^2+(y-centroid[2])^2))

p3= ggplot(UT_current, aes(x = ztemp, y = zn)) +
  geom_text(aes(label = Year), size = 4) +
  geom_text(data = subset(UT_current, Year == 2020), aes(label = "2020"), color = "red", size = 4) +
  #xlim(-3,3)+
  #ylim(-1,2)+
  geom_contour(data=grid%>%filter(distance<3),aes(x=x,y=y,z=distance),colour='black',
               binwidth=1,linetype=2)+
  labs(title = "UT 01",y = 'Number of Dry Days Z-score',x = 'Annual Average Temperature Z-score')+
  theme_classic()+
  theme(
    text = element_text(size = 13, family = "sans"),
    plot.title = element_text(size = 20, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12))
################################################
centroid <- c(median(CO_current$ztemp),median(CO_current$zn))
# Create a grid for contour interpolation
grid <- expand.grid(
  x = seq(min(CO_current$ztemp), max(CO_current$ztemp), length.out = 100),
  y = seq(min(CO_current$zn), max(CO_current$zn), length.out = 100)
)
grid$distance <- with(grid,sqrt((x-centroid[1])^2+(y-centroid[2])^2))

p4= ggplot(CO_current, aes(x = ztemp, y = zn)) +
  geom_text(aes(label = Year), size = 4) +
  geom_text(data = subset(CO_current, Year == 2018), aes(label = "2018"), color = "red", size = 4) +
  #xlim(-3,3)+
  #ylim(-1,2)+
  geom_contour(data=grid%>%filter(distance<4),aes(x=x,y=y,z=distance),colour='black',
               binwidth=1,linetype=2)+
  labs(title = "CO 01",y = 'Number of Dry Days Z-score',x = 'Annual Average Temperature Z-score')+
  theme_classic()+
  theme(
    text = element_text(size = 13, family = "sans"),
    plot.title = element_text(size = 20, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12))

################################################
centroid <- c(median(WY_current$ztemp),median(WY_current$zn))
# Create a grid for contour interpolation
grid <- expand.grid(
  x = seq(min(WY_current$ztemp), max(WY_current$ztemp), length.out = 100),
  y = seq(min(WY_current$zn), max(WY_current$zn), length.out = 100)
)
grid$distance <- with(grid,sqrt((x-centroid[1])^2+(y-centroid[2])^2))

p5= ggplot(WY_current, aes(x = ztemp, y = zn)) +
  geom_text(aes(label = Year), size = 4) +
  geom_text(data = subset(WY_current, Year == 2012), aes(label = "2012"), color = "red", size = 4) +
  geom_contour(data=grid%>%filter(distance<3),aes(x=x,y=y,z=distance),colour='black',
               binwidth=1,linetype=2)+
  labs(title = "WY 01",y = 'Number of Dry Days Z-score',x = 'Annual Average Temperature Z-score')+
  theme_classic()+
  theme(
    text = element_text(size = 13, family = "sans"),
    plot.title = element_text(size = 20, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12))

library(ggpubr)

figure <- ggarrange(p2,p1,p3,p4,p5,
                    ncol = 2, nrow = 3)
# export with hor - 850, vert - 1100

