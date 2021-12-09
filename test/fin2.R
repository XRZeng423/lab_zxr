library(dplyr)
library(readr)

setwd("E:/data/fin")

# read the data and rename 
data<-read.csv("scp-1205.csv",F)
data<-data[ , c(1,2,3,4,5,7,8,9,10)]
colnames(data)<-c('countyname', 'state', 'healthplanname' , 'typeofplan',
                  'countyssa' , 'eligibles', 'enrollees', 'penetration' , 
                  'ABrate')

# exclude PuertoRico and Guam
data<-subset(data, state != "GU " & state !="PR ")

# fill na with 0
data[is.na(data)] <- 0

# add enrollee and penetration dummies
data$more_than_10_enrollees<-ifelse(data$enrollees >10, 1, 0)
data$more_than_0.5_penetration<-ifelse(data$penetration > 0.5, 1, 0)

# sum by group 
numberofplans<-aggregate(cbind(more_than_10_enrollees,more_than_0.5_penetration,enrollees)~countyname,data,sum)
colnames(numberofplans)<-c("countyname","numberofplans1","numberofplans2","totalenrollees")

# remove duplicated countyname
data1<-data[!duplicated(data$countyname), c('countyname', 'state','countyssa','eligibles')]

# merge countyname, state, countyssa, eligibles and the group sum
data2<-merge(data1,numberofplans,by="countyname")

# compute the percentage
data2$totalpenetration<-data2$totalenrollees/data2$eligibles

# sort by state and county
data2 <- data2[order(data2$state,data2$countyname),]

# the first two rows might not be normal county, delete them
data2<-data2[3:nrow(data2),]  

write.csv(data2,"new_dataset.csv")
