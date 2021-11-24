library(lubridate)
library(ggplot2)
library(tidyr)
library(knitr)
library(plyr)
#options("scipen"=100, "digits"=4)

# Data Construction

data<-read.csv("E:\\data\\micro\\RA_21_22.csv")
data$total_wealth<-data$asset_total-data$debt_total
data$housing_wealth<-data$asset_housing-data$debt_housing

library(spatstat)
library("dplyr") 


race_total_wealth<-data %>% 
  group_by(race,year) %>% 
  summarise(weighted.median(total_wealth, weight, na.rm = TRUE))
colnames(race_total_wealth)[3]<-"total_wealth"
race_total_wealth
ggplot(data = race_total_wealth, mapping = aes(x = year, y = total_wealth, colour = race)) + 
  geom_line(size=1)+
  scale_color_manual(values = c("#0072BD", "#D95319", "#EDB120",'darkred'))+ 
  theme_bw()+ 
  theme(legend.position = "bottom") 

#write.csv(race_total_wealth,"C:\\Users\\lenovo\\Desktop\\micro\\race_total_wealth.csv")

education_total_wealth<-data %>% 
  group_by(education,year) %>% 
  summarise(weighted.median(total_wealth, weight, na.rm = TRUE))
colnames(education_total_wealth)[3]<-"total_wealth"
ggplot(data = education_total_wealth, mapping = aes(x = year, y = total_wealth, colour = education)) + 
  geom_line(size=1)+
  scale_color_manual(values = c("#0072BD", "#D95319", "#EDB120",'darkred'))+ 
  theme_bw()+ 
  theme(legend.position = "bottom") 
#write.csv(education_total_wealth,"C:\\Users\\lenovo\\Desktop\\micro\\education_total_wealth.csv")

data2<-subset(data,race=='black'|race=='white')
race_housing_wealth<-data2 %>% 
  group_by(race,year) %>% 
  summarise(weighted.median(housing_wealth, weight, na.rm = TRUE))
colnames(race_housing_wealth)[3]<-"housing_wealth"
race_housing_wealth
ggplot(data = race_housing_wealth, mapping = aes(x = year, y = housing_wealth, colour = race)) + 
  geom_line(size=1)+
  scale_color_manual(values = c("#0072BD", "#D95319", "#EDB120",'darkred'))+ 
  theme_bw()+ 
  theme(legend.position = "bottom") 
#write.csv(race_housing_wealth,"C:\\Users\\lenovo\\Desktop\\micro\\race_housing_wealth.csv")


data3<-subset(data2,age>=25)
race_housing_wealth2<-data3 %>% 
  group_by(race,year) %>% 
  summarise(weighted.median(housing_wealth, weight, na.rm = TRUE))
colnames(race_housing_wealth2)[3]<-"housing_wealth"
race_housing_wealth2
ggplot(data = race_housing_wealth2, mapping = aes(x = year, y = housing_wealth, colour = race)) + 
  geom_line(size=1)+
  scale_color_manual(values = c("#0072BD", "#D95319", "#EDB120",'darkred'))+ 
  theme_bw()+ 
  theme(legend.position = "bottom") 
#write.csv(race_housing_wealth2,"C:\\Users\\lenovo\\Desktop\\micro\\race_housing_wealth2.csv")

race_income<-data3 %>% 
  group_by(race,year) %>% 
  summarise(weighted.median(income, weight, na.rm = TRUE))
colnames(race_income)[3]<-"income"
race_income
ggplot(data = race_income, mapping = aes(x = year, y = income, colour = race)) + 
  geom_line(size=1)+
  scale_color_manual(values = c("#0072BD", "#D95319", "#EDB120",'darkred'))+ 
  theme_bw()+ 
  theme(legend.position = "bottom") 
#write.csv(race_income,"C:\\Users\\lenovo\\Desktop\\micro\\race_income.csv")


data3$non_housing_wealth<-data3$total_wealth-data3$housing_wealth
race_non_housing_wealth<-data3 %>% 
  group_by(race,year) %>% 
  summarise(weighted.median(non_housing_wealth, weight, na.rm = TRUE))
colnames(race_non_housing_wealth)[3]<-"non_housing_wealth"
race_non_housing_wealth
ggplot(data = race_non_housing_wealth, mapping = aes(x = year, y = non_housing_wealth, colour = race)) + 
  geom_line(size=1)+
  scale_color_manual(values = c("#0072BD", "#D95319", "#EDB120",'darkred'))+ 
  theme_bw()+ 
  theme(legend.position = "bottom") 
#write.csv(race_non_housing_wealth,"C:\\Users\\lenovo\\Desktop\\micro\\race_non_housing_wealth.csv")



data4<-data3
data4$proportion<-data3$housing_wealth/data3$total_wealth
data4[is.na(data4)]<-0
  
# ?????? ??????
race_housing_wealth3<-data4 %>% 
  group_by(race,year) %>% 
  summarise(weighted.median(proportion, weight, na.rm = TRUE))
colnames(race_housing_wealth3)[3]<-"housing_wealth_proportion"
race_housing_wealth3
ggplot(data = race_housing_wealth3, mapping = aes(x = year, y = housing_wealth_proportion, colour = race)) + 
  geom_line(size=1)+
  scale_color_manual(values = c("#0072BD", "#D95319", "#EDB120",'darkred'))+ 
  theme_bw()
#write.csv(race_housing_wealth3,"C:\\Users\\lenovo\\Desktop\\micro\\race_housing_wealth3.csv")

subset(race_housing_wealth3,race=="black"&year=="2010")[3]-subset(race_housing_wealth3,race=="black"&year=="2007")[3]
subset(race_housing_wealth3,race=="white"&year=="2010")[3]-subset(race_housing_wealth3,race=="white"&year=="2007")[3]

#Which group had the largest loss in housing wealth, where 2007 is defined as the base period? Please answer this question both in dollar terms and in proportional terms
# https://www.stlouisfed.org/publications/regional-economist/july-2012/household-financial-stability--who-suffered-the-most-from-the-crisis

race_housing_wealth4<-data3 %>% 
  group_by(education,race,year) %>% 
  summarise(weighted.median(housing_wealth, weight, na.rm = TRUE))
colnames(race_housing_wealth4)[4]<-"housing_wealth"

a<-NA
for (i in 1:nrow(race_housing_wealth4)){
  a[i]<-paste(race_housing_wealth4[i,1],race_housing_wealth4[i,2])
}
race_housing_wealth4$type<-a

race_housing_wealth5<-race_housing_wealth4[,c(3,4,5)]
ggplot(data = race_housing_wealth5, mapping = aes(x = year, y = housing_wealth, colour = type)) + 
  geom_line(size=1)+
  scale_color_manual(values = c("#0072BD", "#D95319", "#EDB120",'darkred','grey','blue'))+ 
  theme_bw()+ 
  theme(legend.position = "bottom") 
#write.csv(race_housing_wealth4,"C:\\Users\\lenovo\\Desktop\\micro\\race_housing_wealth4.csv")


#verification
data1<-data[,c(1,2,6,13)]
a<-subset(data1,year=='1992')
b<-subset(a,race=='black')
weighted.median(b$housing_wealth,b$weight)

library("plotrix")
weighted.hist(x=b$housing_wealth,w=b$weight,freq=F,col=rainbow(11))


