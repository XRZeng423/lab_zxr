library(foreign)
library(tidyverse)
library(stargazer)
library(fixest)
library(kableExtra)

setwd("E:/data/fin")

data<-read.dta("cps_wages_lfp.dta")
data1<-data[!is.na(data$lfp),]

# 1 Please summarize the key trends for wages and labor force participation

total<-data1
skill<-subset(data1,skilled==1)
unskill<-subset(data1,skilled==0)

# wage 
wage_total<-aggregate(total$wage, by=list(year=total$year),mean,na.rm=TRUE, na.action=NULL)
wage_skill<-aggregate(skill$wage, by=list(year=skill$year),mean,na.rm=TRUE, na.action=NULL)
wage_unskill<-aggregate(unskill$wage, by=list(year=unskill$year),mean,na.rm=TRUE, na.action=NULL)

wage_ave<-cbind(wage_total,wage_skill[2],wage_unskill[2])
colnames(wage_ave)<-c("year","total","skilled","unskilled")

stargazer(wage_ave,
          type = 'text', 
          digits=2, align=T,
          median=T,
          title = "Summary Statistics (Average Wage)",
          keep=c("total","skilled","unskilled"))

#lfp

total$lfp_dummy<- ifelse(total$lfp == "In labor force", 100, 0)
lfp_total<-aggregate(total$lfp_dummy, by=list(year=total$year),mean,na.rm=TRUE, na.action=NULL)

skill$lfp_dummy<- ifelse(skill$lfp == "In labor force", 100, 0)
lfp_skill<-aggregate(skill$lfp_dummy, by=list(year=skill$year),mean,na.rm=TRUE, na.action=NULL)

unskill$lfp_dummy<- ifelse(unskill$lfp == "In labor force", 100, 0)
lfp_unskill<-aggregate(unskill$lfp_dummy, by=list(year=unskill$year),mean,na.rm=TRUE, na.action=NULL)

lfp_percent<-cbind(lfp_total,lfp_skill[2],lfp_unskill[2])
colnames(lfp_percent)<-c("year","total","skilled","unskilled")

stargazer(lfp_percent,
          type = 'text', 
          digits=2, align=T,
          median=T,
          title = "Summary Statistics (Labor Market Participation)",
          keep=c("total","skilled","unskilled"))

# plot
wage_ave %>% 
  ggplot(aes(x =year)) +
  geom_line(aes(y = total,colour="total")) +
  geom_line(aes(y = skilled,colour="skilled")) +
  geom_line(aes(y = unskilled,colour="unskilled")) +
  ylab("Average Wage") +
  xlab("Year") +
  ggtitle("Average Wage over Time") +
  theme_bw() +
  theme(legend.position="right") +
  theme(legend.title=element_blank())

lfp_percent %>% 
  ggplot(aes(x =year)) +
  geom_line(aes(y = total,colour="total")) +
  geom_line(aes(y = skilled,colour="skilled")) +
  geom_line(aes(y = unskilled,colour="unskilled")) +
  ylab("Labor Force Participation") +
  xlab("Year") +
  ggtitle("Labor Force Participation over Time") +
  theme_bw() +
  theme(legend.position="right") +
  theme(legend.title=element_blank())


# 2 Among men older than age 25, which groups of people have had the biggest changes in labor force participation?
data2 <- subset(data1,sex == "male" & age_group != "age < 25" )

#skill
total<-data2
skill<-subset(data2,skilled==1)
unskill<-subset(data2,skilled==0)

# wage 
wage_total<-aggregate(total$wage, by=list(year=total$year),mean,na.rm=TRUE, na.action=NULL)
wage_skill<-aggregate(skill$wage, by=list(year=skill$year),mean,na.rm=TRUE, na.action=NULL)
wage_unskill<-aggregate(unskill$wage, by=list(year=unskill$year),mean,na.rm=TRUE, na.action=NULL)

wage_ave<-cbind(wage_total,wage_skill[2],wage_unskill[2])
colnames(wage_ave)<-c("year","total","skilled","unskilled")


#lfp

total$lfp_dummy<- ifelse(total$lfp == "In labor force", 100, 0)
lfp_total<-aggregate(total$lfp_dummy, by=list(year=total$year),mean,na.rm=TRUE, na.action=NULL)

skill$lfp_dummy<- ifelse(skill$lfp == "In labor force", 100, 0)
lfp_skill<-aggregate(skill$lfp_dummy, by=list(year=skill$year),mean,na.rm=TRUE, na.action=NULL)

unskill$lfp_dummy<- ifelse(unskill$lfp == "In labor force", 100, 0)
lfp_unskill<-aggregate(unskill$lfp_dummy, by=list(year=unskill$year),mean,na.rm=TRUE, na.action=NULL)

lfp_percent<-cbind(lfp_total,lfp_skill[2],lfp_unskill[2])
colnames(lfp_percent)<-c("year","total","skilled","unskilled")

# plot
wage_ave %>% 
  ggplot(aes(x =year)) +
  geom_line(aes(y = total,colour="total")) +
  geom_line(aes(y = skilled,colour="skilled")) +
  geom_line(aes(y = unskilled,colour="unskilled")) +
  ylab("Average Wage") +
  xlab("Year") +
  ggtitle("Average Wage over Time for Male over 25") +
  theme_bw() +
  theme(legend.position="right") +
  theme(legend.title=element_blank())

lfp_percent %>% 
  ggplot(aes(x =year)) +
  geom_line(aes(y = total,colour="total")) +
  geom_line(aes(y = skilled,colour="skilled")) +
  geom_line(aes(y = unskilled,colour="unskilled")) +
  ylab("Labor Force Participation") +
  xlab("Year") +
  ggtitle("Labor Force Participation over Time for Male over 25") +
  theme_bw() +
  theme(legend.position="right") +
  theme(legend.title=element_blank())

round(((lfp_percent[40,c(2:4)]-lfp_percent[1,c(2:4)])/lfp_percent[1,c(2:4)])*100,2)


# group: white

total<-data2
white<-subset(data2,white==1)
unwhite<-subset(data2,white==0)

#lfp

total$lfp_dummy<- ifelse(total$lfp == "In labor force", 100, 0)
lfp_total<-aggregate(total$lfp_dummy, by=list(year=total$year),mean,na.rm=TRUE, na.action=NULL)

white$lfp_dummy<- ifelse(white$lfp == "In labor force", 100, 0)
lfp_white<-aggregate(white$lfp_dummy, by=list(year=white$year),mean,na.rm=TRUE, na.action=NULL)

unwhite$lfp_dummy<- ifelse(unwhite$lfp == "In labor force", 100, 0)
lfp_unwhite<-aggregate(unwhite$lfp_dummy, by=list(year=unwhite$year),mean,na.rm=TRUE, na.action=NULL)

lfp_percent<-cbind(lfp_total,lfp_white[2],lfp_unwhite[2])
colnames(lfp_percent)<-c("year","total","white","not_white")

stargazer(lfp_percent,
          type = 'text', 
          digits=2, align=T,
          median=T,
          title = "Summary Statistics (Labor Market Participation)",
          keep=c("total","white","not_white"))

# plot

lfp_percent %>% 
  ggplot(aes(x =year)) +
  geom_line(aes(y = total,colour="total")) +
  geom_line(aes(y = white,colour="white")) +
  geom_line(aes(y = not_white,colour="not_white")) +
  ylim(50, 90) +
  ylab("Labor Force Participation") +
  xlab("Year") +
  ggtitle("Labor Force Participation over Time") +
  theme_bw() +
  theme(legend.position="right") +
  theme(legend.title=element_blank())

round(((lfp_percent[40,c(2:4)]-lfp_percent[1,c(2:4)])/lfp_percent[1,c(2:4)])*100,2)

# group: hispan
data2$hispan_dummy <- ifelse(data2$hispan == "not hispanic", 0, 1)


total<-data2
hispanic<-subset(data2,hispan_dummy==1)
unhispanic<-subset(data2,hispan_dummy==0)


#lfp

total$lfp_dummy<- ifelse(total$lfp == "In labor force", 100, 0)
lfp_total<-aggregate(total$lfp_dummy, by=list(year=total$year),mean,na.rm=TRUE, na.action=NULL)

hispanic$lfp_dummy<- ifelse(hispanic$lfp == "In labor force", 100, 0)
lfp_hispanic<-aggregate(hispanic$lfp_dummy, by=list(year=hispanic$year),mean,na.rm=TRUE, na.action=NULL)

unhispanic$lfp_dummy<- ifelse(unhispanic$lfp == "In labor force", 100, 0)
lfp_unhispanic<-aggregate(unhispanic$lfp_dummy, by=list(year=unhispanic$year),mean,na.rm=TRUE, na.action=NULL)

lfp_percent<-cbind(lfp_total,lfp_hispanic[2],lfp_unhispanic[2])
colnames(lfp_percent)<-c("year","total","hispanic","not_hispanic")

# plot

lfp_percent %>% 
  ggplot(aes(x =year)) +
  geom_line(aes(y = total,colour="total")) +
  geom_line(aes(y = hispanic,colour="hispanic")) +
  geom_line(aes(y = not_hispanic,colour="not_hispanic")) +
  ylim(50, 90) +
  ylab("Labor Force Participation") +
  xlab("Year") +
  ggtitle("Labor Force Participation over Time") +
  theme_bw() +
  theme(legend.position="right") +
  theme(legend.title=element_blank())

round(((lfp_percent[40,c(2:4)]-lfp_percent[1,c(2:4)])/lfp_percent[1,c(2:4)])*100,2)

# group: age

total<-data2
young<-subset(data2,age_group=="25 <= age < 45")
middle_age<-subset(data2,age_group=="45 <= age < 65")
old<-subset(data2,age_group=="65 <= age")

#lfp

total$lfp_dummy<- ifelse(total$lfp == "In labor force", 100, 0)
lfp_total<-aggregate(total$lfp_dummy, by=list(year=total$year),mean,na.rm=TRUE, na.action=NULL)

young$lfp_dummy<- ifelse(young$lfp == "In labor force", 100, 0)
lfp_young<-aggregate(young$lfp_dummy, by=list(year=young$year),mean,na.rm=TRUE, na.action=NULL)

middle_age$lfp_dummy<- ifelse(middle_age$lfp == "In labor force", 100, 0)
lfp_middle_age<-aggregate(middle_age$lfp_dummy, by=list(year=middle_age$year),mean,na.rm=TRUE, na.action=NULL)

old$lfp_dummy<- ifelse(old$lfp == "In labor force", 100, 0)
lfp_old<-aggregate(old$lfp_dummy, by=list(year=old$year),mean,na.rm=TRUE, na.action=NULL)


lfp_percent<-cbind(lfp_total,lfp_young[2],lfp_middle_age[2],lfp_old[2])
colnames(lfp_percent)<-c("year","total","young","middle_age","old")

# plot

lfp_percent %>% 
  ggplot(aes(x =year)) +
  geom_line(aes(y = total,colour="total")) +
  geom_line(aes(y = young,colour="young")) +
  geom_line(aes(y = middle_age,colour="middle_age")) +
  geom_line(aes(y = old,colour="old")) +
  ylim(10, 100) +
  ylab("Labor Force Participation") +
  xlab("Year") +
  ggtitle("Labor Force Participation over Time") +
  theme_bw() +
  theme(legend.position="right") +
  theme(legend.title=element_blank())

round(((lfp_percent[40,c(2:5)]-lfp_percent[1,c(2:5)])/lfp_percent[1,c(2:5)])*100,2)


# 3


data3 <- data1 %>% 
  group_by(year) %>% 
  mutate(avgWage = mean(wage, na.rm = TRUE)) %>% #taking average wage for each year %>% 
  filter(lfp != "") %>% 
  mutate(lfpDummy = ifelse(lfp == "In labor force", 100, 0)) %>% 
  mutate(lfpPercent = mean(lfpDummy, na.rm = TRUE))

data3$growWage<-c(NA,diff(data3$avgWage))

fit0 = feols(lfpPercent~year,  data3)

fit1 = feols(lfpPercent~avgWage,  data3)

fit2 = feols(lfpPercent~growWage,  data3)

fit3 = feols(lfpPercent~skilled,  data3)

fit4 = feols(lfpPercent~white,  data3)

fit5 = feols(lfpPercent~age_group,  data3)


x<-etable(fit0,fit1 , fit2,fit3, fit4,fit5,cluster = "year")
kable(x, "html") %>%
  kable_styling(full_width = F)
