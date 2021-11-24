library(readr)
library(dplyr)        
library(tidyr)        
library(reshape2)
library(ggpubr)
library(lubridate)
library(ggplot2)
library(grid)
library(gridExtra)
library(knitr)
library(plyr)
library(psych)
library(data.table)
library(xts)
library(tidyverse)

# import and preprocess data
bond<-read.csv("E:\\data\\SIEPR\\imm_bond_prices.csv")
class<-read.csv("E:\\data\\SIEPR\\ssd_def_class.csv")
dates<-read.csv("E:\\data\\SIEPR\\ssd_def_dates.csv")

bond[,c("principal","p_open","p_last","p_high","p_low","annual_coupon")]<-as.numeric(unlist(bond[,c("principal","p_open","p_last","p_high","p_low","annual_coupon")]))
#describe(bond)
#describe(class)
#describe(dates)

# 1.1 merge data
class$country<-tolower(class$country)
merge1 <- left_join(dates, class, by = c("country"))
merge2 <- merge(bond, merge1, by = c("country","year"))

#describe(merge1)
#describe(merge2)

# 1.2 describe the success of merge
table_list<-list(class,dates,merge1,bond,merge2)

nrow<-NA
num_country<-NA
num_year<-NA
for (i in 1:5){
  table<-as.data.frame(table_list[i])
  nrow[i]<-nrow(table)
  num_country[i]<-length(unique(table$country))
  num_year[i]<-length(unique(table$year))
}
merge_table<-cbind(nrow,num_country, num_year )
rownames(merge_table) = c("class","dates","merge1","bond","merge2")

# 1.3 fill na in p_last
merge2 = merge2[order(merge2[3],merge2[2],merge2[4]),]
merge3<-data.table(merge2)
merge4<-merge3 %>%
  group_by(id) %>%
  mutate(next_p_open = dplyr::lead(as.vector(p_open), order_by=id))

merge4$p_last <- ifelse(is.na(merge4$p_last), merge4$next_p_open, merge4$p_last)
#write.csv(merge4,"E:\\data\\SIEPR\\p.csv")

# 2.1 Create a table describing how bonds that ever default differ from bonds that never default
p<-read.csv("E:\\data\\SIEPR\\p.csv")
p<-p[,-1]
data21<-p[,c('year','id','month','new_ep','principal','p_open','p_high','p_low','p_last','annual_coupon')]

sum<-aggregate(data21$new_ep, by=list(bond=data21$id),sum)
never_default_bond<-unlist(subset(sum, x==0))
default_bond<-unlist(subset(sum, x!=0))

describe(subset(data21, id %in% never_default_bond))
describe(subset(data21, id %in% default_bond))

data21$label<-NA
data21[which(data21$id %in% never_default_bond),'label']<-0
data21[which(data21$id%in%default_bond),'label']<-1
head(data21)


# ANOVA test & visualization

var_list<-c("principal","p_open","p_high","p_low","p_last","annual_coupon")
aov <- c(paste0('aov',1:6))
plot<-c(paste0('p',1:6))
for (i in 1:6){
  #ANOVA
  var<-var_list[i]
  aov1 <- aov(data21[[var]]~label, data21)     
  q<-summary(aov1)
  assign(aov[i],q)
  
  #visualization
  a<-ggboxplot(data21, x = "label", y = var, 
               color = "label", palette = "jco",
               order = c("0", "1"),
               ylab = var, xlab = "default")
  assign(plot[i],a)
}
aov1
aov2
aov3
aov4
aov5
aov6

ggarrange(p1,p2,p3,p4,p5,p6, 
          ncol = 3, nrow = 2, align = "v")


# 2.2 Create a table describing how bonds from countries with different defaulter types differ.
data22<-p[,c('year','id','month','defclass','principal','p_open','p_high','p_low','p_last','annual_coupon')]

Perpetual_bond<-subset(data22, defclass=="Perpetual")
Minor_bond<-subset(data22, defclass=="Minor")
Serial_bond<-subset(data22, defclass=="Serial")

describe(Perpetual_bond)
describe(Minor_bond)
describe(Serial_bond)



aov <- c(paste0('aov',1:6))
plot<-c(paste0('p',1:6))

for (i in 1:6){
  #ANOVA
  var<-var_list[i]
  aov1 <- aov(data22[[var]]~defclass, data22)     
  q<-summary(aov1)
  assign(aov[i],q)
  
  #visualization
  a<-ggboxplot(data22, x = "defclass", y = var,  
               color = "defclass", palette = "jco",
               order = c("Perpetual", "Minor", "Serial"),
               ylab = var, xlab = "defclass")

  
  assign(plot[i],a)
}
aov1
aov2
aov3
aov4
aov5
aov6

ggarrange(p1,p2,p3,p4,p5,p6, 
          ncol = 3, nrow = 2, align = "v")

# 2.3 Create a time series plot describing the evolution of sovereign bond prices over time for the different defaulter types

bond_list<-list(Perpetual_bond,Minor_bond,Serial_bond)
plot <- c(paste0('p',1:3))
for (i in 1:3){
  Perpetual_bond<-as.data.frame(bond_list[i])
  Perpetual_bond$date<-as.Date(unlist(paste(unlist(Perpetual_bond$year),unlist(Perpetual_bond$month),"1",sep="-")),"%Y-%m-%d")
  
  # plot all prices time series for one type
  dat <- c(paste0('dat',1:4))
  plots<-c(paste0('plots',1:4))
  var_list<-c("p_high","p_open","p_low","p_last")
  for (j in 1:4){
    var<-var_list[j]
    agg<-aggregate(Perpetual_bond[[var]], by=list(date=Perpetual_bond$date),mean)
    assign(dat[j],agg)
    a<-ggplot(data = na.omit(agg), aes(x=date, y=x)) +  
      geom_line()+
      theme_bw()+
      ggtitle(var)+ylab("price")
    assign(plots[j],a)

  }
  
  p<-ggarrange(plots1, plots2,plots3,plots4, 
               ncol = 1, nrow = 4, align = "v")
  
  # loop for all types
  assign(plot[i],p)
}

ggarrange(p1, p2,p3, 
          ncol = 3, nrow = 1, align = "h")

# 3
# First paste the country name and ep_id to create a event variable
p<-read.csv("E:\\data\\SIEPR\\p.csv")
p<-p[,-1]
event<-subset(p, new_ep==1)
event$event<-paste(unlist(event$country),unlist(event$ep_id),sep="-")
event<-as.data.frame(unique(event[,c('year','event')]))
event<-event[order(event['event'],event['year']),]
event$event_label<-seq(1,nrow(event),1)
# Then link year and event, the dataset contains 58 events as followed
head(event)

# Loop over events. For each event, grab the info within two years of the default
# For each event year, find the window years (event year+-2).
# Expand the event-year dataset by replicating 5 times each row. Link each event year with five window years
# Merge the event-year dataframe with the raw dataframe


event.expanded <- event[rep(1:nrow(event),each=5),]

window_years<-NA
for (i in 1:length(unique(event$event))){
  event_year<-event[i,'year']
  window_years[seq((5*i-4),5*i,1)]<-seq((event_year-2),(event_year+2),1)
}

event.expanded$window_years<-window_years

colnames(p)[2]<-"window_years"

test<-merge(event.expanded,p,by="window_years",all=TRUE) #963302 obs
head(test)
data3 = test[order(test[4],test[2],test[1]),]
data3$tte<-data3$window_years-data3$year
data3$post<-NA
data3[which(data3$tte < 0),'post']<-0
data3[which(data3$tte >= 0),'post']<-1
data31<-data3[,c("window_years","year","tte","post","event","event_label","country","id","month","p_last","defclass")]
colnames(data31)[2]<-"event_year"
colnames(data31)[1]<-"year"


# delete rows with missing values in prices
data32<-na.omit(data31)#588363 obs


# create defaulter variale, if event_country == country, then defaulter==1
data32$event_country<-str_sub(data32$event,1,nchar(data32$event)-2)
data32$defaulter<-NA
data32[which(data32$country==data32$event_country),'defaulter']<-1
data32[which(data32$country!=data32$event_country),'defaulter']<-0

# See the code for regression in .do file
