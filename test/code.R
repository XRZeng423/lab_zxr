library(readr)
library(tidyverse)
library(stargazer)
library(fixest)

setwd("E:/data/is")

#1-a
revenue <- read.fwf("Revenue.txt",header=F, sep = "", dec = ".")

revenue <-read.csv("Revenue.csv",header=F)

colnames(revenue)<-c('logrev','rest_id','time')
revenue<-mutate_all(revenue, function(x) as.numeric(as.character(x)))

#1-b
filelist<-paste("month",formatC(seq(01:10), width =2, flag = 0),sep = "")
pathlist<-paste(filelist,"csv",sep = '.')

read <- lapply(pathlist , read.csv)
star <- do.call("rbind", read)
star<-mutate_all(star, function(x) as.numeric(as.character(x)))

#1-c
dat<-merge(star,revenue, by=c("rest_id","time"),all.x=TRUE)
dim(dat)


#2-a
for (i in sort(unique(dat$stars))){
  stargazer(subset(dat,stars==i),
            type = 'text', 
            digits=2, align=T,
            title = paste("Summary Statistics: stars",i),
            keep = 'logrev')
}

#2-b

ggplot(dat, aes(x = score, y = logrev,colour=as.character(stars))) +
  geom_point(size = 0.5, alpha = 0.5) + 
  geom_smooth(data = filter(dat, score <= 0.5 | score>0), method = "lm") +
  geom_smooth(data = filter(dat, score <= 1 | score>0.5), method = "lm") +
  geom_smooth(data = filter(dat, score <= 1.5 | score>1), method = "lm") +
  geom_smooth(data = filter(dat, score <= 2.5 | score>2), method = "lm") +
  geom_smooth(data = filter(dat, score <= 3 | score>2.5), method = "lm") +
  geom_smooth(data = filter(dat, score <= 3.5 | score>3), method = "lm") +
  geom_smooth(data = filter(dat, score <= 4 | score>3.5), method = "lm") +
  geom_smooth(data = filter(dat, score <= 4.5 | score>4), method = "lm") +
  geom_smooth(data = filter(dat, score <= 5 |score > 4.5), method = "lm") +
  geom_vline(xintercept =sort(unique(dat$stars)) ) +
  labs(x = "True Yelp Score", y = "Store Revenue (log)",fill = "Stars") 


#2-c

#no

ggplot(dat, aes(x = score, fill = as.character(stars))) +
  geom_histogram(binwidth = 0.25, color = "white", boundary = 1)+
  geom_vline(xintercept =sort(unique(dat$stars)) )
 
#3-a
fit1<-lm(logrev~score,dat)
stargazer(fit1, type="text")      

#3-b
fit2<-lm(logrev~stars,dat)
stargazer(fit1,fit2, type="text")    

#3-c
dat$score2<-dat$stars-dat$score
dat$sign<-ifelse(dat$score2 >= 0, 1, -1)

ggplot(dat, aes(x = score2, y = logrev, colour=as.character(sign))) +
  geom_point(size = 0.5, alpha = 0.5)+
  # Add vertical line
  geom_vline(xintercept =0 ) + 
  geom_smooth(data = filter(dat, score2 <= 0), method = "lm") +
  geom_smooth(data = filter(dat, score2 > 0), method = "lm") +
  # Add labels
  labs(x = "Stars-Score", y = "Store Revenue (log)",fill = "Stars") 

#3-d
fit3 = feols(logrev~stars,  dat)

fit4 = feols(logrev~stars | time, dat)

fit5 = feols(logrev~stars | time+rest_id, dat)

etable(fit3, fit4, fit5, 
       cluster = "rest_id", headers = c("without","time", "time+rest_id"))
