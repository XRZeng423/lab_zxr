library(readr)
library(lubridate)

library(xts)
library(PerformanceAnalytics)
library(lubridate)
library(quantmod)
library(dplyr)



source("C:/Users/carol/Desktop/asset_allocation/cal_ret.R")

setwd("C:\\Users\\carol\\Desktop\\factor")
trading_data <- read_delim("data/trading data.txt", 
                           "\t", escape_double = FALSE, col_types = cols(Trdmnt = col_date(format = "%Y/%m/%d")), 
                           trim_ws = TRUE)
trading_data <- arrange(trading_data,trading_data$Stkcd,trading_data$Trddt)

#View(trading_data)
##tradind data的收益率已经是月底的收盘价除以月初开盘价算出了的。而披露的日期设置在了1号，所以月收益不需要错开一个月份。
##计算每个月月初建仓对应的一、二、三个月收益

trading_data$Mclprc1m <-c(trading_data$Mclsprc[-(1:1)],NA)
trading_data$Mclprc2m <-c(trading_data$Mclsprc[-(1:2)],NA,NA)
trading_data$Mclprc3m <-c(trading_data$Mclsprc[-(1:3)],NA,NA,NA)

trading_data$Mretwd1m <- trading_data$Mclprc1m / trading_data$Mclsprc-1
trading_data$Mretwd2m <- trading_data$Mclprc2m / trading_data$Mclsprc-1
trading_data$Mretwd3m <- trading_data$Mclprc3m / trading_data$Mclsprc-1
##一个是三月后收盘价，一个是收益率
pricing_factor_data <- read_delim("data/pricing factor data.txt", 
                                  "\t", escape_double = FALSE, col_types = cols(Accper = col_date(format = "%Y/%m/%d")), 
                                  trim_ws = TRUE)
pricing_factor_data$Accper <- as.Date(pricing_factor_data$Accper)+ 1
pricing_factor_data <- arrange(pricing_factor_data,pricing_factor_data$Stkcd,pricing_factor_data$Accper)

meiguzhibiao <- read_delim("data/meiguzhibiao.txt", 
                           "\t", escape_double = FALSE, col_types = cols(Accper = col_date(format = "%Y/%m/%d")), 
                           trim_ws = TRUE)
meiguzhibiao$Accper <- as.Date(meiguzhibiao$Accper)+ 1

#计算价值因子资产收益率（季度，要转为月度）

trading_months <- sort(unique(as.Date(trading_data$Trdmnt)))[1:181]
nmonth <- length(trading_months)
seaport <- data.frame(r_sa = matrix(NA,nmonth,1))
seaport <- cbind(trading_months, seaport)
for (i in seq(1,nmonth, by=3)){
  x <- pricing_factor_data$F100501A[pricing_factor_data$Accper == trading_months[i]]
  y <- pricing_factor_data$Stkcd [pricing_factor_data$Accper == trading_months[i]]
  #x储存对应的因子信息，y储存证券代码，如果要改因子，就改x对应的列就行了。
  #每个月的指标，删去NA所在的行
  y <- y[!is.na(x)]
  x <- x[!is.na(x)]
  stockn <- length(x)
  a <- stockn*ratio
  b <- stockn*(1-ratio)
  
  sealong <- data.frame(r= matrix(NA,length(trading_data$Mretwd3m[is.element(trading_data$Stkcd,y[rank(x)>b]) & trading_data$Trdmnt== trading_months[i]]),1))
  sealong$r <- trading_data$Mretwd3m[is.element(trading_data$Stkcd,y[rank(x)>b]) & trading_data$Trdmnt== trading_months[i]] *(-1)
  sealong$tic <- trading_data$Stkcd[is.element(trading_data$Stkcd,y[rank(x)>b]) & trading_data$Trdmnt== trading_months[i]]
  
  seashort <-data.frame(r= matrix(NA,length(trading_data$Mretwd3m[is.element(trading_data$Stkcd,y[rank(x)<a]) & trading_data$Trdmnt== trading_months[i]]),1))
  seashort$r <- trading_data$Mretwd3m[is.element(trading_data$Stkcd,y[rank(x)<a]) & trading_data$Trdmnt== trading_months[i]]
  seashort$tic <- trading_data$Stkcd[is.element(trading_data$Stkcd,y[rank(x)<a]) & trading_data$Trdmnt== trading_months[i]]
  
  sealong_R01<-trading_data$Mretwd1m[is.element(trading_data$Stkcd,y[rank(x)>b]) & trading_data$Trdmnt== trading_months[i]] *(-1)
  sealong_R02<-trading_data$Mretwd2m[is.element(trading_data$Stkcd,y[rank(x)>b]) & trading_data$Trdmnt== trading_months[i]] *(-1)
  sealong_R03<-trading_data$Mretwd3m[is.element(trading_data$Stkcd,y[rank(x)>b]) & trading_data$Trdmnt== trading_months[i]] *(-1)
  
  sealong$r1<-sealong_R01
  sealong$r2<-((1+sealong_R02)/(1+sealong_R01))-1
  sealong$r3<-((1+sealong_R03)/(1+sealong_R02))-1
  
  seashort_R01<-trading_data$Mretwd1m[is.element(trading_data$Stkcd,y[rank(x)<a]) & trading_data$Trdmnt== trading_months[i]]
  seashort_R02<-trading_data$Mretwd2m[is.element(trading_data$Stkcd,y[rank(x)<a]) & trading_data$Trdmnt== trading_months[i]] 
  seashort_R03<-trading_data$Mretwd3m[is.element(trading_data$Stkcd,y[rank(x)<a]) & trading_data$Trdmnt== trading_months[i]] 
  
  seashort$r1<-seashort_R01
  seashort$r2<-((1+seashort_R02)/(1+seashort_R01))-1
  seashort$r3<-((1+seashort_R03)/(1+seashort_R02))-1
  
  
  seaport$r_sa[(i-2)] <- (mean(seashort$r1,na.rm=TRUE) + mean(sealong$r1,na.rm=TRUE))
  seaport$r_sa[(i-1)] <- (mean(seashort$r2,na.rm=TRUE) + mean(sealong$r2,na.rm=TRUE))
  seaport$r_sa[i] <- (mean(seashort$r3,na.rm=TRUE) + mean(sealong$r3,na.rm=TRUE))

}


value<-seaport
colnames(value)<-c("trading_months","value")
value$value[which(is.infinite(value$value))]<-NA

# 计算每股因子资产收益率（季度）

trading_months <- sort(unique(as.Date(trading_data$Trdmnt)))[1:181]
nmonth <- length(trading_months)
seaport <- data.frame(r_sa = matrix(NA,nmonth,1))
seaport <- cbind(trading_months, seaport)
for (i in seq(1,nmonth, by=3)){
  x <- meiguzhibiao$F091001A[meiguzhibiao$Accper == trading_months[i]]
  y <- meiguzhibiao$Stkcd [meiguzhibiao$Accper == trading_months[i]]
  #x储存对应的因子信息，y储存证券代码，如果要改因子，就改x对应的列就行了。
  #每个月的指标，删去NA所在的行
  y <- y[!is.na(x)]
  x <- x[!is.na(x)]
  stockn <- length(x)
  a <- stockn*ratio
  b <- stockn*(1-ratio)
  
  sealong <- data.frame(r= matrix(NA,length(trading_data$Mretwd3m[is.element(trading_data$Stkcd,y[rank(x)>b]) & trading_data$Trdmnt== trading_months[i]]),1))
  sealong$r <- trading_data$Mretwd3m[is.element(trading_data$Stkcd,y[rank(x)>b]) & trading_data$Trdmnt== trading_months[i]] *(-1)
  sealong$tic <- trading_data$Stkcd[is.element(trading_data$Stkcd,y[rank(x)>b]) & trading_data$Trdmnt== trading_months[i]]
  
  seashort <-data.frame(r= matrix(NA,length(trading_data$Mretwd3m[is.element(trading_data$Stkcd,y[rank(x)<a]) & trading_data$Trdmnt== trading_months[i]]),1))
  seashort$r <- trading_data$Mretwd3m[is.element(trading_data$Stkcd,y[rank(x)<a]) & trading_data$Trdmnt== trading_months[i]]
  seashort$tic <- trading_data$Stkcd[is.element(trading_data$Stkcd,y[rank(x)<a]) & trading_data$Trdmnt== trading_months[i]]
  
  sealong_R01<-trading_data$Mretwd1m[is.element(trading_data$Stkcd,y[rank(x)>b]) & trading_data$Trdmnt== trading_months[i]] *(-1)
  sealong_R02<-trading_data$Mretwd2m[is.element(trading_data$Stkcd,y[rank(x)>b]) & trading_data$Trdmnt== trading_months[i]] *(-1)
  sealong_R03<-trading_data$Mretwd3m[is.element(trading_data$Stkcd,y[rank(x)>b]) & trading_data$Trdmnt== trading_months[i]] *(-1)
  
  sealong$r1<-sealong_R01
  sealong$r2<-((1+sealong_R02)/(1+sealong_R01))-1
  sealong$r3<-((1+sealong_R03)/(1+sealong_R02))-1
  
  seashort_R01<-trading_data$Mretwd1m[is.element(trading_data$Stkcd,y[rank(x)<a]) & trading_data$Trdmnt== trading_months[i]]
  seashort_R02<-trading_data$Mretwd2m[is.element(trading_data$Stkcd,y[rank(x)<a]) & trading_data$Trdmnt== trading_months[i]] 
  seashort_R03<-trading_data$Mretwd3m[is.element(trading_data$Stkcd,y[rank(x)<a]) & trading_data$Trdmnt== trading_months[i]] 
  
  seashort$r1<-seashort_R01
  seashort$r2<-((1+seashort_R02)/(1+seashort_R01))-1
  seashort$r3<-((1+seashort_R03)/(1+seashort_R02))-1
  
  
  seaport$r_sa[(i-2)] <- (mean(seashort$r1,na.rm=TRUE) + mean(sealong$r1,na.rm=TRUE))
  seaport$r_sa[(i-1)] <- (mean(seashort$r2,na.rm=TRUE) + mean(sealong$r2,na.rm=TRUE))
  seaport$r_sa[i] <- (mean(seashort$r3,na.rm=TRUE) + mean(sealong$r3,na.rm=TRUE))
  
}


per_share<-seaport
colnames(per_share)<-c("trading_months","per_share")
per_share$per_share[which(is.infinite(per_share$per_share))]<-NA

#计算市值因子资产收益率（月度）
i <- 1
ratio <- 0.2
#nmonth <- 182
#trading_months<-unique(as.Date(trading_data$Trdmnt))
trading_months <- sort(unique(as.Date(trading_data$Trdmnt)))[1:181]
nmonth <- length(trading_months)
seaport <- data.frame(r_sa = matrix(NA,nmonth,1))
for (i in 1:nmonth){
  trading_lastmonth <- as.Date("2005-01-01")+months(i-1)
  trading_month <- as.Date("2005-01-01")+months(i)
  x <- trading_data$Msmvttl [trading_data$Trdmnt == trading_lastmonth]
  y <- trading_data$Stkcd [trading_data$Trdmnt == trading_month]
  
  #x储存对应的因子信息，y储存证券代码，如果要改因子，就改x对应的列就行了。
  #每个月的指标，删去NA所在的行
  y <- y[!is.na(x)]
  x <- x[!is.na(x)]
  stockn <- length(x)
  a <- stockn*ratio
  b <- stockn*(1-ratio)
  
  sealong <- data.frame(r= matrix(NA,length(trading_data$Mretwd[is.element(trading_data$Stkcd,y[rank(x)>b]) & trading_data$Trdmnt== trading_months[i]]),1))
  sealong$r <- trading_data$Mretwd[is.element(trading_data$Stkcd,y[rank(x)>b]) & trading_data$Trdmnt== trading_months[i]] 
  sealong$tic <- trading_data$Stkcd[is.element(trading_data$Stkcd,y[rank(x)>b]) & trading_data$Trdmnt== trading_months[i]]
  
  seashort <-data.frame(r= matrix(NA,length(trading_data$Mretwd[is.element(trading_data$Stkcd,y[rank(x)<a]) & trading_data$Trdmnt== trading_months[i]]),1))
  seashort$r <- trading_data$Mretwd[is.element(trading_data$Stkcd,y[rank(x)<a]) & trading_data$Trdmnt== trading_months[i]]*(-1)
  seashort$tic <- trading_data$Stkcd[is.element(trading_data$Stkcd,y[rank(x)<a]) & trading_data$Trdmnt== trading_months[i]]
  
  seaport$r_sa[i] <- (mean(seashort$r,na.rm=TRUE) + mean(sealong$r,na.rm=TRUE))
}


size<-cbind(trading_months,seaport)
colnames(size)<-c("trading_months","size")

#计算流动性因子资产收益率（月度）
trading_data$ILLIQ <- abs(trading_data$Mretwd)/trading_data$Mnvaltrd/trading_data$Ndaytrd

i <- 1
ratio <- 0.2
#nmonth <- 182

nmonth <- length(trading_months)
seaport <- data.frame(r_sa = matrix(NA,nmonth,1))
for (i in 1:nmonth){
  trading_lastmonth <- as.Date("2005-01-01")+months(i-1)
  trading_month <- as.Date("2005-01-01")+months(i)
  x <- trading_data$ILLIQ [trading_data$Trdmnt == trading_lastmonth]
  y <- trading_data$Stkcd [trading_data$Trdmnt == trading_month]
  
  #x储存对应的因子信息，y储存证券代码，如果要改因子，就改x对应的列就行了。
  #每个月的指标，删去NA所在的行
  y <- y[!is.na(x)]
  x <- x[!is.na(x)]
  stockn <- length(x)
  a <- stockn*ratio
  b <- stockn*(1-ratio)
  
  sealong <- data.frame(r= matrix(NA,length(trading_data$Mretwd[is.element(trading_data$Stkcd,y[rank(x)>b]) & trading_data$Trdmnt== trading_months[i]]),1))
  sealong$r <- trading_data$Mretwd[is.element(trading_data$Stkcd,y[rank(x)>b]) & trading_data$Trdmnt== trading_months[i]] 
  sealong$tic <- trading_data$Stkcd[is.element(trading_data$Stkcd,y[rank(x)>b]) & trading_data$Trdmnt== trading_months[i]]
  
  seashort <-data.frame(r= matrix(NA,length(trading_data$Mretwd[is.element(trading_data$Stkcd,y[rank(x)<a]) & trading_data$Trdmnt== trading_months[i]]),1))
  seashort$r <- trading_data$Mretwd[is.element(trading_data$Stkcd,y[rank(x)<a]) & trading_data$Trdmnt== trading_months[i]]*(-1)
  seashort$tic <- trading_data$Stkcd[is.element(trading_data$Stkcd,y[rank(x)<a]) & trading_data$Trdmnt== trading_months[i]]
  
  seaport$r_sa[i] <- (mean(seashort$r,na.rm=TRUE) + mean(sealong$r,na.rm=TRUE))
}


liquidity<-cbind(trading_months,seaport)
colnames(liquidity)<-c("trading_months","liquidity")

#计算动量因子资产收益率（月度）
trading_data$Mclprc12m <-c(trading_data$Mclsprc[-(1:12)],NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
trading_data$Momentum <- trading_data$Mclprc12m / trading_data$Mclsprc-1

i <- 1
ratio <- 0.2
#nmonth <- 182

nmonth <- length(trading_months)
seaport <- data.frame(r_sa = matrix(NA,nmonth,1))
for (i in 1:nmonth){
  trading_lastmonth <- as.Date("2005-01-01")+months(i-1)
  trading_month <- as.Date("2005-01-01")+months(i)
  x <- trading_data$Momentum [trading_data$Trdmnt == trading_lastmonth]
  y <- trading_data$Stkcd [trading_data$Trdmnt == trading_month]
  
  #x储存对应的因子信息，y储存证券代码，如果要改因子，就改x对应的列就行了。
  #每个月的指标，删去NA所在的行
  y <- y[!is.na(x)]
  x <- x[!is.na(x)]
  stockn <- length(x)
  a <- stockn*ratio
  b <- stockn*(1-ratio)
  
  sealong <- data.frame(r= matrix(NA,length(trading_data$Mretwd[is.element(trading_data$Stkcd,y[rank(x)>b]) & trading_data$Trdmnt== trading_months[i]]),1))
  sealong$r <- trading_data$Mretwd[is.element(trading_data$Stkcd,y[rank(x)>b]) & trading_data$Trdmnt== trading_months[i]] *(-1)
  sealong$tic <- trading_data$Stkcd[is.element(trading_data$Stkcd,y[rank(x)>b]) & trading_data$Trdmnt== trading_months[i]]
  
  seashort <-data.frame(r= matrix(NA,length(trading_data$Mretwd[is.element(trading_data$Stkcd,y[rank(x)<a]) & trading_data$Trdmnt== trading_months[i]]),1))
  seashort$r <- trading_data$Mretwd[is.element(trading_data$Stkcd,y[rank(x)<a]) & trading_data$Trdmnt== trading_months[i]]
  seashort$tic <- trading_data$Stkcd[is.element(trading_data$Stkcd,y[rank(x)<a]) & trading_data$Trdmnt== trading_months[i]]
  
  seaport$r_sa[i] <- (mean(seashort$r,na.rm=TRUE) + mean(sealong$r,na.rm=TRUE))
}


Momentum<-cbind(trading_months,seaport)
colnames(Momentum)<-c("trading_months","Momentum")



#计算反转因子资产收益率（月度）
trading_data$Mclprc1m <-c(trading_data$Mclsprc[-1],NA)
trading_data$Reversal <- trading_data$Mclprc1m / trading_data$Mclsprc-1

i <- 1
ratio <- 0.2
#nmonth <- 182

nmonth <- length(trading_months)
seaport <- data.frame(r_sa = matrix(NA,nmonth,1))
for (i in 1:nmonth){
  trading_lastmonth <- as.Date("2005-01-01")+months(i-1)
  trading_month <- as.Date("2005-01-01")+months(i)
  x <- trading_data$Reversal [trading_data$Trdmnt == trading_lastmonth]
  y <- trading_data$Stkcd [trading_data$Trdmnt == trading_month]
  
  #x储存对应的因子信息，y储存证券代码，如果要改因子，就改x对应的列就行了。
  #每个月的指标，删去NA所在的行
  y <- y[!is.na(x)]
  x <- x[!is.na(x)]
  stockn <- length(x)
  a <- stockn*ratio
  b <- stockn*(1-ratio)
  
  sealong <- data.frame(r= matrix(NA,length(trading_data$Mretwd[is.element(trading_data$Stkcd,y[rank(x)>b]) & trading_data$Trdmnt== trading_months[i]]),1))
  sealong$r <- trading_data$Mretwd[is.element(trading_data$Stkcd,y[rank(x)>b]) & trading_data$Trdmnt== trading_months[i]] *(-1)
  sealong$tic <- trading_data$Stkcd[is.element(trading_data$Stkcd,y[rank(x)>b]) & trading_data$Trdmnt== trading_months[i]]
  
  seashort <-data.frame(r= matrix(NA,length(trading_data$Mretwd[is.element(trading_data$Stkcd,y[rank(x)<a]) & trading_data$Trdmnt== trading_months[i]]),1))
  seashort$r <- trading_data$Mretwd[is.element(trading_data$Stkcd,y[rank(x)<a]) & trading_data$Trdmnt== trading_months[i]]
  seashort$tic <- trading_data$Stkcd[is.element(trading_data$Stkcd,y[rank(x)<a]) & trading_data$Trdmnt== trading_months[i]]
  
  seaport$r_sa[i] <- (mean(seashort$r,na.rm=TRUE) + mean(sealong$r,na.rm=TRUE))
}


Reversal<-cbind(trading_months,seaport)
colnames(Reversal)<-c("trading_months","Reversal")

#跟沪深300作比较
hs300 <- read_delim("data/hs300.txt", 
                                  "\t", escape_double = FALSE, col_types = cols(Accper = col_date(format = "%Y/%m/%d")), 
                                  trim_ws = TRUE)

benchmark<-hs300$Idxrtn


#放在一个表里
factor_return<-cbind(per_share, value,size,liquidity, Reversal, Momentum,benchmark)
factor_return<-factor_return[,c(1,2,4,6,8,10,12,13)]
factor_return<-na.omit(factor_return)


#看一下各自表现
factor_return<-xts(factor_return[,-1],ymd(factor_return[,1]))


tab.perf(factor_return)


charts.PerformanceSummary(factor_return, date.format="%Y%m")

#esquisse::esquisser()
