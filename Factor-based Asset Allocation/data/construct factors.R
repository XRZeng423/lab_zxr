library(readr)
library(lubridate)

library(xts)
library(PerformanceAnalytics)
library(lubridate)
library(quantmod)
library(dplyr)
setwd("E:/data")
trading_data <- read_delim("trading data.txt", 
                           "\t", escape_double = FALSE, col_types = cols(Trdmnt = col_date(format = "%Y/%m/%d")), 
                           trim_ws = TRUE)
#trading_data <- trading_data[order(trading_data$Stkcd),]


##trading data的收益率已经是月底的收盘价除以月初开盘价算出了的。而披露的日期设置在了1号，所以月收益不需要错开一个月份。
##计算每个月月初建仓对应的一、二、三个月收益

trading_data$Mclprc1m <-c(trading_data$Mclsprc[-(1:1)],NA)
trading_data$Mclprc2m <-c(trading_data$Mclsprc[-(1:2)],NA,NA)
trading_data$Mclprc3m <-c(trading_data$Mclsprc[-(1:3)],NA,NA,NA)

trading_data$Mretwd1m <- trading_data$Mclprc1m / trading_data$Mclsprc-1
trading_data$Mretwd2m <- trading_data$Mclprc2m / trading_data$Mclsprc-1
trading_data$Mretwd3m <- trading_data$Mclprc3m / trading_data$Mclsprc-1
##一个是三月后收盘价，一个是收益率
pricing_factor_data <- read_delim("pricing factor data.txt", 
                                  "\t", escape_double = FALSE, col_types = cols(Accper = col_date(format = "%Y/%m/%d")), 
                                  trim_ws = TRUE)
pricing_factor_data$Accper <- as.Date(pricing_factor_data$Accper)+ 1
pricing_factor_data <- arrange(pricing_factor_data,pricing_factor_data$Stkcd,pricing_factor_data$Accper)

# high value factor
trading_months <- sort(unique(as.Date(trading_data$Trdmnt)))[1:181]
nmonth <- length(trading_months)
seaport <- data.frame(r_sa = matrix(NA,nmonth,1))
seaport <- cbind(trading_months, seaport)
ratio = 0.2
for (i in seq(1,nmonth, by=3)){
  x <- pricing_factor_data$F100401A[pricing_factor_data$Accper == trading_months[i]]
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
value[,2]<-1/seaport[,2]
colnames(value)<-c("trading_months","High Value")

write.csv(value,file ="results\\value.csv",)

# profitability
profitability_data <- read_delim("profitability.txt", 
                           "\t", escape_double = FALSE, col_types = cols(Trdmnt = col_date(format = "%Y/%m/%d")), 
                           trim_ws = TRUE)
profitability_data<-na.omit(profitability_data)
profitability_data$Accper <- as.Date(profitability_data$Accper)+ 1
profitability_data <- arrange(profitability_data,profitability_data$Stkcd,profitability_data$Accper)

trading_months <- sort(unique(as.Date(trading_data$Trdmnt)))[1:181]
nmonth <- length(trading_months)
seaport <- data.frame(r_sa = matrix(NA,nmonth,1))
seaport <- cbind(trading_months, seaport)
ratio = 0.2
for (i in seq(1,nmonth, by=3)){
  x <- profitability_data$F051401B[profitability_data$Accper == trading_months[i]]
  y <- profitability_data$Stkcd [profitability_data$Accper == trading_months[i]]
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


profitability<-seaport
colnames(profitability)<-c("trading_months","High Profit")

write.csv(profitability,file ="results\\profitability.csv",)



# investment
investment_data <- read_delim("investment.txt", 
                                 "\t", escape_double = FALSE, col_types = cols(Trdmnt = col_date(format = "%Y/%m/%d")), 
                                 trim_ws = TRUE)
investment_data<-na.omit(investment_data)
investment_data$Accper <- as.Date(investment_data$Accper)+ 1
investment_data <- arrange(investment_data,investment_data$Stkcd,investment_data$Accper)
investment_data<-subset(investment_data, Typrep =='A')

trading_months <- sort(unique(as.Date(trading_data$Trdmnt)))[1:181]
nmonth <- length(trading_months)
seaport <- data.frame(r_sa = matrix(NA,nmonth,1))
seaport <- cbind(trading_months, seaport)
ratio = 0.2
for (i in seq(1,nmonth, by=3)){
  x <- investment_data$F080601A [investment_data$Accper == trading_months[i]]
  y <- investment_data$Stkcd [investment_data$Accper == trading_months[i]]
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


investment<-seaport
colnames(investment)<-c("trading_months","Low investment")

write.csv(investment,file ="results\\investment.csv",)


# payout
payout_data <- read_delim("payout.txt", 
                              "\t", escape_double = FALSE, col_types = cols(Trdmnt = col_date(format = "%Y/%m/%d")), 
                              trim_ws = TRUE)
payout_data<-na.omit(payout_data)
payout_data$Accper <- as.Date(payout_data$Accper)+ 1
payout_data <- arrange(payout_data,payout_data$Stkcd,payout_data$Accper)
payout_data<-subset(payout_data, Typrep =='A')

trading_months <- sort(unique(as.Date(trading_data$Trdmnt)))[1:181]
nmonth <- length(trading_months)
seaport <- data.frame(r_sa = matrix(NA,nmonth,1))
seaport <- cbind(trading_months, seaport)
ratio = 0.2
for (i in seq(1,nmonth, by=3)){
  x <- payout_data$F110301B[payout_data$Accper == trading_months[i]]
  y <- payout_data$Stkcd [payout_data$Accper == trading_months[i]]
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


payout<-seaport
colnames(payout)<-c("trading_months","High Payout")

write.csv(payout,file ="results\\payout.csv",)


# BAB
trading_data <- read_delim("trading data.txt", 
                           "\t", escape_double = FALSE, col_types = cols(Trdmnt = col_date(format = "%Y/%m/%d")), 
                           trim_ws = TRUE)
trading_data <- trading_data[order(trading_data$Stkcd),]
trading_data <- na.omit(trading_data)

market_data <- read_delim("market return.txt", 
                          "\t", escape_double = FALSE, col_types = cols(Trdmnt = col_date(format = "%Y/%m/%d")), 
                          trim_ws = TRUE)


data_length <- length(unique(trading_data$Stkcd))

beta_factor<-data.frame(Stkcd = unique(trading_data$Stkcd),beta=matrix(NA,data_length,1))




for(i in unique(trading_data$Stkcd)){
  stock_data<-trading_data[which(trading_data$Stkcd==i),c(3,10)]

  table<-merge(stock_data,market_data,by="Trdmnt")

  lm<-lm(table$Mretwd~table$Cmretwdeq)

  beta<-lm$coef[2]

  beta_factor[which(beta_factor$Stkcd==i),2]<-beta


}




