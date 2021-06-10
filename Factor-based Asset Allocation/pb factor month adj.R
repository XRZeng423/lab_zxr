library(readr)
library(lubridate)
trading_data <- read_delim("C:/Users/hasee/Desktop/work documents/data/trading data/trading data.txt", 
                           "\t", escape_double = FALSE, col_types = cols(Trdmnt = col_date(format = "%Y/%m/%d")), 
                           trim_ws = TRUE)

#trading_data$ILLIQ <- abs(trading_data$Mretwd)/trading_data$Mnvaltrd/trading_data$Ndaytrd
pricing_factor_data <- read_delim("C:/Users/hasee/Desktop/work documents/data/pricing factor data.txt", 
                                  "\t", escape_double = FALSE, col_types = cols(Accper = col_date(format = "%Y/%m/%d")), 
                                  trim_ws = TRUE)
financial_stock_code <- read_csv("C:/Users/hasee/Desktop/work documents/data/financial stock code.txt", 
                                 col_types = cols(Stkcd = col_number()))
#delete financial stock
for (i in 1:length(financial_stock_code$Stkcd)){
  pricing_factor_data <- pricing_factor_data[pricing_factor_data$Stkcd!=financial_stock_code$Stkcd[i],]
  trading_data <- trading_data[trading_data$Stkcd!=financial_stock_code$Stkcd[i],]
}
nmonth <- 145
ratio <- 0.2
trading_lastmonth <- as.Date("2005-01-01")+months(c(0:(nmonth-1))-c(0:(nmonth-1))%%3)
trading_month <- as.Date("2005-01-01")+months(0:(nmonth-1))
#做一个调整的交易价格数据，把空缺的地方填上NA，方便后面进行计算
trading_data_adj <- data.frame(Stkcd = matrix(NA,nmonth*length(unique(trading_data$Stkcd)),1),
                              Trdmnt = matrix(NA,nmonth*length(unique(trading_data$Stkcd)),1),
                              Mclsprc = matrix(NA,nmonth*length(unique(trading_data$Stkcd)),1))
trading_data_adj$Stkcd <- rep(unique(trading_data$Stkcd),each=nmonth)
trading_data_adj$Trdmnt <- trading_month
for(i in 1:length(trading_data_adj$Stkcd)){
  if(length(trading_data$Mclsprc[(trading_data$Stkcd==trading_data_adj$Stkcd[i]) & (trading_data$Trdmnt==trading_data_adj$Trdmnt[i])])){
  trading_data_adj$Mclsprc[i] <- trading_data$Mclsprc[(trading_data$Stkcd==trading_data_adj$Stkcd[i]) & (trading_data$Trdmnt==trading_data_adj$Trdmnt[i])]
  }
}

seaport <- data.frame(r_longshort = matrix(NA,nmonth,1),r_short = matrix(NA,nmonth,1),r_long =matrix(NA,nmonth,1))
##做一个放结果的数据框
results_des <- data.frame(SA = matrix(NA,5,1),row.names = c("MEAN","SD","SR","TSTAT","PVALUE"))

for (i in 4:nmonth){
  x <- pricing_factor_data$F100401A [pricing_factor_data$Accper == trading_lastmonth[i]]
  y <- pricing_factor_data$Stkcd [pricing_factor_data$Accper == trading_lastmonth[i]]
  #x储存对应的因子信息，y储存证券代码，如果要改因子，就改x对应的列就行了。
  #每个月的指标，删去X为NA的所在的行
  y <- y[!is.na(x)]
  x <- x[!is.na(x)]
  stockn <- length(x)
  a <- stockn*ratio
  b <- stockn*(1-ratio)
  #PB子是越小越好，y[rank(x)<a]是后20%的需要做多的证券代码，y[rank(x)>b]是前20%要做空的证券代码

  sealong <-data.frame(firstmonth_price= matrix(NA,length(trading_data_adj$Stkcd[is.element(trading_data_adj$Stkcd,y[rank(x)<a]) & trading_data_adj$Trdmnt==trading_month[i]]),1))
  seashort <- data.frame(firstmonth_price= matrix(NA,length(trading_data_adj$Stkcd[is.element(trading_data_adj$Stkcd,y[rank(x)>b]) & trading_data_adj$Trdmnt==trading_month[i]]),1))
  
  sealong$firstmonth_price <- trading_data_adj$Mclsprc[is.element(trading_data_adj$Stkcd,y[rank(x)<a]) & trading_data_adj$Trdmnt==(trading_lastmonth[i]-months(1))]
  seashort$firstmonth_price <- trading_data_adj$Mclsprc[is.element(trading_data_adj$Stkcd,y[rank(x)>b]) & trading_data_adj$Trdmnt==(trading_lastmonth[i]-months(1))]
  
  sealong$lastmonth_price <- trading_data_adj$Mclsprc[is.element(trading_data_adj$Stkcd,y[rank(x)<a]) & trading_data_adj$Trdmnt==(trading_month[i]-months(1))]
  seashort$lastmonth_price <- trading_data_adj$Mclsprc[is.element(trading_data_adj$Stkcd,y[rank(x)>b]) & trading_data_adj$Trdmnt==(trading_month[i]-months(1))]
  
  sealong$thismonth_price <- trading_data_adj$Mclsprc[is.element(trading_data_adj$Stkcd,y[rank(x)<a]) & trading_data_adj$Trdmnt==trading_month[i]]
  seashort$thismonth_price <- trading_data_adj$Mclsprc[is.element(trading_data_adj$Stkcd,y[rank(x)>b]) & trading_data_adj$Trdmnt==trading_month[i]]
  
  sealong <- na.omit(sealong)
  seashort <- na.omit(seashort)
  
  seaport$r_long[i] <- sum(sealong$thismonth_price/sealong$firstmonth_price)/sum(sealong$lastmonth_price/sealong$firstmonth_price)-1
  seaport$r_short[i] <- sum(seashort$thismonth_price/seashort$firstmonth_price)/sum(seashort$lastmonth_price/seashort$firstmonth_price)-1
  seaport$r_longshort[i] <- seaport$r_long[i] - seaport$r_short[i]
  
}

  results_des[1,1] <- mean(seaport[,1],na.rm = TRUE)*12
  results_des[2,1] <- sd(seaport[,1],na.rm = TRUE)*sqrt(12)
  results_des[3,1] <- results_des[1,1]/results_des[2,1]
  tresult <- t.test(seaport[,1],na.rm = TRUE)
  results_des[4,1] <- as.numeric(tresult$statistic)
  results_des[5,1] <- as.numeric(tresult$p.value)



write.csv(seaport,file ="C:\\Users\\hasee\\Desktop\\work documents\\data\\results\\portfolio\\pb-portfolio-month-adj.csv",)
write.csv(results_des,file ="C:\\Users\\hasee\\Desktop\\work documents\\data\\results\\factor result\\pb-result-month-adj.csv",)
