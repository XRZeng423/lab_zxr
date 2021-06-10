library(readr)
library(lubridate)

setwd("E:/factors_XuranZENG")
trading_data <- read_delim("data/trading data.txt", 
                        "\t", escape_double = FALSE, col_types = cols(Trdmnt = col_date(format = "%Y/%m/%d")), 
                        trim_ws = TRUE)


#View(trading_data)
##tradind datad的收益率已经是月底的收盘价除以月初开盘价算出了的。而披露的日期设置在了1号，所以月收益不需要错开一个月份。
##计算每个月月初建仓对应的三个月收益，一个是三月后收盘价，一个是收益率
#trading_data$Mclprc3m <-c(trading_data$Mclsprc[-(1:3)],NA,NA,NA)
#trading_data$Mretwd3m <- trading_data$Mclprc3m / trading_data$Mclsprc-1
trading_data$ILLIQ <- abs(trading_data$Mretwd)/trading_data$Mnvaltrd/trading_data$Ndaytrd



##以第一个股票的时间为例，取出想要的月份的数据
##seaport是储存收益的向量，ratio是多空比例的参数
i <- 1
ratio <- 0.2
nmonth <- 181
##nmonth是投资的月数，ricing_factor_data的第一列也是头寸的创建时间
seaport <- data.frame(r_sa = matrix(NA,nmonth,1),r_vol = matrix(NA,nmonth,1),r_mvol= matrix(NA,nmonth,1),
                      r_group1 = matrix(NA,nmonth,1),r_group2 = matrix(NA,nmonth,1), r_group3 = matrix(NA,nmonth,1),
                      r_group4 = matrix(NA,nmonth,1),r_group5 = matrix(NA,nmonth,1), aof_group1= matrix(NA,nmonth,1),
                      aof_group2= matrix(NA,nmonth,1),aof_group3= matrix(NA,nmonth,1),aof_group4= matrix(NA,nmonth,1),
                      aof_group5= matrix(NA,nmonth,1))
##做一个放结果的数据框
results_des <- data.frame(SA = matrix(NA,6,1),VOL = matrix(NA,6,1),MVOL = matrix(NA,6,1),group1 = matrix(NA,6,1),
                          group2 = matrix(NA,6,1),group3 = matrix(NA,6,1),group4= matrix(NA,6,1), 
                          group5 = matrix(NA,6,1),row.names = c("MEAN","SD","SR","TSTAT","PVALUE","AOF"))
for (i in 1:nmonth){
  trading_lastmonth <- as.Date("2005-01-01")+months(i-1)
  trading_month <- as.Date("2005-01-01")+months(i)
  x <- trading_data$Msmvttl[trading_data$Trdmnt == trading_lastmonth]
  y <- trading_data$Stkcd [trading_data$Trdmnt == trading_month]
  #x储存对应的因子信息，y储存证券代码，如果要改因子，就改x对应的列就行了。
  #每个月的指标，删去NA所在的行
  y <- y[!is.na(x)]
  x <- x[!is.na(x)]
  stockn <- length(x)
  a <- stockn*ratio
  b <- stockn*(1-ratio)
  #如果因子是越大越好，y[rank(x)<a]是后30%的需要做空的证券代码，y[rank(x)>b]是前30%要做多的证券代码
  #如果因子是越小越好，那么本算法最后处理收益数据的时候要乘以-1
  #做一个dataframe，放每个月的各种数据。returns_short放的是第i次调仓的空头头寸的各个股票收益率
  
  seashort <-data.frame(r= matrix(NA,length(trading_data$Mretwd[is.element(trading_data$Stkcd,y[rank(x)<a]) & trading_data$Trdmnt==trading_month]),1))
  #依次是短头寸回报率、股票代码、总市值、流通市值
  seashort$r <- trading_data$Mretwd[is.element(trading_data$Stkcd,y[rank(x)<a]) & trading_data$Trdmnt==trading_month]
  seashort$tic <- trading_data$Stkcd[is.element(trading_data$Stkcd,y[rank(x)<a]) & trading_data$Trdmnt==trading_month]
  seashort$vol <- trading_data$Msmvttl[is.element(trading_data$Stkcd,y[rank(x)<a]) & trading_data$Trdmnt==trading_month]
  seashort$mvol <- trading_data$Msmvosd[is.element(trading_data$Stkcd,y[rank(x)<a]) & trading_data$Trdmnt==trading_month]
  
  sealong <- data.frame(r= matrix(NA,length(trading_data$Mretwd[is.element(trading_data$Stkcd,y[rank(x)>b]) & trading_data$Trdmnt==trading_month]),1))
  sealong$r <- trading_data$Mretwd[is.element(trading_data$Stkcd,y[rank(x)>b]) & trading_data$Trdmnt==trading_month] *(-1)
  sealong$tic <- trading_data$Stkcd[is.element(trading_data$Stkcd,y[rank(x)>b]) & trading_data$Trdmnt==trading_month]
  sealong$vol <- trading_data$Msmvttl[is.element(trading_data$Stkcd,y[rank(x)>b]) & trading_data$Trdmnt==trading_month]
  sealong$mvol <- trading_data$Msmvosd[is.element(trading_data$Stkcd,y[rank(x)>b]) & trading_data$Trdmnt==trading_month]
  
  #把每个季度对应的三种加权方法算出来的收益率放在seaprt里
  seaport$r_sa[i] <- (mean(seashort$r,na.rm=TRUE) + mean(sealong$r,na.rm=TRUE))
  seaport$r_vol[i] <- (mean(seashort$r*seashort$vol/sum(seashort$vol),na.rm=TRUE) + mean(sealong$r*sealong$vol/sum(sealong$vol),na.rm=TRUE))
  seaport$r_mvol[i] <- (mean(seashort$r*seashort$mvol/sum(seashort$mvol),na.rm=TRUE) + mean(sealong$r*sealong$mvol/sum(sealong$mvol),na.rm=TRUE))
  
  #按指标进行5等分，看平均收益
  seaport$r_group1[i] <- mean(trading_data$Mretwd[is.element(trading_data$Stkcd,y[rank(x)>(stockn *0.8)]) & trading_data$Trdmnt==trading_month],na.rm=TRUE)
  seaport$r_group2[i] <- mean(trading_data$Mretwd[is.element(trading_data$Stkcd,y[(rank(x)<(stockn *0.8)) & (rank(x)>(stockn *0.6))]) & trading_data$Trdmnt==trading_month],na.rm=TRUE)
  seaport$r_group3[i] <- mean(trading_data$Mretwd[is.element(trading_data$Stkcd,y[(rank(x)<(stockn *0.6)) & (rank(x)>(stockn *0.4))]) & trading_data$Trdmnt==trading_month],na.rm=TRUE)
  seaport$r_group4[i] <- mean(trading_data$Mretwd[is.element(trading_data$Stkcd,y[(rank(x)<(stockn *0.4)) & (rank(x)>(stockn *0.2))]) & trading_data$Trdmnt==trading_month],na.rm=TRUE)
  seaport$r_group5[i] <- mean(trading_data$Mretwd[is.element(trading_data$Stkcd,y[rank(x)<(stockn *0.2)]) & trading_data$Trdmnt==trading_month],na.rm=TRUE)
  
  seaport$aof_group1[i] <-mean(x[rank(x)>(stockn *0.8)])
  seaport$aof_group2[i] <-mean(x[(rank(x)<(stockn *0.8)) & (rank(x)>(stockn *0.6))])
  seaport$aof_group3[i] <-mean(x[(rank(x)<(stockn *0.6)) & (rank(x)>(stockn *0.4))])
  seaport$aof_group4[i] <-mean(x[(rank(x)<(stockn *0.4)) & (rank(x)>(stockn *0.2))])
  seaport$aof_group5[i] <-mean(x[rank(x)<(stockn *0.2)])
  
}

for (j in 1:8){
  results_des[1,j] <- mean(seaport[,j],na.rm = TRUE)*12
  results_des[2,j] <- sd(seaport[,j],na.rm = TRUE)*sqrt(12)
  results_des[3,j] <- results_des[1,j]/results_des[2,j]
  tresult <- t.test(seaport[,j],na.rm = TRUE)
  results_des[4,j] <- as.numeric(tresult$statistic)
  results_des[5,j] <- as.numeric(tresult$p.value)
}
for (j in 4:8){
  results_des[6,j] <- mean(seaport[,j+5],na.rm = TRUE)
}


write.csv(seaport,file ="results\\liquidity\\size_VOL-portfolio.csv",)
write.csv(results_des,file ="results\\liquidity\\size_VOL-result.csv",)
