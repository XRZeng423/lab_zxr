## Tactical Permanent Portfolio

library(xts)
library(PerformanceAnalytics)
library(lubridate)
library(quantmod)
library(TTR)

setwd("C:\\Users\\carol\\Desktop\\asset_allocation")
source("cal_ret.R")
source("funs.R")


# 下载数据
start_date <- "2007-01-01"  
end_date <- "2020-07-14"  

symbols<-c("SPY",  "IEF",  "GLD", "SHY")  

getSymbols(symbols, from=start_date,to=end_date)
asset_price<-list()
for(i in 1:length(symbols)) {
  asset_price[[i]] <- Cl(get(symbols[i]))  
}
asset_price <- do.call(cbind, asset_price) #xts
colnames(asset_price)<-symbols

# 计算过去200日移动平均
MA200_prices<-filter(asset_price/200, rep(1, 200))
colnames(MA200_prices)<-symbols
MA200_prices<-as.data.frame(MA200_prices)
MA200_prices<- cbind(date=as.Date(rownames(as.data.frame(asset_price))),MA200_prices)
MA200_prices<-na.omit(MA200_prices)
MA200_prices<-xts(MA200_prices, order.by=as.Date(MA200_prices[,1]))


# 找到每个月最后一个交易日
period.ends = endpoints(MA200_prices, 'months')
period.ends = period.ends[period.ends > 0] 
n.mos  <- length(period.ends)
last_trading_dates = MA200_prices$date[period.ends]

MA200_prices<-MA200_prices[,-1]
asset_price<-asset_price[rownames(as.data.frame(MA200_prices)),]

# 做一个选择资产的dataframe
asset_selected <- data.frame(date=last_trading_dates,SPY = matrix(NA,n.mos,1),IEF = matrix(NA,n.mos,1),GLD = matrix(NA,n.mos,1))
asset_selected <- asset_selected[,-1]


# 选择收盘价高于MA(200)的资产
for (i in 1:(n.mos)){
  asset_selected[i,] <- as.numeric(asset_price[last_trading_dates,][i,1:3])>=as.numeric(MA200_prices[last_trading_dates,][i,1:3])
}



# 根据过去一个月波动率采用风险平价确定初始权重
daily_return<-daily.return(asset_price)
monthly_return<-monthly.return(asset_price)

#计算波动率
s_sigma<-apply.monthly(daily_return, function(x) 1/StdDev(x))
#先将SHY波动率赋值为0，为了计算不配现金的初始权重
s_sigma$SHY<-0
#将FALSE也就是不选择的资产的波动率赋值为0，为了好算权重
for (i in 1:(n.mos)){
  s_sigma[i,which(asset_selected[i,]==FALSE)]<-0
}
#根据风险平价计算权重
s_sum<-rowSums(s_sigma)
s_weight<-s_sigma/s_sum
#三个资产都不选时，令SHY权重为1
s_weight[is.na(s_weight)] <- c(0,0,0,1)


# 对于选出来的组合，根据60天variance/covariance计算组合年化波动率，如果高于7%，则添加现金使之降到7%以下
weights<-s_weight

# 根据每日收益和每月调仓权重构建组合
portf <- Return.portfolio(daily_return,s_weight, verbose=T)
portf.ret<-portf$returns

#做一个表每天根据过去60天组合收益计算年化波动率
daily_vol<-data.frame(portf.ret,AV = matrix(NA,length(portf.ret),1))
for (i in 60:length(portf.ret)){
  daily_vol[i,2]<-StdDev.annualized(portf.ret[(i-59):i,],scale=252)
}

#从日波动率中取每月月底调仓日的波动率
monthly_vol<- daily_vol[period.ends,]
weights$AV<-monthly_vol$AV
weights<-round(weights,2)

#增加现金使波动率降低
#令现金权重为x，列方程(1-x)^2(WA^2*DA+2WA*WB*COVAV+WB^2*DB)=0.07,求解X





