#input: assets code, date, number of risky assets chosen, length of period to calculate zi

#output: assets returns, weights in csv

#functionality: calculate assets weights based on Generalized Protective Momentum 



library(xts)

library(PerformanceAnalytics)

library(lubridate)

library(quantmod)

library(TTR)

library(ggplot2)



setwd("E:/asset_allocation/ACA")

source("cal_ret.R")


start_date <- "2007-01-01"  
end_date <- "2020-07-14"  

getSymbols("SPY",from=start_date,to=end_date)  # S&P 500
getSymbols("GLD",from=start_date,to=end_date)  # gold
getSymbols("VNQ",from=start_date,to=end_date)  # US real estate
getSymbols("IEF",from=start_date,to=end_date)  #  intermediate-term US Treasuries
getSymbols("TLT",from=start_date,to=end_date)  # long-term US Treasuries

asset_price <- list(SPY, GLD, VNQ, IEF, TLT)
asset_price <- lapply(asset_price, Cl)
asset_price <- as.data.frame(asset_price)
asset_price <- cbind(date=as.Date(rownames(asset_price)),asset_price)
asset_price <- as.data.frame(asset_price)
#asset_price<-xts(asset_price[,-1], ymd(asset_price[,1])) #xts
#t<-1:length(asset_price[,4])
rownames(asset_price)<-1:length(asset_price[,4])

#install.packages("esquisse")
#esquisse::esquisser()

ggplot(asset_price) +
  aes(x = date, y = SPY.Close) +
  geom_line(size = 1L, colour = "#0c4c8a") +
  theme_minimal()


price_channel <- data.frame(SPY = matrix(NA,5,1),GLD = matrix(NA,5,1),VNQ = matrix(NA,5,1),row.names = c("UC_126","UC_252","LC_126","LC_252","close"))

#做一个放weights的表
#列名是数字，第一列是日期
l<-length(asset_price[,4])-250
asset_weight <- data.frame(date=asset_price$date[251:length(asset_price[,4])],SPY = matrix(NA,l,1),GLD = matrix(NA,l,1),VNQ = matrix(NA,l,1),TLT = matrix(NA,l,1),IEF_SPY = matrix(NA,l,1),IEF_VNQ = matrix(NA,l,1),IEF= matrix(NA,l,1), row.names =251:length(asset_price[,4]) )


##列名是日期，第一列是数字
#l<-length(asset_price[,4])-252
#asset_weight <- data.frame(t=251:length(asset_price[,4]),SPY = matrix(NA,l,1),GLD = matrix(NA,l,1),VNQ = matrix(NA,l,1),IEF = matrix(NA,l,1),TLT = matrix(NA,l,1),row.names = asset_price$date[251:length(asset_price[,4])])

asset_weight[1,'SPY'] = 1/3
asset_weight[1,'GLD'] = 1/3
asset_weight[1,'VNQ'] = 1/3
asset_weight[1,'IEF_SPY'] = 0
asset_weight[1,'IEF_VNQ'] = 0
asset_weight[1,'TLT'] = 0
asset_weight[1,'IEF']=0

for(i in 252:length(asset_price[,4])){
  #max(asset_price$SPY.Close[1:252])
  #max(asset_price$SPY.Close[127:252])
  
  #i=252
  #SPY过去252天最高收盘价
  UC_252_SPY<-max(asset_price$SPY.Close[(i-251):i])
  #SPY过去252天最低收盘价
  LC_252_SPY<-min(asset_price$SPY.Close[(i-251):i])
  #SPY过去126天最高收盘价
  UC_126_SPY<-max(asset_price$SPY.Close[(i-125):i])
  #SPY过去126天最低收盘价
  LC_126_SPY<-min(asset_price$SPY.Close[(i-125):i])
  
  #填入SPY的price channel表
  price_channel['UC_126','SPY']<-UC_126_SPY
  price_channel['UC_252','SPY']<-UC_252_SPY
  price_channel['LC_126','SPY']<-LC_126_SPY
  price_channel['LC_252','SPY']<-LC_252_SPY
  price_channel['close','SPY']<-asset_price$SPY.Close[i]
  
  #GLD过去252天最高收盘价
  UC_252_GLD<-max(asset_price$GLD.Close[(i-251):i])
  #GLD过去252天最低收盘价
  LC_252_GLD<-min(asset_price$GLD.Close[(i-251):i])
  #GLD过去126天最高收盘价
  UC_126_GLD<-max(asset_price$GLD.Close[(i-125):i])
  #GLD过去126天最低收盘价
  LC_126_GLD<-min(asset_price$GLD.Close[(i-125):i])
  
  #填入GLD的price channel表
  price_channel['UC_126','GLD']<-UC_126_GLD
  price_channel['UC_252','GLD']<-UC_252_GLD
  price_channel['LC_126','GLD']<-LC_126_GLD
  price_channel['LC_252','GLD']<-LC_252_GLD
  price_channel['close','GLD']<-asset_price$GLD.Close[i]
  
  #VNQ过去252天最高收盘价
  UC_252_VNQ<-max(asset_price$VNQ.Close[(i-251):i])
  #GLD过去252天最低收盘价
  LC_252_VNQ<-min(asset_price$VNQ.Close[(i-251):i])
  #GLD过去126天最高收盘价
  UC_126_VNQ<-max(asset_price$VNQ.Close[(i-125):i])
  #GLD过去126天最低收盘价
  LC_126_VNQ<-min(asset_price$VNQ.Close[(i-125):i])
  
  #填入VNQ的price channel表
  price_channel['UC_126','VNQ']<-UC_126_VNQ
  price_channel['UC_252','VNQ']<-UC_252_VNQ
  price_channel['LC_126','VNQ']<-LC_126_VNQ
  price_channel['LC_252','VNQ']<-LC_252_VNQ
  price_channel['close','VNQ']<-asset_price$VNQ.Close[i]
  
  
  
  #对于第一组
  if(price_channel['close','SPY']>= price_channel['UC_126','SPY']){
    asset_weight[(i-250),'SPY'] = 1/3
    asset_weight[(i-250),'IEF_SPY'] = 0
  } else if(price_channel['close','SPY']<= price_channel['LC_252','SPY']){
    asset_weight[(i-250),'IEF_SPY'] = 1/3
    asset_weight[(i-250),'SPY'] = 0
  } else{
    asset_weight[(i-250),'SPY'] = asset_weight[(i-251),'SPY']
    asset_weight[(i-250),'IEF_SPY'] = asset_weight[(i-251),'IEF_SPY']
  }
  
  
  
  #对于第二组
  if(price_channel['close','GLD']>= price_channel['UC_252','GLD']){
    asset_weight[(i-250),'GLD'] = 1/3
    asset_weight[(i-250),'TLT'] = 0
  } else if(price_channel['close','GLD']<= price_channel['LC_126','GLD']){
    asset_weight[(i-250),'TLT'] = 1/3
    asset_weight[(i-250),'GLD'] = 0
  } else{
    asset_weight[(i-250),'GLD'] = asset_weight[(i-251),'GLD']
    asset_weight[(i-250),'TLT'] = asset_weight[(i-251),'TLT']
  }
  
  #对于第三组
  if(price_channel['close','VNQ']>= price_channel['UC_126','VNQ']){
    asset_weight[(i-250),'VNQ'] = 1/3
    asset_weight[(i-250),'IEF_VNQ'] = 0
  } else if(price_channel['close','VNQ']<= price_channel['LC_252','VNQ']){
    asset_weight[(i-250),'IEF_VNQ'] = 1/3
    asset_weight[(i-250),'VNQ'] = 0
  }  else{
    asset_weight[(i-250),'VNQ'] = asset_weight[(i-251),'VNQ']
    asset_weight[(i-250),'IEF_VNQ'] = asset_weight[(i-251),'IEF_VNQ']
  }
  
  asset_weight[(i-250),'IEF']=asset_weight[(i-250),'IEF_SPY']+asset_weight[(i-250),'IEF_VNQ']
  
}

#计算收益
asset_price<-xts(asset_price[,-1], ymd(asset_price[,1])) #xts
SPY.ret<-periodReturn(asset_price$SPY.Close[251:length(asset_price[,4])],period="daily")
GLD.ret<-periodReturn(asset_price$GLD.Close[251:length(asset_price[,4])],period="daily")
VNQ.ret<-periodReturn(asset_price$VNQ.Close[251:length(asset_price[,4])],period="daily")
IEF.ret<-periodReturn(asset_price$IEF.Close[251:length(asset_price[,4])],period="daily")
TLT.ret<-periodReturn(asset_price$TLT.Close[251:length(asset_price[,4])],period="daily")

returns<-data.frame(list(SPY.ret,  GLD.ret,VNQ.ret,  TLT.ret, IEF.ret))
colnames(returns) <- c("SPY",  "GLD","VNQ",  "TLT","IEF")

#write.csv(returns,"E:\\asset_allocation\\ACA\\assets_returns.csv", row.names = TRUE)
#write.csv(asset_weight,"E:\\asset_allocation\\ACA\\assets_weights.csv", row.names = FALSE)

