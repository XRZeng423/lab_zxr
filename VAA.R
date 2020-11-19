#input: assets code, date, number of risky assets chosen, length of period to calculate zi
#output: assets returns, weights in csv
#functionality: calculate assets weights based on Generalized Protective Momentum 

library(xts)
library(PerformanceAnalytics)
library(lubridate)
library(quantmod)
library(TTR)

setwd("C:\\Users\\carol\\Desktop\\asset_allocation")
source("cal_ret.R")
source("funs.R")



## define strategy function
#input: all_assets, number of risky assets chosen, length of period to calculate zi
#output: assets_weights
#functionality: use GPM to calculate weights of each asset and obtain them in csv form

VAA<-function(asset_price){
  

  
  asset_price<-as.data.frame(asset_price)
  asset_price<- cbind(date=as.Date(rownames(as.data.frame(asset_price))),asset_price)
  
  period.ends = endpoints(asset_price, 'months')
  period.ends = period.ends[period.ends > 0] 
  n.mos  <- length(period.ends)
  last_trading_dates = asset_price$date[period.ends]
  
  
  
  #calculate po
  close<-as.data.frame(asset_price[period.ends,])
  p0<-close[,c(offensive,defensive)  ]
  
  #calculate p1
  p1<-data.frame(SPY = matrix(NA,n.mos,1),EFA = matrix(NA,n.mos,1),EEM = matrix(NA,n.mos,1),AGG = matrix(NA,n.mos,1),LQD = matrix(NA,n.mos,1),IEF = matrix(NA,n.mos,1),SHY = matrix(NA,n.mos,1))
  rownames(p1)<-last_trading_dates
  p1[1,c(offensive,defensive) ]<-NA
  for (i in 2:n.mos){
    p1[i,c(offensive,defensive)]<-close[(i-1),c(offensive,defensive)]
  }

  #calculate p3  
  p3<-data.frame(SPY = matrix(NA,n.mos,1),EFA = matrix(NA,n.mos,1),EEM = matrix(NA,n.mos,1),AGG = matrix(NA,n.mos,1),LQD = matrix(NA,n.mos,1),IEF = matrix(NA,n.mos,1),SHY = matrix(NA,n.mos,1))
  rownames(p3)<-last_trading_dates
  p3[1:3,c(offensive,defensive) ]<-NA
  for (i in 4:n.mos){
    p3[i,c(offensive,defensive)]<-close[(i-3),c(offensive,defensive)]
  }
  
  #calculate p6  
  p6<-data.frame(SPY = matrix(NA,n.mos,1),EFA = matrix(NA,n.mos,1),EEM = matrix(NA,n.mos,1),AGG = matrix(NA,n.mos,1),LQD = matrix(NA,n.mos,1),IEF = matrix(NA,n.mos,1),SHY = matrix(NA,n.mos,1))
  rownames(p6)<-last_trading_dates
  p6[1:6,c(offensive,defensive) ]<-NA
  for (i in 7:n.mos){
    p6[i,c(offensive,defensive)]<-close[(i-6),c(offensive,defensive)]
  }
  
  #calculate p12  
  p12<-data.frame(SPY = matrix(NA,n.mos,1),EFA = matrix(NA,n.mos,1),EEM = matrix(NA,n.mos,1),AGG = matrix(NA,n.mos,1),LQD = matrix(NA,n.mos,1),IEF = matrix(NA,n.mos,1),SHY = matrix(NA,n.mos,1))
  rownames(p12)<-last_trading_dates
  p12[1:12,c(offensive,defensive) ]<-NA
  for (i in 13:n.mos){
    p12[i,c(offensive,defensive)]<-close[(i-12),c(offensive,defensive)]
  }
  
  
  
  #calculate MOM
  
  MOM=12*((p0/p1)-1)+4*((p0/p3)-1)+2*((p0/p6)-1)+1*((p0/p12)-1)
  colnames(MOM)<-c(offensive,defensive) 
  MOM<-na.omit(MOM)


  
  
  #calculate number
  
  n.offensive <- rowSums(MOM[,offensive]>0)

  MOM<-data.frame(MOM, n.offensive=n.offensive)
  
  
  
  
  
  #确定哪些时候选offensive哪些时候选defensive
  MOM$select<-"Defensive"
  MOM[which(MOM$n.offensive==4),'select'] <- "Offensive"
  
  
  
  #If 4个offensive的MOM都大于零，则选取offensive中MOM最大的，权重为1
  MOM$asset<-NA
  for (i in which(MOM$select=="Offensive")){
    MOM[i,'asset'] <-colnames(MOM[which.max(MOM[i,offensive])])
  }
  
  #If 任何一个offensive的MOM小于零，则选取defensive中MOM最大的，权重为1
  for (i in which(MOM$select=="Defensive")){
    MOM[i,'asset'] <- colnames(MOM[(which.max(MOM[i,defensive])+4)])
  }
  
  #做一个权重表
  weights <- data.frame(date=as.Date(rownames(MOM)),asset=MOM$asset,SPY = matrix("SPY",length(MOM[,1]),1),EFA = matrix("EFA",length(MOM[,1]),1),EEM = matrix("EEM",length(MOM[,1]),1),AGG = matrix("AGG",length(MOM[,1]),1),LQD = matrix("LQD",length(MOM[,1]),1),IEF = matrix("IEF",length(MOM[,1]),1),SHY = matrix("SHY",length(MOM[,1]),1))
  
  for (i in 1:length(weights[,1])){
    weights[i,3:9]<-weights[i,3:9]==weights[i,2]
  }
  
  
  
  for (i in 1:length(weights[,1])){
    weights[i,which(weights[i,]==TRUE)]<-1
    weights[i,which(weights[i,]==FALSE)]<-0
  }
  
  
  rownames(weights)<-weights$date
  weights<-weights[,c("date","SPY",  "EFA",  "EEM", "AGG","LQD","IEF","SHY")]
  
  return(weights)

}


## define Returns function
#input: all_assets
#output: assets_returns
#functionality: obtain daily returns of assets in csv form

Returns<-function(asset_price){    
  Returns <- do.call(merge.xts,lapply(colnames(asset_price),function(x){ 
    ret = periodReturn(asset_price[,x],period = "daily");
    colnames(ret) = x;
    return(ret) 
  } ))
  Returns<-as.data.frame(Returns)
  Returns<- cbind(date=as.Date(rownames(Returns)),Returns)

}




#input

start_date <- "2007-01-01"  
end_date <- "2020-07-14"  

symbols<-c("SPY",  "EFA",  "EEM", "AGG","LQD","IEF","SHY","TLT") 
offensive<-c("SPY",  "EFA",  "EEM", "AGG") 
defensive<-c("LQD","IEF","SHY") 

getSymbols(symbols, from=start_date,to=end_date)
asset_price<-list()
for(i in 1:length(symbols)) {
  asset_price[[i]] <- Cl(get(symbols[i]))  
}
asset_price <- do.call(cbind, asset_price) #xts
colnames(asset_price)<-symbols

#get csv

Returns<-Returns(asset_price)
Strategy<-VAA(asset_price)

#write.csv(Returns,"C:\\Users\\carol\\Desktop\\asset_allocation\\VAA\\assets_returns.csv", row.names = FALSE)
#write.csv(Strategy,"C:\\Users\\carol\\Desktop\\asset_allocation\\VAA\\assets_weights.csv", row.names = FALSE)



