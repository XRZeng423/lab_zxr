library(xts)
library(PerformanceAnalytics)
library(lubridate)
library(quantmod)


RAA.Balanced <- function(ret,...){
  ret1<-ret[,1]
  ret2<-ret[,2]
  ret2<-ret[,3]
  weights<-c(0.4, 0.4, 0.2)
  Return.portfolio(ret,weights,...)
}

RAA.Aggressive <- function(ret,...){
  ret1<-ret[,1]
  ret2<-ret[,2]
  ret2<-ret[,3]
  weights<-c(0.8, 0.1, 0.1)
  Return.portfolio(ret,weights,...)
}


start_date <- "2007-01-01"  
end_date <- "2020-07-14"  

symbols<-c("SPY", "VNQ", "IEF","TLT")  

getSymbols(symbols, from=start_date,to=end_date)
asset_price<-list()
for(i in 1:length(symbols)) {
  asset_price[[i]] <- Cl(get(symbols[i]))  
}
asset_price <- do.call(cbind, asset_price) #xts
colnames(asset_price)<-symbols


returns_data <- do.call(merge.xts,lapply(colnames(asset_price),function(x){ 
  ret = periodReturn(asset_price[,x],period = "monthly");
  colnames(ret) = x;
  return(ret) 
} ))



RAA.Balanced.ret<-cbind(returns_data$SPY, returns_data$VNQ, returns_data$IEF)
RAA.Balanced.portf<-RAA.Balanced(RAA.Balanced.ret, rebalance_on="months", verbose=T)

RAA.Aggressive.ret<-cbind(returns_data$SPY, returns_data$VNQ, returns_data$IEF)
RAA.Aggressive.portf<-RAA.Aggressive(RAA.Aggressive.ret, rebalance_on="months", verbose=T)


Balanced.ret<-RAA.Balanced.portf$returns
colnames(Balanced.ret)<-"RAA.Balanced"

Aggressive.ret<-RAA.Aggressive.portf$returns
colnames(Aggressive.ret)<-"RAA.Aggressive"

