
library(xts)
library(PerformanceAnalytics)
library(lubridate)
library(quantmod)
library(TTR)

source("C:/Users/carol/Desktop/asset_allocation/cal_ret.R")


monthly.return<-function(ret){    
  ### this function takes daily prices and outputs monthly returns
  ### Args:
  ### ret: xts object of daily prices
  ### output:
  ### xts of monthly returns
  Returns <- do.call(merge.xts,lapply(colnames(ret),function(x){ 
    
    ret = periodReturn(ret[,x],period = "monthly");
    
    colnames(ret) = x;
    
    return(ret) 
    
  } ))
  
}



QSF<-function(assets_price){
  
  #每个月最后一个交易日计算5个资产的过去3个月的总收益
  R1<-monthly.return(assets_price)
  R3<-lapply(R1, function(x) runSum(x, n = 3, cumulative = FALSE))
  R3<-as.data.frame(R3)
  R3<-na.omit(R3)
  
  R3$max<-NA
  for (i in 1:length(R3[,1])){
    R3[i,'max']<-colnames(sort(R3[i,c("SPY", "QQQ", "EFA", "EEM", "TLT")],decreasing = TRUE)[1:1])
  }
  
  
  period.ends = endpoints(assets_price, 'months')
  period.ends = period.ends[period.ends > 0] 
  n.mos  <- length(period.ends)
  last_trading_dates = as.Date(rownames(as.data.frame(assets_price)))[period.ends]
  

  #每个月最后一个交易日计算5个risk assets中为负的数量
  R3$n <- rowSums(R3[,c("SPY", "QQQ", "EFA", "EEM", "TLT") ]<0)
  
  #做一个权重表
  weights <- R3
  weights[,1:6]<-NA
  
  
  #如果任意一个资产的3个月收益为负(即为负的资产数量n>0)，则全部配置defensive asset
  weights[which(weights[,7]>0),'IEF']<-1
  
  #如果所有资产的3个月收益都为正，则全部配置3个月收益最高的资产

  

  row<-which(weights[,'n']==0)
  col<-R3[which(R3$n==0),'max']
  for (i in 1:length(row)){
    
    weights[row[i],col[i]]<-1
  }
  
  weights[is.na(weights)] <- 0
  
  weights<-weights[,symbols]
  weights<- cbind(date=as.Date(rownames(weights)),weights)

}


## define Returns function
#input: all_assets
#output: assets_returns
#functionality: obtain daily returns of assets in csv form

Returns<-function(all_assets){    
  Returns <- do.call(merge.xts,lapply(colnames(all_assets),function(x){ 
    ret = periodReturn(all_assets[,x],period = "daily");
    colnames(ret) = x;
    return(ret) 
  } ))
  Returns<-as.data.frame(Returns)
  Returns<- cbind(date=as.Date(rownames(Returns)),Returns)
}




#input
symbols<-c("SPY", "QQQ", "EFA", "EEM", "TLT", "IEF")  


start_date <- "2008-01-01"  
end_date <- "2020-07-14"  

#download financial data

getSymbols(symbols, from=start_date,to=end_date)
all_assets<-list()
for(i in 1:length(symbols)) {
  all_assets[[i]] <- Cl(get(symbols[i]))  
}
all_assets <- do.call(cbind, all_assets) #xts
colnames(all_assets)<-symbols
assets_price<-all_assets


#get csv
Returns<-Returns(all_assets)
Strategy<-QSF(all_assets)


write.csv(Returns,"C:\\Users\\carol\\Desktop\\asset_allocation\\QSF\\assets_returns.csv", row.names = FALSE)
write.csv(Strategy,"C:\\Users\\carol\\Desktop\\asset_allocation\\QSF\\assets_weights.csv", row.names = FALSE)

——————————————————————————————————————————————————————————————————————————————————————————————————————————————

returns_data<-xts(Returns[,-1],ymd(Returns[,1])) ## convert to xts

weights_data<-xts(Strategy[,-1],ymd(Strategy[,1])) ## convert to xts


portf <- Return.portfolio(returns_data, weights = weights_data, verbose=T)

portf.ret<-portf$returns
colnames(portf.ret)<-"QSF"



benchmark.ret<-cbind(returns_data$SPY, returns_data$TLT)
benchmark.portf<-portf.sixty_fourty(benchmark.ret, rebalance_on="months", verbose=T)
bench.ret<-benchmark.portf$returns
colnames(bench.ret)<-"Benchmark"

SPY.ret<-returns_data$SPY
colnames(SPY.ret)<-"SPY"

tab.perf(cbind(portf.ret['2015-01-01/2020-01-01'],bench.ret['2015-01-01/2020-01-01'],SPY.ret['2015-01-01/2020-01-01']))
