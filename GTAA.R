
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
  Returns <- do.call(merge.xts,lapply(colnames(asset_price),function(x){ 
    
    ret = periodReturn(asset_price[,x],period = "monthly");
    
    colnames(ret) = x;
    
    return(ret) 
    
  } ))
  
}



GTAA<-function(assets_price,N){

  
  #每个月最后一个交易日计算13个资产的过去1、3、6、12个月的收益（R1, R3, R6, R12）
  R1<-monthly.return(asset_price)
  R1<-as.data.frame(R1)
  R3<-lapply(R1, function(x) runSum(x, n = 3, cumulative = FALSE))
  R3<-as.data.frame(R3)
  R6<-lapply(R1, function(x) runSum(x, n = 6, cumulative = FALSE))
  R6<-as.data.frame(R6)
  R12<-lapply(R1, function(x) runSum(x, n = 12, cumulative = FALSE))
  R12<-as.data.frame(R12)
  
  #对每个资产计算（R1, R3, R6, R12）的平均值
  R_ave<-(R1+R3+R6+R12)/4
  R_ave<-na.omit(R_ave)
  
  #对每个资产计算过去10个月移动平均
  MA200_prices<-filter(asset_price/200, rep(1, 200))
  colnames(MA200_prices)<-asset
  MA200_prices<-as.data.frame(MA200_prices)
  MA200_prices<- cbind(date=as.Date(rownames(as.data.frame(asset_price))),MA200_prices)
  MA200_prices<-na.omit(MA200_prices)
  MA200_prices<-xts(MA200_prices, order.by=as.Date(MA200_prices[,1]))

  R_ave<- cbind(date=as.Date(rownames(as.data.frame(R_ave))),R_ave)
  period.ends = endpoints(R_ave, 'months')
  period.ends = period.ends[period.ends > 0] 
  n.mos  <- length(period.ends)
  last_trading_dates = R_ave$date[period.ends]
  
  MA200_prices<-MA200_prices[last_trading_dates,]
  MA200_prices<-MA200_prices[,asset]
  R_ave<-R_ave[,asset]  
  
  asset_price<-asset_price[last_trading_dates,]
  asset_price<-asset_price[1:length(MA200_prices[,1]),]
  
  
  
  #持有平均值最高的3或6个资产

  selected<-data.frame(date=last_trading_dates,matrix(numeric(0),n.mos,ncol=N))
  for (i in 1:n.mos){
    selected[i,2:(N+1)]<-colnames(sort(R_ave[i,c("IWD", "MTUM", "IWN", "IWM", "EFA", "EEM", "IEF", "BWX", "LQD",  "TLT",  "DBC", "GLD","VNQ")],decreasing = TRUE)[1:N])
  }
  
  
  # 选择收盘价高于MA(200)的资产，做一个调整表，需要调整的地方是false否则是true
  selected.adjust<-selected[1:length(MA200_prices[,1]),]
  for (i in 1:length(MA200_prices[,1])){
    selected.adjust[i,2:(N+1)] <- as.numeric(asset_price[i,as.character(selected.adjust[i,2:(N+1)])])>=as.numeric(MA200_prices[i,as.character(selected.adjust[i,2:(N+1)])])
  }
  
  #做一个权重表
  weights <- R_ave
  weights[,]<-NA
  weights<-cbind(selected,weights)
  weights<-weights[1:length(MA200_prices[,1]),]
  

  for (i in 1:length(MA200_prices[,1])){
    for (j in 2:(N+1)){
      weights[i,weights[i,j]]<-1/N
    }}
  
  #对于收盘价低于MA的资产，权重为0，增加SHV的权重
  selected.adjust$n <- rowSums(selected.adjust==FALSE)
  weights[,"SHV"]<-selected.adjust$n*(1/N)
  
  adjust<-selected.adjust[,1:(N+1)]

  
  for (i in 1:length(MA200_prices[,1])){
    adjust[i,which(adjust[i,]==TRUE)]<-NA
    adjust[i,which(adjust[i,]==FALSE)]<-selected[i,which(adjust[i,]==FALSE)]
  }
  
  for (i in 1:length(MA200_prices[,1])){
    weights[i,as.character(na.omit(as.character(adjust[i,2:(N+1)])))]<-0
  }
  weights[is.na(weights)] <- 0
  
    
  weights<-weights[,-(2:(N+1))]
  
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
symbols<-c("IWD", "MTUM", "IWN", "IWM", "EFA", "EEM", "IEF", "BWX", "LQD",  "TLT",  "DBC", "GLD","VNQ","SHV","SPY")  


start_date <- "2014-01-01"  
end_date <- "2020-07-14"  

#download financial data
 
getSymbols(symbols, from=start_date,to=end_date)
all_assets<-list()
for(i in 1:length(symbols)) {
  all_assets[[i]] <- Cl(get(symbols[i]))  
}
all_assets <- do.call(cbind, all_assets) #xts
colnames(all_assets)<-symbols
asset<-c("IWD", "MTUM", "IWN", "IWM", "EFA", "EEM", "IEF", "BWX", "LQD",  "TLT",  "DBC", "GLD","VNQ","SHV")
asset_price<-all_assets[,asset]

#get csv
Returns<-Returns(all_assets)
GTAA3<-GTAA(asset_price,3)
GTAA6<-GTAA(asset_price,6)

write.csv(Returns,"C:\\Users\\carol\\Desktop\\asset_allocation\\GTAA\\assets_returns.csv", row.names = FALSE)
write.csv(GTAA3,"C:\\Users\\carol\\Desktop\\asset_allocation\\GTAA\\assets_weights3.csv", row.names = FALSE)
write.csv(GTAA6,"C:\\Users\\carol\\Desktop\\asset_allocation\\GTAA\\assets_weights6.csv", row.names = FALSE)

——————————————————————————————————————————————————————————————————————————————————————————————————————————————

returns_data<-xts(Returns[,-1],ymd(Returns[,1])) ## convert to xts

weights_data3<-xts(GTAA3[,-1],ymd(GTAA3[,1])) ## convert to xts
weights_data6<-xts(GTAA6[,-1],ymd(GTAA6[,1])) ## convert to xts

portf3 <- Return.portfolio(returns_data, weights = weights_data3, verbose=T)
portf6 <- Return.portfolio(returns_data, weights = weights_data6, verbose=T)

portf.ret3<-portf3$returns
colnames(portf.ret3)<-"GTAA3"

portf.ret6<-portf6$returns
colnames(portf.ret6)<-"GTAA6"

benchmark.ret<-cbind(returns_data$SPY, returns_data$TLT)
benchmark.portf<-portf.sixty_fourty(benchmark.ret, rebalance_on="months", verbose=T)
bench.ret<-benchmark.portf$returns
colnames(bench.ret)<-"Benchmark"

SPY.ret<-returns_data$SPY
colnames(SPY.ret)<-"SPY"

tab.perf(cbind(portf.ret3['2015-01-01/2020-01-01'],portf.ret6['2015-01-01/2020-01-01'], bench.ret['2015-01-01/2020-01-01'],SPY.ret['2015-01-01/2020-01-01']))
