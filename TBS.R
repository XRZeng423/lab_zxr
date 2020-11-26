
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



TBS<-function(assets_price){
  
  #每个月最后一个交易日计算各个资产的6个月的收益
  R1<-monthly.return(assets_price)
  R6<-lapply(R1, function(x) runSum(x, n = 6, cumulative = FALSE))
  R6<-as.data.frame(R6)
  R6<-na.omit(R6)
  
  #选取6个月收益最高的3个资产
  for (i in 1:length(R6[,1])){
    R6[i,10:12]<-colnames(sort(R6[i,c("SHY", "IEF", "TLT","TIP", "LQD", "HYG", "BNDX", "EMB") ],decreasing = TRUE)[1:3])
  }
  
  #计算所选3个资产收益为正的个数n
  for (i in 1:length(R6[,1])){
    R6[i,'n']<-rowSums(R6[i,as.character(R6[i,10:12])]>0)
  }
  
  #计算所选3个资产d的收益大于BIL的收益的个数p
  for (i in 1:length(R6[,1])){
    R6[i,'p']<-rowSums(R6[i,as.character(R6[i,10:12])]>R6[i,'BIL'])
  }

  #资产收益大于0且大于BIL收益记为TRUE
  for (i in 1:length(R6[,1])){
    R6[i,'B1']<-R6[i,as.character(R6[i,10])]>0 & R6[i,as.character(R6[i,10])]>R6[i,'BIL']
    R6[i,'B2']<-R6[i,as.character(R6[i,11])]>0 & R6[i,as.character(R6[i,11])]>R6[i,'BIL']
    R6[i,'B3']<-R6[i,as.character(R6[i,12])]>0 & R6[i,as.character(R6[i,12])]>R6[i,'BIL']
  }
  
  #做一个权重表
  weights <- R6
  weights[,symbols]<-NA
  
  
  #如果资产收益为正，而且大于BIL的6个月收益，则该资产分配1/3的权重
  #也就是TRUE的资产分配1/3的权重
  #先给所有选出的资产1/3的权重

  for (i in 1:length(weights[,1])){
    for (j in 10:12){
      weights[i,weights[i,j]]<-1/3
    }}
  
  #计算每一行所选资产中False的数量n
  weights$n <- rowSums(weights[,13:15]==FALSE)
  
  #每行BIL的权重为n*(1/3)
  weights[,'BIL']<-weights$n*(1/3)
  
  #将每行FALSE的资产的权重记为0

  for (i in 1:length(weights[,1])){
    weights[i,as.character(weights[i,(which(weights[i,c('B1','B2','B3')]==FALSE)+9)])]<-0
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
symbols<-c("SHY", "IEF", "TLT","TIP", "LQD", "HYG", "BNDX", "EMB", "BIL")  


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
assets_price<-all_assets


#get csv
Returns<-Returns(all_assets)
Strategy<-TBS(all_assets)


#write.csv(Returns,"C:\\Users\\carol\\Desktop\\asset_allocation\\TBS\\assets_returns.csv", row.names = FALSE)
#write.csv(Strategy,"C:\\Users\\carol\\Desktop\\asset_allocation\\TBS\\assets_weights.csv", row.names = FALSE)

——————————————————————————————————————————————————————————————————————————————————————————————————————————————

returns_data<-xts(Returns[,-1],ymd(Returns[,1])) ## convert to xts

weights_data<-xts(Strategy[,-1],ymd(Strategy[,1])) ## convert to xts


portf <- Return.portfolio(returns_data, weights = weights_data, verbose=T)

portf.ret<-portf$returns
colnames(portf.ret)<-"TBS"

equal_weights<-rep(1/ncol(returns_data),ncol(returns_data)) 
bench.ret<-Return.portfolio(returns_data,weights = equal_weights,rebalance_on = "months")
colnames(bench.ret)<-"Benchmark"



tab.perf(cbind(portf.ret['2015-01-01/2020-01-01'],bench.ret['2015-01-01/2020-01-01']))

ret<-cbind(portf.ret['2014-01-01/2020-01-01'], bench.ret['2014-01-01/2020-01-01'])
colnames(ret)<-c("QSF","bench")
charts.PerformanceSummary(ret, date.format="%Y%m")
