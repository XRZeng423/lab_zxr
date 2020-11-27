
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



TDM<-function(assets_price){
  
  #每个月最后一个交易日计算SPY和SCZ的动量得分，动量得分为过去1 3 6个月的收益的平均值
  R1<-monthly.return(assets_price)
  R12<-lapply(R1, function(x) runSum(x, n = 12, cumulative = FALSE)) #N为过去N个月收益
  R12<-as.data.frame(R12)
  R12<-na.omit(R12)
  
  #C1: SPY.ret>VEU.ret
  #C2: SPY.ret>BIL.ret
  #C3: VEU.ret>BIL.ret
  for (i in 1:length(R12[,1])){
    R12[i,'C1']<-R12[i,'SPY']>R12[i,'VEU'] 
    R12[i,'C2']<-R12[i,'SPY']>R12[i,'BIL'] 
    R12[i,'C3']<-R12[i,'VEU']>R12[i,'BIL'] 

  }
  
  #做一个权重表
  weights <- R12
  weights[,symbols]<-NA
  
  #C1和C2都为TRUE，即SPY>VEU且SPY>BIL,则全部配置SPY
  #C1为FALSE且C3为TRUE，即VEU>SPY且VEU>BIL,则全部配置VEU
  #C2和C3都为FALSE，即BIL>VEU且BIL>SPY，则全部配置BND
  weights[which(weights[,'C1']==TRUE & weights[,'C2']==TRUE),'SPY']<-1
  weights[which(weights[,'C1']==FALSE & weights[,'C3']==TRUE),'VEU']<-1
  weights[which(weights[,'C2']==FALSE & weights[,'C3']==FALSE),'BND']<-1
  
  
  weights<-weights[,c("SPY","VEU","BND")]
  
  weights[is.na(weights)] <- 0
  
  
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
symbols<-c("SPY","VEU","BND","BIL")  



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

#现金
#all_assets<-as.data.frame(all_assets)
#cash<-data.frame(date=as.Date(rownames(all_assets)),cash=0)
#all_assets<-cbind(cash, all_assets)
#all_assets<-xts(all_assets[,-1],ymd(all_assets[,1])) ## convert to xts

assets_price<-all_assets



#get csv
Returns<-Returns(all_assets)


Strategy<-TDM(all_assets)


write.csv(Returns,"C:\\Users\\carol\\Desktop\\asset_allocation\\TDM\\assets_returns.csv", row.names = FALSE)
write.csv(Strategy,"C:\\Users\\carol\\Desktop\\asset_allocation\\TDM\\assets_weights.csv", row.names = FALSE)

——————————————————————————————————————————————————————————————————————————————————————————————————————————————

returns_data<-xts(Returns[,-1],ymd(Returns[,1])) ## convert to xts

weights_data<-xts(Strategy[,-1],ymd(Strategy[,1])) ## convert to xts


portf <- Return.portfolio(returns_data, weights = weights_data, verbose=T)

portf.ret<-portf$returns
colnames(portf.ret)<-"TDM"

benchmark.ret<-cbind(returns_data$SPY, returns_data$BND)
benchmark.portf<-portf.sixty_fourty(benchmark.ret, rebalance_on="months", verbose=T)
bench.ret<-benchmark.portf$returns
colnames(bench.ret)<-"Benchmark"

SPY.ret<-returns_data$SPY
colnames(SPY.ret)<-"SPY"

BND.ret<-returns_data$BND
colnames(BND.ret)<-"BND"

VEU.ret<-returns_data$VEU
colnames(VEU.ret)<-"VEU"

tab.perf(cbind(portf.ret['2008-01-01/2020-01-01'],bench.ret['2008-01-01/2020-01-01'],SPY.ret['2008-01-01/2020-01-01'],BND.ret['2008-01-01/2020-01-01'],VEU.ret['2008-01-01/2020-01-01']))



ret<-cbind(portf.ret['2008-01-01/2020-01-01'], bench.ret['2008-01-01/2020-01-01'],SPY.ret['2008-01-01/2020-01-01'],BND.ret['2008-01-01/2020-01-01'],VEU.ret['2008-01-01/2020-01-01'])
colnames(ret)<-c("TDM","bench","SPY","BND","VEU")
charts.PerformanceSummary(ret, date.format="%Y%m")

