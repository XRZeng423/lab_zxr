
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



ADM<-function(assets_price){
  
  #每个月最后一个交易日计算SPY和SCZ的动量得分，动量得分为过去1 3 6个月的收益的平均值
  R1<-monthly.return(assets_price[,c("SPY","SCZ")])
  R1<-as.data.frame(R1)
  R3<-lapply(R1, function(x) runSum(x, n = 3, cumulative = FALSE))
  R3<-as.data.frame(R3)
  R6<-lapply(R1, function(x) runSum(x, n = 6, cumulative = FALSE))
  R6<-as.data.frame(R6)
  MOM<-(R1+R3+R6)/3
  R6<-as.data.frame(R6)
  MOM<-na.omit(MOM)

  #C1: MOM(SPY)>MOM(SCZ)>0
  #C2: MOM(SCZ)>MOM(SPY)>0
  #C3: 两个条件都不满足
  for (i in 1:length(MOM[,1])){
    MOM[i,'C1']<-MOM[i,'SPY']>MOM[i,'SCZ'] & MOM[i,'SCZ']>0
    MOM[i,'C2']<-MOM[i,'SCZ']>MOM[i,'SPY'] & MOM[i,'SPY']>0
    MOM[i,'C3']<-rowSums(MOM[i,c("C1","C2")]==FALSE)==2
  }
  
  #做一个权重表
  weights <- MOM
  weights[,symbols]<-NA
  
  #满足C1条件的SPY权重为1，满足C2条件的SCZ权重为1
  weights[which(weights[,'C1']==TRUE),'SPY']<-1
  weights[which(weights[,'C2']==TRUE),'SCZ']<-1
  
  #满足C3条件的，在TLT和TIP中选择过去一个月月收益高的资产，权重为1
  RB<-monthly.return(assets_price[,c("TLT","TIP")])
  RB<-RB[as.Date(rownames(MOM)),]
  colnames(RB)<-c("TLT.ret","TIP.ret")
  weights<-cbind(weights,RB)
  
  for (i in 1:length(weights[,1])){
    weights[i,'C4']<-weights[i,'TLT.ret']>weights[i,'TIP.ret']
   
  }
  
  weights[which(weights[,'C3']==TRUE & weights[,'C4']==TRUE),'TLT']<-1
  weights[which(weights[,'C3']==TRUE & weights[,'C4']==FALSE),'TIP']<-1
  
  weights<-weights[,symbols]
  
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
symbols<-c("SPY","SCZ","TLT","TIP")  


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


Strategy<-ADM(all_assets)


#write.csv(Returns,"C:\\Users\\carol\\Desktop\\asset_allocation\\ADM\\assets_returns.csv", row.names = FALSE)
#write.csv(Strategy,"C:\\Users\\carol\\Desktop\\asset_allocation\\ADM\\assets_weights.csv", row.names = FALSE)

——————————————————————————————————————————————————————————————————————————————————————————————————————————————

returns_data<-xts(Returns[,-1],ymd(Returns[,1])) ## convert to xts

weights_data<-xts(Strategy[,-1],ymd(Strategy[,1])) ## convert to xts


portf <- Return.portfolio(returns_data, weights = weights_data, verbose=T)

portf.ret<-portf$returns
colnames(portf.ret)<-"ADM"

benchmark.ret<-cbind(returns_data$SPY, returns_data$TLT)
benchmark.portf<-portf.sixty_fourty(benchmark.ret, rebalance_on="months", verbose=T)
bench.ret<-benchmark.portf$returns
colnames(bench.ret)<-"Benchmark"

SPY.ret<-returns_data$SPY
colnames(SPY.ret)<-"SPY"

tab.perf(cbind(portf.ret['2008-01-01/2020-01-01'],bench.ret['2008-01-01/2020-01-01'],SPY.ret['2008-01-01/2020-01-01']))



ret<-cbind(portf.ret['2008-01-01/2020-01-01'], bench.ret['2008-01-01/2020-01-01'],SPY.ret['2008-01-01/2020-01-01'])
colnames(ret)<-c("ADM","bench","SPY")
charts.PerformanceSummary(ret, date.format="%Y%m")
