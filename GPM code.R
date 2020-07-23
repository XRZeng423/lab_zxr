#input: assets code, date, number of risky assets chosen
#output: assets returns, weights in csv
#functionality: calculate assets weights based on Generalized Protective Momentum 

library(xts)
library(PerformanceAnalytics)
library(lubridate)
library(quantmod)

setwd("d:/Users/XuranZENG/Desktop/asset_allocation")
source("cal_ret.R")


## define strategy function
#input: all_assets
#output: assets_weights
#functionality: use GPM to calculate weights of each asset and obtain them in csv form

GPM<-function(all_assets,N){
  #split into two part: Rrisky and Rcp
  Rrisky_price_data<- subset(all_assets, select = Risky)
  Rcp_price_data<-subset(all_assets, select = cp)
  
  #calculate ri 
  #output: Risky.ret(xts)
  Rrisky_price_data <-na.locf(Rrisky_price_data)
  Risky_daily.ret<-na.omit(ROC(Rrisky_price_data))
  
  Risky.ret<-lapply(Rrisky_price_data,function(x) periodReturn(x,period = "monthly"))
  Risky.ret<-as.data.frame(Risky.ret)
  colnames(Risky.ret)<-Risky
  
  #calculate ci
  #output: Risky.cor(num)
  universe_portf.ret<-periodReturn(Rrisky_price_data,period="daily")[-1,]
  Risky.cor<-lapply(Risky_daily.ret, function(x) cor(x, universe_portf.ret))
  Risky.cor<-unlist(Risky.cor)
  
  #calculate zi
  #output: Risky.z(dataframe)
  Risky.z<-as.data.frame(Risky.ret) * (1-Risky.cor)
  
  #calculate number

  n <- rowSums(Risky.z>0)
  
  #calculate weights(wcp and wRisky)
  NR<-length(Risky)
  weights<-data.frame(n=n, wcp=0, wRisky=0)
  weights[which(weights$n<=NR*0.5),'wcp']<-1
  w<-(NR-weights$n)/(NR*0.5)
  weights[which(weights$n>NR*0.5),'wcp']<-w[which(weights$n>NR*0.5)]
  weights[which(weights$n>NR*0.5),'wRisky']<-1-w[which(weights$n>NR*0.5)]
  

  
  
  #select Risky assets
  selected<-as.data.frame(matrix(numeric(0),ncol=N))
  for (i in 1:length(w)){
    selected[i,]<-colnames(sort(Risky.z[i,],decreasing = TRUE)[1:n])
  }
  
  selected <- cbind(date=as.Date(rownames(Risky.z)),selected) #dataframe
  selected<-xts(selected[,-1], ymd(selected[,1])) # convert to xts
  selected <- as.data.frame(selected)
  selected <- cbind(date=as.Date(rownames(Risky.z)),selected) #dataframe
  assets_weights<-data.frame(weights)
  assets_weights<- cbind(date=as.Date(rownames(Risky.z)),assets_weights)
  assets_weights<-merge(selected, assets_weights, by="date")

  #calculate assets weights  
  #weights of crash protection assets
  for(i in cp){
    assets_weights[i]<-0.5*assets_weights$wcp
  }
  
  #weights of risky assets
  N1<-N+1
  for (i in 1:length(w)){
    for (j in 2:N1){
      assets_weights[i,assets_weights[i,j]]<-1/N*assets_weights$wRisky[i]
    }}
  
  assets_weights[is.na(assets_weights)] <- 0
  row_eliminate=4+N
  assets_weights<-subset(assets_weights, select = -c(2:row_eliminate))
  
  
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
Risky<-c("SPY", "QQQ", "IWM", "VGK", "EWJ", "EEM", "VNQ", "DBC", "GLD", "LQD", "HYG", "TLT")  
cp<-c("IEF","SHY")  

start_date <- "2008-01-01"  
end_date <- "2020-07-14"  

N<-4  #number of risky assets chosen

#download financial data
symbols<-merged.list<-c(Risky, cp)    
getSymbols(symbols, from=start_date,to=end_date)
all_assets<-list()
for(i in 1:length(symbols)) {
  all_assets[[i]] <- Cl(get(symbols[i]))  
}
all_assets <- do.call(cbind, all_assets) #xts
colnames(all_assets)<-symbols

#get csv
Returns<-Returns(all_assets)
Strategy<-GPM(all_assets,N)

write.csv(Returns,"d:\\Users\\XuranZENG\\Desktop\\assets_returns.csv", row.names = FALSE)
write.csv(Strategy,"d:\\Users\\XuranZENG\\Desktop\\assets_weights.csv", row.names = FALSE) 
