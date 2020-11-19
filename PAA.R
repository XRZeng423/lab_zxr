#input: assets code, date, number of risky assets chosen, length of period to calculate zi
#output: assets returns, weights in csv
#functionality: calculate assets weights based on Generalized Protective Momentum 

library(xts)
library(PerformanceAnalytics)
library(lubridate)
library(quantmod)
library(TTR)



## define strategy function
#input: all_assets, number of risky assets chosen, length of period to calculate zi
#output: assets_weights
#functionality: use GPM to calculate weights of each asset and obtain them in csv form

PAA<-function(all_assets,N, P){
  #split into two part: Rrisky and Rcp
  Rrisky_price_data<- subset(all_assets, select = Risky)
  Rcp_price_data<-subset(all_assets, select = cp)
  
  Rcp_price_data<-as.data.frame(Rcp_price_data)
  Rcp_price_data<- cbind(date=as.Date(rownames(as.data.frame(Rcp_price_data))),Rcp_price_data)
  
  period.ends = endpoints(all_assets, 'months')
  period.ends = period.ends[period.ends > 0] 
  n.mos  <- length(period.ends)
  last_trading_dates = Rcp_price_data$date[period.ends]
  

  
  #calculate close
  #output: close (xts)
  close<-as.data.frame(Rrisky_price_data[period.ends,])


  #calculate SMA
  SMA<-as.data.frame(filter(close,rep(1/P,P),sides=1))
  colnames(SMA)<-Risky
  SMA<-cbind(last_trading_dates,SMA)
  rownames(SMA)<-SMA[,1]
  SMA<-SMA[,-1]
  
  #calculate MOM
  MOM<-(close/SMA)-1
  MOM<-na.omit(MOM)

  
  #calculate number
  
  n <- rowSums(MOM>0)
  
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
    selected[i,]<-colnames(sort(MOM[i,],decreasing = TRUE)[1:N])
  }
  MOM <- cbind(date=as.Date(rownames(MOM)),MOM) #dataframe
  selected <- cbind(date=as.Date(rownames(MOM)),selected) #dataframe
  selected<-xts(selected[,-1], ymd(selected[,1])) # convert to xts
  selected <- as.data.frame(selected)
  selected <- cbind(date=as.Date(rownames(MOM)),selected) #dataframe
  assets_weights<-data.frame(weights)
  assets_weights<- cbind(date=as.Date(rownames(MOM)),assets_weights)
  assets_weights<-merge(selected, assets_weights, by="date")
  
  #calculate assets weights  
  #weights of crash protection assets
  for(i in cp){
    assets_weights[i]<-0.5*assets_weights$wcp
  }
  
  assets_weights$date <- as.character(assets_weights$date) #dont understand why but it works
  
  #weights of risky assets
  N1<-N+1
  for (i in 1:length(w)){
    for (j in 2:N1){
      assets_weights[i,assets_weights[i,j]]<-(assets_weights$wRisky[i])/N
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

N<-6  #number of risky assets chosen
P<-13  #number of months used to calculate zi (1month, 3months, 6months, etc)

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
Strategy<-PAA(all_assets,N,P)

write.csv(Returns,"C:\\Users\\carol\\Desktop\\asset_allocation\\PAA\\assets_returns.csv", row.names = FALSE)
write.csv(Strategy,"C:\\Users\\carol\\Desktop\\asset_allocation\\PAA\\assets_weights.csv", row.names = FALSE)
