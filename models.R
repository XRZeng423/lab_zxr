

GPM<-function(all_assets,N, P){
  
  all_assets<-wdata
  #split into two part: Rrisky and Rcp
  Rrisky_price_data<- subset(all_assets, select = Risky)
  Rcp_price_data<-subset(all_assets, select = cp)
  
  #calculate ri 
  #output: Risky.ret(xts)
  Rrisky_price_data <-na.locf(Rrisky_price_data)
  Rrisky_price_data <-as.xts(Rrisky_price_data)
  Risky_daily.ret<-na.omit(ROC(Rrisky_price_data))
  
  Risky_monthly.ret<-lapply(Rrisky_price_data,function(x) periodReturn(x,period = "monthly"))
  Risky_monthly.ret<-as.data.frame(Risky_monthly.ret)
  colnames(Risky_monthly.ret)<-Risky
  
  Risky.ret<-lapply(Risky_monthly.ret, function(x) runSum(x, n = P, cumulative = FALSE))#change length of period to calculate zi
  Risky.ret<-as.data.frame(Risky.ret)
  colnames(Risky.ret)<-Risky
  P1<-P-1
  Risky.ret[c(1:P1),]<-Risky_monthly.ret[c(1:P1),] #use monthly return to replace NA
  
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
    selected[i,]<-colnames(sort(Risky.z[i,],decreasing = TRUE)[1:N])
  }
  Risky.z <- cbind(date=as.Date(rownames(Risky_monthly.ret)),Risky.z) #dataframe
  selected <- cbind(date=as.Date(rownames(Risky_monthly.ret)),selected) #dataframe
  selected<-xts(selected[,-1], ymd(selected[,1])) # convert to xts
  selected <- as.data.frame(selected)
  selected <- cbind(date=as.Date(rownames(Risky_monthly.ret)),selected) #dataframe
  assets_weights<-data.frame(weights)
  assets_weights<- cbind(date=as.Date(rownames(Risky_monthly.ret)),assets_weights)
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

