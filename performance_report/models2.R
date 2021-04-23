

GPM<-function(asset_price,N, P){
  
  symbols<-c("SPY",  "QQQ",  "IWM", "VGK","EWJ","EEM","VNQ","DBC",  "GLD",  "LQD", "HYG","TLT","IEF","SHV") 
  Risky<-c("SPY",  "QQQ",  "IWM", "VGK","EWJ","EEM","VNQ","DBC",  "GLD",  "LQD", "HYG","TLT") 
  cp<-c("IEF","SHV") 
  
  colnames(assets_price)<-symbols
  #asset_price<-as.xts(assets_price)
  asset_price<-assets_price
  
  
  #输入xts格式的日收益序列
  #输出各资产权重
  #all_assets<-wdata
  #split into two part: Rrisky and Rcp
  Rrisky_price_data<- subset(asset_price, select = Risky)
  Rcp_price_data<-subset(asset_price, select = cp)
  
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
    assets_weights[i]<-(1/2)*assets_weights$wcp
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
  
  #新增
  returns.daily <-as.xts(na.omit(ROC(asset_price)) )
  returns_data<-returns.daily
  weights_data<-xts(assets_weights[,-1],ymd(assets_weights[,1]))
  portf <- Return.portfolio(returns_data, weights_data, verbose=T, rebalance_on = "months")
  portf.ret<-portf$returns
  
  #计算每月drawdown
  periods.end.idx <- endpoints(portf.ret, 'months')  #all of the month ends and the last date
  periods.end.dt<-rownames(as.data.frame(portf.ret))[periods.end.idx]
  adjust<-assets_weights[-1,]
  for (i in 1:(length(periods.end.idx)-1)){
    adjust[i,'d']<-rownames(as.data.frame(portf.ret))[periods.end.idx[(i+1)]]
    adjust[i,'mdd']<-maxDrawdown(portf.ret[(periods.end.idx[i]+1):periods.end.idx[(i+1)]])
  }
  
  #选出月回撤高于3%的月份下一个月标记为1，调整权重
  adjust[(which(abs(adjust[,'mdd'])>0.03)+1),'adjust']<-TRUE
  adjust<-adjust[-nrow(adjust),]
  
  #若前一个月回撤高于3%，则这一个月全部持有国债，同时计算该月策略回撤，若高于3%则下一个月继续持有国债，若低于3%则下一个月按照策略决定权重
  adjust[which(adjust[,'adjust']==TRUE),symbols]<-0
  adjust[which(adjust[,'adjust']==TRUE),'SHV']<-1
  
  weight.adjust<-adjust[,colnames(assets_weights),drop=TRUE]
  
  
}

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

Returns<-function(all_assets){    
  Returns <- do.call(merge.xts,lapply(colnames(all_assets),function(x){ 
    ret = periodReturn(all_assets[,x],period = "daily");
    colnames(ret) = x;
    return(ret) 
  } ))
  Returns<-as.data.frame(Returns)
  Returns<- cbind(date=as.Date(rownames(Returns)),Returns)
}

GTAA<-function(assets_price,N){
  
  #输入：各资产日收盘价dataframe，选取资产个数N
  #输出：各资产权重
  
  asset<-c("IWD", "MTUM", "IWN", "IWM", "EFA", "EEM", "IEF", "BWX", "LQD",  "TLT",  "DBC", "GLD","VNQ","SHV")
  
  colnames(assets_price)<-asset
  #asset_price<-as.xts(assets_price)
  asset_price<-assets_price
  
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
  MA200_prices<-filter(asset_price/256, rep(1, 256))
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
  
  #新增
  returns.daily <-as.xts(na.omit(ROC(assets_price)) )
  returns_data<-returns.daily
  weights_data<-xts(weights[,-1],ymd(weights[,1]))
  portf <- Return.portfolio(returns_data, weights_data, verbose=T, rebalance_on = "months")
  portf.ret<-portf$returns
  
  #计算每月drawdown
  periods.end.idx <- endpoints(portf.ret, 'months')  #all of the month ends and the last date
  periods.end.dt<-rownames(as.data.frame(portf.ret))[periods.end.idx]
  adjust<-weights
  for (i in 1:(length(periods.end.idx)-1)){
    adjust[i,'d']<-rownames(as.data.frame(portf.ret))[periods.end.idx[(i+1)]]
    adjust[i,'mdd']<-maxDrawdown(portf.ret[(periods.end.idx[i]+1):periods.end.idx[(i+1)]])
  }
  
  #选出月回撤高于3%的月份下一个月标记为1，调整权重
  adjust[(which(abs(adjust[,'mdd'])>0.03)),'adjust']<-TRUE
  #adjust<-adjust[-nrow(adjust),]
  
  
  #若前一个月回撤高于3%，则这一个月全部持有国债，同时计算该月策略回撤，若高于3%则下一个月继续持有国债，若低于3%则下一个月按照策略决定权重
  adjust[which(adjust[,'adjust']==TRUE),symbols]<-0
  adjust[which(adjust[,'adjust']==TRUE),'SHV']<-1
  
  weight.adjust<-adjust[,colnames(weights),drop=TRUE]
  
  
  
}

ADM<-function(assets_price){
  #输入：各资产日收盘价dataframe
  #输出：各资产权重
  
  symbols<-c("SPY","SCZ","TLT","TIP") 
  colnames(assets_price)<-symbols
  
  
  
  #每个月最后一个交易日计算SPY和SCZ的动量得分，动量得分为过去1 3 6个月的收益的平均值
  R1<-monthly.return(as.xts(assets_price[,c("SPY","SCZ")]))
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
  RB<-monthly.return(as.xts(assets_price[,c("TLT","TIP")]))
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
  
  #新增
  returns.daily <-as.xts(na.omit(ROC(assets_price)) )
  returns_data<-returns.daily
  weights_data<-xts(weights[,-1],ymd(weights[,1]))
  portf <- Return.portfolio(returns_data, weights_data, verbose=T, rebalance_on = "months")
  portf.ret<-portf$returns
  
  #计算每月drawdown
  periods.end.idx <- endpoints(portf.ret, 'months')  #all of the month ends and the last date
  periods.end.dt<-rownames(as.data.frame(portf.ret))[periods.end.idx]
  adjust<-weights[-1,]
  for (i in 1:(length(periods.end.idx)-1)){
    adjust[i,'d']<-rownames(as.data.frame(portf.ret))[periods.end.idx[(i+1)]]
    adjust[i,'mdd']<-maxDrawdown(portf.ret[(periods.end.idx[i]+1):periods.end.idx[(i+1)]])
  }
  
  #选出月回撤高于3%的月份下一个月标记为1，调整权重
  adjust[(which(abs(adjust[,'mdd'])>0.03)+1),'adjust']<-TRUE
  adjust<-adjust[-nrow(adjust),]
  
  #若前一个月回撤高于3%，则这一个月全部持有国债，同时计算该月策略回撤，若高于3%则下一个月继续持有国债，若低于3%则下一个月按照策略决定权重
  adjust[which(adjust[,'adjust']==TRUE),symbols]<-0
  adjust[which(adjust[,'adjust']==TRUE),'TIP']<-1
  
  weight.adjust<-adjust[,colnames(weights),drop=TRUE]
  
  
}

RAA.Balanced <- function(ret,...){
  

  #新增
  returns.daily <-as.xts(na.omit(ROC(assets_price)) )
  
  weights<-returns.daily
  weights[,1]<-0.4
  weights[,2]<-0.4
  weights[,3]<-0.2
  
  returns_data<-returns.daily
  portf <- Return.portfolio(returns_data, weights, verbose=T, rebalance_on = "months")
  portf.ret<-portf$returns
  
  #计算每月drawdown
  periods.end.idx <- endpoints(portf.ret, 'months')  #all of the month ends and the last date
  periods.end.dt<-rownames(as.data.frame(portf.ret))[periods.end.idx]
  adjust<-as.data.frame(weights[periods.end.dt])
  for (i in 1:(length(periods.end.idx)-1)){
    adjust[i,'d']<-rownames(as.data.frame(portf.ret))[periods.end.idx[(i+1)]]
    adjust[i,'mdd']<-maxDrawdown(portf.ret[(periods.end.idx[i]+1):periods.end.idx[(i+1)]])
  }
  

  
  #选出月回撤高于3%的月份下一个月标记为1，调整权重
  adjust[(which(abs(adjust[,5])>0.05)+1),'adjust']<-TRUE
  adjust<-adjust[-nrow(adjust),]
  
  #若前一个月回撤高于3%，则这一个月全部持有国债，同时计算该月策略回撤，若高于3%则下一个月继续持有国债，若低于3%则下一个月按照策略决定权重
  adjust[which(adjust[,'adjust']==TRUE),1:3]<-0
  adjust[which(adjust[,'adjust']==TRUE),3]<-1
  
  weight.adjust<-adjust[,colnames(weights),drop=TRUE]
  
}

RAA.Aggressive <- function(ret,...){
  
  
  #新增
  returns.daily <-as.xts(na.omit(ROC(assets_price)) )
  
  weights<-returns.daily
  weights[,1]<-0.8
  weights[,2]<-0.1
  weights[,3]<-0.1
  
  returns_data<-returns.daily
  portf <- Return.portfolio(returns_data, weights, verbose=T, rebalance_on = "months")
  portf.ret<-portf$returns
  
  #计算每月drawdown
  periods.end.idx <- endpoints(portf.ret, 'months')  #all of the month ends and the last date
  periods.end.dt<-rownames(as.data.frame(portf.ret))[periods.end.idx]
  adjust<-as.data.frame(weights[periods.end.dt])
  for (i in 1:(length(periods.end.idx)-1)){
    adjust[i,'d']<-rownames(as.data.frame(portf.ret))[periods.end.idx[(i+1)]]
    adjust[i,'mdd']<-maxDrawdown(portf.ret[(periods.end.idx[i]+1):periods.end.idx[(i+1)]])
  }
  
  
  
  #选出月回撤高于3%的月份下一个月标记为1，调整权重
  adjust[(which(abs(adjust[,5])>0.05)+1),'adjust']<-TRUE
  adjust<-adjust[-nrow(adjust),]
  
  #若前一个月回撤高于3%，则这一个月全部持有国债，同时计算该月策略回撤，若高于3%则下一个月继续持有国债，若低于3%则下一个月按照策略决定权重
  adjust[which(adjust[,'adjust']==TRUE),1:3]<-0
  adjust[which(adjust[,'adjust']==TRUE),3]<-1
  
  weight.adjust<-adjust[,colnames(weights),drop=TRUE]
  
}

TDM<-function(assets_price){
  #输入：各资产日收盘价dataframe
  #输出：各资产权重
  symbols<-c("SPY","VEU","BND","BIL")  
  
  colnames(assets_price)<-symbols
  #assets_price<-as.xts(assets_price)
  
  
  #每个月最后一个交易日计算SPY和SCZ的动量得分，动量得分为过12个月的收益的平均值
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
  
  #新增
  returns.daily <-as.xts(na.omit(ROC(assets_price)) )
  returns_data<-returns.daily
  weights_data<-xts(weights[,-1],ymd(weights[,1]))
  portf <- Return.portfolio(returns_data, weights_data, verbose=T, rebalance_on = "months")
  portf.ret<-portf$returns
  
  #计算每月drawdown
  periods.end.idx <- endpoints(portf.ret, 'months')  #all of the month ends and the last date
  periods.end.dt<-rownames(as.data.frame(portf.ret))[periods.end.idx]
  adjust<-weights[-1,]
  for (i in 1:(length(periods.end.idx)-1)){
    adjust[i,'d']<-rownames(as.data.frame(portf.ret))[periods.end.idx[(i+1)]]
    adjust[i,'mdd']<-maxDrawdown(portf.ret[(periods.end.idx[i]+1):periods.end.idx[(i+1)]])
  }
  
  #选出月回撤高于3%的月份下一个月标记为1，调整权重
  adjust[which(abs(adjust[,'mdd'])>0.05),'adjust']<-TRUE
  #adjust<-adjust[-nrow(adjust),]
  
  #若前一个月回撤高于3%，则这一个月全部持有国债，同时计算该月策略回撤，若高于3%则下一个月继续持有国债，若低于3%则下一个月按照策略决定权重
  adjust[which(adjust[,'adjust']==TRUE),symbols]<-0
  adjust[which(adjust[,'adjust']==TRUE),'BND']<-1
  
  weight.adjust<-adjust[,colnames(weights),drop=TRUE]
  
  
  
}

VAA<-function(asset_price){
  
  symbols<-c("SPY",  "EFA",  "EEM", "AGG","LQD","IEF","SHY") 
  offensive<-c("SPY",  "EFA",  "EEM", "AGG") 
  defensive<-c("LQD","IEF","SHY") 
  
  colnames(assets_price)<-symbols
  #asset_price<-as.xts(assets_price)
  asset_price<-assets_price
  
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
  
  weights[,3:9] <- as.data.frame(lapply(weights[,3:9],as.numeric))
  weights<-weights[,-2]

  #新增
  returns.daily <-as.xts(na.omit(ROC(assets_price)) )
  returns_data<-returns.daily
  weights_data<-xts(weights[,-1],ymd(weights[,1]))
  portf <- Return.portfolio(returns_data, weights_data, verbose=T, rebalance_on = "months")
  portf.ret<-portf$returns
  
  #计算每月drawdown
  periods.end.idx <- endpoints(portf.ret, 'months')  #all of the month ends and the last date
  periods.end.dt<-rownames(as.data.frame(portf.ret))[periods.end.idx]
  adjust<-weights[-1,]
  for (i in 1:(length(periods.end.idx)-1)){
    adjust[i,'d']<-rownames(as.data.frame(portf.ret))[periods.end.idx[(i+1)]]
    adjust[i,'mdd']<-maxDrawdown(portf.ret[(periods.end.idx[i]+1):periods.end.idx[(i+1)]])
  }
  
  #选出月回撤高于3%的月份下一个月标记为1，调整权重
  adjust[(which(abs(adjust[,'mdd'])>0.05)+1),'adjust']<-TRUE
  adjust<-adjust[-nrow(adjust),]
  
  
  #若前一个月回撤高于3%，则这一个月全部持有国债，同时计算该月策略回撤，若高于3%则下一个月继续持有国债，若低于3%则下一个月按照策略决定权重
  adjust[which(adjust[,'adjust']==TRUE),symbols]<-0
  adjust[which(adjust[,'adjust']==TRUE),'IEF']<-1
  
  weight.adjust<-adjust[,colnames(weights),drop=TRUE]
  
  
}

TBS<-function(assets_price){
  
  symbols<-c("SHY", "IEF", "TLT","TIP", "LQD", "HYG", "BNDX", "EMB", "BIL")  
  
  colnames(assets_price)<-symbols
  assets_price<-as.xts(assets_price)
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
  weights$n <- rowSums(weights[,15:17]==FALSE)
  
  #每行现金的权重为n*(1/3)
  weights[,'BIL']<-weights$n*(1/3)
  
  #将每行FALSE的资产的权重记为0
  
  for (i in 1:length(weights[,1])){
    weights[i,as.character(weights[i,(which(weights[i,c('B1','B2','B3')]==FALSE)+9)])]<-0
  }
  
  
  weights[is.na(weights)] <- 0
  
  weights<-weights[,symbols]
  weights<- cbind(date=as.Date(rownames(weights)),weights)
  
  #新增
  returns.daily <-as.xts(na.omit(ROC(assets_price)) )
  returns_data<-returns.daily
  weights_data<-xts(weights[,-1],ymd(weights[,1]))
  portf <- Return.portfolio(returns_data, weights_data, verbose=T, rebalance_on = "months")
  portf.ret<-portf$returns
  
  #计算每月drawdown
  periods.end.idx <- endpoints(portf.ret, 'months')  #all of the month ends and the last date
  periods.end.dt<-rownames(as.data.frame(portf.ret))[periods.end.idx]
  adjust<-weights[-1,]
  for (i in 1:(length(periods.end.idx)-1)){
    adjust[i,'d']<-rownames(as.data.frame(portf.ret))[periods.end.idx[(i+1)]]
    adjust[i,'mdd']<-maxDrawdown(portf.ret[(periods.end.idx[i]+1):periods.end.idx[(i+1)]])
  }
  
  #选出月回撤高于3%的月份下一个月标记为1，调整权重
  adjust[(which(abs(adjust[,'mdd'])>0.03)+1),'adjust']<-TRUE
  adjust<-adjust[-nrow(adjust),]
  
  #若前一个月回撤高于3%，则这一个月全部持有国债，同时计算该月策略回撤，若高于3%则下一个月继续持有国债，若低于3%则下一个月按照策略决定权重
  adjust[which(adjust[,'adjust']==TRUE),symbols]<-0
  adjust[which(adjust[,'adjust']==TRUE),'BIL']<-1
  
  weight.adjust<-adjust[,colnames(weights),drop=TRUE]
  
  
}

PAA<-function(all_assets,N, P){
  
  Risky<-c("SPY", "QQQ", "IWM", "VGK", "EWJ", "EEM", "VNQ", "DBC", "GLD", "LQD", "HYG", "TLT")  
  cp<-c("IEF","SHY")
  symbols<-merged.list<-c(Risky, cp) 
  
  colnames(assets_price)<-symbols
  all_assets<-assets_price
  
  # #test
  # all_assets<-wdata
  # colnames(all_assets)<-symbols
  
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
  
  
  #新增
  returns.daily <-as.xts(na.omit(ROC(all_assets)) )
  returns_data<-returns.daily
  weights_data<-xts(assets_weights[,-1],ymd(assets_weights[,1]))
  portf <- Return.portfolio(returns_data, weights_data, verbose=T, rebalance_on = "months")
  portf.ret<-portf$returns

  #计算每月drawdown
  periods.end.idx <- endpoints(portf.ret, 'months')  #all of the month ends and the last date
  periods.end.dt<-rownames(as.data.frame(portf.ret))[periods.end.idx]
  adjust<-assets_weights[-1,]
  for (i in 1:(length(periods.end.idx)-1)){
    adjust[i,'d']<-rownames(as.data.frame(portf.ret))[periods.end.idx[(i+1)]]
    adjust[i,'mdd']<-maxDrawdown(portf.ret[(periods.end.idx[i]+1):periods.end.idx[(i+1)]])
  }
  
  #选出月回撤高于3%的月份下一个月标记为1，调整权重
  adjust[(which(abs(adjust[,'mdd'])>0.05)+1),'adjust']<-TRUE
  adjust<-adjust[-nrow(adjust),]

  #若前一个月回撤高于3%，则这一个月全部持有国债，同时计算该月策略回撤，若高于3%则下一个月继续持有国债，若低于3%则下一个月按照策略决定权重
  adjust[which(adjust[,'adjust']==TRUE),symbols]<-0
  adjust[which(adjust[,'adjust']==TRUE),'SHY']<-1

  weight.adjust<-adjust[,colnames(assets_weights),drop=TRUE]

}

ACA<-function(asset_price){
  
  symbols<-c('SPY','GLD','VNQ','IEF','TLT')
  colnames(assets_price)<-symbols
  #asset_price<-as.xts(assets_price)
  asset_price<-assets_price
  
  # #test
  # all_assets<-wdata
  # colnames(all_assets)<-symbols
  
  price_channel <- data.frame(SPY = matrix(NA,5,1),GLD = matrix(NA,5,1),VNQ = matrix(NA,5,1),row.names = c("UC_126","UC_252","LC_126","LC_252","close"))
  
  #做一个放weights的表
  #列名是数字，第一列是日期
  l<-length(asset_price[,4])-250
  date<-rownames(as.data.frame(assets_price))
  asset_weight <- data.frame(date=date[251:length(asset_price[,4])],SPY = matrix(NA,l,1),GLD = matrix(NA,l,1),VNQ = matrix(NA,l,1),TLT = matrix(NA,l,1),IEF_SPY = matrix(NA,l,1),IEF_VNQ = matrix(NA,l,1),IEF= matrix(NA,l,1), row.names =251:length(asset_price[,4]) )
  
  
  ##列名是日期，第一列是数字
  #l<-length(asset_price[,4])-252
  #asset_weight <- data.frame(t=251:length(asset_price[,4]),SPY = matrix(NA,l,1),GLD = matrix(NA,l,1),VNQ = matrix(NA,l,1),IEF = matrix(NA,l,1),TLT = matrix(NA,l,1),row.names = asset_price$date[251:length(asset_price[,4])])
  
  asset_weight[1,'SPY'] = 1/3
  asset_weight[1,'GLD'] = 1/3
  asset_weight[1,'VNQ'] = 1/3
  asset_weight[1,'IEF_SPY'] = 0
  asset_weight[1,'IEF_VNQ'] = 0
  asset_weight[1,'TLT'] = 0
  asset_weight[1,'IEF']=0
  
  for(i in 252:length(asset_price[,4])){
    #max(asset_price$SPY[1:252])
    #max(asset_price$SPY[127:252])
    
    #i=252
    #SPY过去252天最高收盘价
    UC_252_SPY<-max(asset_price$SPY[(i-251):i])
    #SPY过去252天最低收盘价
    LC_252_SPY<-min(asset_price$SPY[(i-251):i])
    #SPY过去126天最高收盘价
    UC_126_SPY<-max(asset_price$SPY[(i-125):i])
    #SPY过去126天最低收盘价
    LC_126_SPY<-min(asset_price$SPY[(i-125):i])
    
    #填入SPY的price channel表
    price_channel['UC_126','SPY']<-UC_126_SPY
    price_channel['UC_252','SPY']<-UC_252_SPY
    price_channel['LC_126','SPY']<-LC_126_SPY
    price_channel['LC_252','SPY']<-LC_252_SPY
    price_channel['close','SPY']<-asset_price$SPY[i]
    
    #GLD过去252天最高收盘价
    UC_252_GLD<-max(asset_price$GLD[(i-251):i])
    #GLD过去252天最低收盘价
    LC_252_GLD<-min(asset_price$GLD[(i-251):i])
    #GLD过去126天最高收盘价
    UC_126_GLD<-max(asset_price$GLD[(i-125):i])
    #GLD过去126天最低收盘价
    LC_126_GLD<-min(asset_price$GLD[(i-125):i])
    
    #填入GLD的price channel表
    price_channel['UC_126','GLD']<-UC_126_GLD
    price_channel['UC_252','GLD']<-UC_252_GLD
    price_channel['LC_126','GLD']<-LC_126_GLD
    price_channel['LC_252','GLD']<-LC_252_GLD
    price_channel['close','GLD']<-asset_price$GLD[i]
    
    #VNQ过去252天最高收盘价
    UC_252_VNQ<-max(asset_price$VNQ[(i-251):i])
    #GLD过去252天最低收盘价
    LC_252_VNQ<-min(asset_price$VNQ[(i-251):i])
    #GLD过去126天最高收盘价
    UC_126_VNQ<-max(asset_price$VNQ[(i-125):i])
    #GLD过去126天最低收盘价
    LC_126_VNQ<-min(asset_price$VNQ[(i-125):i])
    
    #填入VNQ的price channel表
    price_channel['UC_126','VNQ']<-UC_126_VNQ
    price_channel['UC_252','VNQ']<-UC_252_VNQ
    price_channel['LC_126','VNQ']<-LC_126_VNQ
    price_channel['LC_252','VNQ']<-LC_252_VNQ
    price_channel['close','VNQ']<-asset_price$VNQ[i]
    
    
    
    #对于第一组
    if(price_channel['close','SPY']>= price_channel['UC_126','SPY']){
      asset_weight[(i-250),'SPY'] = 1/3
      asset_weight[(i-250),'IEF_SPY'] = 0
    } else if(price_channel['close','SPY']<= price_channel['LC_252','SPY']){
      asset_weight[(i-250),'IEF_SPY'] = 1/3
      asset_weight[(i-250),'SPY'] = 0
    } else{
      asset_weight[(i-250),'SPY'] = asset_weight[(i-251),'SPY']
      asset_weight[(i-250),'IEF_SPY'] = asset_weight[(i-251),'IEF_SPY']
    }
    
    
    
    #对于第二组
    if(price_channel['close','GLD']>= price_channel['UC_252','GLD']){
      asset_weight[(i-250),'GLD'] = 1/3
      asset_weight[(i-250),'TLT'] = 0
    } else if(price_channel['close','GLD']<= price_channel['LC_126','GLD']){
      asset_weight[(i-250),'TLT'] = 1/3
      asset_weight[(i-250),'GLD'] = 0
    } else{
      asset_weight[(i-250),'GLD'] = asset_weight[(i-251),'GLD']
      asset_weight[(i-250),'TLT'] = asset_weight[(i-251),'TLT']
    }
    
    #对于第三组
    if(price_channel['close','VNQ']>= price_channel['UC_126','VNQ']){
      asset_weight[(i-250),'VNQ'] = 1/3
      asset_weight[(i-250),'IEF_VNQ'] = 0
    } else if(price_channel['close','VNQ']<= price_channel['LC_252','VNQ']){
      asset_weight[(i-250),'IEF_VNQ'] = 1/3
      asset_weight[(i-250),'VNQ'] = 0
    }  else{
      asset_weight[(i-250),'VNQ'] = asset_weight[(i-251),'VNQ']
      asset_weight[(i-250),'IEF_VNQ'] = asset_weight[(i-251),'IEF_VNQ']
    }
    
    asset_weight[(i-250),'IEF']=asset_weight[(i-250),'IEF_SPY']+asset_weight[(i-250),'IEF_VNQ']
    
  }
  
  assets_weights<-asset_weight[,c(1,2,3,4,5,8)]
  
 
  #新增
  returns.daily <-as.xts(na.omit(ROC(asset_price)) )
  returns_data<-returns.daily
  weights_data<-xts(assets_weights[,-1],ymd(assets_weights[,1]))
  portf <- Return.portfolio(returns_data, weights_data, verbose=T, rebalance_on = "months")
  portf.ret<-portf$returns
  
  #计算每月drawdown
  periods.end.idx <- endpoints(portf.ret, 'days')  #all of the month ends and the last date
  periods.end.dt<-rownames(as.data.frame(portf.ret))[periods.end.idx]
  adjust<-assets_weights[-1,]
  for (i in 1:(length(periods.end.idx)-1)){
    adjust[i,'d']<-rownames(as.data.frame(portf.ret))[periods.end.idx[(i+1)]]
    adjust[i,'mdd']<-maxDrawdown(portf.ret[(periods.end.idx[i]+1):periods.end.idx[(i+1)]])
  }
  
  #选出月回撤高于3%的月份下一个月标记为1，调整权重
  adjust[(which(abs(adjust[,'mdd'])>0.01)+1),'adjust']<-TRUE
  adjust<-adjust[-nrow(adjust),]
  
  #若前一个月回撤高于3%，则这一个月全部持有国债，同时计算该月策略回撤，若高于3%则下一个月继续持有国债，若低于3%则下一个月按照策略决定权重
  adjust[which(adjust[,'adjust']==TRUE),symbols]<-0
  adjust[which(adjust[,'adjust']==TRUE),'TLT']<-1
  
  weight.adjust<-adjust[,colnames(assets_weights),drop=TRUE]
  
}

QSF<-function(assets_price){
  
  symbols<-c("SPY", "QQQ", "EFA", "EEM", "TLT", "IEF")  
  colnames(assets_price)<-symbols
  assets_price<-assets_price
  #assets_price<-as.xts(assets_price)
  
  
  #每个月最后一个交易日计算5个资产的过去3个月的总收益
  R1<-monthly.return(assets_price)
  R3<-lapply(R1, function(x) runSum(x, n = 3, cumulative = FALSE))
  R3<-as.data.frame(R3)
  R3<-na.omit(R3)
  
  R3$max<-NA
  for (i in 1:length(R3[,1])){
    R3[i,'max']<-colnames(sort(R3[i,c("SPY", "QQQ", "EFA", "EEM", "TLT")],decreasing = TRUE)[1:1])
  }
  
  #每个月最后一个交易日计算5个risk assets中为负的数量
  R3$n <- rowSums(R3[,c("SPY", "QQQ", "EFA", "EEM", "TLT") ]<0)
  
  #做一个权重表
  weights <- R3
  weights[,1:6]<-NA
  
  
  #如果任意一个资产的3个月收益为负(即为负的资产数量n>0)，则全部配置defensive asset
  weights[which(weights[,'n']>0),'IEF']<-1
  
  #如果所有资产的3个月收益都为正，则全部配置3个月收益最高的资产
  
  
  
  row<-which(weights[,'n']==0)
  col<-R3[which(R3$n==0),'max']
  for (i in 1:length(row)){
    
    weights[row[i],col[i]]<-1
  }
  
  weights[is.na(weights)] <- 0
  
  weights<-weights[,symbols]
  weights<- cbind(date=as.Date(rownames(weights)),weights)
  
  #新增
  returns.daily <-as.xts(na.omit(ROC(assets_price)) )
  returns_data<-returns.daily
  weights_data<-xts(weights[,-1],ymd(weights[,1]))
  portf <- Return.portfolio(returns_data, weights_data, verbose=T, rebalance_on = "months")
  portf.ret<-portf$returns
  
  #计算每月drawdown
  periods.end.idx <- endpoints(portf.ret, 'months')  #all of the month ends and the last date
  periods.end.dt<-rownames(as.data.frame(portf.ret))[periods.end.idx]
  adjust<-weights[-1,]
  for (i in 1:(length(periods.end.idx)-1)){
    adjust[i,'d']<-rownames(as.data.frame(portf.ret))[periods.end.idx[(i+1)]]
    adjust[i,'mdd']<-maxDrawdown(portf.ret[(periods.end.idx[i]+1):periods.end.idx[(i+1)]])
  }
  
  #选出月回撤高于3%的月份下一个月标记为1，调整权重
  adjust[(which(abs(adjust[,'mdd'])>0.03)+1),'adjust']<-TRUE
  adjust<-adjust[-nrow(adjust),]
  
  #若前一个月回撤高于3%，则这一个月全部持有国债，同时计算该月策略回撤，若高于3%则下一个月继续持有国债，若低于3%则下一个月按照策略决定权重
  adjust[which(adjust[,'adjust']==TRUE),symbols]<-0
  adjust[which(adjust[,'adjust']==TRUE),'IEF']<-1
  
  weight.adjust<-adjust[,colnames(weights),drop=TRUE]
  
  
}

AAA<-function(data,n.top=5,n.mom=6*21,n.vol=1*21,target.sd=0){
  #generic function for adaptive asset allocation. 
  #n.top     # number of momentum positions
  #n.mom     # length of momentum look back
  #n.vol     # length of volatility look back
  

  library(fPortfolio)
  library(timeSeries)
  library(corpcor)
  

  
  data<-assets_price
  returns.daily <-na.omit(ROC(data)) 
  
  model=list()
  model$n.top<-n.top
  model$n.mom<-n.mom
  model$n.vol<-n.vol
  n.classes<-dim(returns.daily)[2]
  periods.end.idx = endpoints(returns.daily, 'months')  #all of the month ends and the last date
  periods.end.idx = periods.end.idx[periods.end.idx >= max(n.mom,n.vol)] #include those with enough data to calc mom and vol
  periods.end.dt<-rownames(as.data.frame(returns.daily))[periods.end.idx]
  n.mos  <- length(periods.end.dt)
  model$periods<-periods.end.dt[2:(n.mos-1)]
  
  results.monthly<-matrix(data=NA,nrow=n.mos-1,ncol=3,
                          dimnames=list(periods.end.dt[2:(n.mos)],c("Port","B6040","EqN")))
  
  allocations<-matrix(data=0,nrow=n.mos-1,ncol=n.classes,
                      dimnames=list(periods.end.dt[2:(n.mos)],names(returns.daily)))
  
  returns.ac<-returns.daily[(periods.end.idx[1]+1):periods.end.idx[n.mos],]
  model$sd.ac<-apply(returns.ac,2,sd)*sqrt(252)
  returns.ac<-apply(1+returns.ac,2,prod)^(12/(n.mos-1))-1
  model$returns.ac<-returns.ac
  
  pspec<-portfolioSpec()
  setEstimator(pspec)="myEstimator"
  Constraints="LongOnly"
  
  for (i in 1:(n.mos-1)){
    # use momentum to figure out which asset classes to use
    mom.returns<-returns.daily[(periods.end.idx[i]-n.mom+1):periods.end.idx[i],]
    mom.rank<-apply(1+mom.returns,2,prod)-1
    mom.rank<-rank(-mom.rank) # select 1 thru n.top
    mom.idx<-mom.rank<=n.top  #use these asset classes    
    # use min variance portfolio to determine the weights of those classes
    vol.returns<-as.timeSeries(returns.daily[(periods.end.idx[i]-n.vol+1):periods.end.idx[i],mom.idx])  
    if (target.sd<=0){
      mv<-suppressWarnings(minvariancePortfolio(vol.returns,pspec,Constraints))
      wts<-getWeights(mv) #wts in the portfolio
    } else {
      mv<-suppressWarnings(targetriskportfolio(vol.returns,target.sd))
      wts<-mv$weights
    }
    # return from next day to last day of next period
    port.return<-returns.daily[(periods.end.idx[i]+1):periods.end.idx[i+1],mom.idx]
    port.return<-apply(1+port.return,2,prod)-1
    port.return<-sum(port.return * wts)
    results.monthly[i,"Port"]<-port.return
    eqn.return<-returns.daily[(periods.end.idx[i]+1):periods.end.idx[i+1],mom.idx]
    eqn.return<-apply(1+eqn.return,2,prod)-1
    eqn.return<-sum(eqn.return * rep(1/n.top,n.top))
    results.monthly[i,"EqN"]<-eqn.return
    allocations[i,mom.idx]<-wts
    allocations<-round(allocations,4)
  }
  
  model$allocations<-as.timeSeries(allocations)
  transactions<-diff(as.matrix(allocations))
  model$turnover.monthly<-as.xts(rowSums(abs(transactions))/2)
  model$returns.monthly<-as.xts(results.monthly)
  model$turnover.mean<-mean(model$turnover.monthly)
  return(as.data.frame(model$allocations))
  

}

myEstimator<-function(x,spec){
  result<-list()
  result$mu<-colMeans(x) #rep(0,dim(x)[2])
  result$Sigma<-corpcor::cov.shrink(x,verbose=F)
  return(result)
}




HRP<-function(assets_price){
  
  returns.daily <-as.xts(na.omit(ROC(assets_price)) )
  returns.daily<-as.data.frame(returns.daily)
  
  periods.end.idx <- endpoints(returns.daily, 'months')  #all of the month ends and the last date
  periods.end.idx <- periods.end.idx+1
  periods.end.dt<-rownames(as.data.frame(returns.daily))[periods.end.idx-1]
  weights<-assets_price[periods.end.dt]
  weights[,]<-NA
  
  for (i in 1:(length(periods.end.idx)-13)){
    data<-returns.daily[(periods.end.idx[i]):(periods.end.idx[i+12]-1),]
    weights[(i+12),]<-hrp(data)
  }
  weights<-na.omit(weights)
  
  #新增
  returns.daily <-as.xts(na.omit(ROC(assets_price)) )
  returns_data<-returns.daily
  weights_data<-weights
  portf <- Return.portfolio(returns_data, weights_data, verbose=T, rebalance_on = "months")
  portf.ret<-portf$returns
  
  #计算每月drawdown
  periods.end.idx <- endpoints(portf.ret, 'month')  #all of the month ends and the last date
  periods.end.dt<-rownames(as.data.frame(portf.ret))[periods.end.idx]
  assets_weights<-as.data.frame(weights)
  adjust<-assets_weights[-1,]
  for (i in 1:(length(periods.end.idx)-1)){
    adjust[i,'d']<-rownames(as.data.frame(portf.ret))[periods.end.idx[(i+1)]]
    adjust[i,'mdd']<-maxDrawdown(portf.ret[(periods.end.idx[i]+1):periods.end.idx[(i+1)]])
  }
  
  #选出月回撤高于3%的月份下一个月标记为1，调整权重
  adjust[(which(abs(adjust[,'mdd'])>0.05)+1),'adjust']<-TRUE
  adjust<-adjust[-nrow(adjust),]
  
  #若前一个月回撤高于3%，则这一个月全部持有国债，同时计算该月策略回撤，若高于3%则下一个月继续持有国债，若低于3%则下一个月按照策略决定权重
  adjust[which(adjust[,'adjust']==TRUE),1:(length(symbol)-1)]<-0
  adjust[which(adjust[,'adjust']==TRUE),length(symbol)]<-1
  
  
  weight.adjust<-adjust[,colnames(assets_weights),drop=TRUE]
  
  return(weight.adjust)
  
}

