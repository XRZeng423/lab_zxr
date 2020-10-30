#Adaptive Asset Allocation

library(xts)

library(fPortfolio)

library(timeSeries)

library(PerformanceAnalytics)

library(corpcor)


myEstimator<-function(x,spec){
  result<-list()
  result$mu<-colMeans(x) #rep(0,dim(x)[2])
  result$Sigma<-corpcor::cov.shrink(x,verbose=F)
  return(result)
}

#find portfolio with max return with a sd less than the target, if none exist, min risk portfolio is returned


start_date <- "2007-01-01"  
end_date <- "2020-07-14"  

symbols<-c("SPY", "EZU", "EWJ", "EEM", "VNQ", "RWX", "IEF", "TLT", "DBC", "GLD")  

getSymbols(symbols, from=start_date,to=end_date)
asset_price<-list()
for(i in 1:length(symbols)) {
  asset_price[[i]] <- Cl(get(symbols[i]))  
}
asset_price <- do.call(cbind, asset_price) #xts
colnames(asset_price)<-symbols

returns.daily <- do.call(merge.xts,lapply(colnames(asset_price),function(x){ 
  ret = periodReturn(asset_price[,x],period = "daily");
  colnames(ret) = x;
  return(ret) 
} ))

aaa<-function(data,n.top=5,n.mom=6*21,n.vol=1*21,target.sd=0){
  #generic function for adaptive asset allocation. 
  #n.top     # number of momentum positions
  #n.mom     # length of momentum look back
  #n.vol     # length of volatility look back

  model=list()
  model$n.top<-n.top
  model$n.mom<-n.mom
  model$n.vol<-n.vol
  n.classes<-dim(returns.daily)[2]
  periods.end.idx = endpoints(returns.daily, 'months')  #all of the month ends and the last date
  periods.end.idx = periods.end.idx[periods.end.idx >= max(n.mom,n.vol)] #include those with enough data to calc mom and vol
  periods.end.dt<-index(returns.daily)[periods.end.idx]
  n.mos  <- length(periods.end.dt)
  model$periods<-periods.end.dt[2:(n.mos-1)]

  results.monthly<-matrix(data=NA,nrow=n.mos-1,ncol=3,
                        dimnames=list(format(periods.end.dt[2:(n.mos)],"%Y-%m-%d"),c("Port","B6040","EqN")))

  allocations<-matrix(data=0,nrow=n.mos-1,ncol=n.classes,
                    dimnames=list(format(periods.end.dt[2:(n.mos)],"%Y-%m-%d"),names(returns.daily)))
  
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
    #b6040 return
    b6040.return<-returns.daily[(periods.end.idx[i]+1):periods.end.idx[i+1],c("SPY","TLT")]
    b6040.return<-apply(1+b6040.return,2,prod)-1
    b6040.return<-sum(b6040.return * c(.6,.4))
    results.monthly[i,"B6040"]<-b6040.return
    allocations[i,mom.idx]<-wts
    allocations<-round(allocations,4)
  }

  model$allocations<-as.timeSeries(allocations)
  transactions<-diff(as.matrix(allocations))
  model$turnover.monthly<-as.xts(rowSums(abs(transactions))/2)
  model$returns.monthly<-as.xts(results.monthly)
  model$turnover.mean<-mean(model$turnover.monthly)
  return(model)
}


minrisk.model<-aaa(n.top=4,n.mom=6*21,n.vol=2*21,target.sd=0)
table.AnnualizedReturns(minrisk.model$returns.monthly)
minrisk.model$returns.ac #returns of individual asset classes
minrisk.model$sd.ac #risk of individual asset classes
minrisk.model$returns.ac/minrisk.model$sd.ac

charts.PerformanceSummary(minrisk.model$returns.monthly,colorset=rich6equal)

weights_data<-minrisk.model$allocations
weights_data<-as.data.frame(weights_data)
date<-rownames(weights_data)
weights_data<-cbind(date, weights_data)

returns_data <- do.call(merge.xts,lapply(colnames(asset_price),function(x){ 
  ret = periodReturn(asset_price[,x],period = "monthly");
  colnames(ret) = x;
  return(ret) 
} ))
returns_data<-returns_data[8:163,]
returns_data<-as.data.frame(returns_data)
returns_data<-cbind(date, returns_data)

charts.PerformanceSummary(minrisk.model$returns.monthly,colorset=rich6equal)


#write.csv(returns_data,"C:\\Users\\carol\\Desktop\\asset_allocation\\AAA\\all_assets.csv", row.names = FALSE) 
#write.csv(weights_data,"C:\\Users\\carol\\Desktop\\asset_allocation\\AAA\\weights_data.csv", row.names = FALSE) 
