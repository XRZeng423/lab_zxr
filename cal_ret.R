### most functions in this script requires PerformanceAnalytics
### functions take daily return series ## can later be modified to self check periodicity

calendar_ret<-function (ret) {
  ### this function takes in a single daily return series and outputs calendar months return
  ### Args:
  ### ret: xts object of daily returns
  ###
  ### output:
  ### calendar_ret: table of calendar return
  
  table.CalendarReturns(apply.monthly(ret,Return.cumulative), digits=2)
}

longest_drawdown<-function(ret){
  ### this function takes daily returns and returns longest drawdowns in months
  ### can take multiple vectors at a time
  ###  Args:
  ###  ret: xts object ofdaily returns
  ###  output:
  ###  longest number of months of drawdown

  apply(ret, 2, function (x) max(findDrawdowns(apply.monthly(x,Return.cumulative))$length))
  ## using apply to do the calculation for each column  
}

best_month_return<-function(ret){
  max(ret.to.monthly(ret))*100
}

worst_month_return<-function(ret){
  min(ret.to.monthly(portf))*100
}

portf.sixty_fourty<-function(ret,...){
  ## this function constructs 60/40 portfolio returns
  ## uses Return.portfolio in PerformanceAnalytics library
  ## ret contains daily xts return series with two assets
  ## ... path through can be rebalance_on="months", "quarters", "years" etc.
  ## ... path through can be verbose = TRUE for all return.portfolio outputs
  
  ret1<-ret[,1]
  ret2<-ret[,2]
  weights<-c(0.6, 0.4)
  Return.portfolio(ret,weights,...)
}


portf.naive_risk_parity<-function(ret,...){
  ## this function constructs naive risk parity portfolio returns
  ## using inverse of previous month return volatility as portfolio weight
  ## uses Return.portfolio in PerformanceAnalytics library
  ## ret contains daily xts return series with multiple assets
  
  
  s_sigma<-apply.monthly(ret, function(x) 1/StdDev(x))
  s_sum<-rowSums(s_sigma)
  s_weight<-s_sigma/s_sum  #xts objects containing weights, rebalance at end of month
  
  Return.portfolio(ret,s_weight,...)
}

gen_cal_annual_ret<-function(ret,ret_col_names){
  
  cal_ret1<-calendar_ret(ret[,1])
  
  cal_portf<-cal_ret1[,13]
  
  for (i in 2:ncol(ret)){
    cal<-calendar_ret(ret[,i])
    cal_portf<-cbind(cal_portf,cal[,13])
  }
  
  cal_annual_ret<-data.frame(cal_portf)
  rownames(cal_annual_ret)<-rownames(cal_ret1)
  colnames(cal_annual_ret)<-ret_col_names
  cal_annual_ret
}



return.nav<-function(ret,bench){
  ### this function takes daily returns and outputs monthly function NAV
  ret[1]<-0
  bench[1]<-0
  portf.daily.nav<-cumprod(1+ret)
  bench.daily.nav<-cumprod(1+bench)
  portf.monthly.nav<-Cl(to.monthly(portf.daily.nav,indexAt="endof"))
  bench.monthly.nav<-Cl(to.monthly(bench.daily.nav,indexAt="endof"))
  portf.cum.monthly.ret<-round((portf.monthly.nav-1)*100,digits=2)
  bench.cum.monthly.ret<-round((bench.monthly.nav-1)*100,digits=2)
  excess.cum.monthly.ret<-portf.cum.monthly.ret-bench.cum.monthly.ret
  
  output<-cbind(portf.monthly.nav,portf.cum.monthly.ret,bench.cum.monthly.ret,excess.cum.monthly.ret)
  names(output)<-c("NAV","Cum Ret", "Benchmark Ret","Excess Ret")
  return(output)
}



ret.to.monthly<-function(ret){
  ### this function takes daily returns and outputs monthly returns
  ### Args:
  ### ret: xts object of daily returns
  ### 
  ### output:
  ### xts of monthly returns
  
  ### alternatively aggregate(ret,as.yearmon,function(x) tail(cumprod(1 + x) -1,1))
  
  ### this function runs faster but somehow doesn't work with calendar.table, longest drawdown functions
  ### to be checked
  
  apply.monthly(ret,Return.cumulative) 
}



tab.perf2<-function(portf, bench){

  ## This function prints performance stats for 
  ## portf: portfolio return series 
  ## bench: benchmark return series
  
   print(sprintf("%25s %6s %6s","","Strategy","Benchmark"))
   print(sprintf("%25s %3.2f%% %3.2f%%","Cumulative Return ",
                 Return.cumulative(portf)*100, Return.cumulative(bench)*100))
   print(sprintf("%25s %3.2f%% %3.2f%%","Annualized Return ",
                 Return.annualized(portf)*100,Return.annualized(bench)*100))
   print(sprintf("%25s %3.2f%% %3.2f%%","Annualized Volatility ",
                 StdDev.annualized(portf)*100,StdDev.annualized(bench)*100))
   print(sprintf("%25s %4.2f %4.2f","Sharpe Ratio ",SharpeRatio.annualized(portf),SharpeRatio.annualized(bench)))
   print(sprintf("%25s %4.2f %4.2f","Sortino Ratio ",SortinoRatio(portf),SortinoRatio(bench)))
   print(sprintf("%25s %3.2f%% %3.2f%%","Max Drawdown ",maxDrawdown(portf)*100,maxDrawdown(bench)*100 ))
   print(sprintf("%25s %3i m %3i m","Longest Drawdown ",longest_drawdown(portf),longest_drawdown(bench)))
   print(sprintf("%25s %3.2f%% %3.2f%%","Best Month Return",max(ret.to.monthly(portf))*100,max(ret.to.monthly(bench))*100 ))
   print(sprintf("%25s %3.2f%% %3.2f%%","Worst Month Return",min(ret.to.monthly(portf))*100,min(ret.to.monthly(bench))*100 ))
   
}

tab.perf<-function(ret){
          table.Arbitrary(ret,
                            metrics=c(
                              "Return.cumulative",
                              "Return.annualized",
                              "StdDev.annualized",
                              "SharpeRatio.annualized",
                              "SortinoRatio",
                              "maxDrawdown"),
                            metricsNames=c(
                              "Cumulative Return",
                              "Annualized Return",
                              "Annualized Volatility",
                              "Sharpe Ratio",
                              "Sortino Ratio",
                              "Max Drawdown"))
}


winMAE<-function(pts){
  
  win.trade<-pts[pts$Net.Trading.PL>0,]
  quantile(win.trade$Pct.MAE,seq(0,0.1,0.01))
}

trade_duration<-function(pts){
  pts$Start<-as.Date(pts$Start)
  pts$End<-as.Date(pts$End)
  pts$duration<-pts$End-pts$Start
  return(pts)
}

durationStats<-function(duration){
  
  summary(as.numeric(duration))
  
  durationSummary <- summary(as.numeric(duration))
  winDurationSummary <- summary(as.numeric(duration[pts$Net.Trading.PL > 0]))
  lossDurationSummary <- summary(as.numeric(duration[pts$Net.Trading.PL <= 0]))
  names(durationSummary) <- names(winDurationSummary) <- names(lossDurationSummary) <- c("Min","Q1","Med", "Mean","Q3","Max")
  dataRow <- data.frame(cbind(round(durationSummary), round(winDurationSummary), round(lossDurationSummary)))
  names(dataRow)<-c("all","win","lose")
  dataRow
}