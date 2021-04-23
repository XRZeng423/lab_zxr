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

cal_ret<-function(portf.ret, bench.ret){
  cal_ret.portf<-apply.monthly(portf.ret[three_years,],Return.cumulative)
  cal_ret.bench<-apply.monthly(bench.ret[three_years,],Return.cumulative)
  cal_ret<-cbind(cal_ret.portf,cal_ret.bench)
  cal_ret<-round(cal_ret,2)
  cal_ret<-cbind(date=as.Date(rownames(as.data.frame(cal_ret))),as.data.frame(cal_ret))
  #cal_ret
  cal_ret <- gather(data = cal_ret, key = "type", value = "return", Strategy, Benchmark)
  
  ggplot(data = cal_ret,aes(x = date,y = return,fill = type))+
    scale_x_date(date_breaks="6 months",date_labels="%D")+
    geom_col(position = 'dodge')
  
}

cor_plot<-function(ret){
  cor<-cor(ret)
  col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
  corrplot(cor, method="color", col=col(200),  
           type="upper", order="hclust", 
           addCoef.col = "black", #添加相关系数
           tl.col="black", tl.srt=45, #修改字体
           
           diag=FALSE 
  )
}

interval_returns_table<-function(date,ret){
  ret_1_week<-Return.cumulative(ret[paste((as.Date(date) - dweeks(1)),date ,sep = "/"),])
  
  ret_1_month<-Return.cumulative(ret[paste((as.Date(date) - dmonths(1)),date ,sep = "/"),])
  
  ret_3_months<-Return.cumulative(ret[paste((as.Date(date) - dmonths(3)),date ,sep = "/"),])
  
  ret_6_months<-Return.cumulative(ret[paste((as.Date(date) - dmonths(6)),date ,sep = "/"),])
  
  ret_ytd<-Return.cumulative(ret[paste(paste(year(Sys.time()),"01","01",sep = "-"),date ,sep = "/"),])
  
  ret_1_year<-Return.cumulative(ret[paste((as.Date(date) - dyears(1)),date ,sep = "/"),])
  
  ret_3_year<-Return.cumulative(ret[paste((as.Date(date) - dyears(3)),date ,sep = "/"),])
  
  ret_periods<-rbind(ret_1_week,ret_1_month,ret_3_months,ret_6_months,ret_ytd,ret_1_year,ret_3_year)
  ret_periods<-round(ret_periods,2)
  rownames(ret_periods)<-c("Last Week","Last Month","Last 3 Months","Last 6 months","Year to Date","Last Year","Last 3 Years")
  
  ret_periods<-as.data.frame(cbind(date=rownames(ret_periods),ret_periods))
  ret_periods <- gather(data =ret_periods, key = "type", value = "return", Strategy, Benchmark)
  
  ret_periods$date <- factor(ret_periods$date,levels=c("Last Week","Last Month","Last 3 Months","Last 6 months","Year to Date","Last Year","Last 3 Years"))
  
  ret_periods<-rbind(ret_1_week,ret_1_month,ret_3_months,ret_6_months,ret_ytd,ret_1_year,ret_3_year)
  ret_periods<-round(ret_periods,2)
  rownames(ret_periods)<-c("Last Week","Last Month","Last 3 Months","Last 6 months","Year to Date","Last Year","Last 3 Years")
  kable(ret_periods)
}

interval_returns_plot<-function(date,ret){
  ret_1_week<-Return.cumulative(ret[paste((as.Date(date) - dweeks(1)),date ,sep = "/"),])
  
  ret_1_month<-Return.cumulative(ret[paste((as.Date(date) - dmonths(1)),date ,sep = "/"),])
  
  ret_3_months<-Return.cumulative(ret[paste((as.Date(date) - dmonths(3)),date ,sep = "/"),])
  
  ret_6_months<-Return.cumulative(ret[paste((as.Date(date) - dmonths(6)),date ,sep = "/"),])
  
  ret_ytd<-Return.cumulative(ret[paste(paste(year(Sys.time()),"01","01",sep = "-"),date ,sep = "/"),])
  
  ret_1_year<-Return.cumulative(ret[paste((as.Date(date) - dyears(1)),date ,sep = "/"),])
  
  ret_3_year<-Return.cumulative(ret[paste((as.Date(date) - dyears(3)),date ,sep = "/"),])
  
  ret_periods<-rbind(ret_1_week,ret_1_month,ret_3_months,ret_6_months,ret_ytd,ret_1_year,ret_3_year)
  ret_periods<-round(ret_periods,2)
  rownames(ret_periods)<-c("Last Week","Last Month","Last 3 Months","Last 6 months","Year to Date","Last Year","Last 3 Years")
  
  ret_periods<-as.data.frame(cbind(date=rownames(ret_periods),ret_periods))
  ret_periods <- gather(data =ret_periods, key = "type", value = "return", Strategy, Benchmark)
  
  ret_periods$date <- factor(ret_periods$date,levels=c("Last Week","Last Month","Last 3 Months","Last 6 months","Year to Date","Last Year","Last 3 Years"))
  
  ggplot(data = ret_periods,aes(x = date,y = return,fill = type))+
    geom_col(position = 'dodge')
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
