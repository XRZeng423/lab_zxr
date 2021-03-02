library(xts)
library(PerformanceAnalytics)
library(lubridate)
library(quantmod)
library(TTR)
library(ggplot2)
library(grid)
library(gridExtra)
library(corrplot)
library(tidyr)
library(knitr)

source("d:\\Users\\XuranZENG\\Desktop\\asset_allocation\\cal_ret.R")
source("d:\\Users\\XuranZENG\\Desktop\\asset_allocation\\models.R")


library(WindR)
w.start()
begintime<-"2013-07-29"  
endtime<-Sys.time() 

symbol<- c(
  "H00300.CSI",  #沪深300
  "H00932.CSI",  #中证主要消费 
  "H00934.CSI",  #中证金融地产
  "H00933.CSI",  #中证医药卫生 
  "H00928.CSI",  #中证能源
  "000016.SH",  #上证50
  "N00140.SH",    #五年国债
  "N11077.SH",  #十年国债  
  "H00905.CSI",  #中证500
  "Au9999.SGE"    #黄金
 

  # "H00935.CSI",  #中证信息技术
  # "950113.SH",   #公司债
  # "399606.SZ",    #创业板
  # "CBA09501.CS"  #中债-0-2年国开行债券指数收益率
)  


#策略
wdata<-as.data.frame(w.wsd(symbol,'close',begintime,endtime))

rownames(wdata)<-as.Date(wdata[,'Data.DATETIME'])

wdata<-subset(wdata,select=-c(ErrorCode,Field,Data.DATETIME))

colnames(wdata)<-symbol

assets_price<-wdata

returns.daily <-as.xts(na.omit(ROC(wdata)) )

# #60/40
# benchmark.ret<-cbind(returns.daily$H00300.CSI, returns.daily$N11077.SH)
# benchmark.portf<-portf.sixty_fourty(benchmark.ret, rebalance_on="months", verbose=T)
# portf.ret<-benchmark.portf$returns

# #RAA
# strategy <- RAA.Aggressive(returns.daily)
# returns.daily<-strategy
# strategy <- RAA.Balanced(returns.daily)
# returns.daily<-strategy

# #TDM
# strategy <- TDM(assets_price)
# returns_data<-monthly.return(as.xts(assets_price))
# weights_data<-xts(strategy[,-1],ymd(strategy[,1])) ## convert to xts
# returns_data<-returns_data[,1:3]
# portf <- Return.portfolio(returns_data, weights_data, verbose=T)
# portf.ret<-portf$returns

# #VAA
# strategy <- VAA(assets_price)
# strategy[,2:8] <- as.data.frame(lapply(strategy[,2:8],as.numeric))

# #GTAA
# #strategy <- GTAA(assets_price,3)
# strategy <- GTAA(assets_price,6)

# #ADM
# strategy <- ADM(assets_price)

# #ACA
# strategy <- ACA(assets_price)

# #QSF
# strategy <- QSF(assets_price)

# #TBS
# strategy <- TBS(assets_price)

# #GPM
# strategy <- GPM(assets_price, 6, 12)

# #PAA
# strategy <- PAA(assets_price, 6, 6)

#AAA
minrisk.model<-AAA(wdata, n.top=5,n.mom=6*21,n.vol=2*21,target.sd=0)
table.AnnualizedReturns(minrisk.model$returns.monthly)
minrisk.model$returns.ac #returns of individual asset classes
minrisk.model$sd.ac #risk of individual asset classes
minrisk.model$returns.ac/minrisk.model$sd.ac
charts.PerformanceSummary(minrisk.model$returns.monthly,colorset=rich6equal)
weights_data<-minrisk.model$allocations
weights_data<-as.data.frame(weights_data)
date<-rownames(weights_data)
weights_data<-cbind(date, weights_data)
returns_data<-returns.daily
weights_data<-xts(weights_data[,-1],ymd(weights_data[,1])) ## convert to xts

#构建组合
returns_data<-monthly.return(as.xts(assets_price))
weights_data<-xts(strategy[,-1],ymd(strategy[,1])) ## convert to xts
portf <- Return.portfolio(returns_data, weights_data, verbose=T)
portf.ret<-portf$returns

#3years
three_years<-'2018-01-01/2020-12-31'

ret.three_years<-portf.ret[three_years,]

charts.PerformanceSummary(ret.three_years, date.format="%Y%m")

tab.perf(ret.three_years)

#1year
three_years<-'2020-01-01/2020-12-31'

ret.three_years<-portf.ret[three_years,]

charts.PerformanceSummary(ret.three_years, date.format="%Y%m")

tab.perf(ret.three_years)

