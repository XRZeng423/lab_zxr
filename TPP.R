## Tactical Permanent Portfolio

library(xts)
library(PerformanceAnalytics)
library(lubridate)
library(quantmod)
library(TTR)

setwd("C:\\Users\\carol\\Desktop\\asset_allocation")
source("cal_ret.R")
source("funs.R")


# 下载数据
start_date <- "2007-01-01"  
end_date <- "2020-07-14"  

symbols<-c("SPY",  "IEF",  "GLD", "SHV")  

getSymbols(symbols, from=start_date,to=end_date)
asset_price<-list()
for(i in 1:length(symbols)) {
  asset_price[[i]] <- Cl(get(symbols[i]))  
}
asset_price <- do.call(cbind, asset_price) #xts
colnames(asset_price)<-symbols

# 计算过去200日移动平均
MA200_prices<-filter(asset_price/200, rep(1, 200))
colnames(MA200_prices)<-symbols
MA200_prices<-as.data.frame(MA200_prices)
MA200_prices<- cbind(date=as.Date(rownames(as.data.frame(asset_price))),MA200_prices)
MA200_prices<-na.omit(MA200_prices)
MA200_prices<-xts(MA200_prices, order.by=as.Date(MA200_prices[,1]))


# 找到每个月最后一个交易日
period.ends = endpoints(MA200_prices, 'months')
period.ends = period.ends[period.ends > 0] 
n.mos  <- length(period.ends)
last_trading_dates = MA200_prices$date[period.ends]

MA200_prices<-MA200_prices[,-1]
asset_price<-asset_price[rownames(as.data.frame(MA200_prices)),]

# 做一个选择资产的dataframe
asset_selected <- data.frame(date=last_trading_dates,SPY = matrix(NA,n.mos,1),IEF = matrix(NA,n.mos,1),GLD = matrix(NA,n.mos,1))
asset_selected <- asset_selected[,-1]


# 选择收盘价高于MA(200)的资产
for (i in 1:(n.mos)){
  asset_selected[i,] <- as.numeric(asset_price[last_trading_dates,][i,1:3])>=as.numeric(MA200_prices[last_trading_dates,][i,1:3])
}



# 根据过去一个月波动率采用风险平价确定初始权重
daily_return<-daily.return(asset_price)
monthly_return<-monthly.return(asset_price)

#计算波动率
s_sigma<-apply.monthly(daily_return,function(x) 1/StdDev(x))
#先将SHY波动率赋值为0，为了计算不配现金的初始权重
s_sigma$SHV<-0
#将FALSE也就是不选择的资产的波动率赋值为0，为了好算权重
for (i in 1:(n.mos)){
  s_sigma[i,which(asset_selected[i,]==FALSE)]<-0
}
#根据风险平价计算权重
s_sum<-rowSums(s_sigma)
s_weight<-s_sigma/s_sum
#三个资产都不选时，令SHV权重为1
s_weight[is.na(s_weight)] <- c(0,0,0,1)

# 对于选出来的组合，根据60天variance/covariance计算组合年化波动率，如果高于7%，则添加现金使之降到7%以下
weights<-s_weight

# 根据每日收益和每月调仓权重构建组合
portf <- Return.portfolio(daily_return,s_weight, verbose=T)
portf.ret<-portf$returns

#做一个表每天根据过去22天组合收益计算年化波动率
daily_vol<-data.frame(portf.ret,AV = matrix(NA,length(portf.ret),1))

for (i in 22:length(portf.ret)){
  daily_vol[i,2]<-StdDev.annualized(portf.ret[(i-21):i,])
}

#从日波动率中取每月月底调仓日的波动率
monthly_vol<- daily_vol[last_trading_dates,]
weights$AV<-monthly_vol$AV

#增加现金使波动率降低
#令现金权重为x，列方程,求解X

#波动率大于0.07的才调整


no.adjust<-subset(weights,weights$AV<=0.07)
adjust<-subset(weights,weights$AV>0.07)
adjust_dates<- as.Date(rownames(as.data.frame(adjust)))

#计算方差协方差
#var.a<-apply.monthly(daily_return$SPY, function(x) var(x))[adjust_dates,]
var.a<-apply.monthly(daily_return$SPY, function(x) var(x))
var.b<-apply.monthly(daily_return$IEF, function(x) var(x))
var.c<-apply.monthly(daily_return$GLD, function(x) var(x))
var.d<-apply.monthly(daily_return$SHV, function(x) var(x))

#cov.ab <- apply.monthly(daily_return[,c('SPY','IEF')], function(x) cov(x))[adjust_dates,2]
cov.ab <- apply.monthly(daily_return[,c('SPY','IEF')], function(x) cov(x))[,2]
cov.ac <- apply.monthly(daily_return[,c('SPY','GLD')], function(x) cov(x))[,2]
cov.ad <- apply.monthly(daily_return[,c('SPY','SHV')], function(x) cov(x))[,2]
cov.bc <- apply.monthly(daily_return[,c('IEF','GLD')], function(x) cov(x))[,2]
cov.bd <- apply.monthly(daily_return[,c('IEF','SHV')], function(x) cov(x))[,2]
cov.cd <- apply.monthly(daily_return[,c('GLD','SHV')], function(x) cov(x))[,2]

WA<-weights$SPY
WB<-weights$IEF
WC<-weights$GLD

A<-cov.ad+cov.bd+cov.cd+var.d
B<-WA^2*(cov.ad+cov.ab+cov.ac+var.a)
C<-WB^2*(cov.ab+cov.bd+cov.bc+var.b)
D<-WC^2*(cov.ac+cov.bc+cov.cd+var.c)
E<-B+C+D
G=(0.07/(252)^0.5)^2


#f<-function(x, WA, WB, WC, WD, cov.ab, cov.ac, cov.ad, cov.bc, cov.bd, cov.cd, var.a, var.b, var.c, var.d) {return(
    #x^2*var.d+(1-x)^2*WA^2*cov.ad+(1-x)^2*WB^2*cov.bd+(1-x)^2*WC^2*cov.cd+
    #x^2*cov.ad+(1-x)^2*WA^2*var.a+(1-x)^2*WB^2*cov.ab+(1-x)^2*WC^2*cov.ac+
    #x^2*cov.bd+(1-x)^2*WA^2*cov.ab+(1-x)^2*WB^2*var.b+(1-x)^2*WC^2*cov.bc+
    #x^2*cov.cd+(1-x)^2*WA^2*cov.ac+(1-x)^2*WB^2*cov.bc+(1-x)^2*WC^2*var.c-(0.07/(252)^0.5)^2
#  )
#result1 <- uniroot(f,c(0,1),WA=WA, WB=WB, WC=WC, WD=WD, cov.ab=cov.ab, cov.ac=cov.ac, cov.ad=cov.ad, cov.bc=cov.bc, cov.bd=cov.bd, cov.cd=cov.cd, var.a=var.a, var.b=var.b, var.c=var.c, var.d=var.d,tol=0.0001)
#手算后发现有些时候该方程无（0，1）之间的解，即无论如何波动率都大于7%，所以改为求年化波动率最小时的解

del=4*(A*G+E*G-A*E)

X1=(2*E-del^0.5)/(2*(A+E))
colnames(X1)<-'X1'
X2=(2*E+del^0.5)/(2*(A+E))
colnames(X2)<-'X2'
X12<-(X1+X2)/2
colnames(X12)<-'X12'
X<-cbind(weights,X1,X2,X12)
X$adjust<-NA

X[which(X$X1>=0 & X$X1<=1),'adjust']<-X[which(X$X1>=0 & X$X1<=1),'X1']
X[which(X$X2>=0 & X$X2<=1),'adjust']<-X[which(X$X2>=0 & X$X2<=1),'X2']

X[which(X$X1<0 & X$X2>1 & X$X12>=0),'adjust']<-0
X[which(X$X1<0 & X$X2>1 & X$X12<0),'adjust']<-1

adjust<-weights
adjust$SHV.ad<-X$adjust
adjust[which(is.na(X$adjust)),'SHV.ad']<-adjust[which(is.na(X$adjust)),'SHV']
adjust$SPY.ad<-adjust$SPY*(1-adjust$SHV.ad)
adjust$IEF.ad<-adjust$IEF*(1-adjust$SHV.ad)
adjust$GLD.ad<-adjust$GLD*(1-adjust$SHV.ad)





weights<-adjust[,c('SPY.ad', 'IEF.ad','GLD.ad','SHV.ad')]
colnames(weights)<-c('SPY', 'IEF','GLD','SHV')


no.adjust_dates<- as.Date(rownames(as.data.frame(no.adjust)))
weights[no.adjust_dates,]<-no.adjust[no.adjust_dates,c('SPY', 'IEF','GLD','SHV')]



#再次求年化波动率

# 根据每日收益和每月调仓权重构建组合
portf <- Return.portfolio(daily_return,weights, verbose=T)
portf.ret<-portf$returns

#做一个表每天根据过去60天组合收益计算年化波动率
daily_vol1<-data.frame(portf.ret,AV = matrix(NA,length(portf.ret),1))
for (i in 22:length(portf.ret)){
  daily_vol1[i,2]<-StdDev.annualized(portf.ret[(i-21):i,])
}

#从日波动率中取每月月底调仓日的波动率
monthly_vol1<- daily_vol1[last_trading_dates,]

