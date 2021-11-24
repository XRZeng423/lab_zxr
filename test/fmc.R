
library(lubridate)
library(ggplot2)
library(grid)
library(gridExtra)
library(tidyr)
library(knitr)
library(plyr)
#options("scipen"=100, "digits"=4)

# Data Construction
# CCM
ccm<-read.csv("E:\\data\\fmc_exam_2020_2021\\fmc_exam_data\\ccm_10_19.csv")

# (a)First, drop observations where total assets are missing as well as where the foreign incorporation code is not "USA". How many observations are dropped in each step? Report the numbers.

# AT=total asset
ccm1<-ccm[!is.na(ccm$at),]
cat(length(ccm[,1])-length(ccm1[,1]),"observations where AT is missing are dropped")

# fic = Foreign Incorporation Code
ccm2 <- subset(ccm1, fic == 'USA')
cat(length(ccm1[,1])-length(ccm2[,1]),"observations where FIC is not USA are dropped")

# (b)Construct the book value of equity
ccm2$bvps<-ccm2$pstkrv
ccm2[which(is.na(ccm2$bvps)),'bvps']<-ccm2[which(is.na(ccm2$bvps)),'pstkl']
ccm2[which(is.na(ccm2$bvps)),'bvps']<-ccm2[which(is.na(ccm2$bvps)),'pstk']
ccm2[which(is.na(ccm2$bvps)),'bvps']<-0
ccm2$be<-ccm2$seq+ccm2$txditc-ccm2$bvps

# (c)Calculate leverage as the ratio of long-term debt over total assets.
# dltt = Long-Term Debt-Total, at=Total Asset
ccm2$lev<-ccm2$dltt/ccm2$at

# CRSP 
# Investigate if there are any permno-month-year duplicates, report your findings and explain why this may happen? Drop these duplicates. How many observations are dropped? Report the numbers.
crsp<-read.csv("E:\\data\\fmc_exam_2020_2021\\fmc_exam_data\\crsp_07_19.csv")

# (a)Permno-month-year duplicates
crsp$date2<-ymd(crsp$date)
crsp$month<-month(crsp$date2)
crsp$year<-year(crsp$date2)
crsp2<-crsp[!duplicated(crsp[,c('PERMNO','month','year')]),]
cat(length(crsp[,1])-length(crsp2[,1]),"observations where PERMNO-MONTH-YEAR duplicates are dropped")

# (b)Calculate the market value of equity
summary(crsp2$PRC)
cat("NA")
crsp2$mv<-crsp2$PRC * crsp2$SHROUT * 10^(-3)

# (c)Construct a proxy variable for market illiquidity.
# return: RETX
# do not include returns from months with a negative price
crsp3<-subset(crsp2, PRC >= 0)
crsp4<-crsp3[,c("PERMNO","year","date2","VOL","RETX","mv","CUSIP","date","mv","COMNAM","NAICS","month")]

# trading volumn in millions of dollars
crsp4$VOL2<-crsp4$VOL*(10)^(-4)
crsp4$RV<-abs(as.numeric(crsp4$RETX))/as.numeric(crsp4$VOL2)
crsp4<-na.omit(crsp4)
ANNUAL_ILLIQ<-aggregate(crsp4$RV,by=list(PERMNO=crsp4$PERMNO, year=crsp4$year),mean)
colnames(ANNUAL_ILLIQ)<-c("PERMNO","year","ILLIQ")


# (d) annualize market equity.
ANNUAL_MV<-subset(crsp4[,c("PERMNO","year","mv","month")], month == 12)
ANNUAL_MV<-ANNUAL_MV[,1:3]
colnames(ANNUAL_MV)<-c("PERMNO","year","MV")

ANNUAL<-merge(ANNUAL_ILLIQ,ANNUAL_MV, by=c("PERMNO","year"),all=FALSE)
ANNUAL[1:5,]
crsp5<-crsp4[,c("PERMNO","year","CUSIP","date","COMNAM","NAICS")]
crsp6<-crsp5[!duplicated(crsp[,c('PERMNO','year')]),]
crsp7<-merge(ANNUAL,crsp6 , by=c("PERMNO","year"),all=FALSE)
crsp7[1:20,]

# 2012 IO 
IO<-read.csv("E:\\data\\fmc_exam_2020_2021\\fmc_exam_data\\IO_use_table.csv",stringsAsFactors=FALSE)

# extract 4-digit code
IO[-1,1]<-substring(IO[-1,1], 1, 4)
IO[1,-2]<-substring(IO[1,-2], 1, 4)

IO<-IO[,-2]
colnames(IO)<-IO[1,]
col<-as.data.frame(colnames(IO))

IO[is.na(IO)]<-0

# Aggregate the table to 4-digit industries.

# Aggregate row
# convert character to numeric
for (i in 2:ncol(IO)){
  IO[,i]<-as.numeric(unlist(IO[,i]))
}
IO[is.na(IO)]<-0
IO2<-aggregate(IO[-1,-1],by=list(input=IO[-1,1]),sum)

# Aggregate column (transpose and apply the same code)
tIO2<-t(IO2)
colnames(tIO2)<-IO2[,1]
ctIO2<-cbind(col,tIO2)

for (i in 2:ncol(ctIO2)){
  ctIO2[,i]<-as.numeric(unlist(ctIO2[,i]))
}

ctIO2[is.na(ctIO2)]<-0

IO3<-aggregate(ctIO2[,-1],by=list(output=(ctIO2[,1])),sum)
IO3[1,]<-colnames(IO3)
IO4<-t(IO3)
colnames(IO4)<-IO4[1,]
IO4<-IO4[-1,-1]
cat("end up with",ncol(IO4),"unique industries")

# Calculate the share of within industry inputs for each output industry.

share<-NA
for (i in 1:nrow(IO4)){
  share[i]=as.numeric(IO4[i,i])/as.numeric(IO4['T005',i])
}
IO4<-rbind(IO4,share)

# Merge crsp2 ccm2
# crsp:CUSIP(8-digits), ccm:cusip(9-digits)
# crsp:monthly, ccm:yearly

ccm3<-ccm2[,c("cusip","datadate","naics","xrd","ni","dvt","lev","be","conm")]
crsp7<-crsp7[,c(-1,-6)]
colnames(crsp7)[4]<-"cusip"
colnames(ccm3)[2]<-"year"
ccm3$cusip<-substring(ccm3$cusip, 1, 8)
ccm3$year<-substring(ccm3$year, 1, 4)
ccm_crsp<-merge(ccm3, crsp7, by = c("cusip","year"), all = FALSE)
ccm_crsp[1:5,]

ccm_crsp1<-ccm_crsp[,c('cusip','year','naics','xrd','ni','dvt','lev','be','ILLIQ','MV'),]
ccm_crsp1$naics<-substring(ccm_crsp1$naics, 1, 4)
ccm_crsp1[1:5,]

share2<-t(rbind(colnames(IO4)[1:length(share)],round(share,4)))
colnames(share2)<-c("naics","share")
ccm_crsp2<-merge(ccm_crsp1,share2[,2:3],by=c("naics"),all=FALSE)
ccm_crsp2<-ccm_crsp2[!duplicated(ccm_crsp2[,c('cusip','year')]),]
ccm_crsp2$bm<-ccm_crsp2$be/ccm_crsp2$MV
ccm_crsp2[1:5,]

# Analysis
# Summary Statistics
summary(ccm_crsp2$be)
summary(ccm_crsp2$lev)
summary(ccm_crsp2$MV)
summary(ccm_crsp2$bm)

# R&D-NAIC
plot_data<-ccm_crsp2[,c("year","xrd","naics")]
plot_data2<-subset(plot_data,year==2012)
plot_data2[is.na(plot_data2)]<-0
plot_data2$xrd<-(plot_data2$xrd)*10^(-3)
plot_data2$naics<-substring(plot_data2$naics,1,2)
plot_data2[1:5,]
ggplot(plot_data2,mapping=(aes(x=naics,y=xrd,group=1)))+geom_bar(stat="identity")


# Panel regressions

ccm_crsp2_pd<-pdata.frame(ccm_crsp2,index=c('cusip','year'))
kable(xtable(ccm_crsp2_pd[1:5,]),digits=4,align="c")

pdim(ccm_crsp2_pd)
# a
ni.pooled.a<-plm(ni~lag(xrd,1)+lag(xrd,2)+lag(xrd,3)+lag(xrd,4)+lag(ni,1)+lag(MV,1)+lag(bm,1)+lag(ILLIQ,1)+lag(lev,1),model='poolin',data=ccm_crsp2_pd)
kable(tidy(ni.pooled.a),digits = 3,caption = "A")
summary(ni.pooled.a)$r.squared


# b
ni.pooled.b<-plm(dvt~lag(xrd,1)+lag(xrd,2)+lag(xrd,3)+lag(xrd,4)+lag(dvt,1)+lag(MV,1)+lag(bm,1)+lag(ILLIQ,1)+lag(lev,1),model='poolin',data=ccm_crsp2_pd)
kable(tidy(ni.pooled.b),digits = 3,caption = "B")
summary(ni.pooled.b)$r.squared

# c
ni.pooled.ca<-plm(ni~lag(xrd,1)+lag(xrd,2)+lag(xrd,3)+lag(xrd,4)+lag(ni,1)+lag(MV,1)+lag(bm,1)+lag(ILLIQ,1)+lag(lev,1)+lag(share,1),model='poolin',data=ccm_crsp2_pd)
kable(tidy(ni.pooled.ca),digits = 3,caption = "CA")
summary(ni.pooled.ca)$r.squared

ni.pooled.cb<-plm(dvt~lag(xrd,1)+lag(xrd,2)+lag(xrd,3)+lag(xrd,4)+lag(dvt,1)+lag(MV,1)+lag(bm,1)+lag(ILLIQ,1)+lag(lev,1)+lag(share,1),model='poolin',data=ccm_crsp2_pd)
kable(tidy(ni.pooled.cb),digits = 3,caption = "B")
summary(ni.pooled.cb)$r.squared

# d

# aggregate industry
ccm_crsp3<-aggregate(ccm_crsp2[,4:12],by=list(industry=ccm_crsp2$naics,year=ccm_crsp2$year),mean)
ccm_crsp3_pd<-pdata.frame(ccm_crsp3,index=c('industry','year'))

# within
ni.pooled.da<-plm(ni~lag(xrd,1)+lag(xrd,2)+lag(xrd,3)+lag(xrd,4)+lag(ni,1)+lag(MV,1)+lag(bm,1)+lag(ILLIQ,1)+lag(lev,1),model='within',data=ccm_crsp3_pd,effect = "individual")
kable(tidy(ni.pooled.da),digits = 3,caption = "DA_within")
summary(ni.pooled.da)$r.squared

ni.pooled.db<-plm(dvt~lag(xrd,1)+lag(xrd,2)+lag(xrd,3)+lag(xrd,4)+lag(dvt,1)+lag(MV,1)+lag(bm,1)+lag(ILLIQ,1)+lag(lev,1),model='within',data=ccm_crsp3_pd,effect = "individual")
kable(tidy(ni.pooled.db),digits = 3,caption = "DB_within")
summary(ni.pooled.db)$r.squared

#e
library(AER)
# a
coeftest(ni.pooled.a, vcov. = vcovNW)

# b
coeftest(ni.pooled.b, vcov. = vcovNW)

