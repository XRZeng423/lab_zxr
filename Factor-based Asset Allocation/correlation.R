library(readr)
library(lubridate)

library(xts)
library(TTR)

library(dplyr)

setwd("C:\\Users\\carol\\Desktop\\factor")
trading_data <- read_delim("data/trading data.txt", 
                           "\t", escape_double = FALSE, col_types = cols(Trdmnt = col_date(format = "%Y/%m/%d")), 
                           trim_ws = TRUE)


pricing_factor_data <- read_delim("data/pricing factor data.txt", 
                                  "\t", escape_double = FALSE, col_types = cols(Accper = col_date(format = "%Y/%m/%d")), 
                                  trim_ws = TRUE)

pricing_factor_data$Accper <- as.Date(pricing_factor_data$Accper)+ 1

#计算非流动性指标
trading_data$ILLIQ <- abs(trading_data$Mretwd)/trading_data$Mnvaltrd/trading_data$Ndaytrd

#计算动量
trading_data$Mclprc12m <-c(trading_data$Mclsprc[-(1:12)],NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
trading_data$Momentum <- trading_data$Mclprc12m / trading_data$Mclsprc-1

#计算反转
trading_data$Mclprc1m <-c(trading_data$Mclsprc[-1],NA)
trading_data$Reversal <- trading_data$Mclprc1m / trading_data$Mclsprc-1

meiguzhibiao <- read_delim("data/meiguzhibiao.txt", 
                           "\t", escape_double = FALSE, col_types = cols(Accper = col_date(format = "%Y/%m/%d")), 
                           trim_ws = TRUE)
meiguzhibiao$Accper <- as.Date(meiguzhibiao$Accper)+ 1

data_length <- length(pricing_factor_data$Stkcd)


#有效因子是PTA(pricing_factor_data$F100501A,季度)，总市值(trading_data$Msmvttl，月度)，非流动性指标(trading_data$ILLIQ,月度)，每股净资产(meiguzhibiao$F091001A,季度)，动量(trading_data$Momentum,月度)，反转(trading_data$Reversal,月度)
factor_data <- data.frame(Stkcd = pricing_factor_data$Stkcd, dates = pricing_factor_data$Accper,
                          pta = pricing_factor_data$F100501A)


trading_data<-trading_data[,c("Stkcd","Trdmnt","ILLIQ","Msmvttl","Momentum","Reversal")]
colnames(trading_data)<-c("Stkcd","dates","ILLIQ","Msmvttl","Momentum","Reversal")


factor_data <-left_join(factor_data,trading_data,by=c("Stkcd","dates" )) 

meiguzhibiao<-meiguzhibiao[,c("Stkcd","Accper","F091001A")]
colnames(meiguzhibiao)<-c("Stkcd","dates","naps")
factor_data <-left_join(factor_data,meiguzhibiao,by=c("Stkcd","dates" )) 


#cor函数不能去掉NA，要手动去掉有NA的行

factor_data<-na.omit(factor_data)

cor1 <- cor(factor_data[,3:8])

write.csv(cor1,"C:\\Users\\carol\\Desktop\\factor\\results\\correlation.csv", row.names = TRUE)
