#检测相关系数的程序
library(readr)
library(lubridate)
trading_data <- read_delim("C:/Users/hasee/Desktop/work documents/data/trading data/trading data.txt", 
                           "\t", escape_double = FALSE, col_types = cols(Trdmnt = col_date(format = "%Y/%m/%d")), 
                           trim_ws = TRUE)
pricing_factor_data <- read_delim("C:/Users/hasee/Desktop/work documents/data/pricing factor data.txt", 
                                  "\t", escape_double = FALSE, col_types = cols(Accper = col_date(format = "%Y/%m/%d")), 
                                  trim_ws = TRUE)
financial_stock_code <- read_csv("C:/Users/hasee/Desktop/work documents/data/financial stock code.txt", 
                                 col_types = cols(Stkcd = col_number()))
trading_data$ILLIQ <- abs(trading_data$Mretwd)/trading_data$Mnvaltrd/trading_data$Ndaytrd

for (i in 1:length(financial_stock_code$Stkcd)){
  pricing_factor_data <- pricing_factor_data[pricing_factor_data$Stkcd!=financial_stock_code$Stkcd[i],]
  trading_data <- trading_data[trading_data$Stkcd!=financial_stock_code$Stkcd[i],]
}

data_length <- length(pricing_factor_data$Stkcd)

factor_data <- data.frame(Stkcd = pricing_factor_data$Stkcd, dates = pricing_factor_data$Accper,
                          pb = pricing_factor_data$F100401A, ILLIQ = matrix(NA,data_length,1),
                          vol = matrix(NA,data_length,1))
#   vol = matrix(NA,data_length,1), volatility = matrix(NA,data_length,1))
#如果没有相关的数据，运用向量运算会返回一个长度为0的向量，既不是NULL也不是NA
for(i in 1:data_length){
  if(!length(trading_data$ILLIQ[trading_data$Trdmnt==factor_data$dates[i] & trading_data$Stkcd==factor_data$Stkcd[i]])){
    factor_data$ILLIQ[i] <- NA
  }else{
    factor_data$ILLIQ[i] <- trading_data$ILLIQ[trading_data$Trdmnt==factor_data$dates[i] & trading_data$Stkcd==factor_data$Stkcd[i]]
  }
  if(!length(trading_data$Msmvttl[trading_data$Trdmnt==factor_data$dates[i] & trading_data$Stkcd==factor_data$Stkcd[i]])){
    factor_data$vol[i] <- NA
  }else{
    factor_data$vol[i] <- trading_data$Msmvttl[trading_data$Trdmnt==factor_data$dates[i] & trading_data$Stkcd==factor_data$Stkcd[i]]
  }
}
#cor函数不能去掉NA，要手动去掉有NA的行
factor_data <- factor_data[!is.na(factor_data$pb),]
factor_data <- factor_data[!is.na(factor_data$ILLIQ),]
factor_data <- factor_data[!is.na(factor_data$vol),]

cor1 <- cor(factor_data[,3:5])
#cor2 <- cor(factor_data)

write.csv(cor1,file ="C:\\Users\\hasee\\Desktop\\work documents\\data\\results\\factor-correlation.csv",)