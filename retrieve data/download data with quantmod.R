## download ETF(or stocks, etc) price data with quantmod
##https://alastairrushworth.github.io/Easily-Download-Stock-price-Data-with-quantmod/

#get Risky assets(SPY, QQQ, IWM, VGK, EWJ, EEM, VNQ, DBC, GLD, LQD, HYG, TLT) and Crash protective asset(IEF)
#daily
#1989-01-02~2016-12-30（可下载的数据不够）
#save as csv

library(quantmod)

start_date <- "2008-01-02"
end_date <- "2016-12-29"

# download financial data
getSymbols("SPY",from=start_date,to=end_date)  # S&P 500
getSymbols("QQQ",from=start_date,to=end_date)  # Nasdaq 100 
getSymbols("IWM",from=start_date,to=end_date)  # Russell 2000
getSymbols("VGK",from=start_date,to=end_date)  # Europe equities
getSymbols("EWJ",from=start_date,to=end_date)  # Japan equities
getSymbols("EEM",from=start_date,to=end_date)  # emerging market equities
getSymbols("VNQ",from=start_date,to=end_date)  # US real estate
getSymbols("DBC",from=start_date,to=end_date)  # commodities 
getSymbols("GLD",from=start_date,to=end_date)  # gold
getSymbols("LQD",from=start_date,to=end_date)  # US corporate bonds
getSymbols("HYG",from=start_date,to=end_date)  # US high yield bonds 
getSymbols("TLT",from=start_date,to=end_date)  # long-term US Treasuries
getSymbols("IEF",from=start_date,to=end_date)  #  intermediate-term US Treasuries

all_assets <- list(SPY, QQQ, IWM, VGK, EWJ, EEM, VNQ, DBC, GLD, LQD, HYG, TLT, IEF)
all_assets <- lapply(all_assets, Cl)
all_assets <- as.data.frame(all_assets)
all_assets <- cbind(date=as.Date(rownames(all_assets)),all_assets)
write.csv(all_assets,"d:\\Users\\XuranZENG\\Desktop\\GPM_data.csv", row.names = FALSE)
