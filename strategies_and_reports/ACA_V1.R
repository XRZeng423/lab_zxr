
library(xts)

library(PerformanceAnalytics)

library(lubridate)

library(quantmod)

library(TTR)



ACA<-function(all_assets){
  
  all_assets<-as.data.frame(all_assets)
  all_assets <- cbind(date=as.Date(rownames(all_assets)),all_assets)
  #all_assets<-xts(all_assets[,-1], ymd(all_assets[,1])) #xts
  #t<-1:length(all_assets[,4])
  rownames(all_assets)<-1:length(all_assets[,4])
  list<-c("RA1","RA2","RA3","DA1","DA2")
  colnames(all_assets)<-c('date',list)
  
  #esquisse::esquisser()
  
  
  
  price_channel <- data.frame(RA1 = matrix(NA,5,1),RA2 = matrix(NA,5,1),RA3 = matrix(NA,5,1),row.names = c("UC_126","UC_252","LC_126","LC_252","close"))
  
  #做一个放weights的表
  #列名是数字，第一列是日期
  l<-length(all_assets[,4])-250
  asset_weight <- data.frame(date=all_assets$date[251:length(all_assets[,4])],RA1 = matrix(NA,l,1),RA2 = matrix(NA,l,1),RA3 = matrix(NA,l,1),DA2 = matrix(NA,l,1),DA1 = matrix(NA,l,1),DA3 = matrix(NA,l,1),DA= matrix(NA,l,1), row.names =251:length(all_assets[,4]) )
  
  
  ##列名是日期，第一列是数字
  #l<-length(all_assets[,4])-252
  #asset_weight <- data.frame(t=251:length(all_assets[,4]),RA1 = matrix(NA,l,1),RA2 = matrix(NA,l,1),RA3 = matrix(NA,l,1),DA = matrix(NA,l,1),DA2 = matrix(NA,l,1),row.names = all_assets$date[251:length(all_assets[,4])])
  
  asset_weight[1,'RA1'] = 1/3
  asset_weight[1,'RA2'] = 1/3
  asset_weight[1,'RA3'] = 1/3
  asset_weight[1,'DA1'] = 0
  asset_weight[1,'DA3'] = 0
  asset_weight[1,'DA2'] = 0
  asset_weight[1,'DA']=0
  
  for(i in 252:length(all_assets[,4])){
    #max(all_assets$RA1[1:252])
    #max(all_assets$RA1[127:252])
    
    #i=252
    #RA1过去252天最高收盘价
    UC_252_RA1<-max(all_assets$RA1[(i-251):i])
    #RA1过去252天最低收盘价
    LC_252_RA1<-min(all_assets$RA1[(i-251):i])
    #RA1过去126天最高收盘价
    UC_126_RA1<-max(all_assets$RA1[(i-125):i])
    #RA1过去126天最低收盘价
    LC_126_RA1<-min(all_assets$RA1[(i-125):i])
    
    #填入RA1的price channel表
    price_channel['UC_126','RA1']<-UC_126_RA1
    price_channel['UC_252','RA1']<-UC_252_RA1
    price_channel['LC_126','RA1']<-LC_126_RA1
    price_channel['LC_252','RA1']<-LC_252_RA1
    price_channel['close','RA1']<-all_assets$RA1[i]
    
    #RA2过去252天最高收盘价
    UC_252_RA2<-max(all_assets$RA2[(i-251):i])
    #RA2过去252天最低收盘价
    LC_252_RA2<-min(all_assets$RA2[(i-251):i])
    #RA2过去126天最高收盘价
    UC_126_RA2<-max(all_assets$RA2[(i-125):i])
    #RA2过去126天最低收盘价
    LC_126_RA2<-min(all_assets$RA2[(i-125):i])
    
    #填入RA2的price channel表
    price_channel['UC_126','RA2']<-UC_126_RA2
    price_channel['UC_252','RA2']<-UC_252_RA2
    price_channel['LC_126','RA2']<-LC_126_RA2
    price_channel['LC_252','RA2']<-LC_252_RA2
    price_channel['close','RA2']<-all_assets$RA2[i]
    
    #RA3过去252天最高收盘价
    UC_252_RA3<-max(all_assets$RA3[(i-251):i])
    #RA2过去252天最低收盘价
    LC_252_RA3<-min(all_assets$RA3[(i-251):i])
    #RA2过去126天最高收盘价
    UC_126_RA3<-max(all_assets$RA3[(i-125):i])
    #RA2过去126天最低收盘价
    LC_126_RA3<-min(all_assets$RA3[(i-125):i])
    
    #填入RA3的price channel表
    price_channel['UC_126','RA3']<-UC_126_RA3
    price_channel['UC_252','RA3']<-UC_252_RA3
    price_channel['LC_126','RA3']<-LC_126_RA3
    price_channel['LC_252','RA3']<-LC_252_RA3
    price_channel['close','RA3']<-all_assets$RA3[i]
    
    
    
    #对于第一组
    if(price_channel['close','RA1']>= price_channel['UC_126','RA1']){
      asset_weight[(i-250),'RA1'] = 1/3
      asset_weight[(i-250),'DA1'] = 0
    } else if(price_channel['close','RA1']<= price_channel['LC_252','RA1']){
      asset_weight[(i-250),'DA1'] = 1/3
      asset_weight[(i-250),'RA1'] = 0
    } else{
      asset_weight[(i-250),'RA1'] = asset_weight[(i-251),'RA1']
      asset_weight[(i-250),'DA1'] = asset_weight[(i-251),'DA1']
    }
    
    
    
    #对于第二组
    if(price_channel['close','RA2']>= price_channel['UC_252','RA2']){
      asset_weight[(i-250),'RA2'] = 1/3
      asset_weight[(i-250),'DA2'] = 0
    } else if(price_channel['close','RA2']<= price_channel['LC_126','RA2']){
      asset_weight[(i-250),'DA2'] = 1/3
      asset_weight[(i-250),'RA2'] = 0
    } else{
      asset_weight[(i-250),'RA2'] = asset_weight[(i-251),'RA2']
      asset_weight[(i-250),'DA2'] = asset_weight[(i-251),'DA2']
    }
    
    #对于第三组
    if(price_channel['close','RA3']>= price_channel['UC_126','RA3']){
      asset_weight[(i-250),'RA3'] = 1/3
      asset_weight[(i-250),'DA3'] = 0
    } else if(price_channel['close','RA3']<= price_channel['LC_252','RA3']){
      asset_weight[(i-250),'DA3'] = 1/3
      asset_weight[(i-250),'RA3'] = 0
    }  else{
      asset_weight[(i-250),'RA3'] = asset_weight[(i-251),'RA3']
      asset_weight[(i-250),'DA3'] = asset_weight[(i-251),'DA3']
    }
    
    asset_weight[(i-250),'DA']=asset_weight[(i-250),'DA1']+asset_weight[(i-250),'DA3']
    
  }
  asset_weight<-asset_weight[,c(1,2,3,4,8,5)]
  colnames(asset_weight)<-c("date",symbols)
  return(asset_weight)
}




#input
RA1<-"SPY"
RA2<-"GLD"
RA3<-"VNQ"
DA1<-"IEF"
DA2<-"TLT"

start_date <- "2007-01-01"  
end_date <- "2020-07-14"  

#download financial data
symbols<-c(RA1,RA2,RA3,DA1,DA2)

getSymbols(symbols, from=start_date,to=end_date)

all_assets<-list()
for(i in 1:length(symbols)) {
  all_assets[[i]] <- Cl(get(symbols[i]))  
}
all_assets <- do.call(cbind, all_assets) #xts
colnames(all_assets)<-symbols

Returns<-function(all_assets){    
  Returns <- do.call(merge.xts,lapply(colnames(all_assets),function(x){ 
    ret = periodReturn(all_assets[,x],period = "daily");
    colnames(ret) = x;
    return(ret) 
  } ))
  Returns<-as.data.frame(Returns)
  Returns<- cbind(date=as.Date(rownames(Returns)),Returns)
}

Returns<-Returns(all_assets)
Strategy<-ACA(all_assets)

#write.csv(returns,"E:\\asset_allocation\\ACA\\assets_returns.csv", row.names = TRUE)
#write.csv(asset_weight,"E:\\asset_allocation\\ACA\\assets_weights.csv", row.names = FALSE)
