

monthly.return<-function(ret){    
  ### this function takes daily prices and outputs monthly returns
  ### Args:
  ### ret: xts object of daily prices
  ### output:
  ### xts of monthly returns
  Returns <- do.call(merge.xts,lapply(colnames(asset_price),function(x){ 
    
    ret = periodReturn(asset_price[,x],period = "monthly");
    
    colnames(ret) = x;
    
    return(ret) 
    
  } ))
  
}

daily.return<-function(ret){    
  ### this function takes daily prices and outputs daily returns
  ### Args:
  ### ret: xts object of daily prices
  ### output:
  ### xts of daily returns
  Returns <- do.call(merge.xts,lapply(colnames(asset_price),function(x){ 
    
    ret = periodReturn(asset_price[,x],period = "daily");
    
    colnames(ret) = x;
    
    return(ret) 
    
  } ))
  

}
