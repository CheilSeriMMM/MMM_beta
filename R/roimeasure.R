roimeasure<-function(startMonth,endMonth){
  
  decompose_dataset<-salesdecomposition()
  grpprice(startMonth,endMonth)

  salesincrement <-data.frame(apply(decompose_dataset, 2,sum))
  salesincrement<-t(salesincrement);
  rownames(salesincrement)<-NULL
  
  stockeddata=stockeddata[order(stockeddata$month,stockeddata$week),]
  min_num <- as.numeric(min(rownames(stockeddata[stockeddata$month==startMonth,])))
  inputvar2<-setdiff(inputvar,ratiolist)
  
  inv_data_unstocked<- subset(unstockeddata, month>=startMonth & month<=endMonth, select=inputvar)   
  inv_data_stocked<- subset(stockeddata, month>=startMonth & month<=endMonth, select=inputvar)   
  
  weekduration=nrow(inv_data_unstocked)
  
  stockedinvestment<-data.frame(lapply(inv_data_stocked,sum))
  
  inv_data_lastweek<- stockeddata[min_num-1,inputvar] 
  
  
  investment_origin<-apply(inv_data_unstocked,2,sum)
  investment_origin<-rbind(0,investment_origin,0.0,0.0,0.0,0.0,0.0,0.0)
  invrownames<-c("log_varname","(a) total Investment", "(b) total Stock generation", "(c) stock used", "(d) ratio (c)/(b)", "(e) real Investment", "(f) sales Increment", "(g) RoI (e)/(f)")
  rownames(investment_origin)<-invrownames
  
  investment<-investment_origin
  
  
  n<-ncol(inv_data_unstocked)
  
  for(p in 1:n){ 
    unlimstock<-0
    newused00<-0
    nameee <-colnames(inv_data_unstocked)[p]
    logname<-paste0("ln",nameee)
    
    investment[1,p]<-logname
    
    investment[3,p]<-investment[2,p]
    investment[4,p]<-investment[2,p]
    
    if(investment[4,p]<=0){
      investment[5,p]<-0
    } else {
      
      denomi<-as.double(investment[3,p])
      nume<- as.double(investment[4,p])
      
      investment[5,p]<- nume/denomi
    }
    
    totalinv<-as.double(investment[2,p])
    ratio<-as.double(investment[5,p])
    investment[6,p]<-totalinv* ratio
    
    
    if(logname%in%colnames(salesincrement)){
      investment[7,p]<- salesincrement[,logname]
    } else if(nameee%in%colnames(salesincrement)){
      investment[7,p]<- salesincrement[,nameee]
      
    } else {
      investment[7,p]<- 0
    }
    if(investment[7,p] ==0){
      investment[8,p] <-0
    } else {
      realinv<-as.double(investment[6,p])
      salesincre<-as.double(investment[7,p])
      
      investment[8,p]<-salesincre/realinv
    }
  }
  
  lnstockedvarlist<-NULL
  
  for(i in 1:length(stockedvarlist)){
    mm<- paste0("ln",stockedvarlist[i])
    lnstockedvarlist<-union(lnstockedvarlist,mm)
  }
  
  noprinputvar<-setdiff(inputvar,c("lnpr","pr",stockedvarlist,lnstockedvarlist))
  investment2<-subset(investment, select=(colnames(investment) %in% noprinputvar) | (investment[1,] %in% noprinputvar))
  
  
  vt<-intersect(inputvar,union(stockedvarlist,lnstockedvarlist))
  nologvt<-NULL
  
  for(i in 1:length(vt)){
    jjj<-sub("ln","",vt[i])
    nologvt<-union(nologvt,jjj)
  }
  
  
  atlinvestment<-subset(investment_origin, select=nologvt)
  atlroiexist<-atlinvestment
  atlroiexist[2,]<-0
  
  a=as.numeric(carryover[carryover$media=="atl",2])
  
  for(p in 1:length(vt)){ 
    
    unlimstock<-0
    newused00<-0
    
    nameee <-nologvt[p]
    logname<-paste0("ln",nameee)
    
    atlinvestment[1,p]<-logname
    atlroiexist[1,p]<-paste0(logname,"exist")
    
    
    for (i in 1:nrow(inv_data_unstocked)){
      inputval<-inv_data_unstocked[i,nameee]

      
      if(inputval==0){
        stocked <-0
      } else{
        stocked<- inputval/(1-a)
      }
      unlimstock <- unlimstock +stocked
    }
    
    atlinvestment[3,p]<-unlimstock
    
    x1<-as.double(stockedinvestment[,nameee])
    x2<-as.double(inv_data_lastweek[[nameee]]*(1-a^(weekduration+1))/(1-a))
    
    newused00 <- (x1-x2)
    atlinvestment[4,p]<- newused00 
    atlroiexist[4,p]<- x2 
    
    if(atlinvestment[4,p]<=0){
      atlinvestment[5,p]<-0
    } else {
      
      denomi<-as.double(atlinvestment[3,p])
      nume<- as.double(atlinvestment[4,p])
      
      atlinvestment[5,p]<- nume/denomi
    }
    
    
    if(nameee%in%grplist){
      if(nameee == "grp_all"){
        totalinv <-as.double(atlinvestment[2,p])*grpprice$pricepergrp
      } else if (nameee == "grp_tv"){
        totalinv <-as.double(atlinvestment[2,p])*grpprice$tvpricepergrp
      } else if (nameee == "grp_ca"){
        totalinv <-as.double(atlinvestment[2,p])*grpprice$catvpricepergrp
      } else if (nameee == "grp_jp"){
        totalinv <-as.double(atlinvestment[2,p])*grpprice$jptvpricepergrp
      }
      
    } else{ 
      totalinv<-as.double(atlinvestment[2,p])
    }
    
    ratio<-as.double(atlinvestment[5,p])
    atlinvestment[6,p]<-totalinv* ratio 
    
    
    if(logname%in%colnames(salesincrement)){
      atlinvestment[7,p]<- salesincrement[,logname] * (newused00/x1)
      atlroiexist[7,p]<- salesincrement[,logname] * (x2/x1)
    } else if(nameee%in%colnames(salesincrement)){
      atlinvestment[7,p]<- salesincrement[,nameee] * (newused00/x1)
      atlroiexist[7,p]<- salesincrement[,nameee] * (x2/x1)
    } else {
      atlinvestment[7,p]<- 0
      atlroiexist[7,p]<- 0
    }
    
    if(atlinvestment[7,p] ==0){
      atlinvestment[8,p] <-0
    } else {
      realinv<-as.double(atlinvestment[6,p])
      salesincre<-as.double(atlinvestment[7,p])
      
      atlinvestment[8,p]<-salesincre/realinv
    }
  }
  
  
  investment3<-cbind(investment2,atlinvestment,atlroiexist)
  
  plpl<-as.numeric(investment3[7,])/sum(as.numeric(investment3[7,]))
  
  investment3 <- data.frame(investment3, stringsAsFactors=FALSE)
  investment3[9,] <- plpl
  rownames(investment3)[9] <-'sales Increment(ratio to total)'
  
  
  return(investment3)
}
