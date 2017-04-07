grpprice<-function(startMonth,endMonth){
  
  grpprice<-NULL
  
  alltvinputtotal<-sum(stockeddata[(stockeddata$month>=startMonth & stockeddata$month<=endMonth),c('All_TV')])
  allgrpinputtotal<-sum(stockeddata[(stockeddata$month>=startMonth & stockeddata$month<=endMonth),c('grp_all')])
  
  grpprice$pricepergrp <- alltvinputtotal/allgrpinputtotal
  
  tvinputtotal<-sum(stockeddata[(unstockeddata$month>=startMonth & stockeddata$month<=endMonth),c('tv')])   
  tvgrpinputtotal<-sum(stockeddata[(unstockeddata$month>=startMonth & stockeddata$month<=endMonth),c('grp_tv')])   
  
  grpprice$tvpricepergrp <- tvinputtotal/tvgrpinputtotal
  
  catvinputtotal<-sum(stockeddata[(unstockeddata$month>=startMonth & stockeddata$month<=endMonth),c('cable_tv')])   
  catvgrpinputtotal<-sum(stockeddata[(unstockeddata$month>=startMonth & stockeddata$month<=endMonth),c('grp_ca')])   
  
  grpprice$catvpricepergrp <- catvinputtotal/catvgrpinputtotal     
  
  jptvinputtotal<-sum(stockeddata[(unstockeddata$month>=startMonth & stockeddata$month<=endMonth),c('jp')])   
  jptvgrpinputtotal<-sum(stockeddata[(unstockeddata$month>=startMonth & stockeddata$month<=endMonth),c('grp_jp')])   
  
  grpprice$jptvpricepergrp <- jptvinputtotal/jptvgrpinputtotal      
  
  assign('grpprice',grpprice,envir = .GlobalEnv) 
} 
