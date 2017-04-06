grpprice<-function(
  startMonth,
  endMonth
){
    
    transformed1<-data
    transformed2<-unstockeddata
    
    
    
    grpprice<-NULL
    alltvinputtotal<-sum(transformed1[(transformed1$month>=startMonth & transformed1$month<=endMonth),c('All_TV')])
    allgrpinputtotal<-sum(transformed1[(transformed1$month>=startMonth & transformed1$month<=endMonth),c('grp_all')])
    
    grpprice$pricepergrp <- alltvinputtotal/allgrpinputtotal
    
    tvinputtotal<-sum(transformed1[(transformed2$month>=startMonth & transformed1$month<=endMonth),c('tv')])
    tvgrpinputtotal<-sum(transformed1[(transformed2$month>=startMonth & transformed1$month<=endMonth),c('grp_tv')])
    grpprice$tvpricepergrp <- tvinputtotal/tvgrpinputtotal
    
    catvinputtotal<-sum(transformed1[(transformed2$month>=startMonth & transformed1$month<=endMonth),c('cable_tv')])
    catvgrpinputtotal<-sum(transformed1[(transformed2$month>=startMonth & transformed1$month<=endMonth),c('grp_ca')])
    grpprice$catvpricepergrp <- catvinputtotal/catvgrpinputtotal
    
    jptvinputtotal<-sum(transformed1[(transformed2$month>=startMonth & transformed1$month<=endMonth),c('jp')])
    jptvgrpinputtotal<-sum(transformed1[(transformed2$month>=startMonth & transformed1$month<=endMonth),c('grp_jp')])
    grpprice$jptvpricepergrp <- jptvinputtotal/jptvgrpinputtotal


    return(grpprice)
}



