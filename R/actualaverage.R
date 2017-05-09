actualaverage=function(
  startMonth,
  endMonth
){
    
    dd=geneqs()
    
    grpprice<-grpprice(startMonth,endMonth)
    allpricepergrp <- grpprice$pricepergrp
    tvpricepergrp <- grpprice$tvpricepergrp
    capricepergrp <- grpprice$catvpricepergrp
    jppricepergrp <- grpprice$jptvpricepergrp
    
    
    sim_data2<-subset(stockeddata, month>=startMonth & month<=endMonth)
    param2<-data.frame(lapply(sim_data2,mean))
    
    out = subset(param2, select=c(gsub("ln","",depvar),inputvar))

    yy=intersect(grplist, inputvar)
    
    if(length(yy)>0){
      for(i in 1:length(yy)){
        kk=paste0(yy[i],"_cost")
        kkc=out[1,yy[i]]*get(paste0(gsub("grp_","",yy[i]),"pricepergrp"))
        out=cbind(out,kkc)
        setnames(out, old="kkc", new=kk)
      }
    }

    return(out)
}
