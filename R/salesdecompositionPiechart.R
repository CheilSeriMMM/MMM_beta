salesdedicationPiechart<-function(startMonth, endMonth){
  
  investment3<-roimeasure(startMonth, endMonth)
 
  tmp=as.data.frame(t(investment3[c(2:nrow(investment2)),]),stringsAsFactors=F)
  
  for(i in 1:ncol(tmp)) {
    tmp[,c(i)]=as.numeric(tmp[,c(i)])
  }
 
  tmp$sales_perc=round(tmp$`(f) sales Increment`/sum(tmp$`(f) sales Increment`)*100,1)
  tmp$label=paste(row.names(tmp),paste0(tmp$sales_perc,"%"),sep=" / ")
  
  library(plotrix)
  pie1<-pie(tmp$`(f) sales Increment`, labels = tmp$label, explode=0.1, radius=.9, labelcex = 1.2,  start=0.7)
  
  
  return(pie1)
  
}
