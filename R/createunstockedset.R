createunstockedset=function(){
  log_data=unstockeddata[,]
  
  logadj<-5
  ration_adj<-0.00001
  
  for(i in 4:ncol(log_data)) {
    if(grepl('ratio_',colnames(log_data)[i])) {
      log_data[,i]=log(log_data[,i]+ration_adj)
    } else {
      log_data[,i]=log(log_data[,i]+logadj)
    }
  }
  
  colnames(log_data)[4:ncol(log_data)]=paste('ln',colnames(log_data)[4:ncol(log_data)],sep="")
  
  nrow(unique(unstockeddata[,c(1:3)]))
  nrow(unique(log_data[,c(1:3)]))
  
  data=merge(unstockeddata,log_data,by=c('quarter','month','week'))
  write.csv(data,"unstockeddata.csv",row.names=F)
  
  return(data)
  
}
