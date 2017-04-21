createstockedset <- function(unstockeddata, carryover, time) {
  
  if("R.utils" %in% rownames(installed.packages())) {
    library(R.utils)
  } else {
    install.packages("R.utils")
    library(R.utils)
  }
  
  
  if(isPackageLoaded("dplyr")) {
    detach("package:dplyr", unload=TRUE)
  }
  
  stockedcand=carryover$media
  
  
  for(i in 1:ncol(unstockeddata)){
    va=colnames(unstockeddata)[i]
    
    if(va%in%stockedcand){
      unstockeddata[,va] = filter(unstockeddata[,va], filter=carryover[carryover$media==va,2], method="recursive")
    }
  }

  
  stockeddata=unstockeddata
  log_data=stockeddata[,]

  logadj<-5
  ration_adj<-0.00001
  
  
  for(i in 4:ncol(log_data)) {
    if(grepl('ratio_', colnames(log_data)[i])) {
      log_data[,i]=log(log_data[,i]+ration_adj)
    } else {
      log_data[,i]=log(log_data[,i]+logadj)
    }
  }
  
  colnames(log_data)[4:ncol(log_data)]=paste('ln',colnames(log_data)[4:ncol(log_data)],sep="")
  
  stockeddata=merge(stockeddata, log_data, by=time)
  
  write.csv(stockeddata,"stockeddata.csv",row.names=F)
  
  library(dplyr)
  return(stockeddata)
}
