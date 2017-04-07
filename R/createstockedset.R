createstockedset <- function(carryover) {
  
  detach("package:dplyr", unload=TRUE)
  stockedcand=carryover[carryover$lambda > 0,]
  
  for(i in 1:nrow(stockedcand)) {
    if(stockedcand$media=="digital") {
      mediavar=digitalvar
    } else if(stockedcand$media=="atl") {
      mediavar=atlvar
    } else {
      print("Error occured during carryover process! Check that module!!")
    }
    idx=colnames(unstockeddata) %in% mediavar
    unstockeddata[,idx] = filter(unstockeddata[,idx], filter=stockedcand$lambda, method="recursive")
  }
  
  stockeddata=unstockeddata
  write.csv(stockeddata,"stockeddata.csv",row.names=F)
  
  library(dplyr)
  return(stockeddata)
  
}
