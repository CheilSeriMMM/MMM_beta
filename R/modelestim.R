modelestim<-function(excl){
  if(excl==0){
    fit <- sem(model, data =data, meanstructure = TRUE)
  } else {
  
  fit <- sem(model, data =data[-c(1:excl),], meanstructure = TRUE)
  }
  
  return(fit)
  
}

