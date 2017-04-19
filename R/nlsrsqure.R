nlsrsqure=function(
  data,
  dependent, 
  nlsobject
){
  mn=mean(data[,dependent])
  data[,"tss"]= (data[,dependent]-mn)^2
  value=1-sum(residuals(nlsobject)^2)/sum(data$tss)
  return(value)
}