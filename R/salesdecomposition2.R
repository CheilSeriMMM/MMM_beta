salesdecomposition2=function(
  decom_variable
  ){
  
  resltset<-geneqs()  
  
  a<-min(stockeddata$month)
  b<-max(stockeddata$month)
  
  grpprice<-grpprice(a,b)
  allpricepergrp <- grpprice$pricepergrp
  tvpricepergrp <- grpprice$tvpricepergrp
  capricepergrp <- grpprice$catvpricepergrp
  jppricepergrp <- grpprice$jptvpricepergrp
  
  
  y_hat=with(stockeddata,exp(eval(parse(text=resltset$formula))))
  
  n=length(decom_variable)

  decompose=stockeddata[,time]
  
  for(i in 1:n){
    temp=stockeddata
    aa=decom_variable[i]
    if(gsub("ln","",aa)%in%ratiolist){
      temp[,decom_variable[i]]=log(logratioadj)
    } else {
      temp[,decom_variable[i]]=log(logadj)
    }
    
    temp1=with(temp,exp(eval(parse(text=resltset$formula))))
    temp2=y_hat-temp1
    decompose=cbind(decompose,temp2)
  }
  
  decompose=data.frame(decompose)
  decompose$y=exp(stockeddata[,depvar])
  colnames(decompose)=c(time,decom_variable,depvar)
  decompose$base=decompose[,depvar]-apply(decompose[,c(1:n)],1,sum)
  
  return(decompose)
}

