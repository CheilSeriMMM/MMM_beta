
salesdecomposition=function(){
  
  geneqs()
  
  dependent<-y
  decom_variable<-inputvar
  
  y_hat=with(data, exp(eval(parse(text=full_equation))))
  n=length(decom_variable)
  
  decompose=NULL
  for(i in 1:n){
    temp=data
    temp[,decom_variable[i]]=0
    temp1=with(temp,exp(eval(parse(text=full_equation))))
    temp2=y_hat-temp1
    decompose=cbind(decompose,temp2)
  }
  decompose=data.frame(decompose)
  decompose$y=data[,dependent]
  colnames(decompose)=c(decom_variable,dependent)
  decompose$base=decompose[,dependent]-apply(decompose[,c(1:n)],1,sum)
  
  return(decompose)
}


