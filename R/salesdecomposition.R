
salesdecomposition=function(decom_variable){

y_hat=with(stockeddata,exp(eval(parse(text=resltset$formula))))
n=length(decom_variable)
decompose=NULL

for(iin1:n){
temp=stockeddata
temp[,decom_variable[i]]=0
temp1=with(temp,exp(eval(parse(text=resltset$formula))))
temp2=y_hat-temp1
decompose=cbind(decompose,temp2)
}

decompose=data.frame(decompose)
decompose$y=exp(stockeddata[,depvar])
colnames(decompose)=c(decom_variable,depvar)
decompose$base=decompose[,depvar]-apply(decompose[,c(1:n)],1,sum)

assign('decompose_dataset',decompose,envir=.GlobalEnv)
}

