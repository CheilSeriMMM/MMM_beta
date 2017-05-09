directindirect=function(
  startMonth,
  endMonth
){  
    library(data.table)
    
    resltset=geneqs()  
    
    grpprice<-grpprice(startMonth,endMonth)
    allpricepergrp <- grpprice$pricepergrp
    tvpricepergrp <- grpprice$tvpricepergrp
    capricepergrp <- grpprice$catvpricepergrp
    jppricepergrp <- grpprice$jptvpricepergrp
    
    
    Est=parameterEstimates(model_fit)
    constant=as.numeric(subset(Est, op=='~1'&lhs==depvar, select=c(est))$est)


    makes=subset(Est, op=='~', select=c(lhs, op, rhs, est)) 
    indirectvar=unique(subset(makes, lhs!=depvar)$rhs)
    
    coefset = subset(makes, lhs==depvar, select=c(rhs,est))
    coefset[coefset$var=="lnbtl","coef"]
    
    setnames(coefset, old = c("rhs","est"), new=c("var","coef"))
    medvar=setdiff(unique(makes$lhs),depvar)
    
    a=paste0("ln",ratiolist)
    nu_atl_tmp<- intersect(atllist, gsub("ln","",inputvar))
    
    nu_atl<-NULL
    for(i in 1:length(nu_atl_tmp)){
      aa<-nu_atl_tmp[i]
      if(aa%in%grplist){
        nu_atl<-c(nu_atl, paste0(aa,"*",paste0(sub("grp_","",aa),"pricepergrp")))
      }else {
        nu_atl<-c(nu_atl,aa)
      }
      
    } 
    
    
    nu_atl<-paste(nu_atl,collapse = "+")
    nu_digital<-paste(intersect(digitallist, gsub("ln","",inputvar)),collapse = "+")
    nu_btl<-paste(intersect(btllist, gsub("ln","",inputvar)),collapse = "+")
    
    
    pp = intersect(a,c('lnratio_atl_tocomp', 'lnratio_btl_tocomp', 'lnratio_digital_tocomp'))
    
    
    nulist=NULL
    denolist=NULL
    
    for(i in 1:length(a)){
      nu_tmp=sub("lnratio_","",a[i])
      nu_tmp=sub("_tocomp","",nu_tmp) 
      if(nu_tmp%in%c("atl","btl","digital")){
        nulist=c(nulist,get(paste0("nu_",nu_tmp)))  
      } else{
        nulist=c(nulist,nu_tmp)
      }
      denolist=c(denolist,paste0("comp_",nu_tmp))
    }
    
  
    a1=data.frame(denominator=denolist,
                  numerator=nulist,
                  var=rep(a,stringsAsFactors=F))
    
    temp1=coefset[!coefset$var %in% a,]
    temp2=coefset[coefset$var %in% a,]
    
    
    
    for(i in 1:nrow(temp2)){
      temp=a1[a1$var==temp2[i,1],]
      temp2[i,"numerator"] = paste0("log","(",temp[,"numerator"],")")
      temp2[i,"denominator"] = paste0("log","(",temp[,"denominator"],")")
    }
    
    
    temp1$coef_new=paste0("(",temp1[,"coef"],")")
    temp1$mult=paste(temp1[,"coef_new"],temp1[,1],sep="*")
    equation_temp1=paste(temp1$mult,collapse="+")
    
    
    temp2$coef_new=paste0("(",temp2[,"coef"],")")
    temp2$mult=paste(temp2[,"coef_new"],"*",temp2[,"numerator"],"-",temp2[,"coef_new"],"*",temp2[,"denominator"],sep="")
    equation_temp2=paste(temp2$mult,collapse="+")
    
    equation_temp<-paste0(equation_temp1,"+",equation_temp2)
    equation=paste(equation_temp,constant,sep="+")

    decom_variable=paste0("ln",setdiff(inputvar,ratiolist))  
    n=length(decom_variable)
    
    decompose=stockeddata[,time]
    
    tmpdata = stockeddata
    
    y_hat = with(stockeddata,exp(eval(parse(text=equation))))

    
    for(i in 1:n){
      temp=tmpdata
      aa=decom_variable[i]
      if(gsub("ln","",aa)%in%ratiolist){
        temp[,decom_variable[i]]=log(logratioadj)
      } else {
        temp[,decom_variable[i]]=log(logadj)
      }
      temp1=with(temp,exp(eval(parse(text=equation))))
      temp2=y_hat-temp1
      decompose=cbind(decompose,temp2)
    }
    
  
    colnames(decompose)=c(time, decom_variable)
    decompose=subset(decompose, month>=startMonth & month<=endMonth)
    
    total = subset(salesdecomposition(), month>=startMonth & month<=endMonth)
  
    direct=t(data.frame(lapply(decompose,sum)))
    totalincrements=t(data.frame(lapply(subset(total,select=decom_variable),sum)))
    

    aa=data.frame(cbind(total=totalincrements,direct=direct))
    colnames(aa) = c("total", "direct")
    
    k=length(row.names(aa))
    for(i in 1:k){
      if(row.names(aa)[i]%in%indirectvar){
        
      } else{
        aa[i,"direct"]=aa[i,"total"]
      }
      
    }
    
    aa$indirect = aa$total-aa$direct
    aa$direct_rate = aa$direct/aa$total
    row.names(aa)=gsub("ln","", row.names(aa))

    return(aa)
  }
  
