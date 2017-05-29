make_objfunc_gradient=function(
  startMonth,
  endMonth
  ){
  
  dd=geneqs()
  dffincoeff = dd$coefset
  
  
  grpprice<-grpprice(startMonth,endMonth)
  
  allpricepergrp <- grpprice$pricepergrp
  tvpricepergrp <- grpprice$tvpricepergrp
  capricepergrp <- grpprice$catvpricepergrp
  jppricepergrp <- grpprice$jptvpricepergrp

  
  vg=intersect(dffincoeff$var, c(inputvar,paste0("ln",inputvar)))

  a=dffincoeff[dffincoeff$var%in%vg,]
  
  lefts<-rep('x[', nrow(a))
  ggg<-paste(lefts, seq(1:nrow(a)),sep="")
  ggg<-paste(ggg, ']', sep="")
  a$vars<-ggg
  
  
  for(i in 1:nrow(a)){
    nolog3 <- sub("ln","",a[i,"var"]) 
    
    if(nolog3%in%atllist){            
      a[i,"group"]<-"atl"  
    } else if(nolog3%in%btllist){            
      a[i,"group"]<-"btl"  
    } else if(nolog3%in%digitallist){            
      a[i,"group"]<-"digital"  
    } else {
      a[i,"group"]<-"other"
    }
    
  }
  
  a$multiplier<-a$coef-1
  a$multiplier<-paste('(', a$multiplier, ')', sep="")
  a$coef<-paste('(',a$coef,')', sep="")
  
  a$deriv<-paste(a$vars, a$multiplier, sep="^")
  a$deriv<-paste('(', a$deriv, ')', sep="")
  a$deriv<-paste(a$coef, a$deriv, sep='*')
  a$raws<-paste(a$vars, a$coef, sep="^")
  a$raws<-paste('(',a$raws,')',sep="")
  
  a$partialdiff<-1
  
  for(i in (1:nrow(a))){
    a[i,"partialdiff"]<-paste(a$raws[-i], collapse="*")
  }
  
  
  
  
  for(i in 1:nrow(a)){
    noln <- sub("ln","",a[i,"var"])
    if(noln%in%grplist){                            
      if(noln=="grp_all"){
        grpprice <- allpricepergrp       
      } else if(noln=="grp_tv"){
        grpprice <- tvpricepergrp
      } else if(noln=="grp_ca"){
        grpprice <- capricepergrp
      } else{
        grpprice <- jppricepergrp
      }
    } else {
      grpprice <-1
    }
    
    a[i,"vars_expdt"] <- paste(a[i,"vars"],grpprice,sep='*')
    a[i,"multiplier_expdt"] <- grpprice
  }
  
  
  
  a$partialdiff<-paste(a$deriv, a$partialdiff, sep="*")
  a$partialdiff<-paste("baserev",a$partialdiff, sep="*")
  
  objfunc_origin<-paste(a$raws, collapse="*")
  
  spnedfunc<-paste(a$vars_expdt, collapse="+")
  spnedfunc<-paste0("(",spnedfunc)
  spnedfunc<-paste0(spnedfunc,")")
  
  a[,"partialdiff_expdt"]<-a[,"multiplier_expdt"]
  

  
  digitalra=NULL
  atlra=NULL
  btlra=NULL
  

  if("lnratio_atl_tocomp"%in%dffincoeff$var){
    atlratiocoeff<-dffincoeff[which(dffincoeff$var=="lnratio_atl_tocomp"),2]
    atlnusubset<-subset(a,group=="atl")
    atlnu<-paste(atlnusubset$vars,collapse="+")
    atlnu<-paste0("(",atlnu,")")
    atlra<-paste0("(",atlnu,"^",atlratiocoeff,")")   
  }
  
  if("lnratio_btl_tocomp"%in%dffincoeff$var){
    btlratiocoeff<-dffincoeff[which(dffincoeff$var=="lnratio_btl_tocomp"),2]
    btlnusubset<-subset(a,group=="btl")
    btlnu<-paste(btlnusubset$vars,collapse="+")
    btlnu<-paste0("(",btlnu,")")
    btlra<-paste0("(",btlnu,"^",btlratiocoeff,")") 
  }
  
  if("lnratio_digital_tocomp"%in%dffincoeff$var){
    digitalratiocoeff<-dffincoeff[which(dffincoeff$var=="lnratio_digital_tocomp"),2]
    digitalnusubset<-subset(a,group=="digital")
    digitalnu<-paste(digitalnusubset$vars,collapse="+")
    digitalnu<-paste0("(",digitalnu,")")
    digitalra<-paste0("(",digitalnu,"^",digitalratiocoeff,")")   
  }

  
   if(is.null(digitalra)&is.null(atlra)&is.null(btlra)){
    ratio_func=NULL
  } else { 
    ratio_func<-paste(c(digitalra,atlra,btlra),collapse="*")   
  }
  
  objfunc<-paste(c(objfunc_origin,ratio_func),collapse="*")
  
  finobjfunc<-paste('-1',objfunc, sep="*")
  
  
  
  a$comp_ratio_nu=1
  a$partialdiff_ratio=1
  
   for(i in 1:nrow(a)){
    if(a[i,"group"]=="digital"&"lnratio_digital_tocomp"%in%dffincoeff$var){
      nn<-paste(digitalratiocoeff,digitalnu,sep="*")      
      nn<-paste0(nn,"^","(",digitalratiocoeff-1,")")
      nn<-paste(nn,digitalra,sep="*")
      a[i,"comp_ratio_nu"]<-digitalra
      a[i,"partialdiff_ratio"]<-paste("baserev",nn,sep="*")
      
    } else if(a[i,"group"]=="atl"&"lnratio_atl_tocomp"%in%dffincoeff$var){
      nn<-paste(atlratiocoeff,atlnu,sep="*")
      nn<-paste0(nn,"^","(",atlratiocoeff-1,")")
      nn<-paste(nn,atlra,sep="*")
      a[i,"comp_ratio_nu"]<-atlra
      a[i,"partialdiff_ratio"]<-paste("baserev",nn,sep="*")
    } else if(a[i,"group"]=="btl"&"lnratio_btl_tocomp"%in%dffincoeff$var){
      nn<-paste(btlratiocoeff,btlnu,sep="*")
      nn<-paste0(nn,"^","(",btlratiocoeff-1,")")
      nn<-paste(nn,btlra,sep="*")
      a[i,"comp_ratio_nu"]<-btlra
      a[i,"partialdiff_ratio"]<-paste("baserev",nn,sep="*")
    } 
    
  }
  

  if(is.null(ratio_func)){
    a$final_partialdiff2<-a$partialdiff
  } else {
    a$final_partialdiff2<-paste0(a$partialdiff,"*",ratio_func,"+",objfunc_origin,"*",a$partialdiff_ratio)
  }
        
      
      
      
  a$final_partialdiff1<-paste(a$final_partialdiff2,a$multiplier_expdt,sep="-")
  
  
  b=NULL
  for(i in 1:nrow(a)){
    b[i]=parse(text=paste0(a[i,"final_partialdiff1"]))
  }
  
  
  d=NULL
  for(i in 1:nrow(a)){
    d[i]=parse(text=paste0(a[i,"final_partialdiff2"]))
  }
  
  
  c=NULL
  for(i in 1:nrow(a)){
    c[i]=parse(text=a[i,"partialdiff_expdt"])
  }
  
  
  objfuncbase1<-paste("baserev",finobjfunc,sep="*")
  objfuncbase2<-paste("baserev",objfunc,sep="*")
  
  
  objfunc1<-paste(objfuncbase1,spnedfunc,sep="+")
  objfunc2<-spnedfunc
  
  
  gradient1<-b
  
  
  eval_g_ineq1func<- paste(spnedfunc,"(totalspends_mean)",sep="-")
  eval_g_ineq1_gradient<-c 
  
  
  eval_g_ineq2func<- paste("opt_revtarget",objfuncbase2,sep="-")
  eval_g_ineq2_gradient<-d 
  
  
  out=list(objfunc=objfunc1, spendfunc=objfunc2, objgrad=gradient1, budlimineq = eval_g_ineq1func, budlimineqgrad = eval_g_ineq1_gradient, targetineq=eval_g_ineq2func, targetineqgrad=eval_g_ineq2_gradient)
             
  return(out)
  }

