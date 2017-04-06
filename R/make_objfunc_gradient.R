
make_objfunc_gradient<-function(
  dffincoeff, 
  atllist, 
  grpgroup, 
  pricepergrp, 
  digitalra, 
  atlra, 
  digitalnu){
  
  atlratiocoeff<-dffincoeff[which(dffincoeff2$var=="lnratio_atlad_tocomp"),2]
  digitalratiocoeff<-dffincoeff[which(dffincoeff2$var=="lnratio_digitalad_tocomp"),2]
  conversionrate<-1
  a<-dffincoeff
  
  lefts<-rep('x[', nrow(a))
  ggg<-paste(lefts, seq(1:nrow(a)),sep="")
  ggg<-paste(ggg, ']', sep="")
  a$vars<-ggg
  
  
  for(i in 1:nrow(a)){
    nolog3 <- sub("ln","",a[i,"var"]) 
    
    if(nolog3%in%atllist){             ##### atllist
      a[i,"group"]<-"atl"  
    } else {
      a[i,"group"]<-"digital"
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
    
    if(noln%in%grpgroup){                              ### grpgroup
      if(noln=="grp_all"){
        grpprice <- pricepergrp *conversionrate        ### pricepergrp
      } else if(noln=="grp_tv"){
        grpprice <- tvpricepergrp*conversionrate
      } else if(noln=="grp_ca"){
        grpprice <- catvpricepergrp*conversionrate
      } else{
        grpprice <- jptvpricepergrp*conversionrate
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
  
  # for(i in (1:nrow(a))){
  #   #rem <-paste(a$vars_expdt[-i], collapse="+")
  #   #a[i,"partialdiff_expdt"]<-paste(a[i,"multiplier_expdt"], rem,sep="+")
  #   a[i,"partialdiff_expdt"]<-a[i,"multiplier_expdt"]
  # }
  
  digitalnusubset<-subset(a,group=="digital")
  digitalnu<-paste(digitalnusubset$vars,collapse="+")
  digitalnu<-paste0("(",digitalnu,")")
  
  digitalra<-paste0("(",digitalnu,"^",digitalratiocoeff,")")    ###digitalratiocoeff
  
  atlnusubset<-subset(a,group=="atl")
  atlnu<-paste(atlnusubset$vars,collapse="+")
  atlnu<-paste0("(",atlnu,")")
  atlra<-paste0("(",atlnu,"^",atlratiocoeff,")")             ###atlratiocoeff
  
  ratio_func<-paste(digitalra,atlra,sep="*")               ###digitalra
  
  
  objfunc<-paste(objfunc_origin,digitalra,sep="*")
  objfunc<-paste(objfunc,atlra,sep="*")                     ###atlra
  finobjfunc<-paste('-1',objfunc, sep="*")
  
  
  
  
  for(i in 1:nrow(a)){
    if(a[i,"group"]=="digital"){
      nn<-paste(digitalratiocoeff,digitalnu,sep="*")      ###digitalnu
      nn<-paste0(nn,"^","(",digitalratiocoeff-1,")")
      nn<-paste(nn,atlra,sep="*")
      a[i,"comp_ratio_nu"]<-digitalra
      
    } else if(a[i,"group"]=="atl"){
      nn<-paste(atlratiocoeff,atlnu,sep="*")
      nn<-paste0(nn,"^","(",atlratiocoeff-1,")")
      nn<-paste(nn,digitalra,sep="*")
      a[i,"comp_ratio_nu"]<-atlra
    }
    a[i,"partialdiff_ratio"]<-paste("baserev",nn,sep="*")
  }
  
  
  a$final_partialdiff2<-paste0(a$partialdiff,"*",ratio_func,"+",objfunc_origin,"*",a$partialdiff_ratio)
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
  
  gradient1<-b # 목적함수의 gradient
  #gradient2<-d # 목표매출 case 부등식 제약함수의 gradient
  
  
  eval_g_ineq1func<- paste(spnedfunc,"(totalspends_mean)",sep="-")
  eval_g_ineq1_gradient<-c # 예산제약 부등호 제약의 gradient
  
  
  eval_g_ineq2func<- paste("opt_revtarget",objfuncbase2,sep="-")
  eval_g_ineq2_gradient<-d # 목표매출 부등호 제약의 gradient  
}

