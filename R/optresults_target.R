optresults_budlim=function(
  startMonth,
  endMonth,
  time,
opt_revtarget){
  
  library('nloptr')
opt_revtarget=opt_revtarget
  dd=geneqs()
  
  yhatinput<-subset(stockeddata, month>=startMonth & month<=endMonth)
  
  grpprice<-grpprice(startMonth,endMonth)
  allpricepergrp <- grpprice$pricepergrp
  tvpricepergrp <- grpprice$tvpricepergrp
  capricepergrp <- grpprice$catvpricepergrp
  jppricepergrp <- grpprice$jptvpricepergrp
  
  
  yhatinput<-data.frame(t(colMeans(yhatinput)))
  yhatinput_base<-yhatinput
  
  optcntrvar2<-intersect(dd$coefset$var, c(inputvar, paste0("ln",inputvar)))
  
    
  for(i in 1:ncol(yhatinput_base)){
    if(colnames(yhatinput_base)[i]%in%optcntrvar2){
      yhatinput_base[1,i]<-0
    }
  }

  baserev<-exp(with(yhatinput_base, eval(parse(text=full_equation))))
  
  mediaspend_original=subset(stockeddata, select=inputvar)
  
  tv_cost=NULL
  
  for (i in 1:length(inputvar)){
    if(inputvar[i]=="grp_all"){
      mediaspend_original$tvinput<-with(mediaspend_original, grp_all*allpricepergrp)
    } else if (inputvar[i]%in%grplist){
      nn = gsub("grp_","",inputvar[i])
      tv_cost[,nn] = with(mediaspend_original, inputvar[i]*get(paste0(nn,"pricepergrp")))
    }
  }
  
  
  if(!is.null(tv_cost)){
    mediaspend_original$tvinput=colSums(tv_cost)
  }
  

  mediaspend_original$total<-rowSums(subset(mediaspend_original, select=setdiff(inputvar,grplist)))+ mediaspend_original$tvinput

  mediaspend<-data.frame(lapply(mediaspend_original, function(x) ifelse(x==0,NA,x)))

  media_total_mean=apply(mediaspend,2, mean, na.rm=T)
  media_total_sd=apply(mediaspend, 2, sd, na.rm=T)
  
  media_LB_total <- media_total_mean - 2*media_total_sd
  media_UB_total <- media_total_mean + 2*media_total_sd

  
  
  tmp = cbind(time=stockeddata[,time],mediaspend)
  colnames(tmp)[1]=time
  
  mediaspend_sim<-subset(tmp, month>=startMonth & month<=endMonth)
  
  
  
  copy<-mediaspend_sim
  
  totalspends_UB<-mean(mediaspend_sim$total)+2*sd(mediaspend_sim$total)
  totalspends_LB<-mean(mediaspend_sim$total)-2*sd(mediaspend_sim$total)
  
  

  LB_can=media_total_mean[["total"]]-2*media_total_sd[["total"]]

  if(totalspends_LB<0&LB_can>0){
    totalspends_LB <- LB_can
  } else {
    totalspends_LB <- 0
  }

 
  mediaspend_sim2<-mediaspend_sim[,-c(1,ncol(mediaspend_sim)-1,ncol(mediaspend_sim))]
  
  media_mean = apply(mediaspend_sim2, 2, mean, na.rm=T)
  media_sd = apply(mediaspend_sim2, 2, sd, na.rm=T)
  
  media_LB <- media_mean - 2*media_sd
  media_UB <- media_mean + 2*media_sd
  media_UB2 <- media_mean + 5*media_sd


  for(i in 1:length(media_UB)){
    if(is.na(media_UB[i])){
      nm <- names(media_LB[i])
      media_UB[nm]<-media_UB_total[nm]
    }
  }

  for(i in 1:length(media_UB2)){
    if(is.na(media_UB2[i])){
      nm <- names(media_LB[i])
      media_UB2[nm]<-media_UB_total[nm]
    }
  }




  media_mean_x0 = media_mean
  media_mean_x0[is.na(media_mean)|media_mean_x0==0]<-2
  media_LB[is.na(media_LB)|media_LB<=0]<-0.000001

  
  
  tmp2 = data.frame(media_mean_x0)
  rn2=row.names(tmp2)
  
  for (i in 1:length(rn2)){
    if(rn2[i]%in%grplist){
      value = tmp2[row.names(tmp2)==rn2[i],]
      tmp2[row.names(tmp2)==rn2[i],]=value*get(paste0(gsub("grp_","",rn2[i]),"pricepergrp"))
    }
  }
  
  
  
  totalspends_mean<-as.numeric(lapply(tmp2,sum))

  funcs=make_objfunc_gradient(startMonth,endMonth)
  
  
   eval_f1 <- function( x ) {
    return(a=list( "objective" = eval(parse(text=funcs$objfunc)),
                   "gradient" = -1*unlist(lapply(funcs$objgrad,function(s) eval(s)))
    )
    )
  }

   
   eval_g_ineq1 <- function( x ) {
     
     return(b=list( "constraints"= eval(parse(text=funcs$budlimineq)),
                   "jacobian"= unlist(lapply(funcs$budlimineqgrad,function(s) eval(s)  ))) 
             ) 
   } 
   
   

  x0 = c(data.frame(media_mean_x0)[,1])
  lb1 = c(data.frame(media_LB)[,1])
  ub1 = c(data.frame(media_UB)[,1])

  local_opts <- list( "algorithm" = "NLOPT_LD_MMA",
                      "xtol_rel"  = 1.0e-50 )
  opts1 <- list( "algorithm" = "NLOPT_LD_AUGLAG",
                 "xtol_rel"  = 1.0e-50,
                 "maxeval"   = 50000,
                 "local_opts" = local_opts )

  res1 <- nloptr( x0=x0,
                  eval_f=eval_f1,
                  lb=lb1,
                  ub=ub1,
                  eval_g_ineq=eval_g_ineq1,
                  opts=opts1)

  
  opt_results1<-data.frame(exist = media_mean, optimum = res1$solution)

  opt_results1_log<-t(opt_results1)
  opt_results1_log<-data.frame(log(opt_results1_log))
  colnames(opt_results1_log)<-paste('ln', colnames(opt_results1_log), sep="")
  
  opt_results1_log<-data.frame(lapply(opt_results1_log, function(x) ifelse(is.na(x),0,x)))
  

  yhatinput_opt1<-yhatinput
  
  
  yhatinput_opt1[,colnames(opt_results1_log)]<-opt_results1_log[2,colnames(opt_results1_log)]

  
  yhat1<-exp(with(yhatinput_opt1, eval(parse(text=full_equation))))
  yhat0<-exp(with(yhatinput, eval(parse(text=full_equation))))

  
  rn=row.names(opt_results1)
  opt_results1<-data.frame(lapply(opt_results1, function(x) ifelse(is.na(x),0,x)))
  
  row.names(opt_results1)=rn
  
  
  opt_results1$exist_invest = opt_results1$exist
  opt_results1$exist_portion = 0
  opt_results1$optimum_invest = opt_results1$optimum
  opt_results1$optimum_portion = 0

  
  for (i in 1:length(rn)){
    if(rn[i]%in%grplist){
      value = opt_results1[row.names(opt_results1)==rn[i],"exist"]
      cst=value*get(paste0(gsub("grp_","",rn[i]),"pricepergrp"))
      opt_results1[row.names(opt_results1)==rn[i], "exist_invest"] = cst
    }
  }
  
  for (i in 1:length(rn)){
    if(rn[i]%in%grplist){
      value = opt_results1[row.names(opt_results1)==rn[i],"optimum"]
      cst=value*get(paste0(gsub("grp_","",rn[i]),"pricepergrp"))
      opt_results1[row.names(opt_results1)==rn[i], "optimum_invest"] = cst
    }
  }
  
  
  opt_results1=rbind(opt_results1, investment=colSums(opt_results1))
  
  inv1=as.numeric(opt_results1[row.names(opt_results1)=="investment","exist_invest"])
  inv2=as.numeric(opt_results1[row.names(opt_results1)=="investment","optimum_invest"])
  
  opt_results1$exist_portion = with(opt_results1, as.numeric(exist_invest)/inv1)
  opt_results1$optimum_portion = with(opt_results1, as.numeric(optimum_invest)/inv2)
  
  opt_results1=rbind(opt_results1, sales=c("","",yhat0,"", yhat1,""))
  profit_exist = as.numeric(opt_results1[row.names(opt_results1)=="sales","exist_invest"]) - as.numeric(opt_results1[row.names(opt_results1)=="investment","exist_invest"])  
  profit_optimum = as.numeric(opt_results1[row.names(opt_results1)=="sales","optimum_invest"]) - as.numeric(opt_results1[row.names(opt_results1)=="investment","optimum_invest"])  
  opt_results1=rbind(opt_results1, profit=c("","",profit_exist,"",profit_optimum,""))
  
  
  return(opt_results1)
  
}
