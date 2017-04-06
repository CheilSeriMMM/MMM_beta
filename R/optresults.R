
optresults<-function(startmon, endmon, opt_revtarget_origin, optcntvar, inputvar){

  yhatinput<-subset(transformed1, month>=startmon & month<=endmon)
  cprp<-sum(transformed1$All_TV)/sum(transformed1$grp_all)
  yhatinput<-data.frame(t(colMeans(yhatinput)))

  yhatinput_base<-yhatinput

  optcntrvar2<-setdiff(inputvar,"lnpr")

  for(i in 1:ncol(yhatinput_base)){
    if(colnames(yhatinput_base)[i]%in%optcntrvar2){
      yhatinput_base[1,i]<-0
    }
  }

  searchratio<- 1+with(yhatinput_base,(ad_brand+ad_keyword)/(video+banner1+banner2))
  baserev<-exp(with(yhatinput_base, eval(parse(text=ResultFormula))))

  comp_atl<-with(yhatinput_base, monitor_tv_others_all+monitor_catv_others_all+monitor_jp_others_all)

  baserev<-(baserev*searchratio^(digitalratiocoeff)*pricepergrp^atlratiocoeff)/(((yhatinput_base[1,"comp_digital"])^digitalratiocoeff)*(comp_atl^atlratiocoeff))
  opt_revtarget<-(3*10^(10)/4.3)


  mediaspend_original<-subset(transformed1, select=c("month", gsub("ln","",optcntrvar)))
  mediaspend_original$tvinput<-with(mediaspend_original, grp_all*cprp)
  mediaspend_original$total<-rowSums(mediaspend_original)-mediaspend_original[,"grp_all"]-mediaspend_original[,"month"]

  mediaspend<-data.frame(lapply(mediaspend_original, function(x) ifelse(x==0,NA,x)))

  media_LB_total <- apply(mediaspend, 2, mean, na.rm=T) - 2*apply(mediaspend, 2, sd, na.rm=T)
  media_UB_total <- apply(mediaspend, 2, mean, na.rm=T) + 2*apply(mediaspend, 2, sd, na.rm=T)
  media_total_mean<- apply(mediaspend,2, mean, na.rm=T) #



  mediaspend_sim<-subset(mediaspend, month>=startmon & month<=endmon, select=c(gsub("ln","",optcntrvar),"total"))

  copy<-mediaspend_sim
  totalspends_UB<-mean(mediaspend_sim$total)*1.2
  totalspends_mean<-mean(mediaspend_sim$total)

  LB_can<-mean(mediaspend$total)-2*sd(mediaspend$total)

  if(LB_can>0){
    totalspends_LB <- LB_can
  } else {
    totalspends_LB <- 0
  }

  mediaspend_sim<-mediaspend_sim[,-ncol(mediaspend_sim)]

  media_LB <- apply(mediaspend_sim, 2, mean, na.rm=T) - 1*apply(mediaspend_sim, 2, sd, na.rm=T)
  media_UB <- apply(mediaspend_sim, 2, mean, na.rm=T) + 2*apply(mediaspend_sim, 2, sd, na.rm=T)

  media_UB2 <- apply(mediaspend_sim, 2, mean, na.rm=T) + 5*apply(mediaspend_sim, 2, sd, na.rm=T)

  length(media_LB)
  names(media_LB[1])


  for(i in 1:length(media_UB)){
    if(is.na(media_UB[i])){
      nm <- names(media_LB[i])
      media_UB[nm]<-media_total_mean[nm]
    }
  }

  for(i in 1:length(media_UB2)){
    if(is.na(media_UB2[i])){
      nm <- names(media_LB[i])
      media_UB2[nm]<-media_total_mean[nm]
    }
  }



  media_mean<-apply(mediaspend_sim, 2, mean, na.rm=T)
  media_mean_x0<-media_mean
  media_mean_x0[is.na(media_mean)|media_mean_x0==0]<-2
  media_LB[is.na(media_LB)|media_LB==0]<-0.000001
  mediaspend$total<-NULL

  totalspends_mean

   eval_f1 <- function( x ) {
    return( a=list( "objective" =eval(parse(text=objfunc1)),
                    "gradient" = -1*unlist(lapply(gradient1,function(s) eval(s)))
    )
    )
  }



  eval_g_ineq1 <- function( x ) {
    constr <- rbind(eval(parse(text=eval_g_ineq1func)))

    grad   <-rbind(unlist(lapply(eval_g_ineq1_gradient,function(s) eval(s))))
    return( list( "constraints"=constr, "jacobian"=grad ) )
  }

  x0 <- c(data.frame(media_mean_x0)[,1])

  lb1<-c(data.frame(media_LB)[,1])
  ub1<-c(data.frame(media_UB)[,1])

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

  print(res1)



  opt_results1<-data.frame(exist = media_mean, optimum = res1$solution)
  opt_results2<-data.frame(exist = media_mean, optimum = res2$solution)

  opt_results1_temp<-data.frame(t(opt_results1))[2,]
  opt_results2_temp<-data.frame(t(opt_results2))[2,]

  opt_digital1<-0
  opt_digital2<-0

  for(i in 1:length(opt_results1_temp)){
    if(colnames(opt_results1_temp)[i]%in%digitallist){
      opt_digital1<- opt_digital1 + opt_results1_temp[i]
      opt_digital2<- opt_digital2 + opt_results2_temp[i]
    }
  }

  opt_digital1<-opt_digital1*searchratio
  opt_digital2<-opt_digital2*searchratio


  opt_atl1<-0
  opt_atl2<-0
  for(i in 1:length(opt_results1_temp)){
    if(colnames(opt_results1_temp)[i]%in%atllist){
      opt_atl1<-opt_atl1 + opt_results1_temp[i]
      opt_atl2<-opt_atl2 + opt_results2_temp[i]
    }
  }


  opt_results1_log<-t(opt_results1)
  opt_results1_log<-data.frame(log(opt_results1_log))
  colnames(opt_results1_log)<-paste('ln', colnames(opt_results1_log), sep="")


  opt_results2_log<-t(opt_results2)
  opt_results2_log<-data.frame(log(opt_results2_log))
  colnames(opt_results2_log)<-paste('ln', colnames(opt_results2_log), sep="")


  opt_results1_log<-data.frame(lapply(opt_results1_log, function(x) ifelse(is.na(x),0,x)))
  opt_results2_log<-data.frame(lapply(opt_results2_log, function(x) ifelse(is.na(x),0,x)))


  yhatinput_opt1<-yhatinput
  yhatinput_opt2<-yhatinput


  yhatinput_opt1[,colnames(opt_results1_log)]<-opt_results1_log[2,colnames(opt_results1_log)]
  yhatinput_opt2[,colnames(opt_results2_log)]<-opt_results2_log[2,colnames(opt_results2_log)]


  yhatinput_opt1[,"lnratio_digitalad_tocomp"] <- log(opt_digital1/yhatinput_opt1[,"comp_digital"])
  yhatinput_opt2[, "lnratio_digitalad_tocomp"] <- log(opt_digital2/yhatinput_opt1[,"comp_digital"])


  yhatinput_opt1[,"lnratio_atlad_tocomp"] <-  log((opt_atl1* (cprp))/comp_atl)
  yhatinput_opt2[, "lnratio_atlad_tocomp"] <- log((opt_atl2* (cprp))/comp_atl)


  yhat1<-exp(with(yhatinput_opt1, eval(parse(text=ResultFormula))))
  yhat2<-exp(with(yhatinput_opt2, eval(parse(text=ResultFormula))))
  yhat0<-exp(with(yhatinput, eval(parse(text=ResultFormula))))
  mean(yhatinput$book_amount)


  mean(yhatinput$residual)


  opt_results1[row.names(opt_results1)=='grp_all', ] <- opt_results1[row.names(opt_results1)=='grp_all', ] * (cprp)* conversionrate
  opt_results2[row.names(opt_results2)=='grp_all', ] <- opt_results2[row.names(opt_results2)=='grp_all', ] * (cprp)* conversionrate


  fin_rslt <- opt_results1
  fin_rslt <- cbind(fin_rslt, optimum2=opt_results2$optimum)
  vn<-union(row.names(fin_rslt),"total")
  fin_rslt<-data.frame(lapply(fin_rslt, function(x) ifelse(is.na(x),0,x)))
  fin_rslt<-rbind(fin_rslt, colSums(fin_rslt))
  fin_rslt<-t(fin_rslt)
  colnames(fin_rslt)<-vn

  fin_rslt<-cbind(fin_rslt,yhat=c(yhat0,yhat1,yhat2))
  fin_rslt<-data.frame(fin_rslt)
  fin_rslt$profit<- fin_rslt$yhat-fin_rslt$total
  fin_rslt[,"profit"]<-fin_rslt[,"yhat"]-fin_rslt[,"total"]

  return(fin_rslt)

  print(fin_rslt)
  print(res1$solution)
  print(paste0("real sales:",mean(yhatinput$book_amount)))
  print(paste0("res2$sol:", res2$solution))
  print(paste0("res2$sol:", res2$objective))
  print(paste0("res1$objective:",res1$objective))

}

