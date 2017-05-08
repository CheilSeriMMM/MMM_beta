twogroupsopt_budgetlimit = function(
  startMonth,
  endMonth,
  group1, 
  group2
  ){
  
  rslt=geneqs()
  
  grpprice<-grpprice(startMonth,endMonth)
  
  allpricepergrp <- grpprice$pricepergrp
  tvpricepergrp <- grpprice$tvpricepergrp
  capricepergrp <- grpprice$catvpricepergrp
  jppricepergrp <- grpprice$jptvpricepergrp
  
  
  
  ResultFormula<-full_equation
  sim_data = subset(data, month>=startMonth & month<=endMonth)
  sum <- data.frame(lapply(sim_data, sum))
  
  group1all=0
  
  for(i in 1:length(group1)){
    if(group1[i]%in%grplist){
      grpprice=get(paste0(gsub("grp_","",group1[i]),"pricepergrp"))
      group1all = group1all + sum[,group1[i]] * grpprice
    } else {
    group1all = group1all + sum[,group1[i]]
    }
    }
  
  
  group2all=0
  for(i in 1:length(group2)){
    if(group2[i]%in%grplist){
      grpprice=get(paste0(gsub("grp_","",group2[i]),"pricepergrp"))
      group2all = group2all + sum[,group2[i]] * grpprice
    } else {
      group2all = group2all + sum[,group2[i]]
    }
  }
  
  
  max_budget <- group1all + group2all
  
  param<-data.frame(apply(sim_data, 2, mean))
  param<-t(param); 
  rownames(param)<-NULL
  param<-as.data.frame(param)
  
  
  grp1=intersect(group1,grplist)
  new1=NULL
  
  if(length(grp1)>0){
  for(i in 1:length(grp1)){
    if(grp1[i]=="grp_all"){
      new1=c(new1,"tv_all")
    } else {
      new1=c(new1,gsub("grp_","",grp1[i]))
    }
  }
  }
  newgroup1 = c(group1, new1)
  
  
  grp2=intersect(group2,grplist)
  new2=NULL
  if(length(grp2)>0){
  for(i in 1:length(grp2)){
    if(grp2[i]=="grp_all"){
      new2=c(new2,"tv_all")
    } else {
      new2=c(new2,gsub("grp_","",grp2[i]))
    }
  }
  }
  newgroup2 = c(group2, new2)
  
  
  case = simulation_set(c(newgroup1,newgroup2))
  sim_set <- data.frame(param[rep(1,length(case)), ])
  newparam<-subset(param, select = setdiff(newgroup1,grplist))
  
  varn1 <- colnames(newparam)[which(newparam == max(newparam))]
  varnval1<- max(newparam)
  
  
  logadj1<-0.00001
  
  
  sim_set[,varn1]<-case
  sim_set[,paste0("ln",varn1)]<-log(case+logadj)
  
  tvlist = c("tv_all", "tv", "ca", "jp", "ip")

  
  dd<-0
  for (i in 1:ncol(newparam)){
    vvarn=colnames(newparam)[i]
    vvarnval=newparam[,vvarn]/varnval1
    sim_set[,vvarn]=case * vvarnval 
    sim_set[,paste0("ln",vvarn)]=log(case)+log(vvarnval)
    dd = dd+vvarnval
    if(vvarn%in%tvlist){
      if(vvarn=="tv_all"){
        sim_set[,"grp_all"] = sim_set[,"tv_all"]/allpricepergrp
        sim_set[,"lngrp_all"] = log(sim_set[,"grp_all"])
      } else {
        tmpgrp=paste0("grp_",vvarn)
        grpprice = get(paste0(vvarn,"pricepergrp"))
        sim_set[,tmpgrp] = sim_set[,vvarn]/grpprice
        tmplngrp = paste0("ln",tmpgrp)
        sim_set[,tmplngrp] = log(sim_set[,tmpgrp])
      }
    }
  }  
  
  sim_set[,"group1all"]<-(case*dd)
  sim_set[,"group1timeconversion"]<-(case*dd*timeconversion)
  
  buli_sim<-subset(sim_set,group1all<max_budget)
  buli_sim$group2all<-with(buli_sim, (max_budget-group1all))
  buli_sim$group2timeconversion<-with(buli_sim, group2all*timeconversion)
  
  newparam2<-subset(param, select = setdiff(newgroup2,grplist))
  varnval2 = as.numeric(lapply(newparam2,sum))
  
  for (i in 1:ncol(newparam2)){
    aa=colnames(newparam2)[i]
    aarate<- newparam2[,aa]/varnval2
    buli_sim[,aa]=with(buli_sim, group2all*aarate)
    buli_sim[,paste0("ln",aa)]=log(buli_sim[,aa])

  
    if(aa%in%tvlist){
      if(aa=="tv_all"){
        buli_sim[,"grp_all"] = buli_sim[,"tv_all"]/allpricepergrp
        buli_sim[,"lngrp_all"] = log(buli_sim[,"grp_all"])
      } else {
        tmpgrp=paste0("grp_",aa)
        grpprice = get(paste0(aa,"pricepergrp"))
        buli_sim[,tmpgrp] = buli_sim[,aa]/grpprice
        tmplngrp = paste0("ln",tmpgrp)
        buli_sim[,tmplngrp] = log(buli_sim[,tmpgrp])
      }
    }
  }  
  

  buli_sim$yhat<-with(buli_sim, exp(eval(parse(text=ResultFormula)))) 
  buli_sim$yhattimeconversion<-with(buli_sim, timeconversion*(exp(eval(parse(text=ResultFormula)))))
  
  
  if(grepl("ln",depvar)){
    tmpdepvar = gsub("ln","",depvar)
  } else {
    tmpdepvar = depvar
  }
  
  realrev = param[1,tmpdepvar]
  
  
  for (i in 1:nrow(buli_sim)){
    ss = buli_sim[i,"group1timeconversion"]
    if(ss>=group1all){
      jjj<- i
      estim3 <- as.double(buli_sim[i,"yhattimeconversion"])
      adjust3 <- (realrev-estim3)
      break
    }
  }
  
  realgroup1all = group1all/(group1all+group2all)
  
  buli_sim$yhathat4<-with(buli_sim, timeconversion*(exp(eval(parse(text=ResultFormula))))+adjust3)
  buli_sim$group1rate<-with(buli_sim, group1timeconversion/(group1timeconversion+group2timeconversion))
  plot<-ggplot(buli_sim, aes(x=group1rate, y=yhathat4))+geom_line()+ geom_point(aes(x=realgroup1all, y=realrev), size=3, colour='blue')+scale_y_continuous(labels=comma)+scale_x_continuous(labels=comma)
  
  twogroupsopt<-subset(buli_sim, select=c(group1all,group2all,yhathat4))
  
  write.csv(twogroupsopt,"twogroupsopt.csv")
  return(plot)
}
