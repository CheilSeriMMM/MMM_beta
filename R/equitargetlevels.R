equitargetlevels<-function(startpoint,
                            endpoint,
                            numberoflevels,
                            group1,
                            group2,
                            startMonth,
                            endMonth,
                            x_range,
                            y_range,
                            bilconversion
                           
){

    modelrslt<-geneqs()
    
    revtarget<-NULL
    
    graphset_equirev<-NULL
    
    grpprice<-grpprice(startMonth,endMonth)
    allpricepergrp <- grpprice$pricepergrp
    tvpricepergrp <- grpprice$tvpricepergrp
    capricepergrp <- grpprice$catvpricepergrp
    jppricepergrp <- grpprice$jptvpricepergrp
    
    ResultFormula<-full_equation 
    
    m<-(endpoint-startpoint)/numberoflevels 
    
    for(i in 0:numberoflevels){
      revt<- startpoint + m*i
      revtarget<-union(revtarget,revt)
    }


    logadj1<-0.00001
    
    sim_data = subset(stockeddata, month>=startMonth & month<=endMonth)
    sum<-data.frame(lapply(sim_data, mean))
    


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
    
    
    param<-data.frame(apply(sim_data, 2, mean))
    param<-t(param); 
    rownames(param)<-NULL
    param<-as.data.frame(param)

    realrev<-param[,gsub("ln","",depvar)]

  
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
    
    
    case = simulation_set(setdiff(c(newgroup1,newgroup2),grplist))
    
    
    
    
    
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
    
    
    
    result<-NULL
    group1sim<-sim_set
    group1sim$yhat<-with(group1sim, exp(eval(parse(text=ResultFormula))))
    group1sim$yhat_time<-with(group1sim, timeconversion*exp(eval(parse(text=ResultFormula))))
    estim<-0
    jjj<-0
    adjust<-0
    
    
    
    for (i in 1:nrow(group1sim)){
      ss<- group1sim[i,"group1timeconversion"]
      if(ss>=group1all){
        jjj<- i
        estim <- group1sim[i,"yhat_time"]
        adjust <- (realrev-estim)
        break
      }
    }
    
    
    
    group1sim$yhat_time<-with(group1sim, (timeconversion*exp(eval(parse(text=ResultFormula))))+adjust)
    equirevsimset<-group1sim
    
    
    
    for(i in 1:length(group2)){
      aa<-paste0("ln",group2[i])

      if(i==1){
        equirevsimset[,group2[i]]<-1
      } else {
        equirevsimset[,group2[i]]<-0
      }
      equirevsimset[,aa]<-0
    }
    
    
    temp<-subset(param,select=group2)
    
    
    for(i in 1: ncol(temp)){
      if(colnames(temp)[i]%in%grplist){
        gg = colnames(temp)[i]
        if(gg=="grp_all"){
          temp[,gg] = temp[,gg]*allpricepergrp
        } else {
        temp[,gg]<-temp[,gg]*get(paste0(sub("grp_","",gg),"pricepergrp"))
      }
      }
    }
    
    group2rep <- colnames(temp)[which(temp[1,] == max(temp))]
    group2max = max(temp)
    
    
    for(i in 1:length(temp)){
      bb<-paste0("ln",colnames(temp)[i]) 
      
      if(bb%in%modelrslt$coefset$var){
        temp[2,i]<-modelrslt$coefset[modelrslt$coefset$var==bb,2]
      } else {
        temp[2,i]<-0
      }
      temp[3,i]<-temp[1,i]/group2max 
    }
    
    
    const<-0
    
    for(i in 1:ncol(temp)){
      if(temp[3,i]>0){
        const<-const+(temp[2,i]*log(temp[3,i]))  
      }
    }
    
    coefsum<-rowSums(temp,dims=1)[2]
    ratesum<-rowSums(temp,dims=1)[3]
    
    
    
    nn<-paste0("lnratio_",group2,"_tocomp")
    
    
    if(nn%in%modelrslt$coefset$var){
      racoef<-modelrslt$coefset[modelrslt$coefset$var==nn,][2]
    } else {
      racoef<-0
    }
    
    const <- as.double(const+ratesum*racoef)
    coefsum <- as.double(coefsum + racoef)
    
    
    graphset_equirev<-NULL
    


    gprice=1
    if(length(group2)==1& group2[1]%in%grplist){
      if(group2[1]=="grp_all"){
        gprice=allpricepergrp
      } else {
        gprice= get(paste0(sub("grp_","",group2[1]),"pricepergrp"))
      }
    }
    
    
    for(j in 1:length(revtarget)){
    
      equirevsimset$logyhat<- with(equirevsimset,eval(parse(text=ResultFormula)))
      aa<-log((revtarget[j]/timeconversion)-adjust)
      equirevsimset$group2all<-with(equirevsimset, exp((aa-logyhat-const)/coefsum)*ratesum*gprice)
      equirevsimset[,"group2timeconversion"]<-equirevsimset[,"group2all"]*timeconversion
      a<-subset(equirevsimset, select=c("group1timeconversion","group2timeconversion"))
      targetj<- revtarget[j]/bilconversion
      sttargetj<-paste0(as.character(targetj)," bil.")
      a$media <- sttargetj
      graphset_equirev <-rbind(graphset_equirev,a)
    }
    
    
    graphset_equirev$spendingsum<-graphset_equirev[,"group1timeconversion"] + graphset_equirev[,"group2timeconversion"]
    minsumset<-ddply(graphset_equirev, .(media), summarise, min=min(spendingsum, na.rm=T))
    minpoint<-graphset_equirev[which(graphset_equirev$spendingsum %in% minsumset$min), c(1,2,3)]
    minpoint2<-graphset_equirev[which(graphset_equirev$spendingsum %in% minsumset$min), ]
    
    graphset_equirev2 = graphset_equirev
    colnames(graphset_equirev)=c(paste(group1,collapse = "_"), paste(group2,collapse = "_"), "media","spendingsum")
    name=c(group1,group2)
    write.csv(graphset_equirev,paste0(paste(name,collapse = "_"),"eqlevel.csv"))
    
    p1 = ggplot(graphset_equirev2, aes(x=group1timeconversion, y=group2timeconversion, colour=media))+geom_line() + scale_y_continuous(labels=comma, limits=y_range)+scale_x_continuous(labels=comma,limits=x_range)
    p2 = p1+geom_point(data=minpoint, aes(x=group1timeconversion, y=group2timeconversion),size=3, group=1)
    p3 = p2+geom_point(aes(x=group1all, y=group2all), color='red', size=10, shape=43)
    
    
    out=list(minpointset=minpoint2,data=graphset_equirev,graph=p3)
    return(out)
}  
   
    
