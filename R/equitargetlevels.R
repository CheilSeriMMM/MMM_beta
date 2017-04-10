equitargetlevels<-function(startpoint,
                            endpoint,
                            numberoflevels,
                            group1,
                            group2,
                            sim_base_month,
                            x_range,
                            y_range
                           
){





modelrslt<-geneqs()

revtarget<-NULL

graphset_equirev<-NULL

grpprice<-grpprice(sim_base_month,sim_base_month)
allpricepergrp <- grpprice$pricepergrp
tvpricepergrp <- grpprice$tvpricepergrp
capricepergrp <- grpprice$catvpricepergrp
jppricepergrp <- grpprice$jptvpricepergrp

ResultFormula<-full_equation 

m<-(endpoint-startpoint)/numberoflevels 

for(i in 1:numberoflevels){
  revt<- startpoint + m*(i-1)
  revtarget<-union(revtarget,revt)
}






logadj1<-0.00001


sim_data<-subset(stockeddata, month==sim_base_month)
sum<-data.frame(lapply(sim_data, sum))


list1<-get(paste0(group1,"list"))
group1input<-intersect(inputvar,list1)

list2<-get(paste0(group2,"list"))
group2input<-intersect(inputvar,list2)

realgroup1all<-0

for(i in 1:length(group1input)){
  if(group1input[i]%in%grplist){
    gprice<-get(paste0(sub("grp_","",group1input[i]),"pricepergrp"))
    su<-sum[,group1input[i]] * gprice
    realgroup1all <- realgroup1all + su
  } else {
    realgroup1all <- realgroup1all + sum[,group1input[i]] 
  }
}






realgroup2all<-0
for(i in 1:length(group2input)){
  if(group2input[i]%in%grplist){
    gprice2<-get(paste0(sub("grp_","",group2input[i]),"pricepergrp"))
    su2<-sum[,group2input[i]] * gprice2
    realgroup2all <- realgroup2all + su2
  } else {
    realgroup2all <- realgroup2all + sum[,group2input[i]]
  }
}


realrev<-sum[,sub("ln","",depvar)]


param<-data.frame(apply(sim_data, 2, mean))
param<-t(param); 
rownames(param)<-NULL
param<-as.data.frame(param)


newparam<-subset(param, select = group1input)


varn1 <- colnames(newparam)[which(newparam[1,] == max(newparam))]
varnval1<- max(newparam)

logadj1<-0.00001

case1<-simulation_set(varn1)

sim_set <- data.frame(param[rep(1,length(case1)), ])
sim_set[,varn1]<-case1
sim_set[,paste0("ln",varn1)]<-log(case1)


dd<-0

for (i in 1:ncol(newparam)){
  vvarn<-colnames(newparam)[i]
  vvarnval<-newparam[,vvarn]/varnval1
  sim_set[,vvarn]<-case1 * vvarnval
  sim_set[,paste0("ln",vvarn)]<-log(case1)+log(vvarnval+logadj1)
  dd<-dd+vvarnval
}  


sim_set[,paste0(group1,"time")]<-0


for(i in 1:ncol(newparam)){
  pp<-colnames(newparam)[i]
  
  if(pp%in%grplist){
    gprice<-get(paste0(sub("grp_","",pp),"pricepergrp"))
    sim_set[,paste0(group1,"time")]<-sim_set[,paste0(group1,"time")] + sim_set[,pp]*gprice 
  } else {
    sim_set[,paste0(group1,"time")]<-sim_set[,paste0(group1,"time")] + sim_set[,pp]
  }
  
}
sim_set[,paste0(group1,"time")]<-sim_set[,paste0(group1,"time")]*timeconversion


result<-NULL
group1sim<-sim_set
group1sim$yhat<-with(group1sim, exp(eval(parse(text=ResultFormula))))
group1sim$yhat_time<-with(group1sim, timeconversion*exp(eval(parse(text=ResultFormula))))
estim<-0
jjj<-0
adjust<-0



for (i in 1:nrow(group1sim)){
  ss<- group1sim[i,paste0(group1,"time")]
  if(ss>=realgroup1all){
    jjj<- i
    estim <- group1sim[i,"yhat_time"]
    adjust <- (realrev-estim)
    break
  }
}



group1sim$yhat_time<-with(group1sim, (timeconversion*exp(eval(parse(text=ResultFormula))))+adjust)
equirevsimset<-group1sim




lngroup2input<-paste0("ln",group2input)


for(i in 1:length(group2input)){
  aa<-paste0("ln",group2input[i])
  
  if(i==1){
    equirevsimset[,group2input[i]]<-1
  } else {
    equirevsimset[,group2input[i]]<-0
  }
  
  equirevsimset[,aa]<-0
}


temp<-subset(param,select=get(paste0(group2,"list")))

for(i in 1: ncol(temp)){
  if(colnames(temp)[i]%in%grplist){
    gg<-colnames(temp)[i]
    temp[,gg]<-temp[,gg]*get(paste0(sub("grp_","",gg),"pricepergrp"))
  }
}


group2rep <- colnames(temp)[which(temp[1,] == max(temp))]
group2max = max(temp)

for(i in 1:length(temp)){
  aa<-paste0("ln",colnames(temp)[i]) 
  
  if(aa%in%modelrslt$coefset$var){
    temp[2,i]<-modelrslt$coefset[modelrslt$coefset$var==aa,2]
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



nn<-paste0("lnratio_",group2,"ad_tocomp")


if(nn%in%modelrslt$coefset$var){
  racoef<-modelrslt$coefset[modelrslt$coefset$var==nn,][2]
} else {
  racoef<-0
}

const <- as.double(const+ratesum*racoef)
coefsum <- as.double(coefsum + racoef)


graphset_equirev<-NULL

for(j in 1:length(revtarget)){
  nrow(equirevsimset)
  equirevsimset$logyhat<- with(equirevsimset,eval(parse(text=ResultFormula)))
  aa<-log((revtarget[j]/timeconversion)+adjust)
  equirevsimset$group2all<-with(equirevsimset, exp((aa-logyhat-const)/coefsum)*ratesum)
  equirevsimset[,paste0(group2,"time")]<-equirevsimset[,"group2all"]*timeconversion
  a<-subset(equirevsimset, select=c(paste0(group1,"time"), paste0(group2,"time")))
  targetj<- revtarget[j]/10^(9)
  sttargetj<-paste0(as.character(targetj)," bil.")
  a$media <- sttargetj
  graphset_equirev <-rbind(graphset_equirev,a)
}


graphset_equirev$spendingsum<-graphset_equirev[,paste0(group1,"time")] + graphset_equirev[,paste0(group2,"time")]
minsumset<-ddply(graphset_equirev, .(media), summarise, min=min(spendingsum, na.rm=T))
minpoint<-graphset_equirev[which(graphset_equirev$spendingsum %in% minsumset$min), c(1,2,3)]


graphset_equirev2<-graphset_equirev
names(graphset_equirev2)[1]<-"group1"
names(graphset_equirev2)[2]<-"group2"

p1<-ggplot(graphset_equirev2, aes(x=group2, y=group1, colour=media))+geom_line() + scale_y_continuous(labels=comma, limits=y_range)+scale_x_continuous(labels=comma,limits=x_range)

p2<-p1+geom_point(data=minpoint, aes(x=, y=as.factor(paste0(group2,"time"))),size=3, group=1)

p3<-p2+geom_point(aes(x=realgroup1all, y=realgroup2all), color='red', size=10, shape=43)


return(p1)
}
