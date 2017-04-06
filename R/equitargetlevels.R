equitargetlevels<-function(startpoint,
                           endpoint,
                           numberoflevels,
                           group1,
                           group2,
                           sim_base_month
                           ){

    
    modelrslt<-geneqs(model,fit)
  
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
      revt<- startpoint + m*i
      revtarget<-union(revtarget,revt)
    }
    
    
    case1<-simulation_set(group1)
    case2<-simulation_set(group2)
  
    logadj1<-0.00001
    
    
    
    sim_data<-subset(data, month==sim_base_month, select=c(inputvar,y,complist))
    sum<-data.frame(lapply(sim_data, sum))
    
    
    list1<-paste0(group1,"list")
    group1input<-intersect(inputvar,list1)
    
    list2<-paste0(group2,"list")
    group2input<-intersect(inputvar,list2)
  
    realgroup1all<-0
    for(i in 1:length(group1input)){
      realgroup1all <- realgroup1all + sum[,group1input[i]]
    }
    
    realgroup2all<-0
    for(i in 1:length(group2input)){
      realgrou21all <- realgroup2all + sum[,group1input[i]]
    }     
    
           
    realrev<-sum[,y]
                
    param<-data.frame(apply(sim_data, 2, mean))
    param<-t(param); 
    rownames(param)<-NULL
    param<-as.data.frame(param)
                
    
    newparam<-subset(param, select = group1input)
    
    varn1 <- colnames(newparam)[which(param == max(newparam))]
    varnval1<- max(newparam)
      
    logadj1<-0.00001
      
    sim_set <- data.frame(param[rep(1,length(case)), ])
    
    sim_set[,varn1]<-group1case
    sim_set[,paste0("ln",varn1)]<-log(case)
    

    dd<-0
    for (i in 1:ncol(newparam)){
      
      vvarn<-colnames(newparam)[i]
      vvarnval<-newparma[,vvarn]/varnval1
      sim_set[,vvarn]<-case * vvarnval
      sim_set[,paste0("ln",vvarn)]<-log(case)+log(vvarnval+logadj1)
      dd<-dd+vvarnval
      }  
      
    
    sim_set[,paste0(group1,"time")]<-(case*dd*timeconversion)
                
                
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
        estim <- as.double(group1sim[i,paste0(group1,"time")])
        adjust <- (realrev-estim)
        break
        }
     }
    
    
    
    group1sim$yhat_time<-with(group1sim, (group1sim*exp(eval(parse(text=ResultFormula))))+adjust)
    equirevsimset<-group1sim
    
    
    
    
    lngroup2input<-paste0("ln",group2input)
    
  
    for(i in 1:length(lngroup2input)){
      aa<-lngroup2input[i]
      equirevsimset[,aa]<-0
    }
  
    
    temp<-subset(param,select=input2group)
    
    
    for(i in 1: ncol(temp)){
      if(colnames(temp)[i]%in%grplist){
        gg<-colnames(temp)[i]
        temp[,gg]<-temp[,gg]*get(paste0(sub("grp_","",gg),"pricepergrp"))
      }
    }
    
    ################################################################
    temp2<-t(temp)
    row.names(colnames(temp))
    
    group2rep = #temp에서 값이 제일 큰 변수명
    group2max = #temp에서 제일 큰 값
    temp2$coef = #modelrslt$coefset서 coefficient 가져와서 넣기 
    temp2$rate = # temp의 value를group2max로 나눈 값 
    ratesum = #temp2$rate의 sum
    coefsum = #temp2$coef의 sum  
    #################################################################  
      
      
    const<-0
    
    for(i in 1:nrow(temp2)){
    const<-const+(temp2[i,"coef"]*log(temp2[i,"rate"]))  
    }
    
    
    
      
    graphset_equirev<-NULL
    
    for(j in 1:length(revtarget)){
    equirevsimset$logyhat<- with(equirevsimset,eval(parse(text=ResultFormula)))
    equirevsimset$group2all<-with(equirevsimset, exp((log((revtarget[i]/timeconversion))-logyhat-const)/coefsum)*ratesum)
    
    equirevsimset[,paste0(group2,"time")]<-equirevsimset[,"group2all"]*timeconversion
    
    a<-subset(equirevsimset, select=c(paste0(group1,"time"), paste0(group2,"time")))
    targetj<- revtarget[j]/10^(9)
    sttargetj<-paste0(as.character(targetj)," bil.")
    a$media <- sttargetj
    graphset_equirev <-rbind(graphset_equirev,a)

    rm(a)
    }
 
 

    
    graphset_equirev$spendingsum<-graphset_equirev[,paste0(group1,"time")] + graphset_equirev[,paste0(group2,"time")]
    minsumset<-ddply(graphset_equirev, .(media), summarise, min=min(spendingsum, na.rm=T))
    minpoint<-graphset_equirev[which(graphset_equirev$spendingsum %in% minsumset$min), c(1,2,3)]

        
    
    p_1<-ggplot(graphset_equirev, aes(x=as.factor(paste0(group1,"time")), y=as.factor(paste0(group2,"time")), colour=media))+geom_line()+
      scale_y_continuous(labels=comma, limits=c(10000000, 1300000000))+scale_x_continuous(labels=comma,limits=c(10000000, 1300000000))
    
    
    
    p_2<-p_1+geom_point(data=minpoint, aes(x=, y=tvinput_4_real),size=3, group=1)
    
    p_3<-p_2+geom_point(aes(x=realdigitalall, y=realatlall*0.5533), color='red', size=10, shape=43)
    
    
    
    rm(exdata)
    
    exdata<-subset(equirevsimset,select="digital4")
    
    for(p in 1:length(revtarget)){
      ca_na <- paste0("tvinput4_real",p)
      ca_data<- subset(equirevsimset,select=ca_na)
      exdata<-cbind(exdata,ca_data)
    }
    
    exdata_total<-subset(equirevsimset,select="digital4")
    
    for(p in 1:length(revtarget)){
      ca_na <- paste0("totalinput_real",p)
      ca_data<- subset(equirevsimset,select=ca_na)
      exdata_total<-cbind(exdata_total,ca_data)
      
    }
    
    rm(vanavec)
    rm(vanavec1)
    rm(vanavec2)
    
    
    vanavec1<-c("digital4")
    vanavec2<-NULL
    
    for(q in 1:length(revtarget)){
      targetq<- revtarget[q]/10^(9)
      sttargetq<-paste0(as.character(targetq)," bil.")
      titargetq<-paste0(as.character(targetq)," bil._input")
      
      vanavec1<-union(vanavec1,sttargetq)
      vanavec2<-union(vanavec2,titargetq)
    }
    
    vanavec <- union(vanavec1,vanavec2)
    
    
    exdata<-merge(exdata,exdata_total,by="digital4")
    exdata<-data.frame(lapply(exdata, function(x) ifelse(is.na(x),"",x)))
    testdd<-exdata
    setnames(exdata,vanavec)
    
    #rm(graphset_equirev)
    write.csv(exdata,"equirev.csv")
    write.csv(graphset_equirev,"graphset_equirev.csv")


    return(p_3)
}
