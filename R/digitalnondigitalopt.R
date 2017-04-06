digitalnondigitalopt <- function(
  sim_base_month
){
  
  geneqs()
  grpprice<-grpprice(sim_base_month,sim_base_month)
  ResultFormula<-full_equation
  
  
  
  sim_data <- subset(data, month==sim_base_month, select=c(inputvar, y,complist))
  sum <- data.frame(lapply(sim_data, sum))
  
  
  digitalinput<-intersect(inputvar,digitalist)
  nondigitalinput<-setdiff(inputvar,digitalinput)
  
  
  
  realdigitalall<-0
  
  for(i in 1:length(digitalinput)){
    realdigitalall <- realdigitalall + sum[,digitalinput[i]]
  }
  
  
  
  realnondigitalall<-0
  for(i in 1:length(nondigitalinput)){
    realnondigitalall <- realnondigitalall + sum[,digitalinput[i]]
  }  
  
  
  
  max_budget <- realdigitalall + realnondigitalall
  
  param<-data.frame(apply(sim_data, 2, mean))
  param<-t(param); 
  rownames(param)<-NULL
  param<-as.data.frame(param)
  
  sim_set <- data.frame(param[rep(1,length(case)), ])
  
  newparam<-subset(param, select = digitalinput)
  
  varn1 <- colnames(newparam)[which(param == max(newparam))]
  varnval1<- max(newparam)
  

  case<-simulation_set(digital) 
  logadj1<-0.00001
  

  sim_set <- data.frame(param[rep(1,length(case)), ])
  sim_set[,varn1]<-case
  sim_set[,paste0("ln",varn1)]<-log(case)
  
  
  #### digital 
  dd<-0
  for (i in 1:ncol(newparam)){
    
    vvarn<-colnames(newparam)[i]
    vvarnval<-newparma[,vvarn]/varnval1
    sim_set[,vvarn]<-case * vvarnval #로그 변환 변수는 동일하게 변환하여 넣는다.
    sim_set[,paste0("ln",vvarn)]<-log(case)+log(vvarnval+logadj1)
    dd<-dd+vvarnval
  }  
  
  
  sim_set[,"digital4"]<-(case*dd*timeconversion)
  buli_sim<-subset(sim_set,digital4<max_budget)
  
  
  
  ############################  digital vs non-digital 구도로

  
  buli_sim$nondigitalall4<-with(buli_sim, (max_budget-digital4))
  buli_sim$nondigitalall<-with(buli_sim, (max_budget-digital4)/timeconversion)
  
    
  newparam2<-subset(param, select = nondigitalinput)
  varn2 <- colnames(newparam2)[which(param == max(newparam2))]
  varnval2<- max(newparam2)
  
  
  for(i in 1:length(nondigitalinput)){
    aa<- nondigitalinput[i]
    aarate<- newparam2[,aa]/varnval2
    buli_sim[,aa]<-with(buli_sim, nondigitalall*(1/aarate))
    buli_sim[,paste0("ln",aa)]<-log(buli_sim[,aa])
    
    aagrp<-paste0("grp_",aa)
    
    if(aagrp%in%grpgroup){
      grprice <- grpprice[aagrp]
      buli_sim[,aagrp] <- buli_sim[,aa]/grprice
      buli_sim[,paste0("ln",aagrp)]<- log(buli_sim[,aagrp])
    }
    
  } 
  

    buli_sim$yhat<-with(buli_sim, exp(eval(parse(text=ResultFormula)))) 
    buli_sim$yhat4<-with(buli_sim, timeconversion*(exp(eval(parse(text=ResultFormula)))))
    
    
    for (i in 1:nrow(buli_sim)){
      ss<- buli_sim[i,"digital4"]
      if(ss>=realdigitalall){
        jjj<- i
        estim3 <- as.double(buli_sim[i,"yhat4"])
        adjust3 <- (realrev-estim3)
        break
      }
    }
    
    buli_sim$yhathat4<-with(buli_sim, 4.3*(exp(eval(parse(text=ResultFormula))))+adjust3)
    
    plot<-ggplot(buli_sim, aes(x=digital4, y=yhathat4)) +geom_line()+ geom_point(aes(x=realdigitalall, y=realrev), size=3, colour='blue')+scale_y_continuous(labels=comma)+scale_x_continuous(labels=comma)
    
    digitalnondigitalopt<-subset(buli_sim, select=c(digital4,nondigitalall4,yhathat4))
    
    write.csv(budgetlimit,"digitalnondigitalopt.csv")
    
    return(plot)
}