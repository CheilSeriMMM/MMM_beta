carryoverratebasic=function(
  dependent,
  marketsize,
  media1,
  media2,
  marketvar,
  data,
  mode
){
  
  
  dd<-c(dependent,marketsize,media1,media2,marketvar)
  for(j in 1:length(dd)){
    v<-dd[j]
    for(i in 2:nrow(data)){
      data[i,paste0("l",v)]<-data[i-1,v] 
    }
  }
  
  
  dd1<-c(dependent,marketsize,marketvar)
  for(j in 1:length(dd1)){
    v<-dd1[j]
    for(i in 3:nrow(data)){
      data[i,paste0("ll",v)]<-data[i-2,v] 
    }
  }
  
  
  if(mode==1){
    
    varlist<-c(marketsize, media1, marketvar)
    
    initval<-list(const=0)
    
    tmpform<-paste0(dependent,"~","l",dependent)
    dd<-lm(tmpform,data= data,na.action = "na.exclude")
    tmplist<-list()
    tmplist["lambda"] <- as.numeric(dd$coefficients[2])
    initval<-append(initval,tmplist)
    
    
    for(i in 1:length(varlist)){
      tmpform<-paste0(dependent,"~",varlist[i])
      dd<-lm(tmpform,data= data,na.action = "na.exclude")
      tmplist <- list()
      tmplist[[ paste0("b",i-1) ]] <- as.numeric(dd$coefficients[2])
      initval<-append(initval,tmplist)
    }
    
    
    partform_single_market<-"(x -lambda*lx)"
    partform_lagy<-"lambda*ly"
    partform_ad<-"b1*media1"
    
    form_marketsize<- paste("b0",gsub("x",marketsize,partform_single_market),sep="*")
    form_lagy<-gsub("y",dependent,partform_lagy)
    form_ad<-gsub("media1",media1,partform_ad)
    
    formpartvec<-c(form_marketsize,form_lagy,form_ad)
    
    
    for(i in 1:length(marketvar)){
      n<-1+i
      aa<-paste0("b",n)
      name<-paste0("form",n)
      formtmp<-paste(aa,gsub("x",marketvar[i],partform_single_market),sep="*")
      assign(name,formtmp)
      formpartvec<-c(formpartvec,get(name))
    }
    
    tmpformula <- paste(formpartvec,collapse = "+") 
    
    formula<-paste0(dependent,"~","const+",tmpformula)
    
  } else if (mode==2){
    
    varlist<-c(marketsize, media1, media2, marketvar)
    
    initval<-list(const=0, lambda = 0, delta=0)
    
    for(i in 1:length(varlist)){
      tmpform<-paste0(dependent,"~",varlist[i])
      dd<-lm(tmpform,data= data,na.action = "na.exclude")
      tmplist <- list()
      tmplist[[ paste0("b",i-1) ]] <- as.numeric(dd$coefficients[2])
      initval<-append(initval,tmplist)
    }
    
    
    partform1<-"(lambda+delta)*ly+(lambda*delta)*lly"
    partform2_1<-"(x-delta*lx)"
    partform2_2<-"(x-lambda*lx)"
    partform3<-"(x-(lambda+delta)*lx-(lambda*delta)*llx)"
    
    
    form_y<-gsub("y",dependent,partform1)
    form_marketsize<-paste("b0",gsub("x",marketsize,partform3),sep="*")
    form1<-paste("b1",gsub("x",media1,partform2_1),sep="*")
    form2<-paste("b2",gsub("x",media2,partform2_2),sep="*")
    
    
    formpartvec<-c(form_y,form_marketsize,form1,form2)
    
    for(i in 1:length(marketvar)){
      n<-2+i
      aa<-paste0("b",n)
      name<-paste0("form",n)
      formtmp<-paste(aa,gsub("x",marketvar[i],partform3),sep="*")
      assign(name,formtmp)
      formpartvec<-c(formpartvec,get(name))
    }
    
    tmpformula <- paste(formpartvec,collapse = "+") 
    
    formula<-paste0(dependent,"~","const+",tmpformula)
    
  } else {
    
    print("=============== error ================")
    
  }
  
  
  rslt<-list(formula=formula, startlist=initval, data=data)
  
  return(rslt)
  
} 
