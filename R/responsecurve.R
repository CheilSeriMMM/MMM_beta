responsecurve=function(
  startMonth, 
  endMonth, 
  media){
  dd<-geneqs()
  
  ResultFormula<-dd$formula
  data<-stockeddata
  
  
  
  grpprice<-grpprice(startMonth,endMonth)
  allpricepergrp <- grpprice$pricepergrp
  tvpricepergrp <- grpprice$tvpricepergrp
  capricepergrp <- grpprice$catvpricepergrp
  jppricepergrp <- grpprice$jptvpricepergrp
  
 
  
  if(media=="atl"){
    group <- intersect(atllist, inputvar)
  } else if(media=="btl"){
    group <- intersect(btllist, inputvar)
  } else if(media=="digital"){
    group <- intersect(digitallist, inputvar)
  } else if(media %in% colnames(data)){
    group <- c(media)
  }  else{
      print("err: media is not a variable")
    }
     sim_data2<-subset(data, month>=startMonth & month<=endMonth)
      
    param2<-data.frame(apply(sim_data2, 2, mean))  
    param2<-t(param2); 
    rownames(param2)<-NULL 
  case<-simulation_set(media)
  
  
  zz=intersect(group, grplist) 

  altcase=NULL
  
  if(length(zz)>0){
    aa=subset(param2,select=zz)
    vv=max(aa)
    maxgrp=colnames(aa)[which(aa[1,]==vv)]
    tmpmax = aa[,maxgrp]*5
    tmpmax2 = tmpmax*get(paste0(gsub("grp_","",maxgrp),"pricepergrp"))
    altcase=seq(from=0, to=tmpmax, by= tmpmax/1000) 

    if(tmpmax2>max(case)){
      case=seq(from=min(case), to=tmpmax2, by= (tmpmax2-min(case))/1000) 
      }
    
  }
  
  

  
  graphset <- NULL
  
  for(i in 1:length(group)){
    
    media1<-group[i]
    withlog<-paste0("ln",media1)
    

      
    sim_set2 <- data.frame(param2[rep(1,length(case)), ])
      
    if(media1%in%grplist){
      dd=gsub("grp_","",media1)
      sim_set2[, media1]<-altcase
      sim_set2[,"input"]<-altcase*get(paste0(dd,"pricepergrp"))
      sim_set2[, withlog]<-log(altcase)
      } else {
      sim_set2[,"input"]<-case
      sim_set2[, withlog]<-log(case)
      sim_set2[, media1]<-case
    }


    sim_set2[,"yhat"]<-with(sim_set2, exp(eval(parse(text=ResultFormula))))
    a<-subset(sim_set2, select=c(input, yhat))
    a$media <- media1
      
    graphset<-rbind(graphset,a)
    
    rm(sim_set2)
    
    
  }
 
  write.csv(graphset,paste0(paste(group,collapse = "_"),"responsecurve.csv"))
  library(scales)

    out=ggplot(graphset, aes(x=input, y=yhat, colour=media))+
    geom_line()+
    scale_y_continuous(labels=comma)+scale_x_continuous(labels=comma)+
    ggtitle(media)
  
  return(out)
}
