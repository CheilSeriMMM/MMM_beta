responsecurve <- function(startMonth, endMonth, media){
  geneqs()
  ResultFormula<-full_equation
  
  
  if(is.vector(media)){
    group <- media
  } else if(media=="atl"){
    group <- atllist
  } else if(media=="btl"){
    group <- btllist
  } else if(media=="digital"){
    group <- btllist
  } else {
    if(media %in% colnames(media)){
    group <- c(media)}
    else{
      print("err: media is not a variable")
    }
  }
  
  
  case<-simulation_set(media)
  graphset <- NULL
  
  for(i in 1:length(group)){
    
    media1<-group[i]
    nolog<-sub("ln","",media1)
    
    sim_data2<-subset(data, month>=startMonth & month<=endMonth)
      
    param2<-data.frame(apply(sim_data2, 2, mean))  
    param2<-t(param2); 
    rownames(param2)<-NULL
      

    sim_set2 <- data.frame(param2[rep(1,length(case)), ])
      
    sim_set2[,"input"]<-case
    sim_set2[, nolog]<-case
    sim_set2[, media1]<-log(case)
    sim_set2[,"yhat"]<-with(sim_set2, exp(eval(parse(text=ResultFormula))))
      
    a<-subset(sim_set2, select=c(input, yhat))
    a$media <- nolog
      
    graphset<-rbind(graphset,a)
    }
  

  library(scales)

    out=ggplot(graphset_digital, aes(x=input, y=yhat, colour=media))+
    geom_line()+
    scale_y_continuous(labels=comma)+scale_x_continuous(labels=comma)+
    ggtitle("매체별 반응곡선 - Digital")
  
  return(out)
}

