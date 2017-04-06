basicmodelfitting = function(){
  
  library(reshape2)
  library(ggplot2)
  
  Est <- parameterEstimates(model_fit) 
  depvar <- Est$lhs[1]
  numOfeq <- length(model)
  temp <- unlist(strsplit(model,"~"))[seq(2,2*numOfeq,by=2)]
  independent <- strsplit(temp,"\\+")
  
  temp=Est[Est$lhs==depvar & Est$rhs %in% independent[[1]],c('lhs','rhs','est')]
  temp$mult=paste(temp$est,temp$rhs,sep="*")
  temp1=temp[1,'mult']
  for(i in 2:nrow(temp)){
    if(temp$est[i]<0){
      temp1=paste(temp1,temp$mult[i],sep="")
    } else {
      temp1=paste(temp1,temp$mult[i],sep="+")
    }
  }
  
  temp=Est[Est$lhs==depvar & nchar(Est$rhs)==0,'est']
  if(temp<0){
    temp1=paste(temp1,temp,sep="")
  } else{
    temp1=paste(temp1,temp,sep="+")
  }
  

  y_hat=with(stockeddata, exp(eval(parse(text=temp1))))
    
  temp=data.frame(time=c(1:length(y_hat)),y=exp(stockeddata[,depvar]),y_hat=y_hat)
  temp_melt=melt(temp,id='time')
    
    ggplot()+
      geom_line(aes(x=time, y=value, colour=variable), data=temp_melt, stat="identity") +
      ggtitle("모델 정확도") + 
      theme(legend.position="right", legend.direction="vertical", legend.title = element_blank()) +
      labs(fill="") +
      labs(x='time', y="sales amount") +
      theme_bw() +
      theme(axis.line = element_line(size=1, colour = "black"), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), panel.border = element_blank(),
            panel.background = element_blank())
    
  }
