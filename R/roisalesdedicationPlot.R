roisalesdedicationPlot<-function(){
  
  investment3<-roimeasure()
  
  plottingset<-investment3[c(8,9),]
  plottingset[3,]<-colnames(plottingset)
  plottingset<-data.frame(t(plottingset), stringsAsFactors=FALSE)
  colnames(plottingset)<-c('RoI', 'sales_increment', 'media')
  
  
  plottingset$RoI<-as.numeric(plottingset$RoI)
  plottingset$sales_increment<-as.numeric(plottingset$sales_increment)
  
  plots<-ggplot(plottingset, aes(y=RoI, x=sales_increment, colour=media))+geom_point(aes(size=sales_increment))
  plots+geom_vline(xintercept=mean(plottingset$sales_increment), linetype="dashed", color = "red")+geom_hline(yintercept=7, linetype="dashed", color = "red")+geom_text(data=plottingset, aes(label=row.names(plottingset)), vjust=1.5)+guides(colour=FALSE)
  
  
  return(plots)
}

