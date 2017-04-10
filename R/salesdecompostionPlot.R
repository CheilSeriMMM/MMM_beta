salesdecompositionPlot=function(time){
library(reshape2)
library(ggplot2)
library(plotly)
library(RColorBrewer)

  
decompose_dataset=salesdecomposition()
  
decompose_dataset$time=stockeddata[,time]
decompose1=melt(decompose_dataset[,colnames(decompose_dataset)!=depvar],id=colnames(decompose_dataset)[ncol(decompose_dataset)])


ggplot()+
geom_area(aes(y=value,x=time,fill=variable),data=decompose1,stat="identity")+
theme(legend.position="right",legend.direction="vertical")+
labs(fill="")+
ggtitle("salesdecomposition")+
labs(x="week",y="salesamount")+
scale_fill_manual(values=brewer.pal(n=length(decompose_dataset)-2,name="Blues"))+
theme_bw()+
theme(axis.line=element_line(size=1,colour="black"),panel.grid.major=element_blank(),
panel.grid.minor=element_blank(),panel.border=element_blank(),
panel.background=element_blank())

}
