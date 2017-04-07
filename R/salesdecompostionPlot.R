salesdecompositionPlot=function(){    
  
  library(reshape2)  
  library(ggplot2)  
  library(plotly)  
  library(RColorBrewer)    
  
  decompose_dataset$time=c(1:nrow(decompose_dataset))  
  temp=melt(decompose_dataset[,colnames(decompose_dataset)!=depvar],id=colnames(decompose_dataset)[ncol(decompose_dataset)])      
  
  ggplot() +    
  geom_area(aes(y = value, x = time, fill = variable), data = temp, stat="identity") +    
  theme(legend.position="right", legend.direction="vertical") +    
  labs(fill="") +    
  ggtitle("sales decomposition") +    
  labs(x="week", y="sales amount") +    
  scale_fill_manual(values=brewer.pal(n = ncol(decompose_dataset)-2, name = "Blues")) +    
  theme_bw() +    
  theme(axis.line = element_line(size=1, colour = "black"), panel.grid.major = element_blank(),          
        panel.grid.minor = element_blank(), panel.border = element_blank(),          
        panel.background = element_blank())  
}
