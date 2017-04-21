createcarryoverdf=function(
   mediagroup1, 
   mediagroup2
   ){
  
  carryover <- data.frame(media=character(), lambda=numeric(), stringsAsFactors=F)
  dd=c(mediagroup1$media,mediagroup2$media)
   
  for(i in 1:length(dd)){
   
    carryover[i,1]=dd[i]
  if(dd[i]%in%mediagroup1$media){
    carryover[i,2]=mediagroup1$carryover
  } else if (dd[i]%in%mediagroup2$media){
    carryover[i,2]=mediagroup2$carryover
  } else {
    print("Error! Check media groups!!")
  }
  }
  
   return(carryover)
 }
