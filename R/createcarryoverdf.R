createcarryoverdf=function(
   mediagroup1, 
   mediagroup2,
   data
   ){
  

  carryover <- data.frame(media=character(), lambda=numeric(), stringsAsFactors=F)
  dd=data.frame(rbind(mediagroup1,mediagroup2))
   
  
  for(i in 1:nrow(dd)){
   
   mg=dd$media[i]
  carryovertmp <- data.frame(media=character(), lambda=numeric(), stringsAsFactors=F)  
    
  if(mg=="digital"|mg=="atl"|mg=="btl") {
    mediavar=get(paste0(mg,"list"))
    } else if(!is.null(mg)&!is.na(mg)){
      aa=mg
      aalist=paste0(aa,"list")
      if(aa%in%colnames(data)){
        mediavar= aa
      } else if (is.object(aalist)){
        mediavar = aalist
      } else {
        print("Error! Check media groups!!")
      } }
    else {
        print("Error! Check media groups!!")
      }
  
  for(j in 1:length(mediavar)){
    carryovertmp[j,1]=mediavar[j]
    carryovertmp[j,2]=dd$carryover[i]
  }
  
    carryover= rbind(carryover, carryovertmp)
  }
  
  
   return(carryover)
 }
