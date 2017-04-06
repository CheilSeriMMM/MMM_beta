simulation_set <- function(media) {
  media1<-as.character(media)
  if(media1=="digital") {
    idx=colnames(data) %in% intersect(digitallist, gsub("ln","",inputvar))
  } else if(media1=="atl") {
    idx=colnames(data) %in% intersect(atllist, gsub("ln","",inputvar))
  } else if(media1=="btl") {
    idx=colnames(data) %in% intersect(btllist, gsub("ln","",inputvar))
  } else {
    idx = gsub("ln","",media)
  }
  
  min=min(colMeans(data[,idx]))
  max=max(colMeans(data[,idx]))
  
  FROM=0.1*min
  TO=1000*max  
  BY=(TO-FROM)/1000
  out=seq(from=FROM, to=TO, by=BY) 
  
  return(out)
}
