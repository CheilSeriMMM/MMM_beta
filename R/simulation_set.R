simulation_set <- function(media) {
  media1<-as.character(media)
  data<-stockeddata
  if(media1=="digital") {
    idx=colnames(data) %in% intersect(digitallist, gsub("ln","",inputvar))
  } else if(media1=="atl") {
    idx=colnames(data) %in% intersect(atllist, gsub("ln","",inputvar))
  } else if(media1=="btl") {
    idx=colnames(data) %in% intersect(btllist, gsub("ln","",inputvar))
  } else {
    idx = colnames(data) %in% gsub("ln","",media)
  }
  
  if(length(which(idx))==1) {
    min=min(colMeans(data)[idx])
    max=max(colMeans(data)[idx])
  } else if(length(which(idx)) > 1) {
    min=min(colMeans(data[,idx]))
    max=max(colMeans(data[,idx]))
  } else {
    print("Error!")
  }
  
  FROM=0.01*min
  TO=20*max  
  BY=(TO-FROM)/10000
  out=seq(from=FROM, to=TO, by=BY) 
  
  return(out)
}
