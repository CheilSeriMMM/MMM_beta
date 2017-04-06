createstockedset=function(){
  week_transformed <- unstockeddata 
  
  carryrate <- 0.449^(1/4)
  
  colnames(week_transformed)
  target_var <- c(8:17, 31:50)
  
  for (i in 1 : length(target_var)){
    j <- target_var[i]
    week_transformed[,j] <- filter(week_transformed[,j], filter = carryrate, method = "recursive")
  }
  
  ATLad_share_stocked<-ddply(week_transformed, .(week), summarise, 
                             adshare=sum(monitor_tv_saida, monitor_catv_saida, monitor_jp_saida, monitor_radio_saida, na.rm=TRUE)/
                               sum(monitor_tv_saida, monitor_catv_saida, monitor_jp_saida, monitor_radio_saida,monitor_tv_others_all, monitor_catv_others_all, monitor_jp_others_all, monitor_radio_others_all, na.rm=TRUE))
  
  week_transformed$ms_ATL <- ATLad_share_stocked$adshare 
  
  ATLad_ratio_stocked<-ddply(week_transformed, .(week), summarise, adshare=sum(All_TV, radio, na.rm=TRUE)/sum(monitor_tv_others_all, monitor_catv_others_all, monitor_jp_others_all, monitor_radio_others_all, na.rm=TRUE))
  week_transformed$ATL_ad_ratio_to_comp <- ATLad_ratio_stocked$adshare
  
  write.csv(week_transformed, 'data.csv', row.names=F)
  
  return(week_transformed)
}
