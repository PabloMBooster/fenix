biom_survey_dist_tallaslance_png = function(data = data, output = output, file = "muestreos",cex = 0.7){
  
  
  nobs   = length(seq_along(sort(unique(data$LATITUD_INICIAL), decreasing = T)))
  nbases = nobs/42
  
  data = data[order(data$LATITUD_INICIAL, decreasing = T),]
  
  if(nbases < 1){
    png(file = paste0(output,"/", file,"1.png"),  width = 850, 
        height = 820, units = "px", res = 140)
    plot_biom_survey_dist_tallaslance(data = data, cex = cex)
    dev.off()
  }
  if(nbases > 1 & nbases < 2){
    data1 = data[data$LATITUD_INICIAL %in% unique(data$LATITUD_INICIAL)[1:(7*6)],]
    data2 = data[data$LATITUD_INICIAL %in% unique(data$LATITUD_INICIAL)[(7*6+1):nobs],]
    
    png(file = paste0(output,"/", file,"1.png"),  width = 850, 
        height = 820, units = "px", res = 140)
    plot_biom_survey_dist_tallaslance(data = data1, cex = cex)
    dev.off()
    
    png(file = paste0(output,"/", file,"2.png"),  width = 850, 
        height = 820, units = "px", res = 140)
    plot_biom_survey_dist_tallaslance(data = data2, cex = cex)
    dev.off()
  }
  if(nbases > 2 & nbases < 3){
    data1 = data[data$LATITUD_INICIAL %in% unique(data$LATITUD_INICIAL)[1:(7*6)],]
    data2 = data[data$LATITUD_INICIAL %in% unique(data$LATITUD_INICIAL)[(7*6+1):7*6*2],]
    data3 = data[data$LATITUD_INICIAL %in% unique(data$LATITUD_INICIAL)[(7*6*2+1):nobs],]
    
    png(file = paste0(output,"/", file,"1.png"),  width = 850, 
        height = 820, units = "px", res = 140)
    plot_biom_survey_dist_tallaslance(data = data1, cex = cex)
    dev.off()
    
    png(file = paste0(output,"/", file,"2.png"),  width = 850, 
        height = 820, units = "px", res = 140)
    plot_biom_survey_dist_tallaslance(data = data2, cex = cex)
    dev.off()
    
    png(file = paste0(output,"/", file,"3.png"),  width = 850, 
        height = 820, units = "px", res = 140)
    plot_biom_survey_dist_tallaslance(data = data3, cex = cex)
    dev.off()
  }
  if(nbases > 3 & nbases < 4){
    data1 = data[data$LATITUD_INICIAL %in% unique(data$LATITUD_INICIAL)[1:(7*6)],]
    data2 = data[data$LATITUD_INICIAL %in% unique(data$LATITUD_INICIAL)[(7*6+1):7*6*2],]
    data3 = data[data$LATITUD_INICIAL %in% unique(data$LATITUD_INICIAL)[(7*6*2+1):7*6*3],]
    data4 = data[data$LATITUD_INICIAL %in% unique(data$LATITUD_INICIAL)[(7*6*3+1):nobs],]
    
    
    png(file = paste0(output,"/", file,"1.png"),  width = 850, 
        height = 820, units = "px", res = 140)
    plot_biom_survey_dist_tallaslance(data = data1, cex = cex)
    dev.off()
    
    png(file = paste0(output,"/", file,"2.png"),  width = 850, 
        height = 820, units = "px", res = 140)
    plot_biom_survey_dist_tallaslance(data = data2, cex = cex)
    dev.off()
    
    png(file = paste0(output,"/", file,"3.png"),  width = 850, 
        height = 820, units = "px", res = 140)
    plot_biom_survey_dist_tallaslance(data = data3, cex = cex)
    dev.off()
    
    png(file = paste0(output,"/", file,"4.png"),  width = 850, 
        height = 820, units = "px", res = 140)
    plot_biom_survey_dist_tallaslance(data = data4, cex = cex)
    dev.off()
  }
  
}