biom_survey_map_midlen = function(baseDat, outFolder = ".", outFile  = "LongMedia.png", xLim = c(-83, -70), yLim = c(-20, -3), Pch = 16,
                       Cols = 1, PalCols = colorRampPalette(c('red', "yellow", "green", 'blue')),
                       CexPoint = 0.9, widthFig = 700, heightFig = 820,
                       Add = FALSE, save = FALSE, Label = FALSE,  spType = "Anchoveta,anchovetaperuana,peladilla"  ,
                       lenUnits = "cm", portImport = 1, addIsoAreas = F, Legend = TRUE, LatTck = 2){
  
  require(geoR)
  
  if(is.null(outFolder)){
    outFolder <- getwd()
  }
  if(save){
    png(filename = file.path(outFolder, outFile), width = widthFig, height = heightFig,
        units = "px", res = 130)
  }
  
  mapa_peru(area_iso = addIsoAreas)
  # ------------------------------
  
  rbPal = PalCols
  
  baseDat = baseDat[which(baseDat[, "NOMBRE_COMERCIAL"] == spType), ]
  baseDat$indLance = paste0(baseDat[, "EMBARCACION"], baseDat[, "REO_NNUMLAN"])
  indLance2 = unique(baseDat$indLance)
  
  tmpout = NULL
  for(i in seq_along(indLance2)){
    
    temp_biom = subset(x = baseDat, subset = baseDat$indLance == indLance2[i])
    
    midlen = (sum(temp_biom[, "LONGITUD_ESPECIE"] * temp_biom[, "FREC_SIMPLE"], na.rm = T))/sum(temp_biom[, "FREC_SIMPLE"], na.rm = T)
    tmp2 = data.frame(lon = temp_biom[1, "LONGITUD_INICIAL"], lat = temp_biom[1, "LATITUD_INICIAL"],
                      midlen = midlen)
    tmpout = rbind(tmpout, tmp2)
    
  }
  
  require(RColorBrewer)
  require(fields)
  cols5 = brewer.pal(n = 9, name = "Greens")[3:7]
  cols3 = brewer.pal(n = 9, name = "Blues")[3:7]
  cols4 = brewer.pal(n = 9, name = "Oranges")[3:7]
  cols6 = brewer.pal(n = 9, name = "Greys")[3:6]
  
  scale.color3 = designer.colors(80, cols3)
  scale.color4 = designer.colors(40, cols4)
  scale.color5 = designer.colors(25, cols5)
  scale.color6 = designer.colors(55, cols6)
  scale.color2 = c(scale.color3, scale.color4, scale.color5, scale.color6)
  
  tmpout2 = tmpout[complete.cases(tmpout), ]
  tmpout2$indCol = scale.color2[round(tmpout2$midlen,1)*10]
  if(Label){
    
    text(x = tmpout2$lon, y = tmpout2$lat, labels = round(tmpout2$midlen, 1),
         col = Cols, cex = CexPoint)
    
  } else {
    
    
    points(x = tmpout2$lon, y = tmpout2$lat, pch = Pch, col = tmpout2$indCol, cex = CexPoint)
    
    if(Legend){
      posX = -84
      posY = -19.5
      diffX = 0.3
      diffY = 7
      
      legend.krige(x.leg = c(posX, posX+diffX), y.leg = c(posY, posY+diffY), scale.vals = c(8,12,14.5),
                   values = 0:20, vertical=T, col = scale.color2,
                   offset.leg = 1)
      
      
      text(x = (posX+diffX/2), y = (posY+diffY)*0.95, labels = paste0("Talla media \n (cm)"),
           cex = 0.8, font = 2)
      
    }
    
  }
  
  # ------------------------------
  
  if(save){
    dev.off()
  }
  
}
