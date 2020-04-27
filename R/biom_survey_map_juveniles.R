biom_survey_map_juveniles = function(baseDat, outFolder = ".", outFile = "LancesJuveniles.png", xlim = c(-86,-70), ylim = c(-21, -3), Pch = 16,
                          Cols = rainbow(6), CexPoint = 0.9, widthFig = 700, heightFig = 820,legend2 = F, posX = NULL, posY = NULL,
                          Add = FALSE, save = FALSE, spType = "Anchoveta,anchovetaperuana,peladilla",
                          lenUnits = "cm", AdLength = 12, portImport = 1, addIsoAreas = F, Legend = TRUE,
                          LatTck = 2, n_perfil = 1){
  
  require(geoR)
  if(is.null(outFolder)){
    outFolder <- getwd()
  }
  if(save){
    png(filename = file.path(outFolder, outFile), width = widthFig, height = heightFig,
        units = "px", res = 130)
  }
  
  mapa_peru(xlim = xlim, ylim = ylim, area_iso = addIsoAreas, n_perfil = n_perfil)
  rbPal = colorRampPalette(c('blue', "green", "yellow", 'red'))
  PalCols = rbPal(101)
  
  baseDat = baseDat[which(baseDat[, "NOMBRE_COMERCIAL"] == spType), ]
  baseDat$indLance = paste0(baseDat[, "EMBARCACION"], baseDat[, "REO_NNUMLAN"])
  indLance2 = unique(baseDat$indLance)
  
  for(i in seq_along(indLance2)){
    
    temp_biom = subset(x = baseDat, subset = baseDat$indLance == indLance2[i])
    
    tmpjuv = temp_biom[which(temp_biom[ ,"LONGITUD_ESPECIE"] < AdLength), "FREC_SIMPLE"]
    tmpadu = temp_biom[which(temp_biom[ ,"LONGITUD_ESPECIE"] >= AdLength), "FREC_SIMPLE"]
    
    porc = round(sum(tmpjuv, na.rm = T)/(sum(tmpjuv, na.rm = T) +  sum(tmpadu, na.rm = T))*100)
    porc2 = porc + 1
    
    points(x = temp_biom[, "LONGITUD_INICIAL"],
           y = temp_biom[, "LATITUD_INICIAL"],
           pch = Pch, col = PalCols[porc2], cex = CexPoint)
  }
  
  if(Legend){
    if(isTRUE(legend2)){
      posX = posX + (n_perfil-1)*-3
      posY = posY
      diffX = 0.2
      diffY = 3
    }else{
      posX = -84 + (n_perfil-1)*-3
      posY = -19.5
      diffX = 0.3
      diffY = 7
    }
    legend.krige(x.leg = c(posX, posX+diffX), y.leg = c(posY, posY+diffY),
                 values = 0:100, vertical=T, col = PalCols,
                 offset.leg = 1)
    
    text(x = (posX+diffX/2), y = (posY+diffY)*0.95, labels = "Juveniles (%)",
         cex = 0.8, font = 2)
    
    
  }
  
  if(save){
    dev.off()
  }
  return(invisible())
}
