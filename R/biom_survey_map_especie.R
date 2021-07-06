biom_survey_map_especie = function(baseDat, outFolder = NULL, outFile = "PresenciaEspecie.png", Pch = 16,
                       CexPoint = 0.8, widthFig = 700, heightFig = 820,
                       spType = "Anchoveta,anchovetaperuana,peladilla",spLabels = "Lances anchoveta",
                       addIsoAreas = F, n_perfil = 1, save = F, legend = T){
  
  
  # if(is.null(outFolder)){
  #   mainDir <- getwd()
  #   subDir <- "plots"
  #   
  #   if (file.exists(file.path(mainDir, subDir))){
  #     setwd(file.path(mainDir, subDir))
  #   } else {
  #     dir.create(file.path(mainDir, subDir))
  #     setwd(file.path(mainDir, subDir))
  #   }
  #   filename <- file.path(getwd(), outFile)
  # }else{
  #   filename <- file.path(outFolder, outFile)
  # }
  
  if(is.null(outFolder)){
    outFolder <- getwd()
  }
  if(isTRUE(save)){
    png(filename = file.path(outFolder, outFile), width = widthFig, height = heightFig,
        units = "px", res = 130)
  }
  par(mar = c(4,4,1,1), oma = c(0,0,0,0))
  mapa_peru(area_iso = addIsoAreas, n_perfil = n_perfil)
  points(x = baseDat[, "LONGITUD_INICIAL"],
         y = baseDat[, "LATITUD_INICIAL"],
         pch = Pch, col = "gray80", cex = CexPoint)
  
  points(x = baseDat[which(baseDat[, "NOMBRE_COMERCIAL"] == spType), "LONGITUD_INICIAL"],
         y = baseDat[which(baseDat[, "NOMBRE_COMERCIAL"] == spType), "LATITUD_INICIAL"],
         pch = 16, col = 2, cex = CexPoint)  
  
  
  if(isTRUE(legend)){
    legend("bottomleft", legend = c("Lances totales", spLabels), pch = c(16,16), bty = "n", col = c("gray","red"),
           cex = 0.9)
  }
  
  if(isTRUE(save)){
    dev.off()
  }
  return(invisible())
}