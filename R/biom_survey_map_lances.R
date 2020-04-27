biom_survey_map_lances = function(baseDat, outFolder = ".", outFile = "LancesCrucero.png", xLim = c(-83, -70), yLim = c(-20, -3), Pch = 16,
                       CexPoint = 0.9, widthFig = 700, heightFig = 820,
                       Add = FALSE, save = FALSE, Label = FALSE, portImport = 1, addIsoAreas = F,
                       Legend = TRUE, LatTck = 2){
  
  if(is.null(outFolder)){
    outFolder <- getwd()
  }
  if(save){
    png(filename = file.path(outFolder, outFile), width = widthFig, height = heightFig,
        units = "px", res = 130)
  }
  mapa_peru(area_iso = addIsoAreas)
  
 
  if(Label){
    Barcos = unique(baseDat[, "EMBARCACION"])
    baseDat$LanLab = paste0(substr(x = baseDat[, "EMBARCACION"], start = 1, stop = 2), baseDat[, "REO_NNUMLAN"])
    baseDat$LanLab1 = paste0(baseDat[, "EMBARCACION"], baseDat[, "REO_NNUMLAN"])
    Barcos2 = tolower(Barcos)
    Cols = baseBarcoCol$cols[match(Barcos2, baseBarcoCol$barco)]
    Cols[which(is.na(Cols))] = baseBarcoCol$cols[which(baseBarcoCol$barco == "otros")] #para lanchas
    for(k in seq_along(Barcos)){
      
      tmp2 = baseDat[which(baseDat[, "EMBARCACION"] == Barcos[k]), ]
      idx = unique(tmp2$LanLab1)
      
      for(j in seq_along(idx)){
        tmp3 = tmp2[which(tmp2[, "LanLab1"] == idx[j]), ]
        text(x = tmp3[1, "LONGITUD_INICIAL"], y = tmp3[1, "LATITUD_INICIAL"], labels = tmp3[1, "LanLab"],
             col = Cols[k], cex = CexPoint)
      }
      
    }
    
  } else {
    Barcos = unique(baseDat[, "EMBARCACION"])
    
    Barcos2 = tolower(Barcos)
    Cols = baseBarcoCol$cols[match(Barcos2, baseBarcoCol$barco)]
    Cols[which(is.na(Cols))] = baseBarcoCol$cols[which(baseBarcoCol$barco == "otros")] #para lanchas
    
    for(k in seq_along(Barcos)){
      points(x = baseDat[which(baseDat[, "EMBARCACION"] == Barcos[k]), "LONGITUD_INICIAL"],
             y = baseDat[which(baseDat[, "EMBARCACION"] == Barcos[k]), "LATITUD_INICIAL"],
             pch = Pch, col = Cols[k], cex = CexPoint)
    }
    
    if(Legend){
      Barcos3 = data.frame(barc = Barcos2, col1 = Cols)
      Barcos3$barc = as.character(Barcos3$barc)
      Barcos3$col1 = as.character(Barcos3$col1)
      Barcos3$barc[which(Barcos3$barc == "imarpe iv" | Barcos3$barc == "imarpe v" | Barcos3$barc == "imarpe vi")] = "L/P"
      uniBarc = c("L/P", "olaya", "humboldt", "flores")
      Barcos3$barc[!Barcos3$barc %in% uniBarc] = "E/P"
      vs = aggregate(x = Barcos3, by = list(Barcos3$barc,Barcos3$col1), FUN = unique)
      legend("bottomleft", legend = toupper(vs$barc), pch = Pch, bty = "n", col = vs$col1, cex = 0.9)
    }
    
  }
  if(save){
    dev.off()
  }
  
}




