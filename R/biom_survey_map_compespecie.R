biom_survey_map_compespecie = function(baseDat, outFolder = ".", outFile = "MapPieComposicionSp.png", xLim = c(-83, -70), yLim = c(-20, -3), Pch = 16,
                            Cols = rainbow(6), CexPoint = 0.9, widthFig = 700, heightFig = 820,
                            spLabels = NULL, Add = F, save = F, portImport = 1,
                            addIsoAreas = F, Legend = TRUE, LatTck = 2, n_perfil = 1){
  
  require(mapplots)
  
  if(is.null(outFolder)){
    outFolder <- getwd()
  }
  if(save){
    png(filename = file.path(outFolder, outFile), width = widthFig, height = heightFig,
        units = "px", res = 130)
  }
  
  mapa_peru(area_iso = addIsoAreas, n_perfil = n_perfil )
  selDat = baseDat
  sort(unique(selDat[, "NOMBRE_COMERCIAL"]))
  
  capBySp = by(data = selDat[, "REE_NPESESP"], INDICES = selDat[, "NOMBRE_COMERCIAL"], FUN = unique)
  xb = as.list(capBySp)
  xc = sapply(X = xb, FUN = sum, na.rm = T)
  Sps = names(xc)
  capSp = as.vector(xc)
  outd = data.frame(sp = Sps, cap = capSp)
  outd2 = outd[order(outd$cap, decreasing = T), ]
  
  outd2$sp = as.character(outd2$sp)
  otSp = outd2$sp[6:nrow(outd2)]
  
  for(i in 1:nrow(baseDat)){
    if(baseDat[i, "NOMBRE_COMERCIAL"] %in% otSp){
      baseDat[i, "NOMBRE_COMERCIAL"] = "Otros"
    } else {
      baseDat[i, "NOMBRE_COMERCIAL"] = baseDat[i, "NOMBRE_COMERCIAL"]
    }
  }
  
  baseDat$indLance = paste0(baseDat[, "EMBARCACION"], baseDat[, "REO_NNUMLAN"])
  
  outMatSp = NULL
  idx2 = unique(baseDat$indLance)
  for(i in seq_along(idx2)){
    
    tmp = baseDat[which(baseDat[, "indLance"] == idx2[i]), ]
    tmpMat = .getSpData(dat2 = tmp)
    outMatSp = rbind(outMatSp, tmpMat)
    
  }
  
  xyz = make.xyz(outMatSp$lon,outMatSp$lat,outMatSp$capt,outMatSp$sp)
  xyz$z = xyz$z[,order(names(xyz$z[1,]))]
  
  draw.pie(xyz$x, xyz$y, xyz$z, radius = 0.2, col=Cols, scale = FALSE)
  
  if(Legend){
    
    if(is.null(spLabels)){
      legend("bottomleft", legend = colnames(xyz$z), pch = 15, bty = "n", col = Cols,
             cex = 0.9)
    } else {
      legend("bottomleft", legend = spLabels, pch = 15, bty = "n", col = Cols,
             cex = 0.9)
    }
    
  }
 if(save){
    dev.off()
  }
  return(invisible())
}