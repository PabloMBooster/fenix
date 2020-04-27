biom_survey_map_modas = function(baseDat, outFolder = ".", outFile = "ModaPrincipal.png", xlim=c(-86,-70), ylim=c(-21, -3), Pch = 16,
                      Cols = 1, PalCols = colorRampPalette(c('red', "yellow", "green", 'blue')), Moda = 1,
                      CexPoint = 0.9, widthFig = 700, heightFig = 820, legend2 = F, posX = NULL, posY = NULL,
                      Add = FALSE, save = FALSE, Label = FALSE,  spType = "Anchoveta,anchovetaperuana,peladilla",
                      lenUnits = "cm", portImport = 1, addIsoAreas = F, Legend = TRUE, LatTck = 2, n_perfil = 1){
  
  if(is.null(outFolder)){
    outFolder <- getwd()
  }
  if(save){
    png(filename = file.path(outFolder, outFile), width = widthFig, height = heightFig,
        units = "px", res = 130)
  }
  
  mapa_peru(xlim = xlim, ylim = ylim, area_iso = addIsoAreas, n_perfil = n_perfil)
  # ------------------------------
  
  rbPal = PalCols
  
  baseDat = baseDat[which(baseDat[, "NOMBRE_COMERCIAL"] == spType), ]
  baseDat$indLance = paste0(baseDat[, "EMBARCACION"], baseDat[, "REO_NNUMLAN"])
  indLance2 = unique(baseDat$indLance)
  
  tmpout = NULL
  for(i in seq_along(indLance2)){
    temp_biom = subset(x = baseDat, subset = baseDat$indLance == indLance2[i])

    midlen = estimarModaAnch(len = temp_biom[, "LONGITUD_ESPECIE"], freq = temp_biom[, "FREC_SIMPLE"],
                             tol = 0, nmodes = 4, maxFreq = 0)
    tmp2 = data.frame(lon = temp_biom[1, "LONGITUD_INICIAL"], lat = temp_biom[1, "LATITUD_INICIAL"],
                      midlen = midlen$modas[Moda])
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
  # 
  scale.color2 = c(scale.color3, scale.color4, scale.color5, scale.color6)
  tmpout2 = tmpout[complete.cases(tmpout), ]
  tmpout2$indCol = scale.color2[round(tmpout2$midlen,1)*10]
  if(Label){
    text(x = tmpout$lon, y = tmpout$lat, labels = tmpout$midlen,
         col = Cols[1], cex = CexPoint)
  } else {
    points(x = tmpout2$lon, y = tmpout2$lat, pch = Pch, col = tmpout2$indCol, cex = CexPoint)
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
      
      legend.krige(x.leg = c(posX, posX+diffX), y.leg = c(posY, posY+diffY), scale.vals = c(8,12,14.5),
                   values = 0:20, vertical=T, col = scale.color2,
                   offset.leg = 1)
      
      text(x = (posX+diffX/2), y = (posY+diffY)*0.95, labels = paste0("Modas (cm)"),
           cex = 0.8, font = 2)
    }
  }
  if(save){
    dev.off()
  }
  
}
