biom_survey_map_modas_cohortes <- function (baseDat, outFolder = ".", outFile = "moda_principal_color_cohortes_.png", 
                                            xlim = c(-86, -70), ylim = c(-21, -3), Pch = 16, Cols = 1, 
                                            PalCols = colorRampPalette(c("red", "yellow", "green", "blue")), 
                                            Moda = 1, CexPoint = 0.9, widthFig = 700, heightFig = 820, 
                                            legend2 = F, posX = NULL, posY = NULL, Add = FALSE, save = FALSE, 
                                            Label = FALSE, spType = "Anchoveta,anchovetaperuana,peladilla", 
                                            lenUnits = "cm", portImport = 1, addIsoAreas = F, Legend = TRUE, 
                                            LatTck = 2, n_perfil = 1) 
{
  if (is.null(outFolder)) {
    outFolder <- getwd()
  }
  # if (save) {
  #   png(filename = file.path(outFolder, outFile), width = widthFig, 
  #       height = heightFig, units = "px", res = 130)
  # }
  # par(mar = c(4, 4, 1, 1), oma = c(0, 0, 0, 0))
  # mapa_peru(xlim = xlim, ylim = ylim, area_iso = addIsoAreas, 
  #           n_perfil = n_perfil)
  rbPal = PalCols
  baseDat = baseDat[which(baseDat[, "NOMBRE_COMERCIAL"] == 
                            spType), ]
  baseDat$indLance = paste0(baseDat[, "EMBARCACION"], baseDat[, 
                                                              "REO_NNUMLAN"])
  indLance2 = unique(baseDat$indLance)
  tmpout = NULL
  for (i in seq_along(indLance2)) {
    temp_biom = subset(x = baseDat, subset = baseDat$indLance == 
                         indLance2[i])
    temp_biom = temp_biom[order(temp_biom$LONGITUD_ESPECIE, 
                                decreasing = F), ]
    marcas <- seq(1, 20, by = 0.5)
    freq <- rep(0, length(marcas))
    ubicacionMarcas <- which(marcas %in% temp_biom$LONGITUD_ESPECIE)
    freq[ubicacionMarcas] <- temp_biom$FREC_SIMPLE
    getModa <- modas(x = matrix(freq, nrow = 1), Lmin = 1, 
                     Lmax = 20, dL = 0.5, umbral = 5)
    tmp2 = data.frame(lon = temp_biom[1, "LONGITUD_INICIAL"], 
                      lat = temp_biom[1, "LATITUD_INICIAL"], midlen = getModa[1])
    tmpout = rbind(tmpout, tmp2)
  }
  tmpout = tmpout[tmpout$midlen > 1.5,]
  require(RColorBrewer)
  require(fields)
  varColor = "color3"
  
  tmpout2 = tmpout
  tmpout2$indCol = NULL
  for(i in 1:nrow(tmpout)){
    tmpout2$indCol[i] = escalaColores2[which(escalaColores2$Tallas %in% tmpout2$midlen[i])[1],varColor]
  }
  tmpout2$edad = "prerecluta"
  tmpout2$edad[tmpout2$midlen>=8 & tmpout2$midlen < 12] = "recluta"
  tmpout2$edad[tmpout2$midlen>=12] = "adulto"
  tmpout2$edad[tmpout2$midlen>=14.5] = "adultomayor"
  
  scale.color2 = escalaColores2[,varColor]
  
  if (save) {
    png(filename = file.path(outFolder, outFile), width = widthFig, 
        height = heightFig, units = "px", res = 130)
  } 
  
  par(mar = c(4, 4, 1, 1), oma = c(0, 0, 0, 0))
  mapa_peru(xlim = xlim, ylim = ylim, area_iso = T)
  
  points(x = tmpout2$lon[tmpout2$edad == "prerecluta"], y = tmpout2$lat[tmpout2$edad == "prerecluta"], pch = 21, 
         col = 1, bg = tmpout2$indCol[tmpout2$edad == "prerecluta"], cex = CexPoint)
  points(x = tmpout2$lon[tmpout2$edad == "recluta"], y = tmpout2$lat[tmpout2$edad == "recluta"], pch = 21, 
         col = 1, bg = tmpout2$indCol[tmpout2$edad == "recluta"], cex = CexPoint)
  points(x = tmpout2$lon[tmpout2$edad == "adulto"], y = tmpout2$lat[tmpout2$edad == "adulto"], pch = 21, lwd =1.3,
         col = 1, bg = tmpout2$indCol[tmpout2$edad == "adulto"], cex = CexPoint)
  points(x = tmpout2$lon[tmpout2$edad == "adultomayor"], y = tmpout2$lat[tmpout2$edad == "adultomayor"], pch = 21, lwd =1.3,
         col = 1, bg = tmpout2$indCol[tmpout2$edad == "adultomayor"], cex = CexPoint)
  
  if (Label) {
    text(x = tmpout$lon, y = tmpout$lat, labels = tmpout$midlen, 
         col = Cols[1], cex = CexPoint)
  }
  else {
    #points(x = tmpout2$lon, y = tmpout2$lat, pch = Pch, 
    #col = tmpout2$indCol, cex = CexPoint)
    if (Legend) {
      if (isTRUE(legend2)) {
        posX = posX + (n_perfil - 1) * -3
        posY = posY
        diffX = 0.2
        diffY = 3
      }
      else {
        posX = -84 + (n_perfil - 1) * -3
        posY = -19.5
        diffX = 0.3
        diffY = 7
      }
      legend.krige(x.leg = c(posX, posX + diffX), y.leg = c(posY, 
                                                            posY + diffY), scale.vals = c(8, 12, 14.5), 
                   values = 0:20, vertical = T, col = scale.color2, 
                   offset.leg = 1)
      text(x = (posX + diffX/2), y = (posY + diffY) * 
             0.95, labels = paste0("Modas (cm)"), cex = 0.8, 
           font = 2)
    }
  }
  if (save) {
    dev.off()

  }
}
