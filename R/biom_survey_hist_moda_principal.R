biom_survey_hist_moda_principal = function(Base_datos = Base_datos, spType = spType, save = T, output = dir_output, file = "histogrma_modas_biom"){
  
  Base_datos          = Base_datos[which(Base_datos[, "NOMBRE_COMERCIAL"] == spType), ]
  Base_datos$indLance = paste0(Base_datos[, "EMBARCACION"], "-",Base_datos[, "REO_NNUMLAN"])
  indLance2           = unique(Base_datos$indLance)
  Moda                = 1 # moda principal
  
  tmpout = NULL
  for(i in seq_along(indLance2)){
    print(indLance2[i])
    temp_biom             <- subset(x = Base_datos, subset = Base_datos$indLance == indLance2[i])
    marcas                <- seq(1, 20, by = 0.5)
    freq                  <- rep(0, length(marcas))
    ubicacionMarcas       <- which(marcas %in% temp_biom$LONGITUD_ESPECIE)
    freq[ubicacionMarcas] <- temp_biom$FREC_SIMPLE
    getModa               <- modas(x = matrix(freq, nrow = 1), Lmin = 1, 
                                   Lmax = 20, dL = 0.5, umbral = 5)
    
    tmp2 = data.frame(lon = temp_biom[1, "LONGITUD_INICIAL"], lat = temp_biom[1, "LATITUD_INICIAL"], lance =   temp_biom[1, "REO_NNUMLAN"],
                      midlen = getModa[Moda])
    # midlen = estimarModaAnch(len = temp_biom[, "LONGITUD_ESPECIE"], freq = temp_biom[, "FREC_SIMPLE"],
    #                          tol = 0, nmodes = 4, maxFreq = 0)
    # tmp2 = data.frame(lon = temp_biom[1, "LONGITUD_INICIAL"], lat = temp_biom[1, "LATITUD_INICIAL"], lance =   temp_biom[1, "REO_NNUMLAN"],
    #                   midlen = midlen$modas[Moda])
    tmpout = rbind(tmpout, tmp2)
  }
  tmpout <- tmpout[!is.na(tmpout$midlen),]
  par(mar = c(4,4,0,0), oma = c(1,1,1,1))
  xtallas = as.numeric(names(table(tmpout$midlen)))
  yfreq   = as.numeric(table(tmpout$midlen))
  plot(xtallas, yfreq, xlim = c(2,20), xlab = "Longitud total (cm)", ylab = "Frecuencia modas principales", col = "white")
  for(i in seq(xtallas)){
    segments(x0 = xtallas[i],y0 = 0, x1 = xtallas[i],y1 = yfreq[i], lwd = 8, col = 4)
  }
  axis(1, 2:20)
  axis(2)
  box()
  
  if (isTRUE(save)) {
    dev.copy(png, filename = paste0(output, "/",file,".png"), width = 2000, height = 1000, 
             res = 200)
    dev.off()
  }
}
