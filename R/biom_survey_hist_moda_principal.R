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
                      midlen = getModa[Moda], midlen2 = getModa[Moda+1], midlen3 = getModa[Moda+2])
    # midlen = estimarModaAnch(len = temp_biom[, "LONGITUD_ESPECIE"], freq = temp_biom[, "FREC_SIMPLE"],
    #                          tol = 0, nmodes = 4, maxFreq = 0)
    # tmp2 = data.frame(lon = temp_biom[1, "LONGITUD_INICIAL"], lat = temp_biom[1, "LATITUD_INICIAL"], lance =   temp_biom[1, "REO_NNUMLAN"],
    #                   midlen = midlen$modas[Moda])
    tmpout = rbind(tmpout, tmp2)
  }
  tmpout <- tmpout[!is.na(tmpout$midlen),]
  
  tabla_modas1 = table(tmpout$midlen)
  tabla_modas2 = table(c(tmpout$midlen2,tmpout$midlen3))
  #tabla_modas3 = table(tmpout$midlen3)
  
  tallas = seq(2,20,by=0.5)
  matrix_modas = matrix(0, nrow  = 2, ncol = length(tallas))
  
  
  
  matrix_modas[1, which(tallas %in% as.numeric(names(tabla_modas1)))] = as.numeric(tabla_modas1)
  matrix_modas[2, which(tallas %in% as.numeric(names(tabla_modas2)))] = as.numeric(tabla_modas2)
  #matrix_modas[3, which(tallas %in% as.numeric(names(tabla_modas3)))] = as.numeric(tabla_modas3)
  
  par(mar = c(4,4,0,0), oma = c(1,1,1,1))
  a = barplot(matrix_modas[1,], col = 1, ylab = "Frecuencia modas", xlab = "Tallas (cm)", ylim = c(0, max(matrix_modas)+1))
  axis(1, at = a[seq(1,length(a), by = 2)], labels = seq(2,20, by = 1))
  legend(32, max(matrix_modas), legend = c("Moda pincipal", "Modas secundarias"), col = c(1,4), pch = 15, bty = "n")
  box()
  if (isTRUE(save)) {
    dev.copy(png, filename = paste0(output, "/",file,".png"), width = 2000, height = 1000, 
             res = 200)
    dev.off()
  }
  par(mar = c(4,4,0,0), oma = c(1,1,1,1))
  a = barplot(matrix_modas, col = c(1,4), ylab = "Frecuencia modas", xlab = "Tallas (cm)", ylim = c(0, max(matrix_modas)+1))
  axis(1, at = a[seq(1,length(a), by = 2)], labels = seq(2,20, by = 1))
  legend(32, max(matrix_modas), legend = c("Moda pincipal", "Modas secundarias"), col = c(1,4), pch = 15, bty = "n")
  box()
  if (isTRUE(save)) {
    dev.copy(png, filename = paste0(output, "/",file,"_all.png"), width = 2000, height = 1000, 
             res = 200)
    dev.off()
  }
}

  