read.biom <- function(data, ...){
  
  
  selCols = c("NOMBRE_OPERACION", "FECHA_INICIO", "FECHA_FIN", "EMBARCACION", "REO_NNUMLAN",
              "LATITUD_INICIAL", "LONGITUD_INICIAL", "LATITUD_FINAL", "LONGITUD_FINAL",
              "FEC_INICIO", "FEC_FIN", "CAPTURA_ACTIVIDAD", "NOMBRE_COMERCIAL",
              "NOMBRE_CIENTIFICO", "REE_NPESGRU", "REE_NPESESP", "LONGITUD_ESPECIE",
              "FREC_SIMPLE", "PESO")
  
  cat(paste0("\n","><>><> 1) Datos a considerar")) 
  cat(paste0("\n","- REO_NNUMLAN: etiqueta de calas"))
  cat(paste0("\n","- REO_VNUMLAN: etiqueta de calas positivas y calas negativas"))
  cat(paste0("\n","- REE_NPESESP: Captura de especie por cala"))
  cat(paste0("\n","- REE_NPESGRU: por definir"))
  cat(paste0("\n","..."))
  cat(paste0("\n","..."))
  cat(paste0("\n","><>><> 2) Antes de iniciar a procesar los datos biometricos filtrar las calas negativas"))
  seq_names <-  names(data)
  variables_faltantes <- NULL 
  for(i in seq_along(selCols)){
    cotejo <- !selCols[i] %in% seq_names
    if(isTRUE(cotejo)){
      variables_faltantes <- c(variables_faltantes, selCols[i])
      cat("><>><> 3) Revisar las siguientes variables")
      print(variables_faltantes)
      baseDat2 <- NULL
      
     }
    }
  
  if(!isTRUE(cotejo)){
      cat(paste0("\n","..."))
      cat(paste0("\n","..."))
      cat(paste0("\n","><>><> 3) Las datos biometricos estan correctos"))
      
      nombre_comercial <- tolower(unique(data$NOMBRE_COMERCIAL))
      anch             <- grep(pattern = "anchovetaperuana",x = nombre_comercial)
      nombre_cientifico<-tolower(unique(data$NOMBRE_CIENTIFICO))
      anch2            <- grep(pattern = "engraulis ringens",x = nombre_cientifico)
      
      print(paste0("\n",paste0("Nombre comercial = ", nombre_comercial[anch])))
      print(paste0("\n",paste0("Nombre cientifico = ", nombre_cientifico[anch2])))
      
      baseDat2 = data[ ,selCols]
      baseDat2[, "LATITUD_INICIAL"]  = abs(baseDat2[, "LATITUD_INICIAL"])*-1
      baseDat2[, "LONGITUD_INICIAL"] = abs(baseDat2[, "LONGITUD_INICIAL"])*-1
      baseDat2[, "EMBARCACION"]      = as.character(baseDat2[, "EMBARCACION"])
      baseDat2[, "EMBARCACION"]      = gsub(pattern = " ", replacement = "", x = baseDat2[, "EMBARCACION"])
      baseDat2[, "NOMBRE_COMERCIAL"] = as.character(baseDat2[, "NOMBRE_COMERCIAL"])
      baseDat2[, "NOMBRE_COMERCIAL"] = gsub(pattern = " ", replacement = "", x = baseDat2[, "NOMBRE_COMERCIAL"])
      #baseDat2$FECINICIO2 = strptime(x = as.character(baseDat2[,"ARRASTRE_EFECTIVO_INICIO"]), format = "%d/%m/%Y %H:%M")
      #baseDat2$FECFIN2 = strptime(x = as.character(baseDat2[,"ARRASTRE_EFECTIVO_FIN"]), format = "%d/%m/%Y %H:%M")
      #baseDat2$DIFFTIME = as.numeric(difftime(time1 = baseDat2$FECFIN2, time2 = baseDat2$FECINICIO2, units = "mins"))
      #baseDat2$DIFFTIME[which(is.na(baseDat2$DIFFTIME) | baseDat2$DIFFTIME <= 0 | baseDat2$DIFFTIME > 100)] = 15
      baseDat2$CERP_cod = gsub("\\D", "", baseDat2[,"NOMBRE_OPERACION"])
      baseDat2$CERP_cod = paste0("C", baseDat2$CERP_cod)
      #gsub("\\D", "", dat$NOMBRE_OPERACION)
    }
  return(baseDat2)
}


