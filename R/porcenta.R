# PORCENTAS ---------------------------------------------------------------

#dirUrl = "http://www.imarpe.pe/imarpe/archivos/reportes/imarpe_rpelag_porfinal"
DownloadPorcenta <- function(directorio, dirUrl, inicio, fin, ...){
  tiempo <- seq(as.Date(inicio), as.Date(fin), by = "day")
  tiempo2 <- strftime(tiempo,format="%d%m%Y")  
  for (i in 1:length(tiempo2))
  {
    url <- paste0(dirUrl, tiempo2[i], ".xlsx")
    destfile <- paste0(directorio,"imarpe_rpelag_porfinal",tiempo2[i],".xlsx")   
    try(download.file(url,destfile,method="internal",mode="wb"))
  }  
}

ReadPorcenta <- function(directorio, inicio, fin,...){
  out.porcenta  = list()
  list.porcenta = NULL
  
  tiempo    <- seq(as.Date(inicio), as.Date(fin), by = "day")
  tiempo2 <- strftime(tiempo,format="%d%m%Y")  
  
  desembarque     <- NULL
  n.embarcaciones <- NULL
  e.muestreadas   <- NULL
  p.juveniles     <- NULL
  moda            <- NULL
  
  for(i in 1:length(tiempo2))
  {
    file_name <- paste(directorio,paste("imarpe_rpelag_porfinal",tiempo2[i],".xlsx",sep=""),sep="")
    wb <- tryCatch({
      loadWorkbook(file_name)        
    }, error=function(e) {
      message("Not Found",file_name)
      return(wb)
    })
    
    desemb  <- readWorksheet(wb, sheet="reporte",startRow = 11, startCol = 3,endRow = 12, endCol = 40) 
    n.embar <- readWorksheet(wb, sheet="reporte",startRow = 12, startCol = 3, endRow = 13, endCol = 40) 
    e.muest <- readWorksheet(wb, sheet="reporte",startRow = 13, startCol = 3, endRow = 14, endCol = 40) 
    p.juv   <- readWorksheet(wb, sheet="reporte",startRow = 14, startCol = 3, endRow = 15, endCol = 40) 
    mod     <- readWorksheet(wb, sheet="reporte",startRow = 15, startCol = 3, endRow = 16, endCol = 40)  
    
    porcenta = print(tiempo2[i])
    list.porcenta = rbind(list.porcenta, porcenta)
    
    desemb1  <- t(matrix(as.numeric(desemb)))
    desembarque <-rbind(desembarque,desemb1)
    
    n.embar1  <- t(matrix(as.numeric(n.embar)))
    n.embarcaciones <-rbind(n.embarcaciones,n.embar1)
    
    e.muest1  <- t(matrix(as.numeric(e.muest)))
    e.muestreadas <-rbind(e.muestreadas,e.muest1)
    
    p.juv1  <- t(matrix(as.numeric(p.juv)))
    p.juveniles <-rbind(p.juveniles,p.juv1)
    
    mod1  <- t(matrix(as.numeric(mod)))
    moda <-rbind(moda,mod1)    
  }
  
  #desembarque0 <- desembarque
  puertos_porcentas <-  c("Paita","Paita","Parachique","Parachique","Chicama","Chicama",
                          "Chimbote","Chimbote","Samanco","Samanco","Casma","Casma",
                          "Huarmey","Huarmey","Supe","Supe","Vegueta","Vegueta","Huacho","Huacho",
                          "Chancay","Chancay","Callao","Callao","T.Mora","T.Mora",
                          "Pisco","Pisco","Atico","Atico","Planchada","Planchada","Quilca","Quilca",
                          "Mollendo","Mollendo","Ilo","Ilo")  
  
  tipo <- c(rep(c("Ind", "Ind Mad"),length(puertos_porcentas)/2))
  puerto <- c("tiempo",as.character(tiempo))
  
  desembarque[is.na(desembarque)] <- 0
  n.embarcaciones[is.na(n.embarcaciones)] <- 0
  e.muestreadas[is.na(e.muestreadas)] <- 0
  p.juveniles[is.na(p.juveniles)] <- 0
  moda[is.na(moda)] <- 0
  ##
  desembarque <- data.frame(desembarque)
  n.embarcaciones  <- data.frame(n.embarcaciones)
  e.muestreadas <- data.frame(e.muestreadas)
  p.juveniles <- data.frame(p.juveniles)
  moda <- data.frame(moda)
  ##
  desembarque <- data.frame(rbind(tipo,desembarque));names(desembarque) <- puertos_porcentas
  n.embarcaciones <- data.frame(rbind(tipo,n.embarcaciones));names(n.embarcaciones) <- puertos_porcentas
  e.muestreadas <- data.frame(rbind(tipo,e.muestreadas));names(e.muestreadas) <- puertos_porcentas
  p.juveniles <- data.frame(rbind(tipo,p.juveniles));names(p.juveniles) <- puertos_porcentas
  moda <- data.frame(rbind(tipo,moda));names(moda) <- puertos_porcentas
  ##
  desembarque <- cbind(puerto,desembarque)
  n.embarcaciones <- cbind(puerto,n.embarcaciones)
  e.muestreadas <- cbind(puerto,e.muestreadas)
  p.juveniles <- cbind(puerto,p.juveniles)
  moda <- cbind(puerto,moda)
  
  out.porcenta$desembarque = desembarque
  out.porcenta$n.embarcaciones = n.embarcaciones
  out.porcenta$e.muestreadas = e.muestreadas
  out.porcenta$p.juveniles = p.juveniles
  out.porcenta$moda = moda
  
  return(out.porcenta)
}

reportePorcentaExcel = function(objeto, ...){
  
  library(openxlsx)
  
  wb <- createWorkbook()
  
  options("openxlsx.borderStyle" = "thin")
  options("openxlsx.borderColour" = "#4F81BD")
  
  ## Add worksheets
  addWorksheet(wb, "desembarque")
  addWorksheet(wb, "embarcaciones")
  addWorksheet(wb, "embarcaciones_muestreadas")
  addWorksheet(wb, "juveniles")
  addWorksheet(wb, "moda")
  
  writeData(wb, "desembarque", out$desembarque, startCol = 1, startRow = 1, rowNames = FALSE)
  writeData(wb, "embarcaciones", out$n.embarcaciones, startCol = 1, startRow = 1, rowNames = FALSE)
  writeData(wb, "embarcaciones_muestreadas", out$e.muestreadas, startCol = 1, startRow = 1, rowNames = FALSE)
  writeData(wb, "juveniles", out$p.juveniles, startCol = 1, startRow = 1, rowNames = FALSE)
  writeData(wb, "moda", out$moda, startCol = 1, startRow = 1, rowNames = FALSE)
  
  saveWorkbook(wb, paste(out_excel,"/",file, sep = ""), overwrite = TRUE)
  
}


dataFrame = function(data, ...){
  
  require(lubridate)
  
  data = data[-1,]
  for(i in 1:ncol(data)){
    if(i == 1){
      data[,i] = as.Date(as.character(data[,i]), format = "%Y-%m-%d")
      data[,i] = month(as.POSIXlt(data[,i], format="%Y-%m-%d"))
    }else{
      data[,i] = as.numeric(as.character(data[,i]))  
    }
  } 
  return(data)
}

DoMonthCatch <- function(data) {
  
  catchMonth = NULL
  for(i in unique(data[, 1])){
    mdata = data[data$puerto == i, ]
    catch  = apply(mdata[-1], 2, sum)
    catchMonth   = rbind(catchMonth, catch)
  }
  return(catchMonth)
}





