
# # spatial catch ---------------------------------------------------------


spatial_catch_lat <- function(data, fecha_inicio, fecha_final, lat_superior, lat_inferior, ...){
  
  if (!require(akima)) {
    stop("Install akima package")
  }
  if (!require(fields)) {
    stop("Install fields package")
  }
  
  area <- area_isoparalitoral(dist_costa = captura$dc, latitude = captura$lat)
  captura$GLat <- area$lat
  captura$GDc  <- area$dc
  
  matrix_cacth_lat <- tapply(captura$catch, list(captura$GLat, captura$fecha), mean)
  matrix_cacth_lat[is.na(matrix_cacth_lat)] <- 0
  
  vector_date <- seq.Date(fecha_inicio, fecha_final, by = "day")
  
  matrix_cacth_lat0 <- data.frame(matrix(0, ncol = length(vector_date), nrow = length(seq(lat_superior,lat_inferior, by = 0.5))))
  rownames(matrix_cacth_lat0) <- seq(lat_superior,lat_inferior, by = 0.5)
  colnames(matrix_cacth_lat0) <- vector_date
  
  matrix_cacth_lat0[which(row.names(matrix_cacth_lat0) %in% row.names(matrix_cacth_lat)), which(colnames(matrix_cacth_lat0) %in% colnames(matrix_cacth_lat))] <- matrix_cacth_lat
  matrix_cacth_lat0 <- matrix_cacth_lat0[which(as.Date(colnames(matrix_cacth_lat0), format = "%Y-%m-%d") %in% fecha_inicio):which(as.Date(colnames(matrix_cacth_lat0), format = "%Y-%m-%d") %in% fecha_final)]
  
  matriz_captura <- matrix_cacth_lat0
  matriz_captura <- t(matriz_captura)
  matriz_captura <- matriz_captura[,ncol(matriz_captura):1]
  
  xLab = rownames(matriz_captura)
  xLab = gsub(pattern = "X", replacement = "", x = xLab)
  xLab = gsub(pattern = ".", replacement = "-", x = xLab, fixed = T)
  
  xi = dim(matriz_captura)[1] # longitud del vector de dias
  yi = dim(matriz_captura)[2] # longitud del vector latitud
  
  akima.bic <- bicubic.grid(x = 1:length(xLab),y = seq(lat_superior,lat_inferior,0.5),z = as.matrix(matriz_captura),
                            nx = xi, ny = yi, xlim = c(0,xi),dx = 0.05, dy = 0.05/14)
  akima.bic$z[akima.bic$z < 0] <- 0
  
  xLab <- as.Date(xLab, format = "%Y-%m-%d")
  format(xLab, format = "%Y-%m")
  
  labels_month       <- format(seq(fecha_inicio, fecha_final, by = "month"), "%b-%Y")
  labels_month       <- gsub(".-", "-", labels_month)
  
  start_year         <- grep(pattern = "Ene",x = labels_month)
  if(length(start_year) > 1){
    labels_month[-start_year] <- substring(labels_month[-start_year],1,3)  
  }
  
  return(list(akima.bic = akima.bic, matriz_captura = matriz_captura, labels_month = labels_month, xLab = xLab))
}

