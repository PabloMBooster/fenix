TallasSeg_lat <- function(data, tallas = seq(5, 20, by = 0.5), tallas_juv = seq(5, 11.5, by = 0.5)){
  require(dplyr)
  namesVar    <- paste0("X", tallas)
  namesVarJuv <- paste0("X", tallas_juv)
  
  TallasSeg  <- tallas
  TallasGlat <- lapply(split(data, data$Glat, drop = TRUE), function(x){
    
    x[is.na(x)] <- 0
    nobs        <- nrow(x)
    
    if(nobs > 1){
      outTallasPon                   <- x[,namesVar]
      outTallasPon[outTallasPon > 0] <- 0
      for(i in 1:nobs){
        outTallasPon[i,] <- fenix::CALfreqPonderada(tallas = TallasSeg, frecuencia = x[i,namesVar], a = 0.0043, b = 3.1803, captura = x$descarga_t[i])  
      }
      outTallasPon       <- apply(outTallasPon, 2, sum, na.rm = T)
    }else{
      outTallasPon       <- fenix::CALfreqPonderada(tallas = TallasSeg, frecuencia = x[,namesVar], a = 0.0043, b = 3.1803, captura = x$descarga_t)  
    }
    
    outTallasPon <- as.numeric(outTallasPon)
    porJuv       <- sum(outTallasPon[1:14])/sum(outTallasPon)*100
    Glat         <- x$Glat[1]
    captura      <- sum(x$descarga_t, na.rm = T)
    n_viajes     <- nobs
    cbind.data.frame(n_viajes, porJuv, Glat, captura, t(outTallasPon))
    
  })
  TallasGlat <- TallasGlat %>% lapply(as.data.frame) %>% bind_rows()
  TallasGlat <- completar_lat(data = TallasGlat)
  return(TallasGlat)
}

TallasSeg_temp <- function(data, captura_total, a = NULL, b = NULL){
  
  if(is.null(a)){
    # valores historicos
    a = 0.0043
    b = 3.1803
  }
  frecuencia          <- as.numeric(apply(data[,as.character(1:31)], 2, sum))
  tallas              <- seq(5, 20, by = 0.5)
  
  tallas_ponderadas   <- CALfreqPonderada(tallas = tallas, frecuencia = frecuencia, a = a, b = b, captura = captura_total)
  
  output_tallas       <- data.frame(tallas = tallas, captura = tallas_ponderadas)
  output_tallas$peso  <- output_tallas$captura*(a*tallas^b) 
  
  por_juv             <- round(sum(output_tallas$captura[output_tallas$tallas < 12])/sum(output_tallas$captura)*100,1)
  peso_juv            <- sum(output_tallas$peso[output_tallas$tallas < 12])
  
  output <- list()
  
  output$output_tallas <- output_tallas
  output$por_juv       <- por_juv
  output$peso_juv      <- peso_juv
  
  return(output)
}


TallasSeg_plot <-function(data, capt_label = F, box = F, ylab = F, kspace = 0.5, capt_real = NULL,
         ylim = c(-21,-3)){
  TallasSeg <- seq(5,20, by = 0.5)
  num_plot = length(data$Glat)
  options(warn=-1)
  
  LatLabelNum <- 2:21
  LatLabel <- c("02","04","05","06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21")
  LatLabel <- paste0(paste0(LatLabel[-length(LatLabel)],"°","00'"),"-",paste0(LatLabel[-1],"°","00'"))
  LatLabel <- rev(LatLabel)
  axis.Lat <- paste0(abs(seq(ylim[1],ylim[2],by = 1)),"°S")
  Encoding(axis.Lat) <- "UTF-8"

  data  <- data[order(data$Glat, decreasing = T), ]
  data0 <- data
  data0[,-c(1:4)] <- 0
  max(data[,-c(1:4)])
  
  porJuv1    <- round(data$porJuv.y, 1)
  porCapt    <- round(data$captura.y/sum(data$captura.y)*100,1)#; capt <- rev(capt)
  if(is.null(capt_real)){
    porCapt    <- round(data$captura.y/sum(data$captura.y)*100,1)#; capt <- rev(capt)
  }else{
    porCapt    <- round(data$captura.y/sum(data$captura.y)*100,1)#; capt <- rev(capt)
    porCapt    <- round(porCapt*capt_real/100,2)
  }

  for(i in 1:nrow(data0)){
    data0[i,-c(1:4)] <- as.numeric(data[i,-c(1:4)]/sum(data[i,-c(1:4)])) + kspace*(i-2)
  }
  
  nlenT <- nrow(data0)*kspace
  plot(NA, axes = F, xlab = "", ylab = "", 
       xlim = c(5,20), ylim = c(0,kspace*num_plot-0.5), xaxs = "i" , yaxs = "i")
  for(i in 1:num_plot){
    
    nlen <- length(table(as.numeric(data0[i,-c(1:4)])))
    if(nlen == 0){
      ytallas <- as.numeric(data0[i,-c(1:4)])  
      colLy <- "white"
    }else{
      ytallas <- as.numeric(data0[i,-c(1:4)])  
      vector <- 1:length(ytallas)
      obs1 <- range(vector[!vector %in% which(ytallas == min(ytallas))])[1] - 1
      obs2 <- range(vector[!vector %in% which(ytallas == min(ytallas))])[2] + 1
      ytallas[-c(obs1:obs2)] <- NA
      colLy <- "blue"
    }
    lines(TallasSeg , ytallas, lwd = 2, col = colLy)
    if(nlen > 0){
      legend(x = 6,y = kspace + 0.5*(i-2), legend = paste0("%J =", porJuv1[i]), bty = "n", cex = 0.65, col = "blue")
      if(is.null(capt_real)){
        legend(x = 15,y = kspace + 0.5*(i-2), legend = paste0("%Capt =", porCapt[i]), bty = "n", cex = 0.65, col = "blue")  
      }else{
        legend(x = 15,y = kspace + 0.5*(i-2), legend = paste0("Capt =", porCapt[i], "mil t"), bty = "n", cex = 0.65, col = "blue")  
      }
      abline(h = kspace + 0.5*(i-3), col = "gray", lty = 2,lwd = 1)
    }
  }
  mtext(side = 1, text = "Longitud total (cm)", line = 2, cex = 1.2)
  if(box == T){
    box()
  }
  if(ylab == T){
    mtext(side = 2, text = "Frecuencia (%)", outer = T, cex = 1.2, line = 2.2)
  }
  if(i == num_plot){
    axis(1, at = seq(4, 20, by = 2), labels = seq(4, 20, by = 2))
    axis(2, seq(0,9.5, length.out = length(axis.Lat)), axis.Lat, las=1, cex.axis=1, hadj=0.5, tck=-0.010)
  }
  abline(h = kspace + 0.5*(num_plot-2), lty = 1)
  abline(v = 12, col = "red", lty = 2,lwd = 2)
  
  return(invisible())
}



TallasSeg_map_int <- function(xlim=c(-86,-70), ylim=c(-21, -3), xlab = "", ylab = "",
         cex_axis = 1, cex_harbor = 1, col_harbor = 1, font_harbor = 2,
         land.col="khaki1", border.map = "khaki1", add1 = FALSE,
         n_perfil = 1, space_perfil = 3,
         area_iso = FALSE, name_area_iso = NULL){
  
  require(maps)
  require(mapdata)
  options(warn=-1)

  axis.Lon <- paste(abs(seq(xlim[1]-1,xlim[2]+1,by = 2)),"°W")
  axis.Lat <- paste(abs(seq(ylim[1],ylim[2],by = 1)),"°S")
  
  Encoding(axis.Lon) <- "UTF-8"
  Encoding(axis.Lat) <- "UTF-8"
  
  xlim2 <- xlim
  if(n_perfil > 1){
    xlim2[1] <- xlim2[1] + (n_perfil-1)*(-space_perfil)
  }
  plot(NA, xlim = xlim2, ylim = ylim, axes = FALSE, xlab = xlab, ylab = ylab, add = add1, xaxs = "i" , yaxs = "i")
  polygon(x = c(linePeru$lon[1], -50, -50, linePeru$lon[23513:2], linePeru$lon[1]),
          y = c(linePeru$lat[1], -24, 0, linePeru$lat[23513:2], linePeru$lat[1]),
          col = land.col)
  lines(linePeru$lon, linePeru$lat, col = "gray40")
  if(n_perfil > 1){
    for(i in 2:n_perfil){
      lines(linePeru$lon + (i-1)*-3, linePeru$lat, col="gray40")
    }
  }
  principalP = puertosPeru[c(2,4,5,7,8,10,12,14,16,17,19),]
  text(principalP$lon, principalP$lat, labels = principalP$puertos, pos=4,
       col = col_harbor, cex = cex_harbor, font = font_harbor)
  
  axis(4,seq(ylim[1],ylim[2],by = 1), axis.Lat, las=1, cex.axis=cex_axis, hadj=0.5, tck=-0.010,labels = NA)

  axis(3,seq(xlim[1]-1,xlim[2]+1,by = 2), tck=-0.01, labels = NA, hadj=0.5)
  axis(1,seq(xlim[1]-1,xlim[2]+1,by = 2), tck=-0.01, labels = NA, hadj=0.5)
  axis(1,seq(xlim[1]-1,xlim[2]+1,by = 2), labels = axis.Lon, cex.axis=cex_axis, line = -0.8, lwd = 0)

  if(isTRUE(area_iso)){
    if(!is.null(name_area_iso)){
      lonlat_areaIso <- lonlat_areaIso[lonlat_areaIso$area %in% name_area_iso,]
    }
    addIsopara(dataIsopara = lonlat_areaIso, ylim = c(-21, -3))
    lines(linePeru$lon, linePeru$lat, col = "gray40")
  }
  return(invisible())
}


TallasSeg_mapaIso <-function(DataTallas, DataTallasLat){
  require(fields)
  scale.color = designer.colors(100, c('blue',
                                       '#007FFF','cyan',
                                       '#7FFF7F','yellow',
                                       '#FF7F00','red'))
  
  layout(matrix(c(1,2), ncol = 2), widths = c(0.6,1))
  par(mar = c(2,2,2,0))
  TallasSeg_plot(DataTallasLat, ylim = c(-21, -2))
  par(mar = c(2,0,2,0.5))
  
  TallasSeg_map_int(xlim = c(-82,-70), ylim = c(-21, -3))
  
  tmp_isoareas = isopara$area %in% unique(DataTallas$area)
  tmp_isopara = isopara[tmp_isoareas, ]
  
  idx_areas = unique(tmp_isopara$area)
  for(i in seq_along(idx_areas)){
    
    temp = subset(x = tmp_isopara, subset = tmp_isopara$area == idx_areas[i])
    temp_data = subset(x = DataTallas, subset = DataTallas$area == idx_areas[i])
    
    captura_area = round(sum(temp_data$captura, na.rm = T))
    captura_area[captura_area <= 2] <- 2
    polygon(x = c(temp$lon[1], temp$lon[2:nrow(temp)], temp$lon[1]), 
            y = c(temp$lat[1], temp$lat[2:nrow(temp)], temp$lat[1]), 
            border = 1, col = scale.color[captura_area])
  }
  require(geoR)
  legend.krige(c(-81,-80.5),c(-20,-13),
               1:100, vertical=T, col=scale.color, offset.leg = 1)
  text(x = -80.9, y = -12.2, labels = "Captura \n(miles t)")
  legend("toprigh", 
         legend = "", bty = "n", cex = 1.4)
  return(invisible())
  
}

completar_lat <- function(data, ...){
  data_new <- data.frame(matrix(NA, ncol = 4, nrow = length(3:21)))
  data_new[is.na(data_new)] <- 0
  names(data_new) <- names(data)[1:4]
  data_new[,"Glat"] <- 3:21
  data_new <- merge(data_new, data, by = "Glat", all = T)
  data_new[is.na(data_new)] <- 0
  data_new <- data_new[,-c(2:4)]
  return(data_new)
}