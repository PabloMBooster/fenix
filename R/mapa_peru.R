mapa_peru <- function(xlim=c(-86,-70), ylim=c(-21, -3), xlab = "", ylab = "",
                       cex_axis = 1, cex_harbor = 1, col_harbor = 1, font_harbor = 2,
                       land.col="khaki1", border.map = "khaki1", add1 = FALSE,
                       n_perfil = 1, space_perfil = 3,
                       area_iso = FALSE, name_area_iso = NULL){

  require(maps)
  require(mapdata)
  options(warn=-1)
  #x11()
  axis.Lon <- paste(abs(seq(xlim[1],xlim[2],by = 2)),"°W")
  axis.Lat <- paste(abs(seq(ylim[1],ylim[2],by = 2)),"°S")

  Encoding(axis.Lon) <- "UTF-8"
  Encoding(axis.Lat) <- "UTF-8"

  xlim2 <- xlim
  if(n_perfil > 1){
    xlim2[1] <- xlim2[1] + (n_perfil-1)*(-space_perfil)
  }

  #par(mar = c(2,2,0.1,0.1), oma = c(2,2,2,2))

  plot(NA, xlim = xlim2, ylim = ylim, axes = FALSE, xlab = xlab, ylab = ylab, add = add1)
  polygon(x = c(linePeru$lon[1], -50, -50, linePeru$lon[23513:2], linePeru$lon[1]),
          y = c(linePeru$lat[1], -24, 0, linePeru$lat[23513:2], linePeru$lat[1]),
          col = land.col)
  # map("worldHires", fill=TRUE, col = land.col, add = TRUE,
  #     xlim = xlim, ylim = ylim, border = border.map)
  box()

  lines(linePeru$lon, linePeru$lat, col = "gray40")

  if(n_perfil > 1){
    for(i in 2:n_perfil){
      lines(linePeru$lon + (i-1)*-3, linePeru$lat, col="gray40")
    }
  }

  principalP = puertosPeru[c(2,4,5,7,8,10,12,14,16,17,19),]
  text(principalP$lon, principalP$lat, labels = principalP$puertos, pos=4,
       col = col_harbor, cex = cex_harbor, font = font_harbor)

  axis(2,seq(ylim[1],ylim[2],by = 2), axis.Lat, las=1, cex.axis=cex_axis, hadj=0.5, tck=-0.010)

  if(n_perfil == 1){
    axis(1,seq(xlim[1],xlim[2],by = 2), tck=-0.01, labels = NA, hadj=0.5)
    axis(1,seq(xlim[1],xlim[2],by = 2), labels = axis.Lon, cex.axis=cex_axis, line = -0.8, lwd = 0)
  }

  if(isTRUE(area_iso)){
    if(!is.null(name_area_iso)){
      lonlat_areaIso <- lonlat_areaIso[lonlat_areaIso$area %in% name_area_iso,]
    }
    addIsopara(dataIsopara = lonlat_areaIso, ylim = c(-21, -3))
    lines(linePeru$lon, linePeru$lat, col = "gray40")
  }
}

# 
# load("C:/pablo/D/github/tasaR/data/AIPData_new.RData")
# load("C:/pablo/D/github/tasaR/data/AIPData_old.RData")
# # 
# head(AIPData_old)
# head(AIPData_new)

#mapa_peru(area_iso = T, name_area_iso = c(12180, 13180))
