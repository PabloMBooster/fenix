mapa_peru <- function(xlim=c(-84,-70), ylim=c(-21, -3), labelsxy = TRUE, ylab = "Latitud", xlab = "Longitud", 
                      all_axis = FALSE, 
                      land.col="khaki1", sea.col="white", names_harbor = TRUE, cex_harbor = 0.65, add = FALSE, 
                      n_perfil = 1, space_perfil = 3){
  
  require(kali)
  
  axis.Lon <- paste(abs(seq(xlim[1],xlim[2],by = 2)),"°W")
  axis.Lat <- paste(abs(seq(ylim[1],ylim[2],by = 2)),"°S")
  
  Encoding(axis.Lon) <- "UTF-8"
  Encoding(axis.Lat) <- "UTF-8"
  
  xlim2 <- xlim
  if(n_perfil > 1){
    xlim2[1] <- xlim2[1] + (n_perfil-1)*(-3)  
  }
  
  plot.map(axes = F,col="red", cex=1, xlim=xlim2, hires = TRUE, ylab = NULL, xlab = NULL, xaxs = "i", yaxs = "i", 
           ylim=ylim, land.col=land.col, sea.col=sea.col, 
           boundaries.col = NA, grid.col = "blue",main="", 
           grid = FALSE, add = add)
  
  lines(linePeru$lon, linePeru$lat, col="gray40")
  
  if(n_perfil > 1){
  for(i in 2:n_perfil){
    lines(linePeru$lon + (i-1)*-3, linePeru$lat, col="gray40")
    }
  }
  
  if(isTRUE(labelsxy)){
    if(n_perfil == 1){
      mtext(xlab, side=1, line=1.5, cex=0.8)
      mtext(ylab, side=2, line=1.8, cex=0.8)
    }else{
      mtext(ylab, side=2, line=1.5, cex=0.8)
    }
  }
  
  if(isTRUE(names_harbor)){
    principalP = puertosPeru[c(2,4,5,7,8,10,12,14,16,17,19),]
    text(principalP$lon, principalP$lat, labels = principalP$puertos, cex=cex_harbor, pos=4, font=1)
    
  }
  axis(2,seq(ylim[1],ylim[2],by = 2), axis.Lat, las=1, cex.axis=0.6, hadj=0.5, tck=-0.010)
  if(n_perfil == 1){
    axis(1,seq(xlim[1],xlim[2],by = 2), tck=-0.01, labels = NA, hadj=0.5)
    axis(1,seq(xlim[1],xlim[2],by = 2), labels = axis.Lon, cex.axis=0.6, line = -0.8, lwd = 0)
  }

  if(all_axis == TRUE){
    axis(3,seq(xlim[1],xlim[2],by = 2), tck=-0.01, labels = NA, hadj=0.5)
    axis(3,seq(xlim[1],xlim[2],by = 2),labels = axis.Lon, cex.axis=0.6, line = -0.5, lwd = 0)
    axis(4,seq(ylim[1],ylim[2],by = 2), axis.Lat, las=1, cex.axis=0.6, hadj=0.5, tck=-0.010)
  }
  #return(invisible)
}