kriging <- function(datageo, LongMin, LongMax, LatMin, LatMax, CellSize, obj, bor, si, sf, wf, wi, lambda, ...){
  
  if (!require(geoR)) 
    stop("package geoR is required to run kriging()")
  
  if (!require(kali)) 
    stop("package kali is required to run map kriging()")
  
  grdg  = expand.grid(x = seq(LongMin + CellSize / 2, LongMax - CellSize / 2, by = CellSize), 
                      y = seq(LatMin + CellSize / 2, LatMax - CellSize / 2, by = CellSize))   # tab coords of the study area
  
  inddentro = point.in.polygon(grdg$x,grdg$y,bor$x,bor$y)
  
  kc  = krige.control(type.krige = 'sk', obj = obj, lambda = lambda)
  z   = krige.conv(data.geo, loc = grdg, krige = kc)
  
  z$predict[which(inddentro == 0)] = NaN
  z$krige.var[which(inddentro == 0)] = NaN
  
  matrizN = cbind(grdg$x, grdg$y, z$predict, z$krige.var)
  matrizN <- data.frame(matrizN)
  names(matrizN) <- c("lon", "lat", "variable", "varianza")
  
  x  = table2grid(matrizN, var = "variable", lat = c(si, sf),FUN = max, lon = c(wi, wf), dx = CellSize)
  x_var  = table2grid(matrizN, var = "varianza", lat = c(si, sf),FUN = max, lon = c(wi, wf), dx = CellSize)
  axes   = createGridAxes(lat = c(si, sf), lon = c(wi, wf), dx = CellSize)
  
  return(list(matrizN = matrizN, x = x, x_var = x_var, axes = axes))
  
}
# 

# require(htmlwidgets)
# require(htmltools)
# require(taiyun)

distEuclidiana = function(x1,x2,y1,y2){
  lon.mn1 = -x1*60*cos(-y1*pi/180)
  lon.mn2 = -x2*60*cos(-y2*pi/180)
  lat.mn1 = -y1*60
  lat.mn2 = -y2*60
  out = sqrt((lon.mn1-lon.mn2)^2+(lat.mn1-lat.mn2)^2)
  return(out)
}
