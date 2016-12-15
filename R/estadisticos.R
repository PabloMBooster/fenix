# ESTADISTICOS ------------------------------------------------------------

# estadisticosDescriptivos <- function(x){
#   if (!require(nortest)) {
#     install.packages("nortest")
#     stop("package nortest is required to run lillie.test()")
#   
#   x <- x[!is.na(x)]
#   
#   n       = length(x)
#   mediana = median(x, na.rm = T)
#   media   = mean(x, na.rm = T)
#   var     = var(x, na.rm = T)
#   sd      = sd(x, na.rm = T)
#   #kurtosis
#   #asimetria
#   #moda    = estima.moda(x)
#   
#   salidas <- data.frame(rbind(n = n, mediana = mediana, media = media, var = var, sd = sd))
#   names(salidas) <- "Estadisticos"
#   #write.table(salidas, paste("estadisticosDescriptivos.csv"),col.names = F)
#   
#   # normadilad
#   km.t <- lillie.test(x)
#   sw.t <- shapiro.test(x)
#   
#   if(km.t$p > 0.05){
#     print(paste("Tiene distribución Normal mediante la prueba de Kormogorov, p = ",round(km.t$p, 3)))
#   } else{
#     print(paste("No tiene distribución Normal mediante la prueba de Kormogorov, p = ",round(km.t$p, 3)))
#   }
#   
#   if(sw.t$p > 0.05){
#     print(paste("Tiene distribución Normal mediante la prueba de Shapiro Wilk, p = ",round(sw.t$p, 3)))
#   } else{
#     print(paste("No tiene distribucion Normal mediante la prueba de Shapiro Wilk, p = ",round(sw.t$p, 3)))
#   } 
#   return(salidas)
# }

# transformacion box cox
inversePower = function(x, lambda=1) {
  out = (lambda*x+1)^(1/lambda)
  return(out)
}

jet.colors <- colorRampPalette(c('#00007F','blue','#007FFF','cyan','#7FFF7F','yellow',
                                 '#FF7F00','red','#7F0000') )

variograma <- function(data.geo, max.dist, intervalo, modelo, ...){  
  vario <- variog(data.geo, max.dist=max.dist, uvec=seq(0,max.dist,intervalo))
  
  meseta <- max(vario$v)
  rango  <- max.dist/3
  par <- c( meseta,rango)
  
  vario.model <- variofit(vario, ini.cov.pars = par, cov.model =  modelo)
  #  s100.xv.ml  <- xvalid(data.geo, model = vario.model)
  
  #  par(mfrow = c(1,2))
  plot(vario, main = modelo) 
  lines(vario.model, col = 2, lwd = 2)  
  #   plot(s100.xv.ml)  
  return(vario.model)
}

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
test.Normality <- function(x){

  list.of.packages <- c("nortest")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  
  library(nortest)
  
  if(!is.numeric(x))
    stop("x is not numeric")
  
  Ad.Test = ad.test(x)
  Cramer.Test = cvm.test(x)
  Pearson.Test = pearson.test(x)
  ShapiroF.Test = sf.test(x)
  Kolmogorov.Test = lillie.test(x)
  Shapiro.Test = shapiro.test(x)
  
  method = c(Ad.Test$method, Cramer.Test$method, Pearson.Test$method,
                    ShapiroF.Test$method, Kolmogorov.Test$method, Shapiro.Test$method)
  
  pvalue = c(Ad.Test$p.value, Cramer.Test$p.value, Pearson.Test$p.value,
             ShapiroF.Test$p.value, Kolmogorov.Test$p.value, Shapiro.Test$p.value)

  normal = rep(NA, length(pvalue))
  for(i in seq_along(pvalue)){ 
    if(!pvalue[i] <0.05) {
     normal[i] <- "SI"
   }else {
       normal[i] <- "NO"
       }
  }
  out = list(); out$method = list()
  out$method = method
  out$result = data.frame(method = method, pvalue = pvalue, normal = normal)  
return(out)
}

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

