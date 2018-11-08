
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


#  ------------------------------------------------------------------------

fun_mean_IC <- function(x, group,...){
  mn      <- aggregate(x, list(group), mean, na.rm = TRUE)
  std     <- aggregate(x, list(group), sd, na.rm = TRUE)
  num.obs <- as.numeric(table(group))
  sterr   <- as.numeric(std$x/sqrt(num.obs))
  mn$sterr<- sterr 
  
  return(mn)
}


# estas funciones deben pasar a SISESATools

# Sinuosidad1 <- function(tetha, distanciaEmision){
#   s <- mean(sin(tetha), na.rm = TRUE)
#   c <- mean(cos(tetha), na.rm = TRUE)
#   p <- mean(distanciaEmision, na.rm = TRUE)
#   b <- sd(distanciaEmision, na.rm = TRUE)
#   
#   out <- 2*(p*((1-(c^2)-(s^2))/((1-c^2)+s^2)+b^2))^(-0.5) 
#   
#   return(out)
# }
# 
# 
# Sinuosidad2 <- function(tetha, distancia_emision){
#   
#   s <- mean(sin(tetha), na.rm = TRUE)
#   c <- mean(cos(tetha), na.rm = TRUE)
#   p <- mean(distancia_emision, na.rm = TRUE)
#   b <- sd(distancia_emision, na.rm = TRUE)
#   
#   out <- 2*(p*((1+c)/(1-c))+b^2)^-0.5
#   return(out)
# }
# 
# Sinuosidad3 <- function(x){
#   
#   out <- sum(x, na.rm = TRUE)/length(x)  
#   return(out)
# }

# 
# equation_angle <- function(x,y){
#   dot.prod <- x%*%y
#   norm.x <- norm(x,type="2")
#   norm.y <- norm(y,type="2")
#   theta <- suppressWarnings(acos(dot.prod / (norm.x * norm.y))*180/pi)
#   return(theta)
# }
# 
# angle <- function(x,y){
#   
#   vec_angle <- NULL
#   for(z in 2:(length(x)-1)){
#     #print(z)
#     A <- matrix(NA,nrow = 2)
#     B <- matrix(NA,nrow = 2)
#     X1 <- as.matrix(x[c(z-1,z)])
#     Y1 <- as.matrix(y[c(z-1,z)])
#     X2 <- as.matrix(x[c(z,z+1)])
#     Y2 <- as.matrix(y[c(z,z+1)])
#     A[1] <- X1[1]-X1[2]
#     A[2] <- Y1[1]-Y1[2]
#     B[1] <- X2[2]-X2[1]
#     B[2] <- Y2[2]-Y2[1]
#     out <- equation_angle(t(A),B)
#     vec_angle <- rbind(vec_angle,out)
#   }
#   vec_angle <- as.numeric(vec_angle)
#   return(vec_angle)
# }


