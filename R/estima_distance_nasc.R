estima_distance_nasc <- function (x, y, x1, y1) 
{
  A3 = y1 * pi/180
  B3 = x1 * pi/180
  A31 = y * pi/180
  B31 = x * pi/180
  num2 = length(x1) - 1
  n = length(x)
  dfinal = NULL
  num = length(y)
  pos.na = c(which(is.na(y) == TRUE), which(is.na(x) == TRUE))
  pos.na
  dc <- NULL
  num2 = length(x1) - 1
  n = length(x)
  dfinal = NULL
  num = length(y)
  pos.na = c(which(is.na(y) == TRUE), which(is.na(x) == TRUE))
  pos.na
  d1 <- NULL
  d2 <- NULL
  for(i in 1:num){
    A=NULL
    B=NULL
    
    A=sin(A3[1:num2]) * sin(A31[i])
    B=cos(A3[1:num2]) * cos(A31[i])*cos(B3[1:num2]-B31[i])
    grad=acos(A+B)
    gradinrad=180*grad/pi
    
    distancia=(60*gradinrad)
    indnonan=which(distancia>0)
    
    if(length(indnonan) == 0){
      dist_min = NA
    }else{
      dist_min1=which.min(distancia[indnonan])
      dist0 = distancia[indnonan]
      dist_min2=which.min(dist0[-which.min(dist0)])
    }
    
    d1 <- rbind(d1,dist_min1)
    d2 <- rbind(d2,dist_min2)
  }
  d1 <- as.numeric(d1)
  suppressWarnings(d1)
  
  d2 <- as.numeric(d2)
  suppressWarnings(d2)
  
  d0 <- cbind(d1, d2)
  return(d0)
}