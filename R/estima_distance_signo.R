estima_distance_signo <- function (x, y, x1, y1) 
{
  
  junk = approx(y1,x1,y)
  signo <- (junk$y-x)/abs((junk$y-x))
  
  
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
  distance_min <- NULL
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
      dist_min=min(distancia[indnonan])  
    }
    
    distance_min <- rbind(distance,dist_min)
  }
  distance_min <- as.numeric(distance_min)
  suppressWarnings(distance_min)
  list(dc = dc, signo = signo)
}