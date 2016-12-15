
# distancias --------------------------------------------------------------

latlon.to.m =
  function(start.lon, start.lat, end.lon, end.lat)
  {
    # converts a distance between two points in lat -lon (decimal)
    # into a meter (horizontal) distance
    # depends on the latitude
    # note lat and lon are in decimal units (e.g. 49.23)
    # compute average latitude
    mid.lat = (start.lat + end.lat)/2
    meters.per.degree <- 111195.
    radians.lat <- (pi * mid.lat)/180
    shrink.factor <- cos(radians.lat)
    delx <- shrink.factor * (start.lon - end.lon)
    dely <- start.lat - end.lat
    meters.dist <- meters.per.degree * sqrt(delx^2 + dely^2)
    return(meters.dist)
    end
  }

distanciaSigno <- function (x, y, x1, y1) 
{
  
  junk = approx(y,x,y1)
  signo <- (junk$y-x1)/abs((junk$y-x1))
  
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
  dc <- NULL
  for (i in 1:num) {
    A = NULL
    B = NULL
    A = sin(A3[1:num2]) * sin(A31[i])
    B = cos(A3[1:num2]) * cos(A31[i]) * cos(B3[1:num2] - 
                                              B31[i])
    grad = acos(A + B)
    gradinrad = 180 * grad/pi
    distancia = (60 * gradinrad)
    indnonan = which(distancia > 0)
    dist_min = min(distancia[indnonan])
    dfinal[i] = (dist_min)
    dc <- rbind(dc, dist_min)
  }
  dc[is.infinite(dc)] <- NA
  #return(dc)
  dc <- as.numeric(dc)
  dc.signo <- dc*signo
  list(dc = dc, dc.signo = dc.signo)
}
 
# junk = approx(shoreline$V2,shoreline$V1,bitacora1$lat)
# bitacora1$distttt = latlon.to.m(junk$y,bitacora1$lat,bitacora1$lon,bitacora1$lat)/1000



