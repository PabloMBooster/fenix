estima_dist <- function (x, y, x1, y1) 
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
  dc <- as.numeric(dc)
  suppressWarnings(dc)
  return(dc)
}