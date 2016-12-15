# dinamicsFleet -----------------------------------------------------------
# calcula el area cubierta por la flota 
coverCala = function(data, idArea, date, gridSise){
  out = NULL
  for(ttime in sort(unique(data[,date]))){
    idtime = which(data[,date] == ttime)
    coverArea = sum(as.numeric(table(data[idtime,idArea]))*(gridSise)^2)
    coverArea = c(ttime, coverArea)
    out = data.frame(rbind(out, coverArea), row.names = NULL)
  }
  names(out) = c("num.dia", "area")
  return(out)
}

