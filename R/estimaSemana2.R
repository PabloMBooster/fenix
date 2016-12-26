
# estima semana 2 ---------------------------------------------------------

estimaSemana2 <- function(inicio.temp, fin.temp, ...){
  
  diasTemporada = seq.Date(as.Date(inicio.temp), as.Date(fin.temp),by =  "day")
  dias =  weekdays(diasTemporada)
  temporada = data.frame(diasTemporada = diasTemporada, dias = dias)
  nSem = c(rep(0, min(which(dias == "lunes"))-1), sort(rep(1:length(dias[dias=="lunes"]),7)))
  nSem[1:length(dias)]
  temporada$semana <- nSem[1:length(dias)]  
  return(temporada)
}
}

