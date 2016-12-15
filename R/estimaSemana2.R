
# estima semana 2 ---------------------------------------------------------

estimaSemana2 <- function(inicio.temp, fin.temp, ...){
  
  diasTemporada = seq.Date(as.Date(inicio.temp), as.Date(fin.temp),by =  "day")
  dias =  weekdays(diasTemporada)
  temporada = data.frame(diasTemporada = diasTemporada, dias = dias)
  inicio.semana = which(temporada$dias[1:7] %in% "lunes")
  num.semana = round((length(temporada$dias)-inicio.semana-inicio.semana)/ 7)
  residuo = length(temporada$dias) - (inicio.semana + num.semana*7)
  temporada$semana = c(rep(0, inicio.semana-1),rep(1:num.semana, each = 7), rep(num.semana + 1, residuo+1))
  
  
  return(temporada)
}

