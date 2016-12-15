findLastPos = function(vector){
  
  out = NULL
  
  if(length(vector) == 1) { out = vector
  } else {
    for(i in 2:length(vector)){
      rest = vector[i] - vector[i-1]
      if(rest > 1) { 
        temp = vector[i-1]
        out = c(out, temp)
      }
    }
    out = c(out, vector[length(vector)])
  }
  return(out)
}


numericDay = function(vector){
  
  out = numeric(length(vector))
  for(i in seq_along(vector)){
    if(vector[i] == "lunes") out[i] = 1
    if(vector[i] == "martes") out[i] = 2
    if(vector[i] == "miércoles") out[i] = 3
    if(vector[i] == "jueves") out[i] = 4
    if(vector[i] == "viernes") out[i] = 5
    if(vector[i] == "sábado") out[i] = 6
    if(vector[i] == "domingo") out[i] = 7
  }
  
  return(out)
}


numericTime = function(vector, format = "%d/%m/%Y"){
  
  out = list()
  if(class(vector)[1] == "POSIXct" | class(vector)[1] == "POSIXlt"){
    tmp = vector
  } else {
    tmp = strptime(x = vector, format = format)
  }
  
  out$name_day = weekdays(tmp)
  out$number_day = numericDay(out$name_day)
  out$day = as.numeric(format(tmp, format = "%d"))
  out$month = as.numeric(format(tmp, format = "%m"))
  out$year = as.numeric(format(tmp, format = "%Y"))
  out$time = out$year + (out$month-1)/12 + (out$day-1)/365 
  
  return(out)
}


#  ------------------------------------------------------------------------

estimaSemana = function(data, date, write = FALSE, dateFormat = "%d/%m/%Y"){
  
  data00 = data
  data00[,c(date)] = as.character(data00[,c(date)])
  data00$date2 = strptime(x = data00[,c(date)], format = dateFormat)
  
  outp = numericTime(vector = data00$date2)
  
  data00$name_day = outp$name_day
  data00$number_day = outp$number_day
  data00$day = outp$day
  data00$month = outp$month
  data00$year = outp$year
  
  data00$time = outp$time
  
  data2 = data00[order(data00$time),]
  
  fday = data2$number_day[1]
  fdate = data2$date2[1]
  ndaystoDom = 7 - fday
  fDom = fdate + ndaystoDom*24*60*60
  
  lday = data2$number_day[length(data2$number_day)]
  ldate = data2$date2[length(data2$date2)]
  lDom = ldate - lday*24*60*60
  
  nsem = as.numeric(lDom - fDom)/7
  
  sundays1 = fDom + (1:nsem*7)*24*60*60
  
  sundays = c(fDom, sundays1)
  mondays = sundays + 1*24*60*60
  
  First = c(fdate, mondays)
  Last = rev(c(ldate, rev(sundays)))
  
  num_First = numericTime(First)$time
  num_Last = numericTime(Last)$time
  
  tmp_1day = numericTime(vector = First)$day
  tmp_1mon = numericTime(vector = First)$month
  tmp_2day = numericTime(vector = Last)$day
  tmp_2mon = numericTime(vector = Last)$month
  
  dateLabel = paste0(tmp_1day, "/", tmp_1mon, "-", tmp_2day, "/", tmp_2mon)
  
  data2$week = NA
  data2$label = NA
  for(i in seq_along(num_First)){
    j = as.character(i)
    if(nchar(j) == 1){
      data2$week[which(data2$time >= num_First[i] & data2$time <= num_Last[i])] = paste0("Sem 0", i)
    } else {
      data2$week[which(data2$time >= num_First[i] & data2$time <= num_Last[i])] = paste0("Sem ", i)
    }
    data2$label[which(data2$time >= num_First[i] & data2$time <= num_Last[i])] = dateLabel[i]
  }
  
  # data2$typeData = "catch"
  
  dataOut = data2[,c(date, "week", "label")]
  
  if(write) { write.csv(dataOut, "weekData.csv", row.names = FALSE) }
  
  return(dataOut)
  
}


#  ------------------------------------------------------------------------


