get_week <- function(date){
  
  week <- rep(NA, length(date))
  data_week = .estimate_week(inicio.temp = min(date), fin.temp = max(date))
  for(i in data_week$daySeason){
    week[date == i] = data_week[data_week$daySeason == i, "week"]
  }
  week <- week + 1
  return(week)
}