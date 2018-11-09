rangeWeek <- function(date, week){
  
  monthE <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Set", "Oct", "Nov", "Dic")
  rdate <- tapply(date, week, range)
  labelSem <- NULL
  
  for(i in 1:length(rdate)){
    mi = as.numeric(substring(rdate[[i]][1], 6,7))
    mf = as.numeric(substring(rdate[[i]][2], 6,7))
    if(mi %in% mf){
      rangeSem <- paste0(paste(substring(rdate[[i]][1], 9,10), substring(rdate[[i]][2], 9,10), sep = "-"), monthE[mi])
    }else{
      rangeSem <- paste(paste0(substring(rdate[[i]][1], 9,10), monthE[mi]), paste0(substring(rdate[[i]][2], 9,10), monthE[mf]), sep = "-")
    }
    labelSem = rbind(labelSem, rangeSem)
  }
  labelSem <- as.vector(labelSem)
  return(labelSem)
}
