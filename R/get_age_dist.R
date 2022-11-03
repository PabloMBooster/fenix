# 
# alk_jurel = read.csv("C:/pablo/D/trabajos/sarda_chilensis/Tallas/data/alk_jurel.csv")
# freq_jurel = read.csv("C:/pablo/D/trabajos/sarda_chilensis/Tallas/data/freq_jurel.csv")
# 
# ALK = alk_jurel[,-1]
# FREQ = freq_jurel[,-1]

get_age_dist <- function(ALK = ALK, FREQ = FREQ, AGE = 0:12){
  
  PROP = ALK/apply(ALK,1, sum)
  PROP[is.na(PROP)] = 0
  
  datAGE = data.frame(AGE = AGE) 
  for(i in 1:ncol(FREQ)){
    OUT = matrix(FREQ[,i], nrow = 1)%*%as.matrix(PROP)
    datAGE = cbind(datAGE, t(OUT))
  }
  names(datAGE) = c("age", names(FREQ))
  row.names(datAGE) = NULL
  return(datAGE)
}

#output = get_age_dist(ALK, FREQ, AGE = 0:12)
