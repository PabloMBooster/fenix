puntos_tierra <- function(x, y){
  x=round(x,2)
  y=round(y,2)
  
  base.corr0 <- NULL
  for (i in 1:length(y)){
    
    base.corr1 <- shoreline[which(y[i] == round(shoreline$V2,2)),]
    base.corr2 <- matrix(base.corr1[1,],nrow=1)
    
    if (dim(base.corr1)[1] == 0){
      base.corr1 <- matrix(NA,nrow=1,ncol=2)
      base.corr2 <- base.corr1[1,]
    }
    
    base.corr2 <- c(base.corr2)
    base.corr0 <- rbind(base.corr0,base.corr2)
    dimnames(base.corr0) <- NULL 
    
  }
  base.corr00 <- data.frame(cbind(base.corr0,matrix(x,ncol=1))) 
  names(base.corr00) <- c("LonL","LatL","LonP") 
  
  base.corr00$distancia <- (as.numeric(base.corr00$LonP) - as.numeric(base.corr00$LonL))*-1
  
  return(base.corr00$distancia)
}
