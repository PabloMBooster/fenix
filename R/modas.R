modas <- function(x, Lmin, Lmax, dL, umbral = 10){
  moda=NULL
  for(i in 1:length(x[,1])){
    vector <- as.numeric(x[i,])
    if(table(vector)[which(max(vector) == names(table(vector)))] >1){
      vector[which.max(vector)] = max(max(vector)) + 0.001
    }
    
    moda0  <- which(diff(sign(diff(vector)))==-2)+1
    if(length(vector[moda0]) > 1){
      for(i in 1:length(vector[moda0])){
        if(vector[moda0[i]] < umbral){
          moda0[i] = NA
        }
      }
    }
    moda0  <- moda0[!is.na(moda0)]
    Lmoda0 <- seq(Lmin,Lmax,by=dL)[moda0]
    Lmoda0 <- Lmoda0[order(vector[moda0], decreasing=TRUE)]
    Lmoda0 <- c(Lmoda0,rep(NA,10-length(moda0)))
    moda   <- rbind(moda,Lmoda0) 
  }
  row.names(moda) <- NULL
  return(moda)
} 