# estima.modas <-  function(tallas,Lmin,Lmax,dL){
#   moda=NULL
#   for(i in 1:length(tallas[,1])){
#     vector <- as.numeric(tallas[i,])
#     moda0 <- which(diff(sign(diff(vector)))==-2)+1
#     Lmoda0 <- seq(Lmin,Lmax,by=dL)[moda0]
#     Lmoda0 <- Lmoda0[order(vector[moda0], decreasing=TRUE)]
#     Lmoda0 <- c(Lmoda0,rep(NA,10-length(moda0)))
#     moda <- rbind(moda,Lmoda0) 
#   }  
#   return(moda)
# }

