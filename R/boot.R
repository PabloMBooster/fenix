boot <-function(data,alfa,k){
  
  n <- sum(floor(table(data[,1])*alfa)) 
  m <- dim(data)[2]
  my <- unique(sort(data[,1,1]))
  nBoot <- array(NA,dim=c(n,m,k))
  
  for(sim in 1:k)
    {  
      base.boot2 <- NULL
        for (j in my)
          {
            base.tiempo <- data[data$tiempo==j,]
            base.tiempo <- base.tiempo[sample(floor(dim(base.tiempo)[1]*alfa),replace=T),]
      
            base.boot2 <- rbind(base.boot2,base.tiempo)      
          }
    
      base.boot2[,1] <- as.numeric(as.character(base.boot2[,1]))
      base.boot2[,2] <- as.numeric(as.character(base.boot2[,2]))    
      nBoot[,,sim] <- as.matrix(base.boot2)
    }  
    return(nBoot)
}
