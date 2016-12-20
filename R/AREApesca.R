area.pesca <- function(dc,lat,grado) {  
    grado <- grado
    dc <- dc
    lat <- lat
    
    intervalos <- (-18.5+3)/(-grado) 
    
    y.tot<-NULL
    for (i in 1:intervalos)
    {
      y <- matrix(cbind(rep(-3-(grado)*(i-1),40),rep(-3-(grado)*i,40)),ncol=2)
      y.tot <- rbind(y.tot,y)
    }

    x.tot <- matrix(cbind(rep(seq(0,195,by=5),dim(y.tot)[1]/40),
                        rep(seq(5,200,by=5),dim(y.tot)[1]/40)),ncol=2)
  
    xy <- data.frame(cbind(x.tot,y.tot))
    names(xy)<-c("x1","x2","y1","y2")
    xy$num.area <- 1:dim(xy)[1]
  
    data <- data.frame(cbind(dc=dc,lat=lat))
    nombreAREA <- NULL
    
    for(j in 1:length(dc))
    {
      pos <- data[j,]
    
      xy$diffx <- NULL
      xy$diffy <- NULL
    
      xy$diffx = abs(pos$dc - xy$x1 + pos$dc - xy$x2)
      xy$diffy = abs(pos$lat - xy$y1 + pos$lat - xy$y2)
      xy$diffxy = xy$diffy + xy$diffx
      NombreArea <- xy[xy$diffxy==min(xy$diffxy),"num.area"]
      
      nombreAREA <- rbind(nombreAREA,NombreArea)
    } 
  dimnames(nombreAREA) <- NULL
  nombreAREA <- as.numeric(nombreAREA)
  return(nombreAREA)
}



