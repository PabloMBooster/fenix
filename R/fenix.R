
estima.cala <- function (xvec)
{
  xvec <- as.matrix(xvec)
  xmatrix <- matrix(NA,nrow=length(xvec),ncol=1)
  for(i in 2:length(xvec))
  {
    if (xvec[i]>xvec[i-1]){
      xmatrix[length(1:i)-(xvec[i]):length(xvec[i])+1] = rep(xvec[i],xvec[i])
    }
    if (xvec[i-1]==xvec[i]){
      xmatrix[i-1] = 1     
    }
    #     print(xmatrix)
  }
  xmatrix <- as.matrix(xmatrix)
  return(xmatrix)
}
#

extrae.numero <- function(x)
{
  n <- nchar(x)
  ext_num <- substring(x, 1:n, 1:n)
  numbers <- 0:9
  num <- NULL
  for (i in ext_num){
    if (i %in% numbers){
      num <- paste(num,i,sep="")
    }
  }
  num <- as.numeric(num)
  return(num)
}
#

distAB <- function(lonA,latA,lonB,latB){
  #   if (!is.na(lonA)&!is.na(latA)&!is.na(lonB)&!is.na(latB)){
  distAB <- acos(signif(sin(latA*pi/180)*sin(latB*pi/180)+cos(latA*pi/180)*cos(latB*pi/180)*cos((lonA-lonB)*pi/180),16))*60/(pi/180)
  # error de redondeo!!!
  #   } else {
  #     distAB <- NA
  #   }
  return(distAB)
}

fill.viaje <- function(x){
  viaje <- rev(x)
  for(i in 2:length(viaje))
  {
    if(is.na(viaje[i]))
    {
      viaje[i] = viaje[i-1]
      #bitacora1$tipog[i] <- bitacora1$tipog[i-1] 
    }
  }
  return(rev(viaje))
}
#

estima.moda <-  function(tallas)
{
  moda=NULL
  
  for(i in 1:length(tallas[,1]))
    
  {
    moda0 <- tallas[i,]
    moda0 <- which.max(moda0)  
    moda <- rbind(moda,moda0)    
  }
  Lmoda <- moda
  ##    
  Lmoda[Lmoda==1]=5
  Lmoda[Lmoda==2]=5.5
  Lmoda[Lmoda==3]=6
  Lmoda[Lmoda==4]=6.5
  Lmoda[Lmoda==5]=7
  Lmoda[Lmoda==6]=7.5
  Lmoda[Lmoda==7]=8
  Lmoda[Lmoda==8]=8.5
  Lmoda[Lmoda==9]=9
  Lmoda[Lmoda==10]=9.5
  Lmoda[Lmoda==11]=10
  Lmoda[Lmoda==12]=10.5
  Lmoda[Lmoda==13]=11
  Lmoda[Lmoda==14]=11.5
  Lmoda[Lmoda==15]=12
  Lmoda[Lmoda==16]=12.5
  Lmoda[Lmoda==17]=13
  Lmoda[Lmoda==18]=13.5
  Lmoda[Lmoda==19]=14
  Lmoda[Lmoda==20]=14.5
  Lmoda[Lmoda==21]=15
  Lmoda[Lmoda==22]=15.5
  Lmoda[Lmoda==23]=16
  Lmoda[Lmoda==24]=16.5
  Lmoda[Lmoda==25]=17
  Lmoda[Lmoda==26]=17.5
  Lmoda[Lmoda==27]=18
  Lmoda[Lmoda==28]=18.5
  Lmoda[Lmoda==29]=19
  Lmoda[Lmoda==30]=19.5
  Lmoda[Lmoda==31]=20  
  
  Lmoda <- as.vector(Lmoda)
  return(Lmoda)
}

estima.dc <- function(x,y){
  
  x1=shoreline$V1
  y1=shoreline$V2
  
  A3=y1*pi/180;B3=x1*pi/180;A31=y*pi/180;B31=x*pi/180
  num2=length(x1)-1
  n=length(x)
  dfinal=NULL
  num=length(y)
  
  pos.na=c(which(is.na(y)==TRUE),which(is.na(x)==TRUE))
  pos.na
  dc <- NULL
  
  num2=length(x1)-1
  n=length(x)
  dfinal=NULL
  
  num=length(y)
  
  pos.na=c(which(is.na(y)==TRUE),which(is.na(x)==TRUE))
  pos.na
  dc <- NULL
  
  for(i in 1:num){
    A=NULL
    B=NULL
    
    A=sin(A3[1:num2]) * sin(A31[i])
    B=cos(A3[1:num2]) * cos(A31[i])*cos(B3[1:num2]-B31[i])
    grad=acos(A+B)
    gradinrad=180*grad/pi
    
    distancia=(60*gradinrad)
    indnonan=which(distancia>0)
    dist_min=min(distancia[indnonan])
    
    dfinal[i]=(dist_min)
    dc <- rbind(dc,dist_min)
  }
  dc[is.infinite(dc)] <- NA
  dc <- as.numeric(dc)
  return(dc)
}

filtrar.filas <- function(data,variable,filtro)
{
  data = data
  variable = variable
  filtro = filtro
  
  frecuencia <- as.numeric(table(variable))
  names.variable <- as.numeric(dimnames(table(variable))[[1]])
  tabla <- data.frame(names.variable,frecuencia)
  datos.filtrados <- tabla[which(tabla$frecuencia > filtro),"names.variable"]
  filas <- which(variable %in% datos.filtrados)
  data2 <- data[filas,]
  return(list(data2=data2,filas=filas))
}

lineSMOOTH2 <- function (x,y,spar)
{
  factorSpline <- 10
  spar = spar
  x <- x1 <- x
  y <- y2 <- y
  index <- is.na(y)
  x <- x[!index]
  y <- y[!index]
  
  modSpline <- smooth.spline(x = x, y = y, spar = spar)
  predictValues <- predict(modSpline, seq(min(x), max(x, na.rm = TRUE)
                                          , length.out = length(x)*factorSpline))  
  return(predictValues)
}

lineSMOOTH3 <- function (x,y,spar,factorSpline)
{
  factorSpline =factorSpline
  spar = spar
  x <- x1 <- x
  y <- y2 <- y
  index <- is.na(y)
  x <- x[!index]
  y <- y[!index]
  
  modSpline <- smooth.spline(x = x, y = y, spar = spar)
  predictValues <- predict(modSpline, seq(min(x), max(x, na.rm = TRUE)
                                          , length.out = length(x)*factorSpline))  
  return(predictValues)
}

remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}


puntos.tierra <- function(x,y){
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

sd.median <-function(x){
  x <- as.numeric(na.omit(x))
  out <- 1/n*sum(abs(x - median(x)))
  return(out)
}

CALfreqPonderada <- function(tallas,frecuencia,a,b,captura){
  
  peso <- (a*tallas^b)*frecuencia
  freqPonderada <- (captura/sum(peso))*peso
  tallasPoderadas <- freqPonderada/(a*tallas^b)
  return(tallasPoderadas)
}

CALtallaMedia <- function(tallas, tallasPoderadas){
  talla <- tallas
  tallaMedia <- 1/n*sum(talla*tallasPoderadas)/sum(tallasPoderadas)
  return(tallaMedia)
}

distancia <- function (x, y, x1, y1) 
{
  A3 = y1 * pi/180
  B3 = x1 * pi/180
  A31 = y * pi/180
  B31 = x * pi/180
  num2 = length(x1) - 1
  n = length(x)
  dfinal = NULL
  num = length(y)
  pos.na = c(which(is.na(y) == TRUE), which(is.na(x) == TRUE))
  pos.na
  dc <- NULL
  num2 = length(x1) - 1
  n = length(x)
  dfinal = NULL
  num = length(y)
  pos.na = c(which(is.na(y) == TRUE), which(is.na(x) == TRUE))
  pos.na
  dc <- NULL
  for (i in 1:num) {
    A = NULL
    B = NULL
    A = sin(A3[1:num2]) * sin(A31[i])
    B = cos(A3[1:num2]) * cos(A31[i]) * cos(B3[1:num2] - 
                                              B31[i])
    grad = acos(A + B)
    gradinrad = 180 * grad/pi
    distancia = (60 * gradinrad)
    indnonan = which(distancia > 0)
    dist_min = min(distancia[indnonan])
    dfinal[i] = (dist_min)
    dc <- rbind(dc, dist_min)
  }
  dc[is.infinite(dc)] <- NA
  return(dc)
}



