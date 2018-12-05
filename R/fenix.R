
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


CALfreqPonderada <- function(tallas,frecuencia,a,b,captura){
  
  peso <- (a*tallas^b)*frecuencia
  freqPonderada <- (captura/sum(peso))*peso
  tallasPoderadas <- freqPonderada/(a*tallas^b)
  return(tallasPoderadas)
}

weighted_frequency <- function(length,frequency,a,b,catch){
  weight  <- (a*length^b)*frequency
  freqW   <- (catch/sum(weight))*weight
  lengthW <- freqW/(a*length^b)
  return(lengthW)
}


CALtallaMedia <- function(tallas, tallasPoderadas){
  talla <- tallas
  tallaMedia <- 1/n*sum(talla*tallasPoderadas)/sum(tallasPoderadas)
  return(tallaMedia)
}

.estimate_week <- function(inicio.temp, fin.temp, ...){
    require(lubridate)
  
    daySeason = seq.Date(as.Date(inicio.temp), as.Date(fin.temp),by =  "day")
    day =  weekdays(daySeason)
    season = data.frame(daySeason = daySeason, day = day)
    nweek = c(rep(0, min(which(day == "lunes"))-1), sort(rep(1:length(day[day=="lunes"]),7)))
    season$week <- nweek[1:length(day)]  
    
    return(season)
}


