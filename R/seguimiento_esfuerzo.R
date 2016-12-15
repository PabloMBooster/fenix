
# seguimiento -------------------------------------------------------------

uniqueNames <- function(data, ...){
  library(gsubfn)
  unwanted_array = list(    'S'='S', 's'='s', 'Z'='Z', 'z'='z', 'À'='A', 'Á'='A', 'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E',
                            'Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N', 'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U',
                            'Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='Ss', 'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c',
                            'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o',
                            'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 'û'='u', 'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y', 'ú'='u', 'í'='i' )
  out <- names(data)
  for(i in seq_along(unwanted_array)){
    out <- gsub(names(unwanted_array)[i],unwanted_array[i],out)
  }
  names(data) <- out
  names(data) <- toupper(names(data))
  
  return(data)
}


numberOfDays <- function(date) {
  m <- format(date, format="%m")
  
  while (format(date, format="%m") == m) {
    date <- date + 1
  }
  return(as.integer(format(date - 1, format="%d")))
}

#
FUNPorDia <- function(data = data, puerto = puerto, nDays = nDays,...){
  #out = list()
  tablaCaptDay = data.frame(matrix(NA, nrow = nDays, ncol = 4))
  names(tablaCaptDay) = c("Dia", "capt", "C/P", "S/P")
  
  tablaCaptDay$Dia = 1:nDays
  
  data        = data[data$PUERTO %in% puerto,]
  if(dim(data)[1]>0){
    captDays    = tapply(data$ANCHOVETA, data$DIA, sum, na.rm = TRUE)
    VesselDays  = table(data$CODIGO, data$DIA) 
    nVesselDays = apply(VesselDays, 2, sum)
    tablaCaptDay$capt[tablaCaptDay$Dia %in% names(captDays)] <- as.numeric(captDays)
    tablaCaptDay[tablaCaptDay$Dia %in% names(nVesselDays),"C/P"] <- as.numeric(nVesselDays)
  }
  #DiasConPesca = length(tablaCaptDay$capt[!is.na(tablaCaptDay$capt)])
  #out$tablaCaptDay = tablaCaptDay
  #out$DiasConPesca = DiasConPesca
  
  return(tablaCaptDay)
} 

#


dataPorDia <- function(data, puerto, nDays = nDays, subName = subName, ...){
  
  PorDia = data.frame(matrix(NA, nrow = nDays+1, ncol = (length(puerto)+1)))
  #total = c(paste("TOTAL",region),"","")
  names(PorDia) = c("DIA", puerto)
  PorDia$DIA = c(0, 1:nDays)
  puertoN = puerto[puerto != ""] 
  
  if(dim(data)[1]>0){
    for(j in 1:(length(puertoN)-1)){
      captPuerto  <- FUNPorDia(IND, puertoN[j], nDays = nDays)    
      #captPuerto  <- captPuerto$tablaCaptDay
      plus0 = seq(1,(length(subName)-2), by = 3 )
      PorDia[2:(nDays+1),(plus0[i]+1):(plus0[i]+3)] <- as.matrix(captPuerto[,-1])
    }
    plus = plus0[-length(plus0)]
    PorDia[-1,plus0[length(plus0)]+1]   <- apply(PorDia[-1,(plus+1)],1,sum,na.rm = T)# 
    PorDia[-1,plus0[length(plus0)]+2] <- apply(PorDia[-1,(plus+2)],1,sum,na.rm = T)
    PorDia[-1,plus0[length(plus0)]+3] <- apply(PorDia[-1,(plus+3)],1,sum,na.rm = T)
    
    PorDia[is.na(PorDia)] <- ""
    PorDia[1,-1] <- subName
    PorDia[PorDia == 0] <- ""
  }
  
  else{
    PorDia[is.na(PorDia)] <- ""
    PorDia[1,-1] <- subName
  }
  #out$PorDia       <- PorDia
  #out$DiasConPesca <- captPuerto$DiasConPesca
  return(PorDia)
}

#
calDia <- function(data, anho, mes){
  
  outcalDia = list()
  date = as.Date(paste(anho,"-",mes,"-",10, sep = "", "%Y-%m-%d"))
  nDays = numberOfDays(date)
  
  puertosNorte = c("PAITA","","","PARACHIQUE-BAYOVAR","","","CHICAMA","","","SALAVERRY","","","CHIMBOTE-COISHCO","","","SAMANCO","","","CASMA","","","TOTAL_N","","") # "TOTAL NORTE"
  puertosCentro = c("HUARMEY","","","SUPE","","","VEGUETA","","","HUACHO","","","CHANCAY","","","CALLAO","","","T. MORA","","","PISCO","","","TOTAL_C","","") # "TOTAL CENTRO"	
  puertosSur = c("ATICO","","","PLANCHADA","","","QUILCA","","","MOLLENDO","","","ILO","","","TOTAL_S","","") # "TOTAL SUR" "TOTAL PERU"
  
  subNameNorte = rep(c("capt", "C/P","S/P"), times = 3, length = length(puertosNorte))
  subNameCentro = rep(c("capt", "C/P","S/P"), times = 3, length = length(puertosCentro))
  subNameSur = rep(c("capt", "C/P","S/P"), times = 3, length = length(puertosSur))
  
  #data$Puerto = toupper(data$PUERTO)
  IND = data[data$TIPO %in% c("IND", "Ind", "ind"),]
  INDMAD = data[data$TIPO %in% c("IND MAD", "Ind Mad", "ind mad", "indmad", "INDMAD", "IND.MAD"),]
  
  # if(puertosNorte[1] == "PAITA") ix = "NORTE"
  # if(puertosCentro[1] == "HUARMEY") ix = "CENTRO"
  # if(puertosSur[1] == "ATICO") ix = "SUE"
  
  PorDiaIndN <- dataPorDia(data = IND, puerto = puertosNorte, nDays = nDays, subName = subNameNorte)
  PorDiaIndC <- dataPorDia(data = IND, puerto = puertosCentro, nDays = nDays, subName = subNameCentro)
  PorDiaIndS <- dataPorDia(data = IND, puerto = puertosSur, nDays = nDays, subName = subNameSur)
  
  PorDiaIndMadN <- dataPorDia(data = INDMAD, puerto = puertosNorte, nDays = nDays, subName = subNameNorte)
  PorDiaIndMadC <- dataPorDia(data = INDMAD, puerto = puertosCentro, nDays = nDays, subName = subNameCentro)
  PorDiaIndMadS <- dataPorDia(data = INDMAD, puerto = puertosSur, nDays = nDays, subName = subNameSur)
  
  IndPorDia    = data.frame(cbind(PorDiaIndN, PorDiaIndC, PorDiaIndS))
  IndMadPorDia = data.frame(cbind(PorDiaIndMadN, PorDiaIndMadC, PorDiaIndMadS))
  
  base = grep(pattern =  "Var.", x = names(IndPorDia), value = TRUE)
  ub   =  as.numeric(unlist(lapply(strsplit(base, split = "Var."), function(xvect) return(xvect[2]))))
  #names(IndPorDia)
  
  names(IndPorDia)[ub] <- ""
  names(IndMadPorDia)[ub] <- ""
  
  outcalDia$IndPorDia    <- IndPorDia
  outcalDia$IndMadPorDia <- IndMadPorDia
  return(outcalDia)
}

# calcula los dias con pesca
DiaConPesca = function(outPordia, ...){
  
  out = list()
  DiaConPescaInd = list()
  DiaConPescaIndMad = list()
  
  DiaConPescaInd$Norte  = data.frame(d = length(outPordia$IndPorDia[-1,23][outPordia$IndPorDia[-1,23] != ""])); row.names(DiaConPescaInd$Norte) <- "Dias con pesca"
  DiaConPescaInd$Centro = data.frame(d = length(outPordia$IndPorDia[-1,51][outPordia$IndPorDia[-1,51] != ""])); row.names(DiaConPescaInd$Centro) <- "Dias con pesca"
  DiaConPescaInd$Sur    = data.frame(d = length(outPordia$IndPorDia[-1,70][outPordia$IndPorDia[-1,70] != ""])); row.names(DiaConPescaInd$Sur) <- "Dias con pesca"
  
  DiaConPescaIndMad$Norte  = data.frame(d = length(outPordia$IndMadPorDia[-1,23][outPordia$IndMadPorDia[-1,23] != ""])); row.names(DiaConPescaIndMad$Norte) <- "Dias con pesca"
  DiaConPescaIndMad$Centro = data.frame(d = length(outPordia$IndMadPorDia[-1,51][outPordia$IndMadPorDia[-1,51] != ""])); row.names(DiaConPescaIndMad$Centro) <- "Dias con pesca"
  DiaConPescaIndMad$Sur    = data.frame(d = length(outPordia$IndMadPorDia[-1,70][outPordia$IndMadPorDia[-1,70] != ""])); row.names(DiaConPescaIndMad$Sur) <- "Dias con pesca"
  
  out$DiaConPescaInd    = DiaConPescaInd
  out$DiaConPescaIndMad = DiaConPescaIndMad
  
  return(out)
}

# tabla captura rangos de barcos
calculaTabla <- function(data, ...){
  
  nombreTabla    <- c("", "VIAJES", "", "CAPTURA",	"CAPAC. DE BODEGA", "",	"REG. BRUTO", "ACUM")	
  subNombre      <- c("CAP. BOD.",	"C/P",	"S/P", "(tons)",	"C/P",	"S/P",	"C/P",	"S/P", "N° EMB")
  CBrangosIndMad <- c("30-50", "51-100", "101-110")
  CBrangosInd    <- c("30-100", "101-200", "201-300", "301-400", "401-900")
  
  INDMAD = data[data$TIPO %in% "IND MAD",]
  IND    = data[data$TIPO %in% "IND",]
  
  # falta incluir un for por region y luego pegar las tablas
  region = c("NORTE", "CENTRO", "SUR")
  
  outTablaIndMad = list(3)
  outTablaInd = list(3)
  out = list()
  
  for(j in seq_along(region)){
    
    nombreTabla[1]     <- region[j]
    TablaIndMad        <- data.frame(matrix(NA, ncol = 9, nrow = 5))
    names(TablaIndMad) <- nombreTabla
    TablaIndMad[1,]    <- subNombre
    TablaIndMad[2:4,1] <- CBrangosIndMad
    TablaIndMad[5,] <- c("Total","","","","","","","","")
    
    INDMAD.j = INDMAD[INDMAD$REG %in% region[j],]
    
    if(dim(INDMAD.j)[1] > 0){
      b1 = INDMAD.j[INDMAD.j$CB >= 30  & INDMAD.j$CB < 51, ]
      b2 = INDMAD.j[INDMAD.j$CB >= 51  & INDMAD.j$CB < 101,]
      b3 = INDMAD.j[INDMAD.j$CB >= 101,]
      
      nViajesIndMad   <- rbind(sum(table(b1$CODIGO)),sum(table(b2$CODIGO)),sum(table(b3$CODIGO)))
      
      sPescaIndMad    <- rbind(sum(table(b1$S.P)),sum(table(b2$S.P)),sum(table(b3$S.P)))
      
      captIndMad      <- rbind(sum(b1$ANCHOVETA,na.rm = T),sum(b2$ANCHOVETA,na.rm = T),
                               sum(b3$ANCHOVETA,na.rm = T))
      
      captCBIndMad    <- rbind(sum(tapply(b1$CB, b1$CODIGO, max, na.rm = TRUE)*table(b1$CODIGO)),
                               sum(tapply(b2$CB, b2$CODIGO, max, na.rm = TRUE)*table(b2$CODIGO)),
                               sum(tapply(b3$CB, b3$CODIGO, max, na.rm = TRUE)*table(b3$CODIGO)))
      
      nEmbarIndMad    <- rbind(length(unique(b1$CODIGO)),length(unique(b2$CODIGO)),
                               length(unique(b3$CODIGO)))
      TablaIndMadTotal<- c("Total",sum(nViajesIndMad),sum(sPescaIndMad),sum(captIndMad),
                           sum(captCBIndMad),"","","",sum(nEmbarIndMad))
      
      TablaIndMad[-c(1,5),2] <- nViajesIndMad
      TablaIndMad[-c(1,5),3] <- sPescaIndMad
      TablaIndMad[-c(1,5),4] <- captIndMad
      TablaIndMad[-c(1,5),5] <- captCBIndMad
      TablaIndMad[-c(1,5),9] <- nEmbarIndMad
      TablaIndMad[5,]   <- TablaIndMadTotal
    } 
    TablaIndMad[is.na(TablaIndMad)] <- ""
    outTablaIndMad[[j]] = TablaIndMad  
  }  
  
  for(j in seq_along(region)){
    TablaInd        <- data.frame(matrix(NA, ncol = 9, nrow = 7))
    nombreTabla[1]  <- region[j]
    names(TablaInd) <- nombreTabla
    TablaInd[1,]    <- subNombre
    TablaInd[2:6,1] <- CBrangosInd 
    TablaInd[7,]    <- c("Total","","","","","","","","")
    
    IND.j = IND[IND$REG %in% region[j],]
    
    
    if(dim(IND.j)[1] > 0){
      
      a1 = IND.j[IND.j$CB >= 30  & IND.j$CB < 101,]
      a2 = IND.j[IND.j$CB >= 101 & IND.j$CB < 201,]
      a3 = IND.j[IND.j$CB >= 201 & IND.j$CB < 301,]
      a4 = IND.j[IND.j$CB >= 301 & IND.j$CB < 401,]
      a5 = IND.j[IND.j$CB >= 401,]
      
      nViajesInd   <- rbind(sum(table(a1$CODIGO)),sum(table(a2$CODIGO)),sum(table(a3$CODIGO)),
                            sum(table(a4$CODIGO)),sum(table(a5$CODIGO)))
      
      sPescaInd    <- rbind(sum(table(a1$S.P)),sum(table(a2$S.P)),sum(table(a3$S.P)),
                            sum(table(a4$S.P)),sum(table(a5$S.P)))
      
      captInd      <- rbind(sum(a1$ANCHOVETA,na.rm = T),sum(a2$ANCHOVETA,na.rm = T),
                            sum(a3$ANCHOVETA,na.rm = T),sum(a4$ANCHOVETA,na.rm = T),
                            sum(a5$ANCHOVETA,na.rm = T))
      
      captCBInd    <- rbind(sum(tapply(a1$CB, a1$CODIGO, max, na.rm = TRUE)*table(a1$CODIGO)),
                            sum(tapply(a2$CB, a2$CODIGO, max, na.rm = TRUE)*table(a2$CODIGO)),
                            sum(tapply(a3$CB, a3$CODIGO, max, na.rm = TRUE)*table(a3$CODIGO)),
                            sum(tapply(a4$CB, a4$CODIGO, max, na.rm = TRUE)*table(a4$CODIGO)),
                            sum(tapply(a5$CB, a5$CODIGO, max, na.rm = TRUE)*table(a5$CODIGO)))
      
      nEmbarInd    <- rbind(length(unique(a1$CODIGO)),length(unique(a2$CODIGO)),
                            length(unique(a3$CODIGO)),length(unique(a4$CODIGO)),
                            length(unique(a5$CODIGO)))
      TablaIndTotal<- c("Total",sum(nViajesInd),sum(sPescaInd),sum(captInd),
                        sum(captCBInd),"","","",sum(nEmbarInd))
      
      TablaInd[-c(1,7),2] <- as.character(nViajesInd)
      TablaInd[-c(1,7),3] <- as.character(sPescaInd)
      TablaInd[-c(1,7),4] <- as.character(captInd)
      TablaInd[-c(1,7),5] <- as.character(captCBInd)
      TablaInd[-c(1,7),9] <- as.character(nEmbarInd)
      TablaInd[7,]   <- TablaIndTotal
    } 
    TablaInd[is.na(TablaInd)] <- ""
    outTablaInd[[j]]     <- TablaInd
  }
  out$IndMad  <- outTablaIndMad
  out$Ind     <- outTablaInd
  
  return(out)
}


reporteEsfuerzo = function(TablaMes, DConPesca, CapturaDia, output,
                           file, ...){
  
  # TablaMes es un lista
  # DconPesca es un lista
  # CapturaDia es un lista
  # output directorio donde se guarda el reporte
  # file nombre del reporte
  
  library(openxlsx)
  name1 = "Instituto del Mar del Perú"
  name2 = "Unidad de Dinámica de Poblaciones"
  
  wb <- createWorkbook()
  
  options("openxlsx.borderStyle" = "thin")
  options("openxlsx.borderColour" = "#4F81BD")
  
  ## Add worksheets
  addWorksheet(wb, "TOTAL")
  addWorksheet(wb, "Tabla Ind")
  addWorksheet(wb, "Por dia Ind")
  addWorksheet(wb, "IND")
  addWorksheet(wb, "Por dia Ind Mad")
  addWorksheet(wb, "Tabla Ind Mad")
  addWorksheet(wb, "IND MAD")
  
  hs2 <- createStyle(fontColour = "#ffffff", fgFill = "#4F80BD",
                     halign = "center", valign = "center", textDecoration = "Bold",
                     border = "TopBottomLeftRight")
  
  writeData(wb, "TOTAL", data, startCol = 1, startRow = 1, rowNames = FALSE)
  
  writeData(wb, "Tabla Ind", TablaMes$Ind[[1]], startCol = 2, startRow = 7, rowNames = FALSE)
  writeData(wb, "Tabla Ind", DConPesca$DiaConPescaInd[[1]], startCol = 2, startRow = 15, rowNames = TRUE, colNames = FALSE)
  
  
  writeData(wb, "Tabla Ind", TablaMes$Ind[[2]], startCol = 2, startRow = 7+10, rowNames = FALSE)
  writeData(wb, "Tabla Ind", DConPesca$DiaConPescaInd[[1]], startCol = 2, startRow = 25, rowNames = TRUE, colNames = FALSE)
  
  writeData(wb, "Tabla Ind", TablaMes$Ind[[3]], startCol = 2, startRow = 7+10+10, rowNames = FALSE)
  writeData(wb, "Tabla Ind", DConPesca$DiaConPescaInd[[1]], startCol = 2, startRow = 35, rowNames = TRUE, colNames = FALSE)
  
  writeData(wb, "Tabla Ind", name1, startCol = 1, startRow = 1, rowNames = FALSE)
  writeData(wb, "Tabla Ind", name2, startCol = 1, startRow = 2, rowNames = FALSE)
  
  writeData(wb, "Por dia Ind", CapturaDia$IndPorDia, startCol = 2, startRow = 5, rowNames = FALSE)
  writeData(wb, "Por dia Ind", name1, startCol = 1, startRow = 1, rowNames = FALSE)
  writeData(wb, "Por dia Ind", name2, startCol = 1, startRow = 2, rowNames = FALSE)
  
  writeData(wb, "IND", IND, startCol = 1, startRow = 1, rowNames = FALSE)
  
  writeData(wb, "Tabla Ind Mad", TablaMes$IndMad[[1]], startCol = 2, startRow = 7, rowNames = FALSE)
  writeData(wb, "Tabla Ind Mad", DConPesca$DiaConPescaIndMad[[1]], startCol = 2, startRow = 13, rowNames = TRUE, colNames = FALSE)
  
  writeData(wb, "Tabla Ind Mad", TablaMes$IndMad[[2]], startCol = 2, startRow = 7+10, rowNames = FALSE)
  writeData(wb, "Tabla Ind Mad", DConPesca$DiaConPescaIndMad[[2]], startCol = 2, startRow = 23, rowNames = TRUE, colNames = FALSE)
  
  writeData(wb, "Tabla Ind Mad", TablaMes$IndMad[[3]], startCol = 2, startRow = 7+10+10, rowNames = FALSE)
  writeData(wb, "Tabla Ind Mad", DConPesca$DiaConPescaIndMad[[3]], startCol = 2, startRow = 33, rowNames = TRUE, colNames = FALSE)
  
  writeData(wb, "Tabla Ind Mad", name1, startCol = 1, startRow = 1, rowNames = FALSE)
  writeData(wb, "Tabla Ind Mad", name2, startCol = 1, startRow = 2, rowNames = FALSE)
  
  
  writeData(wb, "Por dia Ind Mad", CapturaDia$IndMadPorDia, startCol = 2, startRow = 5, rowNames = FALSE)
  writeData(wb, "Por dia Ind Mad", name1, startCol = 1, startRow = 1, rowNames = FALSE)
  writeData(wb, "Por dia Ind Mad", name2, startCol = 1, startRow = 2, rowNames = FALSE)
  
  writeData(wb, "IND MAD", INDMAD, startCol = 1, startRow = 1, rowNames  = FALSE)
  
  
  saveWorkbook(wb, paste(output,"/",file, sep = ""), overwrite = TRUE)
}
