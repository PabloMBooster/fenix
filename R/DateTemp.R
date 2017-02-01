
# tmpAnchNCS --------------------------------------------------------------
DateTemp = function(x){
  #require(fenix)
  tmpAnchNC$temp_1_i <- as.Date(tmpAnchNC$temp_1_i, format ="%d/%m/%Y") 
  tmpAnchNC$temp_1_f <- as.Date(tmpAnchNC$temp_1_f, format ="%d/%m/%Y") 
  tmpAnchNC$temp_2_i <- as.Date(tmpAnchNC$temp_2_i, format ="%d/%m/%Y") 
  tmpAnchNC$temp_2_f <- as.Date(tmpAnchNC$temp_2_f, format ="%d/%m/%Y") 
  
  id.temp = rep(NA, length(x))
  for(i in 1:length(tmpAnchNC$Anho)){
    if(tmpAnchNC$Anho[i] == 2014){
        id.temp[x >= tmpAnchNC$temp_1_i[i] & x <= tmpAnchNC$temp_1_f[i]] =   paste(tmpAnchNC$Anho[i],"_I", sep = "")
    }else{
      id.temp[x >= tmpAnchNC$temp_1_i[i] & x <= tmpAnchNC$temp_1_f[i]] =   paste(tmpAnchNC$Anho[i],"_I", sep = "")
      id.temp[x >= tmpAnchNC$temp_2_i[i] & x <= tmpAnchNC$temp_2_f[i]] =   paste(tmpAnchNC$Anho[i],"_II", sep = "")
    }
  }
  return(id.temp) 
}



# tmpAnchNC <- read.csv("F:/Datos Bitacoras/tmpAnchNC.csv", sep = ";")
# save(tmpAnchNC, file = "F:/github/fenix/data/tmpAnchNC.RData")
