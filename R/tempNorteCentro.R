
<<<<<<< HEAD
#  tempNorteCentro --------------------------------------------------------

# 
# DateTemp <- function(x, temporada){
# 
#   require(fenix)
#   temporada$temp_1_i <- as.Date(temporada$temp_1_i, format ="%d/%m/%Y")
#   temporada$temp_1_f <- as.Date(temporada$temp_1_f, format ="%d/%m/%Y")
#   temporada$temp_2_i <- as.Date(temporada$temp_2_i, format ="%d/%m/%Y")
#   temporada$temp_2_f <- as.Date(temporada$temp_2_f, format ="%d/%m/%Y")
# 
#   id.temp <- rep(NA, length(x))
#   
#   for(i in 1:length(temporada$Años)){
#     if(temporada$Años[i] == 2014){
#       id.temp[x >= temporada$temp_1_i[i] & x <= temporada$temp_1_f[i]] =   paste(temporada$Años[i],".1", sep = "")
#     }else{
#       id.temp[x >= temporada$temp_1_i[i] & x <= temporada$temp_1_f[i]] =   paste(temporada$Años[i],".1", sep = "")
#       id.temp[x >= temporada$temp_2_i[i] & x <= temporada$temp_2_f[i]] =   paste(temporada$Años[i],".2", sep = "")
#     }
#   }
#   return(id.temp)
# 
# }
=======
#DateTemp <- function(x){
#  require(fenix)
#  temporada$temp_1_i <- as.Date(temporada$temp_1_i, format ="%d/%m/%Y") 
#  temporada$temp_1_f <- as.Date(temporada$temp_1_f, format ="%d/%m/%Y") 
#  temporada$temp_2_i <- as.Date(temporada$temp_2_i, format ="%d/%m/%Y") 
#  temporada$temp_2_f <- as.Date(temporada$temp_2_f, format ="%d/%m/%Y") 
  
#  id.temp = rep(NA, length(x))
#  for(i in 1:length(temporada$Años)){
#    if(temporada$Años[i] == 2014){
#      id.temp[x >= temporada$temp_1_i[i] & x <= temporada$temp_1_f[i]] =   paste(temporada$Años[i],".1", sep = "")
#    }else{
#      id.temp[x >= temporada$temp_1_i[i] & x <= temporada$temp_1_f[i]] =   paste(temporada$Años[i],".1", sep = "")
#      id.temp[x >= temporada$temp_2_i[i] & x <= temporada$temp_2_f[i]] =   paste(temporada$Años[i],".2", sep = "")
#    }
#  }
# return(id.temp) 
#}
>>>>>>> origin/master
