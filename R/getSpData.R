.getSpData = function(dat2){
  
  if(length(which(is.na(dat2[, "REE_NPESESP"]))) > 0){
    dat = dat2[-which(is.na(dat2[, "REE_NPESESP"])), ]
  } else {
    dat = dat2
  }
  
  if(nrow(dat) == 0){
    tmp4 = NULL
  } else {
    tmp2 = by(data = dat[, "REE_NPESESP"], INDICES = dat[, "NOMBRE_COMERCIAL"], FUN = unique)
    tmp3 = sapply(X = tmp2, FUN = sum, na.rm = T)
    tmp4 = data.frame(lon = dat[1, "LONGITUD_INICIAL"], lat = dat[1, "LATITUD_INICIAL"], sp = names(tmp3),
                      capt = as.vector(tmp3))
  }
  return(tmp4)
  
}

