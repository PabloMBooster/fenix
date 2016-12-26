Bhattacharya = function(x, nModes){
  require(mixtools)
  
  x[is.na(x)] = 0
  marcas = seq(5, 20, by = 0.5)
  matriz = as.data.frame(cbind(marcas, x))
  outL = tryCatch({
    normalmixEM(rep(matriz$marcas, matriz$x), k = nModes, maxit = 5000)
  }, error=function(e) {
    message("Not Converge") #,file_name)
    outL <- NULL
    return(outL)
  })
  
  if(!is.null(outL)){
    population = sum(x)*outL$lambda
    tableBhatt = data.frame(Group = seq_along(outL$mu), Mean = outL$mu, Population = population)
    plot(outL, which = 2)
    box()
    out = list(tableBhatt = tableBhatt, outL = outL)
  }else{
    out = NULL
  }
  return(out)
}
