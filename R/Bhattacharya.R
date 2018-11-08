Bhattacharya = function(x, length_class = seq(5, 20, by = 0.5), groups){
  
  
  list.of.packages <- c("mixtools")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  
  library(mixtools)
  
  x[is.na(x)] = 0
  
  matriz = as.data.frame(cbind(length_class, x))
  outL = tryCatch({
    normalmixEM(rep(matriz$length_class, matriz$x), k = groups, maxit = 5000)
  }, error=function(e) {
    message("Not Converge") 
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
