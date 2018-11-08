variograma <- function(data.geo, max.dist, intervalo, modelo, ...){  
  vario <- variog(data.geo, max.dist=max.dist, uvec=seq(0,max.dist,intervalo))
  
  meseta <- max(vario$v)
  rango  <- max.dist/3
  par <- c( meseta,rango)
  
  vario.model <- variofit(vario, ini.cov.pars = par, cov.model =  modelo)
  #  s100.xv.ml  <- xvalid(data.geo, model = vario.model)
  
  #  par(mfrow = c(1,2))
  plot(vario, main = modelo) 
  lines(vario.model, col = 2, lwd = 2)  
  #   plot(s100.xv.ml)  
  return(vario.model)
}