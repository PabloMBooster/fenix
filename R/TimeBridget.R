TimeBridget <- function(trayecto,pesca,busqueda){
  
  por.trayecto <- round(trayecto/(trayecto+pesca+busqueda)*100,1)
  por.pesca    <- round(pesca/(trayecto+pesca+busqueda)*100,1)
  por.busqueda <- round(busqueda/(trayecto+pesca+busqueda)*100,1)
  
  out <- data.frame(cbind(por.trayecto, por.pesca,por.busqueda))
  return(out)
}