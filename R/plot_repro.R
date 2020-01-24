plot_repro <- function(tiempo, indice, patron, sd_patron, valor_crucero = NULL, fecha = as.Date(time, format = "%d/%m/%Y"), 
                       nombre_indice = NULL, legend = TRUE, nombre_indice_legenda = NULL,  nombre_patron = NULL, plot_patron = TRUE,
                        nombre_crucero = NULL, axis_x = TRUE, dl = 3, col_index = 4, space_axis = 1){
  require(plotrix)
  x <- 1:length(indice)
  y <- patron 
  uwi <- sd_patron 
  lwi <- sd_patron 
  
  valor_min = 0
  valor_max = round(max(y)+max(uwi))+dl
  
  if(max(y) < 1){
    valor_max = 0.9
    valor_min = 0.5
  }
  
  if(is.null(nombre_indice)){
    nombre_indice = "nombre indice"
  }
  
  if(plot_patron == FALSE){
    col_bar  = "white"
    col_patron = "white"
  }else{
    col_bar  = "gray30"
    col_patron = "red"
  }
  plotCI(x, y,uwi,lwi, lwd = 1, slty = 1, ui = 2, scol = col_bar,
         xlab="", pt.bg=par("bg"), pch = ".",
         ylab=nombre_indice, ylim = c(valor_min, valor_max),
         axes=F)
  lines(x,y, col = col_patron, lwd = 2)
  lines(x,indice, col = col_index, lwd = 2)
  
  if(!is.null(valor_crucero)){
    points(length(tiempo),valor_crucero, col = 1,pch = 16)  
  }
 
  if(space_axis == 1){
    label_axis = substring(tiempo, 1,7)
    if(isTRUE(axis_x)){
      axis(1,at=1:length(label_axis),labels=label_axis,las=2,cex.axis=1,tcl = -0.2,hadj = 0.8)
      axis(1,at=1:length(label_axis),labels=FALSE,tcl = -0.5)
    }else{
      axis(1,at=1:length(label_axis),labels=FALSE,las=2,cex.axis=1,tcl = -0.2,hadj = 0.8)
      axis(1,at=1:length(label_axis),labels=FALSE,tcl = -0.5)
    }
    
  } 
  if(space_axis == 2){
    label_axis = substring(tiempo, 1,7)
    label_axis[seq(2, length(tiempo), by = 2)] <- NA
    if(isTRUE(axis_x)){
      axis(1,at=1:length(label_axis),labels=label_axis,las=2,cex.axis=1,tcl = -0.2,hadj = 0.8)
      axis(1,at=1:length(label_axis),labels=FALSE,tcl = -0.5)
    }else{
      axis(1,at=1:length(label_axis),labels=FALSE,las=2,cex.axis=1,tcl = -0.2,hadj = 0.8)
      axis(1,at=1:length(label_axis),labels=FALSE,tcl = -0.5)
    }
    
  }
  if(isTRUE(axis_x)){
     axis(1,at=1:length(label_axis),labels=label_axis,las=2,cex.axis=1,tcl = -0.2,hadj = 0.8)
     axis(1,at=1:length(label_axis),labels=FALSE,tcl = -0.5)
  }else{
     axis(1,at=1:length(label_axis),labels=FALSE,las=2,cex.axis=1,tcl = -0.2,hadj = 0.8)
     axis(1,at=1:length(label_axis),labels=FALSE,tcl = -0.5)
  }
  abline(v = which(substring(tiempo,6,7) %in% "01"), col = 1, lty = 2)
  axis(2)
  
  box()
  
  
  if(is.null(nombre_indice_legenda)){
    nombre_indice_legenda <- nombre_indice
  }
  
  if(is.null(nombre_patron)){
    nombre_patron <- "Patron"
  }
  
  if(legend == TRUE){
    if(plot_patron == TRUE){
      
      if(is.null(nombre_crucero)){
        legend("toprigh",c(nombre_patron, nombre_indice_legenda), lty = c(1,1), lwd = c(2,2),col = c(2,col_index),bty = "n")
      }else{
        legend("toprigh",c(nombre_patron, nombre_indice_legenda, nombre_crucero), lty = c(1,1,0), lwd = c(2,2,NA),pch = c(NA,NA,16), col = c(2,col_index,1),bty = "n")
      }
    }
    if(plot_patron == FALSE){
      
      if(is.null(nombre_crucero)){
        legend("toprigh", nombre_indice_legenda, lty = 1, lwd = 2, col = 4, bty = "n")
      }else{
        legend("toprigh",c(nombre_indice_legenda, nombre_crucero), lty = c(1,0), lwd = c(2,NA), pch = c(NA,16), col = c(col_index,1), bty = "n")
      }
    }
  }
}