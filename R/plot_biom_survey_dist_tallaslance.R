plot_biom_survey_dist_tallaslance = function(data = data, cex = cex){
  par(mfrow = c(7,6), mar = c(0,0,0,0), oma = c(4,4,1,1))
  for(i in seq_along(sort(unique(data$LATITUD_INICIAL), decreasing = T))){
    BaseByDate = data[data$LATITUD_INICIAL %in% sort(unique(data$LATITUD_INICIAL), decreasing = T)[i], ]
    
    BaseByDate$TOTAL <- sum(BaseByDate$FREC_SIMPLE)
    BaseByDate <- BaseByDate[order(BaseByDate$LONGITUD_ESPECIE),]
    x = seq(1,20, by = 0.5)
    y = rep(0, length(x)) 
    ID_y = which(x %in% BaseByDate$LONGITUD_ESPECIE) 
    y[ID_y] <-  BaseByDate$FREC_SIMPLE
    y <- y/sum(y)
    
    juv <- round(100*sum(y[x<12]), digits = 0)
    plot(x, y, type = "l", axes = FALSE, xlab = "", ylab = "", xlim = c(1,20), ylim = c(0,0.75),
         xaxs = "i", yaxs = "i", las = 2, col = 2, lwd = 2)
    
    abline(h = 0, lwd = 2.2, col = "gray")
    
    nobs = length(seq_along(sort(unique(data$LATITUD_INICIAL), decreasing = T)))
    
    if(i%%6==1){
      axis(2, las = 2, cex.axis = 0.70)
    }
    abline(v = 12, lty = 2, col = 4)
    
    if(i == nobs){
      axis(1, at = seq(4,20,4), labels = NA)
      axis(1, at = seq(4,20,4), padj = -0.5, cex.axis = 0.70)
    }
    if(i == nobs-1){
      axis(1, at = seq(4,20,4), labels = NA)
      axis(1, at = seq(4,20,4), padj = -0.5, cex.axis = 0.70)
    }
    if(i == nobs-2){
      axis(1, at = seq(4,20,4), labels = NA)
      axis(1, at = seq(4,20,4), padj = -0.5, cex.axis = 0.70)
    }
    if(i == nobs-3){
      axis(1, at = seq(4,20,4), labels = NA)
      axis(1, at = seq(4,20,4), padj = -0.5, cex.axis = 0.70)
    }
    if(i == nobs-4){
      axis(1, at = seq(4,20,4), labels = NA)
      axis(1, at = seq(4,20,4), padj = -0.5, cex.axis = 0.70)
    }
    if(i == nobs-5){
      axis(1, at = seq(4,20,4), labels = NA)
      axis(1, at = seq(4,20,4), padj = -0.5, cex.axis = 0.70)
    }
    
    lat <- floor(BaseByDate$LATITUD_INICIAL[1]*-1)
    if(!is.na(lat)){
      if(nchar(lat) == 1)
      {
        lat <- paste0("0",lat)  
      }
      min <- round(((BaseByDate$LATITUD_INICIAL[1]*-1)-floor(BaseByDate$LATITUD_INICIAL[1]*-1))*60)
      latlabel <- paste0(lat,"° ",min,"'","S")
      dclabel <- paste0(round(BaseByDate$dc[1]), " mn")
    }else{
      latlabel <- ""
      dclabel  <- ""
    }
    
    text(5.2,0.55, paste0("Juv=", juv, "%"), cex = cex) #-3.5+1
    text(5.2,0.45, paste0("n=", BaseByDate$TOTAL[1]), cex = cex) #-5.0+1
    text(14.5,0.65, BaseByDate$LANCE[1], cex = cex, adj = 0.5)
    text(15.8,0.55, latlabel, cex = cex, adj = 0.5)
    text(16.2,0.45, dclabel, cex = cex, adj = 0.5)
    
    box(lwd =1, col = "gray")
  }
  
  mtext(outer = T, side = 1, text = "Longitud total (cm)", line = 2.5, cex = 0.8)
  mtext(outer = T, side = 2, text = "Frecuencia relativa (%)", line = 2.5, cex = 0.8)
  
} 