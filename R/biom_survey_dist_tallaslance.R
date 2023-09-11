
biom_survey_dist_tallaslance = function(data = data, output = output, file = "muestreos", type = "pdf", cex = 0.7){
  
  if(type == "pdf"){
    pdf(file = paste0(outout,"/", file,".pdf"), pointsize = 8)
  }
  if(type == "png"){
    png(file = paste0(outout,"/", file,".png"),  width = 850, 
        height = 820, units = "px", res = 140)
  }
  par(mfrow = c(7,6), mar = c(0,0,0,0), oma = c(4,4,1,1))
  
  for(i in seq_along(sort(unique(Base_datos$LATITUD_INICIAL), decreasing = T))){
    BaseByDate = Base_datos[Base_datos$LATITUD_INICIAL %in% sort(unique(Base_datos$LATITUD_INICIAL), decreasing = T)[i], ]
    
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
    
    imax <- max(seq_along(sort(unique(Base_datos$REO_DFECINE))))
    xx <- c((6*7-5):(6*7),(6*7*2-5):(6*7*2),(6*7*3-5):(6*7*3),(imax-5):imax)
    
    if(i%in%xx){
      axis(1, at = seq(4,20,4), labels = NA)
      axis(1, at = seq(4,20,4), padj = -0.5, cex.axis = 0.70)
    }
    
    if(i%%6==1){
      axis(2, las = 2, cex.axis = 0.70)
    }
    abline(v = 12, lty = 2, col = 4)
    
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
  
  dev.off()
}