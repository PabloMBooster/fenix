biom_survey_dist_tallas <- function(Base_datos, a = 0.0038, b = 3.215, save = T, output, file = "abundancia_biomasa_biom", catch = 2){
  
  freq_survey  <- NULL
  tallas_anc   <- seq(2, 20, by = 0.5)
  
  for(i in unique(Base_datos$LANCE)){
    x <- Base_datos[Base_datos$LANCE %in%  i,]
    x <- x[order(x$LONGITUD_ESPECIE),]
    id_tallas <- which(tallas_anc %in% x$LONGITUD_ESPECIE)
    freq_tallas <- rep(0, length(tallas_anc))
    freq_tallas[id_tallas] <- x$FREC_SIMPLE
    freq_tallas <- freq_tallas/sum(freq_tallas)*100
    freq_tallas <- fenix:::weighted_frequency(length = seq(2,20,by = 0.5),frequency = freq_tallas,a = a, b = b, catch = catch)
    freq_survey <- rbind(freq_survey, freq_tallas)
  }
  
  freq_survey <- data.frame(freq_survey)  
  freq_survey <- apply(freq_survey, 2, sum, na.rm = T)
  freq_survey <- freq_survey/sum(freq_survey)*100
  
  freq_survey <- data.frame(Tallas = seq(2,20, by = 0.5),N = as.numeric(freq_survey))
  
  ## Freq Simple
  write.table(freq_survey, "output/Freq_simple_tallas_Survey.csv", sep = ";", row.names = F)
  
  juv = length(seq(2,11.5, by = 0.5))
  porJuv = round(sum(freq_survey$N[1:juv])/sum(freq_survey$N)*100)
  
  peso = a*freq_survey$Tallas^b
  w =  freq_survey$N*peso
  w = w/sum(w)*100
  porJuvW = round(sum(w[1:juv])/sum(w)*100)
  freq_survey$W = w
  
  byRelBiomY = 2
  byRelAbunY = 2
  juvLim = 12
  juvCol = 2
  
  par(mar = c(4.1, 4.1, 1.1, 1.1), mfrow = c(2, 1), oma = rep(2, 
                                                              4))
  plot(x = freq_survey$Tallas, y = freq_survey$N, type = "l", lwd = 2, col = "red", 
       axes = F, cex.lab = 1.5, ylab = "Frecuencia relativa", xlab = "Longitud total (cm)", 
       ylim = c(0, 2 * (max(freq_survey$N)%/%2 + as.logical(max(freq_survey$N)%%2))))
  axis(1, marcas, labels = NA, cex.axis = 1.2)
  axis(1, marcas[seq(from = 1, by = 2, length.out = length(marcas)/2)], 
       cex.axis = 1.2)
  axis(2, seq(from = 0, to = 2 * (max(freq_survey$N)%/%2 + as.logical(max(freq_survey$N)%%2)), 
              by = byRelAbunY), cex.axis = 1.2, las = 2)
  
  abline(v = juvLim, lty = 2, col = juvCol)
  
  mtext(paste("Juv = ", porJuv, " %", sep = ""), side = 3, line = -2, 
        adj = 0.02, cex = 1.2, font = 2)
  mtext("Abundancia", side = 3, line = -2, adj = 0.98, cex = 1.2, 
        font = 2)
  box()
  plot(x = freq_survey$Tallas, y = freq_survey$W, type = "l", lwd = 2, col = "red", 
       axes = F, cex.lab = 1.5, ylab = "Frecuencia relativa", xlab = "Biomasa", 
       ylim = c(0, 2 * (max(freq_survey$W)%/%2 + as.logical(max(freq_survey$W)%%2))))
  axis(1, marcas, labels = NA, cex.axis = 1.2)
  axis(1, marcas[seq(from = 1, by = 2, length.out = length(marcas)/2)], 
       cex.axis = 1.2)
  axis(2, seq(from = 0, to = 2 * (max(freq_survey$W)%/%2 + as.logical(max(freq_survey$W)%%2)), 
              by = byRelBiomY), cex.axis = 1.2, las = 2)
  abline(v = juvLim, lty = 2, col = juvCol)
  
  mtext(paste("Juv = ", porJuvW, " %", sep = ""), side = 3, line = -2, 
        adj = 0.02, cex = 1.2, font = 2)
  mtext("Biomasa", side = 3, line = -2, adj = 0.98, cex = 1.2, 
        font = 2)
  box()
  if (isTRUE(save)) {
    dev.copy(png, filename = paste0(output, "/",file,".png"), width = 2000, height = 1500, 
             res = 200)
    dev.off()
  }  
}