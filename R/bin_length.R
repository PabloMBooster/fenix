bin_length <- function(x, Lmin, Lmax, bin = 3){
  x[is.na(x)] <- 0
  seq_bin   <- seq(Lmin, Lmax, by = bin)
  l_end     <- Lmax - seq_bin[length(seq_bin)]
  width_bin <- c(diff(seq_bin), l_end+1)
  seq_l     <- seq(1, length(x), by = bin)
  
  size_bin <- NULL
  for(i in seq_along(seq_l)){
    a <- seq_l[i]
    b <- a + width_bin[i]-1
    ibin <- x[a:b]
    size_bin <- c(size_bin, sum(ibin))
  }
  output <- list()
  output$freq <- size_bin
  output$len  <- seq_bin 
  return(output)
}