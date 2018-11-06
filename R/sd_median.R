sd_median <-function(x){
  x <- as.numeric(na.omit(x))
  n <- length(x)
  out <- (1/n)*sum(abs(x - median(x)))
  return(out)
}


