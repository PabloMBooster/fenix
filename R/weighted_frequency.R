weighted_frequency <- function(length,frequency,a,b,catch){
  
  weight  <- (a*length^b)*frequency
  freqW   <- (catch/sum(weight))*weight
  lengthW <- freqW/(a*length^b)
  
  return(lengthW)
}