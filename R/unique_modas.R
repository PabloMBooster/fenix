unique_modas <- function(x){
  return(length(x[!is.na(x)]))
}