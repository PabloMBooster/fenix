extrae_numero <- function(x)
{
  n <- nchar(x)
  ext_num <- substring(x, 1:n, 1:n)
  numbers <- 0:9
  num <- NULL
  for (i in ext_num){
    if (i %in% numbers){
      num <- paste(num,i,sep="")
    }
  }
  num <- as.numeric(num)
  return(num)
}