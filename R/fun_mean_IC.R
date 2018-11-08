fun_mean_IC <- function(x, group,...){
  mn      <- aggregate(x, list(group), mean, na.rm = TRUE)
  std     <- aggregate(x, list(group), sd, na.rm = TRUE)
  num.obs <- as.numeric(table(group))
  sterr   <- as.numeric(std$x/sqrt(num.obs))
  mn$sterr<- sterr 
  
  return(mn)
}