test_norm <- function(x, print_result = TRUE){
  
  list.of.packages <- c("nortest")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  
  library(nortest)
  
  if(!is.numeric(x))
    stop("x is not numeric")
  
  Ad.Test = ad.test(x)
  Cramer.Test = cvm.test(x)
  Pearson.Test = pearson.test(x)
  ShapiroF.Test = sf.test(x)
  Kolmogorov.Test = lillie.test(x)
  Shapiro.Test = shapiro.test(x)
  
  method = c(Ad.Test$method, Cramer.Test$method, Pearson.Test$method,
             ShapiroF.Test$method, Kolmogorov.Test$method, Shapiro.Test$method)
  
  pvalue = c(Ad.Test$p.value, Cramer.Test$p.value, Pearson.Test$p.value,
             ShapiroF.Test$p.value, Kolmogorov.Test$p.value, Shapiro.Test$p.value)
  
  normal = rep(NA, length(pvalue))
  for(i in seq_along(pvalue)){ 
    if(!pvalue[i] <0.05) {
      normal[i] <- "YES"
    }else {
      normal[i] <- "NO"
    }
  }
  out = list(); out$method = list()
  out$method = method
  out$result = data.frame(method = method, pvalue = pvalue, normal = normal)  
  if(isTRUE(print_result)){
    print(out)  
  }
  return(out)
}
