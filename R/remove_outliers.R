remove_outliers <- function(x, na.rm = TRUE, ...) {
  qte <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qte[1] - H)] <- NA
  y[x > (qte[2] + H)] <- NA
  return(y)
}