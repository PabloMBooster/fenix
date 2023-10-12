VectorInVector = function (pattern, tag) 
{
  lenTag <- length(pattern) - 1
  out <- NULL
  for (i in seq(length(tag) - lenTag)) {
    if (isTRUE(identical(tag[seq(i, i + lenTag)], pattern))) 
      out <- c(out, i)
  }
  return(out)
}