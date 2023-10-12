omitCeros = function (vect, nzeros = 1) 
{
  data <- matrix(vect)
  data[data == 0] <- NA
  if ((sum(is.na(data)) == length(data)) | (sum(!is.na(data)) == 
                                            length(data))) 
    return(data)
  if (nzeros > 0) {
    iniIndex <- apply(matrix(apply(data, 1, is.na)), 2, VectorInVector, 
                      pattern = c(TRUE, FALSE))
    finIndex <- apply(matrix(apply(data, 1, is.na)), 2, VectorInVector, 
                      pattern = c(FALSE, TRUE))
    index.fill <- which(!is.na(data))
    index <- NULL
    for (k in seq_len(length(iniIndex))) index = c(index, 
                                                   seq(iniIndex[k] - nzeros + 1, iniIndex[k]))
    for (k in seq_len(length(finIndex))) index = c(index, 
                                                   seq(finIndex[k] + 1, finIndex[k] + nzeros))
    index = index[!index %in% index.fill]
    data[index] <- 0
  }
  return(data)
}