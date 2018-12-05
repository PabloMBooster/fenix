cohort_to_length <- function(cohort,lower,upper,delta=0.5,delta.c=0.1,FUN=ddnorm1, 
         trunc=0.05) {
  # Return a vector with length distribution of a 'cohort' (N,L,S)
  # 'lower' and 'upper' are the (class mark) limits of integration 
  # 'delta' is the length of the class interval (as measured by fishery)
  # 'delta.c' is the length for integration of the length frequency distribution
  # 'FUN' gives the density distribution (normal for one cohort)
  if(length(cohort)!=3) stop("No cohort data recognized (N,L,SD)")
  FUN  = match.fun(FUN)
  marcas = seq(from=lower, to=upper, by=delta)
  n.step	= ceiling(delta/delta.c)
  lower	= lower-0.5*delta 
  upper	= upper+0.5*delta
  n=cohort[1]; l=cohort[2]; sd=cohort[3]
  x	= seq(from=lower,to=upper,by=delta/n.step)
  y	= (1/6)*(FUN(x[-length(x)],n,l,sd)+4*FUN(0.5*(x[-length(x)]+x[-1]),n,l,sd)+FUN(x[-1],n,l,sd))
  #	y	= (1/2)*(FUN(x[-length(x)],n,l,sd)+FUN(x[-1],n,l,sd))
  out	= apply(matrix((delta/n.step)*y,nrow=n.step),2,sum)
  rangoL = round(qnorm(c(trunc/2, 1-trunc/2), mean=l, sd=sd))
  out[marcas<rangoL[1] | marcas>rangoL[2]] = 0
  out = n*out/sum(out, na.rm=TRUE)
  names(out) = marcas
  return(out)
}