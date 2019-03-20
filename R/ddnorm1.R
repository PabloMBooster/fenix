ddnorm1 <- function(xc,  nc, lc ,sdc){
  out = nc * (1/(sqrt(2*pi)*sdc))*exp(-0.5*((xc-lc)/sdc)^2) 
  return(out)
}