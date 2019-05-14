Bhattacharya2 <- function(x, length_class = seq(5, 20, by = 0.5), mixpar, plot = FALSE){
  
    list.of.packages <- c("mixdist")
    new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
    if(length(new.packages)) install.packages(new.packages)
    
    library(mixdist)
    
    x[is.na(x)] = 0
  
    mixdat     = data.frame(legnth = length_class, freq = x)

    outL = tryCatch({
      mix(mixdat = mixdat, mixpar = mixpar, dist = "norm", print.level = 1)
    }, error=function(e){
      message("Not Converge") 
      outL <- NULL
      return(outL)
    })
    
    if(!is.null(outL)){
      res_modas2 = fitted(fit1)
      res_modas2 = res_modas2$joint 
      
      population = sum(res_modas2)
      tableBhatt = data.frame(Group = seq_along(outL$parameters$mu), Mean = outL$parameters$mu, Sigma = outL$parameters$sigma, Population = population)
      if(isTRUE(plot)){
        plot(outL)
      }
      
      cohortes <- data.frame(res_modas2)
      names(cohortes) <- paste0("cohor",1:ncol(cohortes))
      out = list(tableBhatt = tableBhatt, cohortes, outL = outL)
    }else{
      out = NULL
    }
    return(out)
}

# sigma  = seq(2, 2+groups-1, by = 1)
# pi     = rep(1/groups, groups)
# dfPars = data.frame(pi = pi, mu = mu, sigma = sigma)

