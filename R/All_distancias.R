
# distancias --------------------------------------------------------------

latlon.to.m =
  function(start.lon, start.lat, end.lon, end.lat)
  {
    # converts a distance between two points in lat -lon (decimal)
    # into a meter (horizontal) distance
    # depends on the latitude
    # note lat and lon are in decimal units (e.g. 49.23)
    # compute average latitude
    mid.lat = (start.lat + end.lat)/2
    meters.per.degree <- 111195.
    radians.lat <- (pi * mid.lat)/180
    shrink.factor <- cos(radians.lat)
    delx <- shrink.factor * (start.lon - end.lon)
    dely <- start.lat - end.lat
    meters.dist <- meters.per.degree * sqrt(delx^2 + dely^2)
    return(meters.dist)
    end
  }


 
# junk = approx(shoreline$V2,shoreline$V1,bitacora1$lat)
# bitacora1$distttt = latlon.to.m(junk$y,bitacora1$lat,bitacora1$lon,bitacora1$lat)/1000



