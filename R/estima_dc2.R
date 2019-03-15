estima_dc2 <- function(lon, lat, polygon = NULL){
  
  temp = data.frame(lon = lon, lat = lat)
  posiciones = temp[,c("lon", "lat")]
  #- Convert VMS data to SpatialPolygons
  spTa              = SpatialPoints(data.frame(posiciones))
  proj4string(spTa) = CRS("+proj=longlat")
  spTa.proj         = spTransform(spTa, CRS("+proj=utm +zone=18 ellips=WGS84"))
  #- Read shapefile of Peru
  if(is.null(polygon)){
    Peru              = as(PERU_SP, "SpatialPolygons")
    proj4string(Peru) = CRS("+proj=longlat")
    Peru.proj         = spTransform(Peru, CRS("+proj=utm +zone=18 ellips=WGS84"))
    dists = gDistance(spgeom1 = spTa.proj, spgeom2=Peru.proj, byid=T) #
    distance       = as.vector(t(dists*0.00053996)) # convirtiendo de metros a millas nauticas
  }else{
    if(class(polygon) == "SpatialPolygonsDataFrame"){
      stop("polygon is not SpatialPolygonsDataFrame class")
    }
    Peru              = as(polygon, "SpatialPolygons")
    proj4string(Peru) = CRS("+proj=longlat")
    Peru.proj         = spTransform(Peru, CRS("+proj=utm +zone=18 ellips=WGS84"))
    dists = gDistance(spgeom1 = spTa.proj, spgeom2=Peru.proj, byid=T) #
    distance       = as.vector(t(dists*0.00053996)) # convirtiendo de metros a millas nauticas
  }
  return(distance)
}
