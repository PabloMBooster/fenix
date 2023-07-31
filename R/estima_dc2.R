# estima_dc2 <- function(lon, lat, polygon = NULL){
#   
#   require(sp)
#   require(rgeos)
#   
#   temp = data.frame(lon = lon, lat = lat)
#   posiciones = temp[,c("lon", "lat")]
#   #- Convert VMS data to SpatialPolygons
#   spTa              = SpatialPoints(data.frame(posiciones))
#   proj4string(spTa) = CRS("+proj=longlat")
#   spTa.proj         = spTransform(spTa, CRS("+proj=utm +zone=18 ellips=WGS84"))
#   #- Read shapefile of Peru
#   if(is.null(polygon)){
#     Peru              = as(PERU_SP, "SpatialPolygons")
#     proj4string(Peru) = CRS("+proj=longlat")
#     Peru.proj         = spTransform(Peru, CRS("+proj=utm +zone=18 ellips=WGS84"))
#     dists = gDistance(spgeom1 = spTa.proj, spgeom2=Peru.proj, byid=T) #
#     distance       = as.vector(t(dists*0.00053996)) # convirtiendo de metros a millas nauticas
#   }else{
#     if(class(polygon) == "SpatialPolygonsDataFrame"){
#       stop("polygon is not SpatialPolygonsDataFrame class")
#     }
#     Peru              = as(polygon, "SpatialPolygons")
#     proj4string(Peru) = CRS("+proj=longlat")
#     Peru.proj         = spTransform(Peru, CRS("+proj=utm +zone=18 ellips=WGS84"))
#     dists = gDistance(spgeom1 = spTa.proj, spgeom2=Peru.proj, byid=T) #
#     distance       = as.vector(t(dists*0.00053996)) # convirtiendo de metros a millas nauticas
#   }
#   return(distance)
# }

estima_dc2 <- function(lon, lat, polygon = NULL){
  
  require(sf)
  
  # convert to sf and project on a projected coord system
  area <- st_as_sfc(polygon, crs = 4326L)
  area <- st_set_crs(area, 4326L)
  # points with long/lat coords
  pnts <-
    data.frame(
      long = lon,
      lat = lat
    )
  
  # convert to sf with the same crs
  pnts_sf <- st_as_sf(pnts, crs = 4326L, coords = c("long", "lat"))
  pnts_sf = st_set_crs(pnts_sf, 4326L)
  
  # check if crs are equal
  all.equal(st_crs(pnts_sf),st_crs(area))
  output = as.numeric(st_distance(pnts_sf, area)*0.000539957)
  
  return(output)
}