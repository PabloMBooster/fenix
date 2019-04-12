addIsopara = function(dataIsopara = Isopara, Cols =  "khaki1", xLim, yLim){
  # dataIsopara = read.csv("areas-isoparalitorales.csv")
  dataIsopara = subset(dataIsopara, subset = dataIsopara$lat <= yLim[2] & dataIsopara$lat >= yLim[1])
  idx_areas = unique(dataIsopara$area)
  
  for(r in seq_along(idx_areas)){
    temp = subset(x = dataIsopara, subset = dataIsopara$area == idx_areas[r])
    polygon(x = c(temp$lon[1], temp$lon[2:nrow(temp)], temp$lon[1]),
            y = c(temp$lat[1], temp$lat[2:nrow(temp)], temp$lat[1]),
            border = Cols, col = NA)
  }
}