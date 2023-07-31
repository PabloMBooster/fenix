\name{mapa_peru}
\alias{mapa_peru}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Mapa de Peru}
\description{
genera una mascara del mapa de Peru
}
\usage{
mapa_peru()
}
\arguments{
  \item{xlim}{
  rango en longitud}

  \item{ylim}{
  rango en latitud}

  \item{ylab}{
  etiqueta del eje y}

  \item{xlab}{
  etiqueta del eje y}

  \item{land.col}{
  color de la costa peruana}
  
    \item{border.map}{
  color del borde}

  \item{sea.col}{
  color del mar}

  \item{cex_axis}{
  tamanho de letra o simbolo de los ejes}
  
  \item{cex_harbor}{
  tamanho de letra de los puertos}
  
  \item{col_harbor}{
  color de letra de los puertos}
  
  \item{font_harbor}{
  tipo de letra del puerto}

  \item{add1}{
  agregar mapa}

  \item{n_perfil}{
  numero de perfiles en el mapa}

  \item{space_perfil}{
  espacio entre perfiles, por defecto cada 3 grados}
  
  \item{area_iso}{
  agregar areas isopalitorales, por defecto FALSE}
  
  \item{name_area_iso}{
  nombre de las areas isoparalitorales a graficar
  }
}

\details{


}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
mapa_peru()
points(dataLand$lon, dataLand$lat)

space_perfil <- 3
mapa_peru(n_perfil = 2, space_perfil = space_perfil)
points(dataLand$Longitud, dataLand$Latitud)
points(dataLand$Longitud-space_perfil, dataLand$Latitud)

## plot point fishing
head(points_fishing)

points_fishing$week <- get_week(points_fishing$date)
label_week <- rangeWeek(points_fishing$date, points_fishing$week)

n = -3
space = (0:(length(unique(points_fishing$week))-1))*n
points_fishing0 = points_fishing
for(i in 1:length(unique(points_fishing0$week))){
  points_fishing0[points_fishing0$week == sort(unique(points_fishing0$week))[i],"lon"] = points_fishing0[points_fishing0$week == sort(unique(points_fishing0$week))[i],"lon"] + space[i]
}

n_perfil <-  length(unique(points_fishing$week))
mapa_peru(n_perfil = n_perfil)

for(i in 1:length(space)){
  lines(linePeru$lon + space[i], linePeru$lat, col = "gray45")
  text(x = -72 + space[i], y = -19, labels = label_week[i], cex = 0.6)
}
points(points_fishing0$lon, points_fishing0$lat, cex = 0.8, col = 4, pch = 16) 


# dibujar algunas areas iso
mapa_peru(area_iso = T, name_area_iso = c(12180, 13180))

# dibujar areas iso coloreadas + perfiles

points_fishing$dc   = estima_dc(x = points_fishing$lon,y = points_fishing$lat)
area                = area_isoparalitoral(dist_costa = points_fishing$dc ,latitude = points_fishing$lat)
points_fishing$area = area$area
points_fishing      = points_fishing[!is.na(points_fishing$area),]
scale = 0:length(sort(unique(points_fishing$week)))*-3

maxAll = NULL
for(j in sort(unique(points_fishing$week))){
  
  dataj        = points_fishing[points_fishing$week == j,]
  tmp_isoareas = isopara$area %in% dataj$area
  tmp_isopara  = isopara[tmp_isoareas, ]
  idx_areas    = unique(tmp_isopara$area)
  
  maxVar = NULL
  for(i in seq_along(idx_areas)){
    temp = subset(x = tmp_isopara, subset = tmp_isopara$area == idx_areas[i])
    temp_data = subset(x = dataj, subset = dataj$area == idx_areas[i])
    colorVariable = tapply(temp_data$lon, temp_data$area, length) ## aqui modificar las variable y la funcion para colorear las areas
    maxVar = c(max(colorVariable), maxVar)
  }
  maxAll = c(max(maxVar), maxAll)
}

max_valor   = max(maxAll)
scale.color = designer.colors(max_valor, c('#00007F','blue',
                                           '#007FFF','cyan',
                                           '#7FFF7F','yellow',
                                           '#FF7F00','red','#7F0000'))

n_perfil = length(sort(unique(points_fishing$week)))
mapa_peru(n_perfil = n_perfil)

for(j in sort(unique(points_fishing$week))){
  
  dataj        = points_fishing[points_fishing$week == j,]
  tmp_isoareas = isopara$area %in% dataj$area
  tmp_isopara  = isopara[tmp_isoareas, ]
  idx_areas    = unique(tmp_isopara$area)
  
  maxVariable = NULL
  for(i in seq_along(idx_areas)){
    
    temp = subset(x = tmp_isopara, subset = tmp_isopara$area == idx_areas[i])
    temp_data = subset(x = dataj, subset = dataj$area == idx_areas[i])
    
    colorVariable = as.numeric(tapply(temp_data$lon, temp_data$area, length))

    polygon(x = c(temp$lon[1]+scale[j], temp$lon[2:nrow(temp)]+scale[j], temp$lon[1]+scale[j]), 
            y = c(temp$lat[1], temp$lat[2:nrow(temp)], temp$lat[1]), 
            border = scale.color[colorVariable], col = scale.color[colorVariable])
    maxVariable = c(max(colorVariable), maxVariable)
  }
  require(geoR)

  legend.krige(c(scale[length(scale)]-81,scale[length(scale)]-80.5),c(-20,-13),
               1:max_valor, vertical=T, col=scale.color, offset.leg = 1.5)
}

}





% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
