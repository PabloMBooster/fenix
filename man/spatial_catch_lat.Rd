\name{spatial_catch_lat}
\alias{spatial_catch_lat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Captura espacial por latitud
}
\description{
Realiza una interpolacion bicubica por cuadriculas donde el eje x es el tiempo en dias y el eje y es la latitud cada medio grado.
}
\usage{
spatial_catch_lat(data, fecha_inicio, fecha_final, lat_superior, lat_inferior, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
base de datos contiene por viaje la captura, fecha, latitud y longitud
}
  \item{fecha_inicio}{
fecha de inicio
}
  \item{fecha_final}{
fecha finala
}
  \item{lat_superior}{
latitud superior
}
  \item{lat_inferior}{
latitud inferior
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
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
Pablo Marin
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(base_captura)
head(base_captura)

base_captura0    <- base_captura[!is.na(base_captura$lat) & !is.na(base_captura$lon), ]

base_captura0    <- base_captura0[!is.na(base_captura0$catch) & base_captura0$catch > 0, ]
base_captura0$fecha <- as.Date(paste0(base_captura0$year,"-", base_captura0$month, "-", base_captura0$day), format = "\%Y-\%m-\%d")
base_captura0$dc <- estima_dc(base_captura0$lon, base_captura0$lat)

# fechas de analisis
fecha_inicio <- as.Date("2018-01-01", format = "\%Y-\%m-\%d")
fecha_final  <- as.Date("2019-04-01", format = "\%Y-\%m-\%d")
  
# latitud de analisis
lat_superior <- 3
lat_inferior <- 18.5

catch <- spatial_catch_lat(data = captura, fecha_inicio, fecha_final, lat_superior, lat_inferior)

scale.color = designer.colors(ceiling(max(base_fake)), 
                                c("white",'cyan','cyan','cyan','green','green','green','green', 'yellow', 'yellow', 'yellow', 'yellow', 'red', 'red', 'red','black','black','black','black'))

png("captura_espacial.png",width = 17, height = 5, units = "in", res = 140)
  
nf= layout(mat = matrix(c(1,2), nrow = 1, ncol = 2), widths = c(0.9, 0.1))
layout.show(nf)
  
par(mar = c(5, 3.7,0.3,0))
image(catch$akima.bic, col = scale.color, ylim = c(4,18),
        xlab = "", ylab = "", axes = F)
axis(1, at = seq(0,length(catch$xLab), length.out = 13), labels = catch$labels_month, las = 2, cex.axis = 0.75)
axis(2, at = seq(lat_superior,lat_inferior,2),labels = seq(lat_inferior,lat_superior,-2), las = 2)
  
mtext(text = "Latitud (°S)", side = 2, line = 2)
box()
  
par(mar = c(1,1,0,0))
plot(NA, NA, xlim = c(0,1), ylim = c(0,1), axes = FALSE, xlab = "", ylab = "")
legend.krige(x.leg = c(0.1,0.3),y.leg = c(0.2,0.9), 0:ceiling(max(catch$base_catch)),
               col=scale.color,  vertical=T)
text(x = 0.2, y = 0.95, labels = "Tons", cex = 0.8, font = 2)
  
dev.off()

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
