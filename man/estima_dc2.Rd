\name{estima_dc2}
\alias{estima_dc2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Distance to coast
}
\description{
Estimate the distance to shore on a projected map and transformed data. The default function loads a polygon from Peru and estimates the distance to the Peruvian coast. If you want to calculate the distance to the coast of another place to load your polygon, the polygon must have the class "SpatialPolygonsDataFrame", the distance is expressed in nautical miles. This function is 10 times faster than estimate_dc.

%Estima la distancia a costa en un mapa proyectado y datos transformados. La funcion carga por defecto un poligono de Peru y estima la distancia a la costa peruana. Si se desea calcular la distancia a costa de otro lugar cargar su poligono, el poligono debe tener la clase "SpatialPolygonsDataFrame", la distancia es expresada en millas nauticas. Esta funcion es 10 veces mas rapida que estima_dc.
}
\usage{
estima_dc2(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
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
longitude <- c(-77.1,-78.1,-77.3)
latitude <- c(-14,-13.4,-15.1)

dc <- estima_dc2(lon = longitude, lat = latitude, polygon = PERU_SP)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
