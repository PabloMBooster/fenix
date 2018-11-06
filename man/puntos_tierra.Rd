\name{puntos_tierra}
\alias{puntos_tierra}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
points on land
}
\description{
identify the positions in land
}
\usage{
puntos_tierra(x, y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
  longitude}
  \item{y}{
  latitude

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
head(data_land)
mapa.peru()
point(data_land$lon, data_land$lat)

data_land$pLand <- point_land(data_land$lon, data_land$lat)
data_land <- data_land[data_land$pLand > 0, ] # remove point to land

mapa.peru()
point(data_land$lon, data_land$lat)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
