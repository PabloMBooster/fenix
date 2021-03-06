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
if \code{puntos_tierra} return NA you need to check the longitude or latitude.
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
head(dataLand)
dataLand$pLand <- puntos_tierra(x = dataLand$Longitud, y = dataLand$Latitud)

mapa_peru()
points(dataLand$Longitud, dataLand$Latitud, pch = 16, col = 2)
points(dataLand$Longitud[dataLand$pLand < 0], dataLand$Latitud[dataLand$pLand < 0], col = 4, pch = 16)
legend("toprigh", legend = c("sea", "land"), pch = 16, col = c(2, 4), bty = "n")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
