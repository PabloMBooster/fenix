\name{estima_distance}
\alias{estima_distance}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Minimal distance 
}
\description{
Estimate minimun distance. The distance contemplates the curvature of the earth and it is expressed in nautical miles
}
\usage{
estima_dist(x, y, x1, y1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
longitude of the observation
}
  \item{y}{
latitude of the observation
}
  \item{x1}{
longitude base
}
  \item{y1}{
latitude base
}
}
\details{
longitude and latitude of the place for example: coast line, 
island, shelf break etc.
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

head(dataLand)
dataLand$dist <- estima_dist(x = dataLand$Longitude, y = dataLand$Latitude, x1 = shelfbreak$lon, y1 = shelfbreak)


$lat)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
