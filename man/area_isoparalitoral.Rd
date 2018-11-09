\name{area_isoparalitoral}
\alias{area_isoparalitoral}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Area isoparalitoral
}
\description{
knowing the latitude and distance to coast of any point identifies its corresponding isoparalitoral area
}
\usage{
area_isoparalitoral(dist_costa,latitude)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dist_costa}{
distance to coast
}
  \item{latitude}{
latitude
}
}
\details{
the function return distance to coast, latitude and area
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
lon <- c(-71.25, -72.01)
lat <- c(-17.83, -17.14)

data_area <- data.frame(lon = lon, lat = lat)
data_area$dc <- estima_dc(x = lon, y = lat)
area <- area_isoparalitoral(dist_costa = data_area$dc, latitude = data_area$lat)
head(area)

data_area$area <- area$area
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
