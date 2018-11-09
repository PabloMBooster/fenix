\name{estima_distance_signo}
\alias{estima_distance_signo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Minimal distance
}
\description{
Estimate minimun distance. The distance contemplates the curvature of the earth and it is expressed in nautical miles. it also,  determines the location with respect to the point base, positive when it is on the right and negative when it is on the left
}
\usage{
estima_distance_signo(x, y, x1, y1)
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
signo <- estima_distance_signo(x=points_fishing$lon, y=points_fishing$lat, x1 = shelfbreak$lon, y1 = shelfbreak$lat)
mapa_peru()
points(x=points_fishing$lon, y=points_fishing$lat, pch = ".", cex = 4, col = signo$signo + 3)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
