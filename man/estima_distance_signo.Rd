\name{estima_distance_signo}
\alias{estima_distance_signo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Minimal distance
}
\description{
Estimate minimun distance. The distance contemplates the curvature of the earth and it is expressed in nautical miles. it also,  determines the location with respect to the point of origin, positive when it is on the right and negative when it is on the left
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
    latitude of the place
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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x, y, x1, y1) 
{
    junk = approx(y1, x1, y)
    signo <- (junk$y - x)/abs((junk$y - x))
    A3 = y1 * pi/180
    B3 = x1 * pi/180
    A31 = y * pi/180
    B31 = x * pi/180
    num2 = length(x1) - 1
    n = length(x)
    dfinal = NULL
    num = length(y)
    pos.na = c(which(is.na(y) == TRUE), which(is.na(x) == TRUE))
    pos.na
    dc <- NULL
    num2 = length(x1) - 1
    n = length(x)
    dfinal = NULL
    num = length(y)
    pos.na = c(which(is.na(y) == TRUE), which(is.na(x) == TRUE))
    pos.na
    distance_min <- NULL
    for (i in 1:num) {
        A = NULL
        B = NULL
        A = sin(A3[1:num2]) * sin(A31[i])
        B = cos(A3[1:num2]) * cos(A31[i]) * cos(B3[1:num2] - 
            B31[i])
        grad = acos(A + B)
        gradinrad = 180 * grad/pi
        distancia = (60 * gradinrad)
        indnonan = which(distancia > 0)
        if (length(indnonan) == 0) {
            dist_min = NA
        }
        else {
            dist_min = min(distancia[indnonan])
        }
        distance_min <- rbind(distance, dist_min)
    }
    distance_min <- as.numeric(distance_min)
    suppressWarnings(distance_min)
    list(dc = dc, signo = signo)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
