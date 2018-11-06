\name{estima_dc}
\alias{estima_dc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Distance to coast
}
\description{
Estimate distance to coast (dc) in the jurisdictions of the Peruvian sea. 
The distance contemplates the curvature of the earth and it is expressed in nautical miles}
\usage{
estima_dc(x, y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
longitude
}
  \item{y}{
latitud
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
if yoy know estimate the distance to coast in other place you can use \code{distance}.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

longitude <- c(-77.1,-78.1,-77.3)
latitude <- c(-14,-13.4,-15.1)

dc <- estima_dc(x = longitude, y = latitude)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
