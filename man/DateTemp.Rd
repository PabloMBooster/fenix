\name{DateTemp}
\alias{DateTemp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Seasons of the North-Center Stock of the Peruvian anchovy
}
\description{
Determine the days corresponding to each seasons fishing
}
\usage{
DateTemp(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
date
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

dates <- c("25/04/2017", "26/04/2017", "27/04/2017", "28/04/2017")
dates <- as.Date(dates, "\%d/\%m/\%Y")
DateTemp(dates)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
