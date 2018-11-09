\name{rangeWeek}
\alias{rangeWeek}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Range of day by week
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
rangeWeek(date, week)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{date}{
vector of date
}
  \item{week}{
vector of week
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
start <- "25/04/2017"
end <- "30/05/2017"
vector_date <- seq.Date(as.Date(start, "\%d/\%m/\%Y"), as.Date(end, "\%d/\%m/\%Y"), by = "day")
vector_week <- get_week(vector_date)
out <- rangeWeek(vector_date, vector_week)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
