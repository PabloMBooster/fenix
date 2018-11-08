\name{get_week}
\alias{get_week}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get week
}
\description{
Generate a vector of week, the week begin monday and end sunday.
}
\usage{
get_week(date)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{date}{
in format date
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

vector_day <- seq.Date(as.Date("01-01-2018", format = "\%d-\%m-\%Y"), as.Date("01-03-2018", format = "\%d-\%m-\%Y"), by =  "day")
vector_week <- get_week(vector_day)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
