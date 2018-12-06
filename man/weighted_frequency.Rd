\name{weighted_frequency}
\alias{weighted_frequency}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Weighted
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
weighted length frequency to catch
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{length}{
length class
}
  \item{frequency}{
length frequency
}
  \item{a}{
parameter of relation length-weight
}
  \item{b}{
parameter of relation length-weight
}
  \item{catch}{
valor of the catch
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
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# Sample 
frequency <- c(0,0,0,0,0,0,0,0,0,2,7,14,22,15,13,12,17,22,35,28,7,4,1,0,0,0,0,0,0,0,0)
length <- seq(5,20, by = 0.5)

# weighted
catch_frequency = weighted_frequency(length = length, frequency = frequency, a = 0.003925, b = 3.2178, catch =  50000) 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
