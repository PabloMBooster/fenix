\name{cohort_to_length}
\alias{cohort_to_length}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 Cohort to length
}
\description{
Return a vector with length distribution of a 'cohort' (N,L,S)
}
\usage{
cohort_to_length(cohort, lower, upper, delta = 0.5, delta.c = 0.1, FUN = ddnorm1, trunc = 0.05)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cohort}{
is a input of a 'cohort': N (abundance); L (middle length) y S (standard deviation).
}
  \item{lower}{
lower interval
}
  \item{upper}{
upper interval
}
  \item{delta}{
is the length of the class interval (as measured by fishery)
}
  \item{delta.c}{
is the length for integration of the length frequency distribution
}
  \item{FUN}{
gives the density distribution (normal for one cohort)
}
  \item{trunc}{
%%     ~~Describe \code{trunc} here~~
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
