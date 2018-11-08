\name{test_norm}
\alias{test_norm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Tests of normality}
\description{
Use 5 methods for estimate test normality
}
\usage{
test_norm(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
a numeric vector of data values
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
 \item{p.value}{
the p-value for the test.
}

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
x <- rnorm(200, 20, 40)
out <- test_norm(x)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
