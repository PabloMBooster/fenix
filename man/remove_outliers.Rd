\name{remove_outliers}
\alias{remove_outliers}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Remove outliers
}
\description{
Remove outliers from a data vector 
}
\usage{
remove_outliers(x, na.rm = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{na.rm}{
%%     ~~Describe \code{na.rm} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
Determine if that point is further than 1.5 * IQR away from the mean. If so, that point is an outlier and should be eliminated from the data resulting in a new set of data.
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
set.seed(2)
x <- rnorm(100, 30, 40) 
x[100] <- 200
x2 <- remove_outliers(x)

boxplot(cbind(x, x2))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
