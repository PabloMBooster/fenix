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
%%     ~~Describe \code{lower} here~~
}
  \item{upper}{
%%     ~~Describe \code{upper} here~~
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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (cohort, lower, upper, delta = 0.5, delta.c = 0.1, FUN = ddnorm1, 
    trunc = 0.05) 
{
    if (length(cohort) != 3) 
        stop("No cohort data recognized (N,L,SD)")
    FUN = match.fun(FUN)
    marcas = seq(from = lower, to = upper, by = delta)
    n.step = ceiling(delta/delta.c)
    lower = lower - 0.5 * delta
    upper = upper + 0.5 * delta
    n = cohort[1]
    l = cohort[2]
    sd = cohort[3]
    x = seq(from = lower, to = upper, by = delta/n.step)
    y = (1/6) * (FUN(x[-length(x)], n, l, sd) + 4 * FUN(0.5 * 
        (x[-length(x)] + x[-1]), n, l, sd) + FUN(x[-1], n, l, 
        sd))
    out = apply(matrix((delta/n.step) * y, nrow = n.step), 2, 
        sum)
    rangoL = round(qnorm(c(trunc/2, 1 - trunc/2), mean = l, sd = sd))
    out[marcas < rangoL[1] | marcas > rangoL[2]] = 0
    out = n * out/sum(out, na.rm = TRUE)
    names(out) = marcas
    return(out)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
