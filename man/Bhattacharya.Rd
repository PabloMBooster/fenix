\name{Bhattacharya}
\alias{Bhattacharya}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Bhattacharya
}
\description{
Separate size groups of distinct cohorts in the observed length frequency distribution
}
\usage{
Bhattacharya(x, nModes)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
  length frequency
  }
  \item{length_class}{
  length class about the species, anchovy is from 5 to 20 cm LT
  }

  \item{groups}{
  number of groups that you want to separate
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
function (x, nModes) 
{
    require(mixtools)
    x[is.na(x)] = 0
    marcas = seq(5, 20, by = 0.5)
    matriz = as.data.frame(cbind(marcas, x))
    outL = tryCatch({
        normalmixEM(rep(matriz$marcas, matriz$x), k = nModes, 
            maxit = 5000)
    }, error = function(e) {
        message("Not Converge")
        outL <- NULL
        return(outL)
    })
    if (!is.null(outL)) {
        population = sum(x) * outL$lambda
        tableBhatt = data.frame(Group = seq_along(outL$mu), Mean = outL$mu, 
            Population = population)
        plot(outL, which = 2)
        box()
        out = list(tableBhatt = tableBhatt, outL = outL)
    }
    else {
        out = NULL
    }
    return(out)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
