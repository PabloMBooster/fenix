\name{Bhattacharya2}
\alias{Bhattacharya2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Bhattacharya
}
\description{
Analyzes the length frequeney distributions. The middle lengths obtained for each length group
}
\usage{
Bhattacharya2(x, length_class = seq(5, 20, by = 0.5), mixpar, plot = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{length_class}{
%%     ~~Describe \code{length_class} here~~
}
  \item{mixpar}{
%%     ~~Describe \code{mixpar} here~~
}
  \item{plot}{
%%     ~~Describe \code{plot} here~~
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
# Sample (Peruvian anchovy)
sample_frequency <- c(0,0,0,0,0,0,0,0,0,2,7,14,22,15,13,12,17,22,35,28,7,4,1,0,0,0,0,0,0,0,0)
length <- seq(5,20, by = 0.5)

# identify groups
view_modas <- modas(matrix(sample_frequency, nrow = 1), Lmin = 5, Lmax = 20, dL = 0.5, umbral = 10) 

# number of groups
num_modas = apply(view_modas, 1, unique_modas)


# weighted
catch_frequency = weighted_frequency(length = length, frequency = sample_frequency, a = 0.003925, b = 3.2178, catch =  50000) 
plot(length, catch_frequency, type = "l", lwd = 2, ylab = "Catch (number of individuals)", xlab = "Length (cm)")

# par
sigma  = seq(2, 2+groups-1, by = 1)
pi     = rep(1/groups, groups)
dfPars = data.frame(pi = pi, mu = mu, sigma = sigma)
mu     = sort(view_modas[!is.na(view_modas)])

# bhattacharya
out_cohortes = Bhattacharya2(x, length_class = seq(5, 20, by = 0.5), mixpar = dfPars, plot = FALSE) 



}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
