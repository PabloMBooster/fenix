\name{Bhattacharya}
\alias{Bhattacharya}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Bhattacharya
}
\description{
Analyzes the length frequeney distributions. The middle lengths obtained for each length group
}
\usage{
Bhattacharya(x, length_class = seq(5, 20, by = 0.5), groups, mu = NULL, maxit = 5000)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
  length frequency
  }
  \item{length_class}{
  length class 
  }

  \item{groups}{
  number of groups that you want to separate
  }
  \item{mu}{
  starting value of vector of groups means
  }
  \item{maxit}{
  the maximum number of iterations
  }

}
\details{
If necessary, install mixtools package.
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
plot(length, catch_frequency, type = "l", lwd = 2, ylab = "Catch (kg)", xlab = "Length (cm)")

# bhattacharya
out_cohortes = Bhattacharya(x = catch_frequency, groups = num_modas) 


# rebuild the cohorts
cohortes <- NULL
for(i in 1:length(out_cohortes$tableBhatt$Population)){
  N = out_cohortes$tableBhatt$Population[i]
  L = out_cohortes$tableBhatt$Mean[i]
  S = out_cohortes$outL$sigma[i]

  cohortei = cohort_to_length(cohort = c(N, L, S), lower = 5, upper = 20) 
  cohortes = rbind(cohortes, cohortei)
}

# plots
plot(length, catch_frequency, type = "l", lwd = 2, ylab = "Catch (kg)", xlab = "Length (cm)")
lines(length, cohortes[1,], col = 3, lty = 2)
lines(length, cohortes[2,], col = 3, lty = 2)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
