\name{bin_length}
\alias{bin_length}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
bin size
}
\description{
Generate different bin size
}
\usage{
bin_length(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{length frequency
}
  \item{Lmin}{minimum length
}
  \item{Lmax}{maximun length
}
  \item{bin}{bin size
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
# Sample (Bonito)
freq <- c(0, 0,0,0,0,0,0,0, 11,9, 18, 23, 20, 28, 43, 37 26,9,0,2,1,0
,4,5,7,4,1,0,0,2,2,7,9,5,2,2,0,2,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0,0,0,0)

len3 <- bin_length(x = xnx, Lmin = 18, Lmax = 75, bin = 3)
len2 <- bin_length(x = xnx, Lmin = 18, Lmax = 75, bin = 2)
len1 <- bin_length(x = xnx, Lmin = 18, Lmax = 75, bin = 1)

seq_len3 <- seq(18, 75, by = 3)
seq_len2 <- seq(18, 75, by = 2)
seq_len1 <- seq(18, 75, by = 1)

plot(seq_len3, len3$freq, type = "l", col = 2, lwd = 2, ylab = "Frequency", xlab = "Length")
lines(seq_len2, len2$freq, col = 3, lwd = 2)
lines(seq_len1, len1$freq, col = 4, lwd = 2)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
