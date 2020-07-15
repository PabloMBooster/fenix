\name{TallasSeg_mapaIso}
\alias{TallasSeg_mapaIso}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Mapear tallas y captura por areas iso
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
TallasSeg_mapaIso(DataTallas, DataTallasLat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{DataTallas}{
%%     ~~Describe \code{DataTallas} here~~
}
  \item{DataTallasLat}{
%%     ~~Describe \code{DataTallasLat} here~~
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
head(DataTallas) 
DataTallasLat   <- TallasSeg_lat(DataTallas)
DataTallasTemp  <- TallasSeg_temp(DataTallas, captura_total = 1000000)
# map
TallasSeg_mapaIso(DataTallas, DataTallasLat)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
