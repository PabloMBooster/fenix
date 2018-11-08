\name{mapa_peru}
\alias{mapa_peru}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Map of Peru}
\description{
generate a base map from Peru
}
\usage{
mapa_peru()
}
\arguments{
  \item{xlim}{
  is the limit of the longitude}

  \item{ylim}{
  is the limit of the latitude}

  \item{labelsxy}{
  put name longitude and latitude, by default is TRUE}

  \item{ylab}{
  name of the label y, by default is latitud}

  \item{xlab}{
  name of the label x, by default is longitud}

  \item{all_axis}{
  adds 4 axis to the current plot, by default add 2 axis}

  \item{land.col}{
  color of the land}

  \item{sea.col}{
  color or the sea}

  \item{names_harbor}{
  put names of the harbor}

  \item{cex_harbor}{
  font size of the harbor}

  \item{add}{
  add other plot}

  \item{n_perfil}{
  number of perfil, by default only 1}

  \item{space_perfil}{
  space between perfil}
}
\details{
using \code{kali package} and generate a base map from Peru. You need to install \code{kali package} from https://github.com/roliveros-ramos.

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
mapa_peru()
points(dataLand$lon, dataLand$lat)

space_perfil <- 3
mapa_peru(n_perfil = 2, space_perfil = space_perfil)
points(dataLand$Longitud, dataLand$Latitud)
points(dataLand$Longitud-space_perfil, dataLand$Latitud)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
