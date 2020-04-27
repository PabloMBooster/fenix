\name{read.biom}
\alias{read.biom}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Leer datos biometricos de crucero
}
\description{
Leer y analisis rapido de los datos biometricos de los cruceros pelagicos
}
\usage{
read.biom(data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
datos biometricos
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
head(dataBiometria)
dataB <- read.biom(data = dataBiometria)

biom_survey_map_especie(baseDat = dataB, save = F)
biom_survey_map_compespecie(baseDat = dataB, save = F)
biom_survey_map_juveniles(baseDat = dataB, save = F)
biom_survey_map_midlen(baseDat = dataB, save = F)
biom_survey_map_modas(baseDat = dataB, save = F)
biom_survey_map_modas_num(baseDat = dataB, save = F, Legend = F)
biom_survey_map_modas_num(baseDat = dataB, save = F, Legend = F)
biom_survey_map_lances(baseDat = dataB, save = F)
}
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
