\name{plot_repro}
\alias{plot_repro}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plots of reproductive indices
}
\description{
Generate plots about (IGS, IA, FD, CG y FC)
}
\usage{
plot_repro(tiempo, indice, patron, sd_patron, valor_crucero = NULL, nombre_indice = NULL, nombre_indice_legenda = NULL, nombre_patron = NULL, nombre_crucero = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tiempo}{
    time
  }
  \item{indice}{
    the index that is going to plot (IGS, IA, CG, FC o FD)
  }
  \item{patron}{
    the pattern of each reproductive index
  }
  \item{sd_patron}{
    the standard deviation of each reproductive index
  }
  \item{valor_crucero}{
   the value of the reproductive index obtained from the survey
  }
  \item{nombre_indice}{
    name of the reproductive index
  }
  \item{nombre_indice_legenda}{
    name of the reproductive index that will go in the legend of the plot
  }
  \item{nombre_patron}{
   name of the pattern of the reproductive index that will go in the legend of the plot by default is "pattern"
  }
  \item{nombre_crucero}{
  name of the survey that will go in the legend of the plot
  }
  \item{axis_x}{
  put axis x, by default is TRUE
  }
  \item{dl}{
  increases the maximun limit to ylim
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
##################################################################################
require(plotrix)
head(dataRepro)
dataRepro$fecha <- as.Date(dataRepro$fecha, format = "\%d/\%m/\%Y")

# plots

  plot_repro(tiempo = dataRepro$fecha, indice = dataRepro$IGS, patron = dataRepro$IGS_PATRON, sd_patron = dataRepro$IGS_SD, valor_crucero = 5, 
             nombre_indice = "IGS", nombre_indice_legenda = "IGS", nombre_patron = "Patron", nombre_crucero = "Cr_1802_04")
  
  plot_repro(tiempo = dataRepro$fecha, indice = dataRepro$FD, patron = dataRepro$FD_PATRON, sd_patron = dataRepro$FD_SD, valor_crucero = NULL, 
             nombre_indice = "FD", nombre_indice_legenda = "FD", nombre_patron = "Patron", nombre_crucero = NULL)
  
  plot_repro(tiempo = dataRepro$fecha, indice = dataRepro$CG, patron = dataRepro$CG_SD, sd_patron = dataRepro$CG_SD, valor_crucero = NULL, 
             nombre_indice = "CG", nombre_indice_legenda = "CG", nombre_patron = "Patron", nombre_crucero = NULL)
  
  plot_repro(tiempo = dataRepro$fecha, indice = dataRepro$IA, patron = dataRepro$IA_PATRON, sd_patron = dataRepro$IA_SD, valor_crucero = NULL, 
             nombre_indice = "IA", nombre_indice_legenda = "IA", nombre_patron = "Patron", nombre_crucero = NULL)
  
  plot_repro(tiempo = dataRepro$fecha, indice = dataRepro$FC, patron = dataRepro$FC_PATRON, sd_patron = dataRepro$FC_SD, valor_crucero = NULL, 
             nombre_indice = "FC", nombre_indice_legenda = "FC", nombre_patron = "Patron", nombre_crucero = NULL)

  
## ASPECTOS REPRODUCTIVOS
png("aspectos_reproductivos.png", width = 1600, height = 800, res = 150)

layout(matrix(c(1,2,3), 3, 1, byrow = TRUE), heights = c(0.85,0.85,1))
par(mar = c(0.5,4,0,0))
plot_repro(tiempo = dataRepro$fecha, indice = dataRepro$IGS, patron = dataRepro$IGS_PATRON, sd_patron = dataRepro$IGS_SD, valor_crucero = 5, 
           nombre_indice = "IGS", nombre_indice_legenda = "IGS", nombre_patron = "Patron", nombre_crucero = "Cr_1802_04", axis_x = FALSE)
legend("topleft", legend = "a)",bty = "n")
par(mar = c(0.5,4,0,0))
plot_repro(tiempo = dataRepro$fecha, indice = dataRepro$FD, patron = dataRepro$FD_PATRON, sd_patron = dataRepro$FD_SD, valor_crucero = NULL, 
           nombre_indice = "FD", nombre_indice_legenda = "FD", nombre_patron = "Patron", nombre_crucero = NULL, axis_x = FALSE)
legend("topleft", legend = "b)",bty = "n")
par(mar = c(4,4,0,0))
plot_repro(tiempo = dataRepro$fecha, indice = dataRepro$IA, patron = dataRepro$IA_PATRON, sd_patron = dataRepro$IA_SD, valor_crucero = NULL, 
           nombre_indice = "IA", nombre_indice_legenda = "IA", nombre_patron = "Patron", nombre_crucero = NULL)
legend("topleft", legend = "c)",bty = "n")

dev.off()

## CONDICION SOMATICA
png("condicion_somatica.png", width = 1600, height = 800, res = 150)

layout(matrix(c(1,2), 2, 1, byrow = TRUE), heights = c(0.85,1))
par(mar = c(0.5,4,0,0))
plot_repro(tiempo = dataRepro$fecha, indice = dataRepro$FC, patron = dataRepro$FC_PATRON, sd_patron = dataRepro$FC_SD, valor_crucero = NULL, 
           nombre_indice = "FC", nombre_indice_legenda = "FC", nombre_patron = "Patron", nombre_crucero = NULL,  axis_x = FALSE)
legend("topleft", legend = "a)",bty = "n")
par(mar = c(4,4,0,0))
plot_repro(tiempo = dataRepro$fecha, indice = dataRepro$CG, patron = dataRepro$CG_SD, sd_patron = dataRepro$CG_SD, valor_crucero = NULL, 
           nombre_indice = "CG", nombre_indice_legenda = "CG", nombre_patron = "Patron", nombre_crucero = NULL)
legend("topleft", legend = "b)",bty = "n")             
dev.off()
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
