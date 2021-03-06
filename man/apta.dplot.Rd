% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/apta.dplot.R
\docType{methods}
\name{apta.dplot}
\alias{apta.dplot}
\title{To generate the diagnostic plots, based on the function \code{\link[ggplot2]{ggplot}}}
\usage{
\S4method{apta.dplot}{aptakandt}(aptakandt, method = "lm", plottype = "RF",
  title = "Residuals vs Fitted")
}
\arguments{
\item{aptakandt}{an object of Class \code{aptakandt}, which is produced as result after implementing the
function \code{CombineReps}.}

\item{method}{One needs to mention the appropriate argument to implement a particular statistical
model of analysis. "lm" for linear model (lm(y~x)), "loglm" for log linear model (lm(log(y)~x)).
Default method is "lm".}

\item{plottype}{To plot the diagnostic plot "Residuals vs Fitted", mention plottype as "RF", to plot
the "Theoretical Quantiles" vs "Standardised Residuals", mention "QQ", "SL" for "Standardised residuals
vs fitted Value", "Cook" for "Obs.No." vs Cook's distance", "RL" for "Standardised residuals" vs
"Leverage" and "CL" for "Leverage hii" vs "Cook's distance". Default is "RF".}

\item{title}{Mention the title based on the selection of plot. While implementing this function inside the
\code{apta.dplotall}, the title is decided automatically based on the row number of the combined replicates,
obtained after implementing the function \code{CombineReps}.}
}
\value{
Diagnostic plots
}
\description{
The function \code{apta.dplot} can be implemented to plot the diagnostic plots of the
analysis of the data, based on different statistical methods, including, linear model and loglinear model.
The function \code{apta.dplot} acts on an object of Class \code{aptakandt}, produced as a result after
implementing the function \code{CombineReps}. This function is also an auxilliary function to the
function \code{apta.dplotall}.So, the function can be implemented directly through the function
\code{apta.dplotall}. See also \code{\link[Aptakan]{ReadAptakan}}, \code{\link[AptaKan]{CombineReps}},
\code{\link[AptaKan]{apta.dplotall}} and \code{\link[ggplot2]{ggplot}}.
}
\details{
The function \code{apta.dplot} can be implemented to plot the diagnostic plots of the
analysis of the data, based on different statistical models, including, linear model and loglinear model.
The function \code{apta.dplot} acts on the object of Class \code{aptakandt}, produced as a result after
implementing the function \code{CombineReps}. This function is also an auxilliary function to the
function \code{apta.dplotall}.So, the function can be implemented directly through the function
\code{apta.dplotall}. See also \code{\link[Aptakan]{ReadAptakan}}, \code{\link[AptaKan]{CombineReps}},
\code{\link[AptaKan]{apta.dplotall}} and \code{\link[ggplot2]{ggplot}}.
}
\examples{
## Here the examples are shown only with the .csv files and combining replicates on the basis of mean.
##Similarly, they can also be implemented on the other results of combining the replicates on the basis of
## median too through function CombineReps, For complete detail, see the Vignettes.

##To read the .csv file.
file1 <- system.file("exData", "Aptakandata.csv", package = "AptaKan")
read.datacsv <- ReadAptakan(file1)

## To combine the replicates on the basis of mean
cr.mean <- CombineReps(read.datacsv)
cr.mean ## To express all the data
slot(cr.mean,"crepData")  ##To express the combined replicates

##To get first plot
apta.dplot(cr.mean)  ##linear model
apta.dplot(cr.mean, method = "loglm")

## To get second plot
apta.dplot(cr.mean, plottype = "QQ")
apta.dplot(cr.mean, method = "loglm", plottype = "QQ")

## To get third plot
apta.dplot(cr.mean, plottype = "SL")
apta.dplot(cr.mean, method = "loglm", plottype = "SL")

## To get fourth plot
apta.dplot(cr.mean, plottype = "Cook")
apta.dplot(cr.mean, method = "loglm", plottype = "Cook")

## To get fifth plot
apta.dplot(cr.mean, plottype = "RL")
apta.dplot(cr.mean, method = "loglm", plottype = "RL")

## To get sixth plot
apta.dplot(cr.mean, plottype = "CL")
apta.dplot(cr.mean, method = "loglm", plottype = "CL")
}
\author{
Navneet Phogat, Matthias Kohl, \email{Matthias.Kohl@stamats.de}
}
\keyword{CombineReps,}
\keyword{ReadAptaKan,}
\keyword{apta.dplot}
\keyword{aptakandt,}
