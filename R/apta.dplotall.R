#'@name apta.dplotall
#'@aliases apta.dplotall
#'@title To plot all the diagnostic plots, based on the function \code{\link[ggplot2]{ggplot}}
#'@description The function \code{apta.dplotall} can be implemented to plot the diagnostic plots of the
#'analysis of data, based on different statistical methods, including, linear model and loglinear model.
#'The function \code{apta.dplotall} acts on the object of Class \code{aptakandt}, produced as a result after
#'implementing the function \code{CombineReps}. This function is based on the 'function \code{apta.dplot}.
#'See also \code{\link[Aptakan]{ReadAptakan}}, \code{\link[AptaKan]{CombineReps}},
#'\code{\link[AptaKan]{apta.dplot}} and \code{\link[ggplot2]{ggplot}}.
#'@param aptakandt an object of Class \code{aptakandt}, which is produced as a result after implementing the
#'\code{CombineReps} function.
#'@param method One needs to mention the appropriate argument to implement a particular statistical
#'model of analysis. "lm" for linear model (lm(y~x)), "loglm" for log linear model (lm(log(y)~x)).
#'Default method is "lm".
#'@param plottype To plot the diagnostic plot "Residuals vs Fitted", mention plottype as "RF", to plot
#'the "Theoretical Quantiles" vs "Standardised Residuals", mention "QQ", "SL" for "Standardised residuals
#'vs fitted Value", "Cook" for "Obs.No." vs Cook's distance", "RL" for "Standardised residuals" vs
#'"Leverage" and "CL" for "Leverage hii" vs "Cook's distance". Default is "RF".
#'@return all plots of one type, of all the combined replicates, will be returned.
#'@details The function \code{apta.dplot} can be implemented to plot the diagnostic plots of the
#'analysis of data, based on different statistical model, including, linear model and loglinear model.
#'The function \code{apta.dplot} acts on the object of Class \code{aptakandt}, produced as a result after
#'implementing the function \code{CombineReps}. This function is based on the function \code{apta.dplotall}.
#'So, the function can be implemented directly through the function \code{apta.dplotall}.
#'See also \code{\link[Aptakan]{ReadAptakan}}, \code{\link[AptaKan]{CombineReps}}, \code{\link[AptaKan]{apta.dplot}}
#'and \code{\link[ggplot2]{ggplot}}.
#'@author Navneet Phogat, Matthias Kohl, \email{Matthias.Kohl@@stamats.de}
#'@keywords aptakandt, ReadAptaKan, CombineReps, apta.dplotall
#'@examples
#'## Here the examples are shown only with the .csv files and combining replicates on the basis of mean.
#'##Similarly, they can also be implemented on the other results of combining the replicates on the basis of
#'## median too through function CombineReps, For complete detail, see the Vignettes.
#'
#'##To read the .csv file.
#'file1 <- system.file("exData", "Aptakandata.csv", package = "AptaKan")
#'read.datacsv <- ReadAptakan(file1)
#'
#'## To combine the replicates on the basis of mean
#'cr.mean <- CombineReps(read.datacsv)
#'cr.mean ## To express all the data
#'slot(cr.mean,"crepData")  ##To express the combined replicates
#'
#'##To get all the first plot
#'apta.dplotall(cr.mean)  ##linear model
#'apta.dplotall(cr.mean, method = "loglm")
#'
#'## To get all the second plot
#'apta.dplotall(cr.mean, plottype = "QQ")
#'apta.dplotall(cr.mean, method = "loglm", plottype = "QQ")
#'
#'## To get all the third plot
#'apta.dplotall(cr.mean, plottype = "SL")
#'apta.dplotall(cr.mean, method = "loglm", plottype = "SL")
#'
#'## To get all the fourth plot
#'apta.dplotall(cr.mean, plottype = "Cook")
#'apta.dplotall(cr.mean, method = "loglm", plottype = "Cook")
#'
#'## To get all the fifth plot
#'apta.dplotall(cr.mean, plottype = "RL")
#'apta.dplotall(cr.mean, method = "loglm", plottype = "RL")
#'
#'## To get all the sixth plot
#'apta.dplotall(cr.mean, plottype = "CL")
#'apta.dplotall(cr.mean, method = "loglm", plottype = "CL")
#'@export
setMethod("apta.dplotall", signature = "aptakandt", definition =
            function (aptakandt, method = "lm", plottype = "RF"){
              cr.rep <- slot(aptakandt,"crepData")
              row.cr <-unique(row.names(cr.rep))
              r.cr <- nrow(cr.rep)

              plots_list<- list()
              for(i in 1:length(row.cr))
              {
                cr.repplot <- cr.rep[i,]
                plot.crepdata <- as.data.frame(cr.repplot)
                a2.plot <- new("aptakandt", crepData = plot.crepdata, concentration =
                                 as.data.frame(slot(aptakandt,"concentration")))
                plots_list[[i]] <- apta.dplot(a2.plot, method = method, plottype = plottype, title = i)

              }
              do.call(grid.arrange, plots_list)

            })
