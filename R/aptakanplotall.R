#'@name aptakanplotall
#'@aliases aptakanplotall
#'@title To generate the plots on the basis of different models of lm, loglm, lmrob and loglmrob to
#'analyse the data
#'@description Function \code{aptamodelall} can be implemented on an object of class \code{"aptakandt"},
#'which is produced as an output from the function \code{CombineReps} and saves the resulting
#'plots into a grid format. All the plots of a single model can be generated in a single step,
#'with an appropriate numbering on them. This function \code{aptakanplotall} is based on the function
#'\code{aptakanplot}. See \code{\link[Aptakan]{ReadAptakan}}, \code{\link[AptaKan]{CombineReps}},
#'\code{\link[AptaKan]{aptakanplot}} and \code{\link[ggplot2]{qplot}} also.
#'@param aptakandt an object of class \code{"aptakandt"}, which is an output of the function \code{CombineReps}.
#'@param method The default method is "lm", which is linearised model lm(y~x). The other options are
#'"loglm" for lm(log(y)~x),"roblm" for lmrob(y~x), which is robust linear model and "logroblm" for
#'lmrob(log(y)~x) models.
#'@return plots on the basis of different models of analysis of data.
#'@details Allows the user to generate the plots of analysis of the data on the basis of different models,
#'including linear model, log linear model, robust linear model and log robust linear model. It provides
#'the different values of r-square, adj.rsquare, intercept and slope in case of linear, log linear, robust
#'linear and log robust linear models. This function \code{aptakanplotall} is based on the function
#'\code{aptakanplot}. All the plots of a single model can be generated in a single step, with an appropriate
#'numbering on them. More details are provided in the \code{AptaKan} package vignette.
#'@author Navneet Phogat, Matthias Kohl, \email{Matthias.Kohl@@stamats.de}
#'@keywords aptakandt, ReadAptaKan, CombineReps, aptakanplot, aptakanplotall
#'@examples
#'## Here the examples are shown only with the .csv files and combining replicates on the basis of mean.
#'##Similarly, they can also be implemented on the other results of combining the replicates on the basis of
#'## median too through function CombineReps, For complete detail, see the vignettes
#'##To read the .csv file.
#'file1 <- system.file("exData", "Aptakandata.csv", package = "AptaKan")
#'read.datacsv <- ReadAptakan(file1)
#'
#'## To combine the replicates on the basis of mean
#'cr.mean <- CombineReps(read.datacsv)
#'cr.mean ## To express all the data
#'slot(cr.mean,"crepData")  ##To express the combined replicates
#'
#'##Plots on the basis of linear model
#'aptakanplotall(cr.mean)
#'
#'## Plots on the basis of log linear model
#'aptakanplotall(cr.mean, method = "loglm")
#'
#'## Plots on the basis of robust linear model
#'aptakanplotall(cr.mean, method = "roblm")
#'
#'## Plots on the basis of log robust linear model
#'aptakanplotall(cr.mean, method = "logroblm")
#'@export
setMethod("aptakanplotall", signature = "aptakandt", definition =
            function (aptakandt, method = "lm"){
              cr.rep <- slot(aptakandt,"crepData")
              row.cr <-unique(row.names(cr.rep))
              r.cr <- nrow(cr.rep)
              plots_list<- list()
              for(i in 1:length(row.cr))
              {
                cr.rep[1,] <- cr.rep[i,]
                a.crepdata <- as.data.frame(cr.rep)
                a2 <- new("aptakandt",initialData = as.data.frame(slot(aptakandt,"initialData")),
                          crepData = a.crepdata, concentration = as.data.frame(slot(aptakandt,"concentration")))
                plots_list[[i]] <- aptakanplot(a2, method = method, main = i)
              }
              do.call(grid.arrange,plots_list)
            })
