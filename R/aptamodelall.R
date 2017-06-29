#'@name aptamodelall
#'@aliases aptamodelall
#'@title To implement the different models of lm, loglm, lmrob, loglmrob and glm to analyse the data
#'@description Function \code{aptamodelall} is based on the function \code{aptamodel} and can be
#'implemented on an object of class \code{"aptakandt"}, which is produced as an output from
#'the function \code{CombineReps} and saves the resulting data to an object of class \code{"aptakandt"}.
#'See also \code{\link[Aptakan]{ReadAptakan}}, \code{\link[AptaKan]{CombineReps}} and \code{\link[AptaKan]{aptamodel}}.
#'@param aptakandt an object of class \code{"aptakandt"}, which is an output of the function \code{CombineReps}.
#'@param method The default method is "lm", which is linearised model lm(y~x). The other options are
#'"loglm" for lm(log(y)~x),"roblm" for lmrob(y~x), which is robust linear model, "logroblm" for
#'lmrob(log(y)~x) and "glm", which is generalised linear model, which includes the concept of gamma
#'distribution to analyse the data.
#'@param result The default is "lm". The other option is "glm". If the method are out of "lm", "loglm",
#'"roblm" and "logroblm", then, result should be defined as "lm". If the method is "glm", then, the
#'result should be defined as "glm".
#'@return object of Class \code{"aptakandt"}, where results will be saved in modelData slot.
#'@details Allows the user to analyse the data on the basis of different models, including linear model,
#'log linear model, robust linear model, log robust linear model and generalised linear model. It provides
#'the different values of r-square, adj.rsquare, intercept and slope in case of linear, log linear, robust
#'linear and log robust linear models as well as the values of AIC, intercept and slope, in case of generalised
#'linear model. It saves the results in slot modelData of an object of Class \code{"aptakandt"}.
#'This function \code{aptamodelall} is based on \code{aptamodel} function. More details are provided
#'in the\code{AptaKan} package vignette.
#'@author Navneet Phogat, Matthias Kohl, \email{Matthias.Kohl@@stamats.de}
#'@keywords aptakandt, ReadAptaKan, CombineReps, aptamodel, aptamodelall
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
#'##To analyse the data on the basis of linear model
#'lm.result <- aptamodelall(cr.mean)
#'lm.result ## To express all the slots
#'slot(lm.result,"modelData") ##to express the modelData, which contains the final result
#'
#'## To analyse the data on the basis of log linear model
#'loglm.result <- aptamodelall(cr.mean, method = "loglm")
#'loglm.result
#'slot(loglm.result,"modelData")
#'
#'## To analyse the data on the basis of robust linear model
#'roblm.result <- aptamodelall(cr.mean, method = "roblm")
#'roblm.result
#'slot(roblm.result,"modelData")
#'
#'## To analyse the data on the basis of log robust linear model
#'logroblm.result <- aptamodelall(cr.mean, method = "logroblm")
#'logroblm.result
#'slot(logroblm.result,"modelData")
#'
#'## To analyse the data on the basis of generalised linear model
#'glm.result <- aptamodelall(cr.mean, method = "glm", result = "glm")
#'glm.result
#'slot(glm.result,"modelData")
#'@export
setMethod("aptamodelall", signature = "aptakandt", definition =
            function (aptakandt, method = "lm", result = "lm"){
              cr.rep <- slot(aptakandt,"crepData")
              row.cr <-unique(row.names(cr.rep))
              r.cr <- nrow(cr.rep)

              if (result == "lm")
              {
                d = data.frame(rsquare=rep(0,r.cr), adj.rsquare=rep(0,r.cr), intercept=rep(0,r.cr),
                               slope = rep(0,r.cr))
                for(i in 1:length(row.cr))
                {
                  cr.rep.data <- cr.rep[i,]
                  a.crepdata <- as.data.frame(cr.rep.data)
                  a2 <- new("aptakandt",crepData = a.crepdata, concentration =
                              as.data.frame(slot(aptakandt,"concentration")))
                  b <- aptamodel(a2, method = method, result = result)
                  d[i,] <-b
                }
              }
              else if (result == "glm")
              {
                d = data.frame(AIC=rep(0, r.cr), intercept=rep(0,r.cr),
                               slope = rep(0,r.cr))
                for(i in 1:length(row.cr))
                {
                  cr.rep.data <- (cr.rep[i,])
                  a.crepdata <- as.data.frame(cr.rep.data)
                  a2 <- new("aptakandt",crepData = a.crepdata, concentration =
                              as.data.frame(slot(aptakandt,"concentration")))
                  b <- aptamodel(a2, method = "glm", result = result)
                  d[i,] <-b
                }
              }
              else {
                warning("please provide arguments aprropriately!")
              }
              repdata <- as.data.frame(slot(aptakandt,"crepData"))
              conc <- as.data.frame(slot(aptakandt,"concentration"))
              test.data <- new("aptakandt", crepData = repdata,concentration = conc,
                               modelData = d)
              test.data
            }
)
