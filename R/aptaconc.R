#'@name aptaconc
#'@aliases aptaconc
#'@title To compute the concentration from fluorescence
#'@description Function \code{aptaconc} can be implemented on an object of class \code{"aptakandt"},
#'which is produced as an output from the function \code{aptamodelall} and saves the results into
#'\code{data.frame}. See \code{\link[Aptakan]{ReadAptakan}}, \code{\link[AptaKan]{CombineReps}} and
#'\code{\link[AptaKan]{aptamodel}} and \code{\link[AptaKan]{aptamodelall}} also.
#'@param aptakandt an object of class \code{"aptakandt"}, which is an output of the function \code{aptamodelall}.
#'@param fluo fluorescence value for which the concentration is to be calculated. It should be a numeric
#'value provided by the user.
#'@param model "lm" for linear model and robust linear model, while "loglm" for log linear model and robust log
#'linear model.
#'@return an object of Class \code{data.frame}.
#'@details Function \code{aptaconc} computes the concentration of the combined replicates which are produced
#'as a result after implementing the function \code{aptamodelall}. One needs to provide fluorescence for which
#'the concentration is to be calculated.
#'@author Navneet Phogat, Matthias Kohl, \email{Matthias.Kohl@@stamats.de}
#'@keywords aptakandt, ReadAptaKan, CombineReps, aptakan.conc
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
#'slot(lm.result,"modelData") ##to express the modelData
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
#'###Call the function aptaconc
#'apta.conc.lm <- aptaconc(lm.result,fluo = 1000)
## To express the result
#'apta.conc.lm
#'apta.conc.loglm <- aptaconc(loglm.result,fluo = 1019, model = "loglm")
#'apta.conc.loglm
#'apta.conc.roblm <- aptaconc(roblm.result,fluo = 1019, model = "lm")
#'apta.conc.roblm
#'apta.conc.logroblm <- aptaconc(logroblm.result,fluo = 1019, model = "loglm")
#'apta.conc.logroblm
#'@export
setMethod("aptaconc", signature = "aptakandt", definition =
            function(aptakandt,fluo, model = "lm"){
              if(!(is.numeric(fluo))){
                warning("fluo value should be numeric")
              }
              else{
                x <- as.data.frame(slot(aptakandt,"modelData"))

                  rmax <- (max(x[,"rsquare"]))
                  r1 <- x[which(x$rsquare == rmax), ]
                intercept <- r1[,"intercept"]
                slope <- r1[,"slope"]
                switch(model,
                       lm = ((fluo-intercept)/slope),
                       loglm = ((log(fluo)-intercept)/slope)

                )
              }
            }
)

