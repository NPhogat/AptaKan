#'@name apta.conf
#'@aliases apta.conf
#'@title To compute confidence interval of the combined replicates
#'@description Function \code{apta.conf} can be implemented on an object of class \code{"aptakandt"},
#'which is produced as an output from the function \code{CombineReps} and saves the results into a new
#'slot confData of class \code{"aptakandt"}. See \code{\link[AptaKan]{ReadAptakan}} and
#'\code{\link[AptaKan]{CombineReps}} also.
#'@param aptakandt an object of class \code{"aptakandt"}, which is an output of the function \code{CombineReps}.
#'@details  Function \code{apta.conf} computes the 95 percent confidence intervals of the combined replicates
#'which are produced as a result after implementing the function \code{CombineReps}. More details are
#'provided in the \code{AptaKan} package vignette.
#'@return The object of the Class \code{aptakandt}, where the result will be saved in the slot confData.
#'@author Navneet Phogat, Matthias Kohl, \email{Matthias.Kohl@@stamats.de}
#'@keywords aptakandt, ReadAptaKan, CombineReps, aptakan.conf
#'@examples
#'## Here are the examples are shown only with the .csv files and combining replicates on the basis of mean.
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
#'## Compute confidence interval
#'conf <- apta.conf(cr.mean)
#'conf ##To visualise all the data
#'slot(conf,"confData") ## to express the confidence intervals
#'@export
setGeneric("apta.conf", function (aptakandt,...) standardGeneric("apta.conf"))
setMethod("apta.conf", signature = "aptakandt", definition =
            function(aptakandt){
              initdata <- as.data.frame(slot(aptakandt,"initialData"))
              crepdata <- as.data.frame(slot(aptakandt,"crepData"))
              conc <- as.data.frame(slot(aptakandt, "concentration"))
              data <- slot(aptakandt,"crepData")
              data.new <- as.data.frame(data)
              row.cr <-unique(row.names(data.new))
              r.cr <- nrow(data.new)
              d = data.frame(min.value=rep(0,r.cr), max.value=rep(0,r.cr))
              for (i in 1:length(row.cr)){
                x <- data.new[i,]
                x.new <- (x[!is.na(x)])
                sd <- sd(x.new)
                se <- sd/(sqrt(length(x.new)))
                x.new <- as.numeric(x.new)
                x.mean <- mean(x.new)
                CI <- c((x.mean - 2*se), (x.mean + 2*se))
                d[i,] <- CI
              }
              conf.data <- new("aptakandt",initialData = initdata, concentration = conc,
                               crepData = crepdata,confData = d)
              conf.data
            })
