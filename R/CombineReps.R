#'@name CombineReps
#'@aliases CombineReps
#'@title To combine the technical replicates
#'@description Function \code{CombineReps} combines the technical replicates (on the basis of mean or
#'median and their standard deviation) of an object of class \code{"aptakandt"}, which is produced as
#'an output from the function \code{ReadAptakan} and saves the resulting data to an object of
#'class \code{"aptakandt"}. See also See \code{\link[Aptakan]{ReadAptakan}} function.
#'@param aptakandt an object of class \code{"aptakandt"}, which is an output of the function \code{ReadAptakan}.
#'@param calc the mathematical method out of "Mean" or "Median" to combine the technical replicates
#'and "sd" to compute the standard deviation of the replicates. The default method is "Mean".
#'@return object of Class \code{"aptakandt"}, where the result of combined replicates will be saved in the
#'slot repData.
#'@details Allows the user to combine technical replicates on the basis of mean and median as well as to
#'calculate the standard deviation within the replicates. More details are provided in the
#'\code{AptaKan} package vignette.
#'@author Navneet Phogat, Matthias Kohl, \email{Matthias.Kohl@@stamats.de}
#'@keywords aptakandt
#'@examples
#'##To read the .csv file
#'file1 <- system.file("exData", "Aptakandata.csv", package = "AptaKan")
#'read.datacsv <- ReadAptakan(file1)
#'
#'## To express all the data
#'read.datacsv
#'
#'## To read the .txt file
#'file1 <- system.file("exData", "Aptakandata.txt", package = "AptaKan")
#'read.datatxt <- ReadAptakan(file1, type = ".txt")
#'
#'## To express all the data
#'read.datatxt
#'
#'## To combine the replicates on the basis of mean
#'cr.mean <- CombineReps(read.datacsv)
#'cr.mean ## To express all the data
#'slot(cr.mean,"crepData")  ##To express the combined replicates
#'
#'cr.mean.txt <- CombineReps(read.datatxt)
#'cr.mean.txt
#'slot(cr.mean.txt,"crepData")
#'
#'##To combine the replicates on the basis of meadian
#'cr.median <- CombineReps(read.datacsv, calc = "Median")
#'cr.median
#'slot(cr.median,"crepData")
#'
#'##To compute the standard deviation within the replicates
#'cr.sd <- CombineReps(read.datacsv, calc = "sd")
#'cr.sd ##To express all the data
#'slot(cr.sd,"crepData") ##To express the standard deviation
#'@export
setMethod("CombineReps", signature = "aptakandt", definition =
            function (aptakandt, calc = "Mean"){

              res <- slot(aptakandt,"initialData")
              res1 <- res[-c(1,ncol(res))]
              res3 <- as.matrix(res1[-1])
              row.names(res3) <- res1[,"Replicate"]
              x.rep.row <- res1[,"Replicate"]
              x.rep.fac <- factor(x.rep.row, levels = unique(x.rep.row))
              x.rep.unique <- unique(x.rep.row)
              newresM <- matrix(NA,nrow=length(x.rep.unique),ncol(res3))
              dimnames(newresM) <- list(x.rep.unique, colnames(res3))
              if (calc == "Mean"){
                for (i in 1:ncol(res3))
                {
                  res.col <- (res3[,i])
                  res.col.split <- lapply(split(res.col,x.rep.fac), mean, na.rm = TRUE)
                  newresM[,i] <- unlist(res.col.split)
                }
              }
              else if (calc == "Median")
              {
                for (i in 1:ncol(res3))
                {
                  res.col <- (res3[,i])
                  res.col.split <- lapply(split(res.col,x.rep.fac), median, na.rm = TRUE)
                  newresM[,i] <- unlist(res.col.split)
                }
              }

              else if (calc == "sd")
              {
                for (i in 1:ncol(res3))
                {
                  res.col <- (res3[,i])
                  res.col.split <- lapply(split(res.col,x.rep.fac), sd, na.rm = TRUE)
                  newresM[,i] <- unlist(res.col.split)
                }
              }

              else {
                warning("Please provide the appropriate method for computation!")
              }
              repdata <- as.data.frame(newresM)
              conc <- slot(aptakandt,"concentration")
              rep.data <- new("aptakandt",initialData = res, crepData = repdata,concentration = conc)
              rep.data
            }
)
