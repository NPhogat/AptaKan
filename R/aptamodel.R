#'@name aptamodel
#'@aliases aptamodel
#'@title To implement the different models of lm, loglm, lmrob, loglmrob and glm to analyse the data
#'@description Function \code{aptamodel} can be implemented on an object of Class \code{aptakandt},
#'which is produced as an output from the function \code{CombineReps} and saves the resulting
#'data to an object of Class \code{data.frame}. See \code{\link[Aptakan]{ReadAptakan}} and
#'\code{\link[AptaKan]{CombineReps}}. It's an auxilliary function to the function \code{aptamodelall}.
#'@param aptakandt an object of class \code{"aptakandt"}, which is an output of the function \code{CombineReps}.
#'@param method The default method is "lm", which is linearised model lm(y~x). The other options are
#'"loglm" for lm(log(y)~x),"roblm" for lmrob(y~x), which is robust linear model, "logroblm" for
#'lmrob(log(y)~x) and "glm", which is generalised linear model, which includes the concept of gamma
#'distribution to analyse the data.
#'@param result The default is "lm". The other option is "glm". If the method are out of "lm", "loglm",
#'"roblm" and "logroblm", then, result should be defined as "lm". If the method is "glm", then, the
#'result should be defined as "glm".
#'@return object of Class \code{data.frame}.
#'@details Allows the user to analyse the data on the basis of different models, including linear model,
#'log linear model, robust linear model, log robust minear model and generalised linear model. It provides
#'the different values of r-square, adj.rsquare, intercept and slope in case of linear, log linear, robust
#'linear and log robust linear models as well as the values of AIC, intercept and slope in case of generalised
#'linear model. It saves the results in slot modelData of an object of Class \code{aptakandt}.
#'This function \code{aptamodel} is an auxilliary function to the function \code{aptamodelall}.
#'More details are provided in the\code{AptaKan} package vignette.
#'@author Navneet Phogat, Matthias Kohl, \email{Matthias.Kohl@@stamats.de}
#'@keywords aptakandt, ReadAptaKan, CombineReps, aptamodel
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
#'cr.mean ## To express the combined replicates
#'
#'##To analyse the data on the basis of linear model
#'lm.result <- aptamodel(cr.mean)
#'lm.result ## To express the result
#'
#'## To analyse the data on the basis of log linear model
#'loglm.result <- aptamodel(cr.mean, method = "loglm")
#'loglm.result ##To express the result
#'
#'## To analyse the data on the basis of robust linear model
#'roblm.result <- aptamodel(cr.mean, method = "roblm")
#'roblm.result ##To express the result
#'
#'## To analyse the data on the basis of log robust linear model
#'logroblm.result <- aptamodel(cr.mean, method = "logroblm")
#'logroblm.result ##To express the result
#'
#'## To analyse the data on the basis of generalised linear model
#'glm.result <- aptamodel(cr.mean, method = "glm", result = "glm")
#'glm.result  ##To express the result
#'@export
setMethod("aptamodel", signature = "aptakandt", definition =
            function (aptakandt, method = "lm", result = "lm"){
              crepdata <- as.data.frame(slot(aptakandt,"crepData"))
              conc <- as.data.frame(slot(aptakandt, "concentration"))
              data <- crepdata[1,]
              data <- as.vector(data)
              data.m <- as.matrix(rbind(conc,data))
              x <- data.m[1,]
              y <- data.m[2,]

              if (result == "lm")
              {
                if (method == "lm")
                {
                  lm <- lm(y~x)
                  l1 <- summary(lm)$r.squared
                  l2 <- summary(lm)$adj.r.squared
                  l3 <- coef(summary(lm))[1,1]
                  l4 <- coef(summary(lm))[2,1]
                  #l5 <- paste("lm")
                }
                else if (method == "loglm")
                {
                  loglm <- lm(log(y)~x)
                  l1 <- summary(loglm)$r.squared
                  l2 <- summary(loglm)$adj.r.squared
                  l3 <- coef(summary(loglm))[1,1]
                  l4 <- coef(summary(loglm))[2,1]
                }
                else if (method == "roblm")
                {
                  roblm <- lmrob(y~x)
                  l1 <- summary(roblm)$r.squared
                  l2 <- summary(roblm)$adj.r.squared
                  l3 <- coef(summary(roblm))[1,1]
                  l4 <- coef(summary(roblm))[2,1]
                }
                else
                {
                  logroblm <- lmrob(log(y)~x)
                  l1 <- summary(logroblm)$r.squared
                  l2 <- summary(logroblm)$adj.r.squared
                  l3 <- coef(summary(logroblm))[1,1]
                  l4 <- coef(summary(logroblm))[2,1]
                }

                df <- as.data.frame(cbind(l1,l2,l3,l4))
                names(df) <- c("rsquare", "adj.rsquare","intercept","slope")
              }

              else if ((result == "glm")&&(method == "glm"))
              {
                glm <- glm(y~x,family = Gamma(link= log))
                gl1 <- AIC(glm)
                gl2 <- coef(summary(glm))[1,1]
                gl3 <- coef(summary(glm))[2,1]

                df <- as.data.frame(cbind(gl1,gl2,gl3))
                names(df) <- c("AIC","intercept","slope")
              }
              df
            }
)
