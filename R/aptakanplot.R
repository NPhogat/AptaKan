#'@name aptakanplot
#'@aliases aptakanplot
#'@title To generate the plots on the basis of different models of lm, loglm, lmrob and loglmrob
#'to analyse the data.
#'@description Function \code{aptakanplot} can be implemented on an object of class \code{"aptakandt"},
#'which is produced as an output from the function \code{CombineReps} and generates the plots on the
#'basis of different models of lm, loglm, lmrob and loglmrob. See \code{\link[Aptakan]{ReadAptakan}} and
#'\code{\link[AptaKan]{CombineReps}}. It's an auxilliary function to \code{aptakanplotall} function. It
#'can be implemented directly through the function \code{aptakanplotall}. See \code{\link[ggplot2]{qplot}}
#'also.
#'@param aptakandt an object of class \code{"aptakandt"}, which is an output of the function \code{CombineReps}.
#'@param method The default method is "lm", which is linearised model lm(y~x). The other options are
#'"loglm" for lm(log(y)~x),"roblm" for lmrob(y~x), which is robust linear model and "logroblm" for
#'lmrob(log(y)~x) to plot the results to analyse the data.
#'@param main The default is "lm(y~x)". The main heading of the plot can be defined by the user.
#'There is no need to define the main argument, While implementing through the function \code{aptakanplotall}.
#'@return plots of the analysis of data
#'@details Allows the user to generate the plots of analysis of the data on the basis of different models,
#'including linear model,log linear model, robust linear model and log robust linear model. It provides
#'the different values of r-square, adj.rsquare, intercept and slope in case of linear, log linear, robust
#'linear and log robust linear models. This function \code{aptakanplot} is an auxilliary function to
#'the function \code{aptakanplotall}. This function \code{aptakanplot} needs not to be implemented alone.
#'It can implemented directly through the \code{aptakanplotall} function. More details are provided in
#'the \code{AptaKan} package vignette.
#'@author Navneet Phogat, Matthias Kohl, \email{Matthias.Kohl@@stamats.de}
#'@keywords aptakandt, ReadAptaKan, CombineReps, aptamodelall, aptakanplot
#'@examples
#'## Here the examples are shown only with the .csv files and combining replicates on the basis of mean.
#'##Similarly, they can also be implemented on the other results of combining the replicates on the basis of
#'## median too through function CombineReps, For complete detail, see the vignettes.
#'##To read the .csv file.
#'file1 <- system.file("exData", "Aptakandata.csv", package = "AptaKan")
#'read.datacsv <- ReadAptakan(file1)
#'
#'## To combine the replicates on the basis of mean
#'cr.mean <- CombineReps(read.datacsv)
#'cr.mean ## To express all the data
#'slot(cr.mean,"crepData")  ##To express the combined replicates
#'
#'##plot on the basis of linear model
#'lm.plot <- aptakanplot(cr.mean)
#'lm.plot
#'
#'##plot on the basis of log linear model
#'loglm.plot <- aptakanplot(cr.mean, method = "loglm", main = "lm(log(Y)~x)")
#'loglm.plot
#'
#'##plot on the basis of robust linear model
#'roblm.plot <- aptakanplot(cr.mean, method = "roblm", main = "lmrob(y~x)")
#'roblm.plot
#'
#'## plot on the basis of log robust linear model
#'logroblm.plot <- aptakanplot(cr.mean, method = "logroblm", main = "lmrob(log(y)~x)")
#'logroblm.plot
#'@export
setMethod("aptakanplot", signature = "aptakandt", definition =
            function (aptakandt, method = "lm", main = "lm(x~y)"){
              crepdata <- as.data.frame(slot(aptakandt,"crepData"))
              conc <- as.data.frame(slot(aptakandt, "concentration"))
              data <- crepdata[1,]
              data <- as.vector(data)
              data.m <- as.matrix(rbind(conc,data))
              x <- data.m[1,]
              y <- data.m[2,]
              lm.coef <- as.data.frame(coef(lm(y~x)))
              loglm.coef <- as.data.frame(coef(lm(log(y)~x)))
              roblm.coef <- as.data.frame(coef(lmrob(y~x)))
              logroblm.coef <- as.data.frame(coef(lmrob(log(y)~x)))

              if (method == "lm"){
                p <-  qplot(x,y, main = main) + geom_point(colour = "blue", na.rm = TRUE)+
                  xlab("Concentration")+ylab("Fluorescence") +
                  geom_abline(intercept = lm.coef[1,], slope = lm.coef[2,], colour = "red", size = 2, na.rm = TRUE)
              }

              else if (method == "loglm"){
                p <- qplot(x,log(y), main = main)+ geom_point(colour = "blue", na.rm = TRUE)+
                  xlab("Concentration")+ylab("log(Fluorescence)")+
                  geom_abline(intercept = loglm.coef[1,], slope = loglm.coef[2,], colour = "red", size = 2)
              }

              else if (method == "roblm"){
                p <- qplot(x,y, main = main) + geom_point(colour = "blue", na.rm = TRUE)+
                  xlab("Concentration")+ylab("Fluorescence")+
                  geom_abline(intercept = roblm.coef[1,], slope = roblm.coef[2,], colour = "red", size =2)
              }

              else if (method == "logroblm"){
                p <- qplot(x,log(y), main = main)+ geom_point(colour = "blue", na.rm = TRUE)+
                  xlab("Concentration")+ylab("log(Fluorescence)")+
                  geom_abline(intercept = logroblm.coef[1,], slope = logroblm.coef[2,], colour = "red", size = 2)
              }


              else{
                warning("Provide an appropriate method!")
              }
              p
            })
