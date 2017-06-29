#'@name apta.diagplot
#'@aliases apta.diagplot
#'@title To plot the diagnostic plots, based on the function \code{\link[graphics]{plot}} of package
#'\code{graphics}
#'@description The function \code{apta.diagplot} can be implemented to plot the diagnostic plots of the
#'analysis of data, based on different statistical methods, including, linear model, loglinear model,
#'robust linear model, robust log linear model and generalised linear model.The function \code{apta.diagplot}
#'acts on an object of Class \code{aptakandt}, produced as a result after implementing the function
#'\code{CombineReps}. See also \code{\link[Aptakan]{ReadAptakan}}, \code{\link[AptaKan]{CombineReps}},
#'\code{\link[AptaKan]{apta.diagplot}} and \code{\link[graphics]{plot}}.
#'@param aptakandt an object of Class \code{aptakandt}, which is produced as result after implementing the
#'\code{CombineReps} function.
#'@param rowno The default is 1. The number is to be decided on the basis of row no. of combined
#'replicates, for which row, the user wants to plot the diagnostic plots.
#'@param method One needs to mention the appropriate argument to implement a particular statistical
#'model of analysis. "lm" for linear model (lm(y~x)), "loglm" for log linear model (lm(log(y~x))),
#'"roblm" for robust linear model (lmrob(y~x)), "logroblm" for robust log linear model (lmrob(log(y)~x))
#'and "glm" for generalised linear model (glm(y~x, family = Gamma(link = log))).Default method is "lm".
#'@param which which plot is to be ploted out of the 6 diagnostic plots. To get all plots together, one
#'can mention the which argument as 1:5 (To get first five diagnostic plots). The maximum number of plots,
#'which can be implemented in case of lm and loglm are 6, while in case of roblm, logroblm and glm are 5.
#'Default is 1:5. See also \code{\link[graphics]{plot}}.
#'@return The diagnostic plots.
#'@details The function \code{apta.diagplot} can be implemented to plot the diagnostic plots of the
#'analysis of data, based on different statistical methods, including, linear model, loglinear model,
#'robust linear model, robust log linear model and generalised linear model.The function \code{apta.diagplot}
#'acts on an object of Class \code{aptakandt}, produced as a result after implementing the function
#'\code{CombineReps}. This function is also an auxilliary function to the function \code{apta.diagplotall}.
#'So, the function can be implemented directly through the function \code{apta.diagplotall}. See also
#'See also \code{\link[Aptakan]{ReadAptakan}}, \code{\link[AptaKan]{CombineReps}},
#'\code{\link[AptaKan]{apta.diagplot}} and \code{\link[graphics]{plot}}. See the details in the vignette of
#'the package \code{AptaKan}.
#'@author Navneet Phogat, Matthias Kohl, \email{Matthias.Kohl@@stamats.de}
#'@keywords aptakandt, ReadAptaKan, CombineReps, apta.diagplotall
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
#'##To plot on the basis of linear model for row no. 1
#'apta.diagplot(cr.mean, which = 1:6)
#'
#'## To plot on the basis of log linear model for row no. 2
#'apta.diagplot(cr.mean, rowno = 2, method = "loglm", which = 1:6)
#'
#'## To plot on the basis of robust linear model for row no. 1
#'apta.diagplot(cr.mean, method = "roblm", which = 1:5)
#'
#'## To plot on the basis of log robust linear model for row no. 5
#'apta.diagplot(cr.mean, rowno = 5, method = "logroblm", which = 1:5)
#'
#'## To plot on the basis of generalised linear model for row no. 3
#'apta.diagplot(cr.mean, rowno = 3, method = "glm", which = 1:5)
#'@export
setMethod("apta.diagplot", signature = "aptakandt", definition =
            function (aptakandt, rowno =1, method = "lm", which = 1:5){
              crepdata <- as.data.frame(slot(aptakandt,"crepData"))
              conc <- as.data.frame(slot(aptakandt, "concentration"))
              data.new <- crepdata[rowno,]
              data.kd <- as.vector(data.new)
              data.m <- as.matrix(rbind(conc,data.kd))
              x <- data.m[1,]
              y <- data.m[2,]

              if (method == "lm"){
                model <- lm(y~x)
              }

              else if (method == "loglm"){
                model <- lm(log(y)~x)
              }
              else if(method == "roblm"){
                model <- lmrob(y~x)
              }

              else if(method == "logroblm"){
                model <- lmrob(log(y)~x)
              }

              else if(method == "glm"){
                model <- glm(y~x,family = Gamma(link= log))
              }
              else{
                stop("Please provide the method out of lm, loglm, roblm and logroblm!")
              }
              par(mfrow = c(2,3))
              plot(model,which = which)
            })

