#'@name apta.dplot
#'@aliases apta.dplot
#'@title To generate the diagnostic plots, based on the function \code{\link[ggplot2]{ggplot}}
#'@description The function \code{apta.dplot} can be implemented to plot the diagnostic plots of the
#'analysis of the data, based on different statistical methods, including, linear model and loglinear model.
#'The function \code{apta.dplot} acts on an object of Class \code{aptakandt}, produced as a result after
#'implementing the function \code{CombineReps}. This function is also an auxilliary function to the
#'function \code{apta.dplotall}.So, the function can be implemented directly through the function
#'\code{apta.dplotall}. See also \code{\link[Aptakan]{ReadAptakan}}, \code{\link[AptaKan]{CombineReps}},
#'\code{\link[AptaKan]{apta.dplotall}} and \code{\link[ggplot2]{ggplot}}.
#'@param aptakandt an object of Class \code{aptakandt}, which is produced as result after implementing the
#'function \code{CombineReps}.
#'@param method One needs to mention the appropriate argument to implement a particular statistical
#'model of analysis. "lm" for linear model (lm(y~x)), "loglm" for log linear model (lm(log(y)~x)).
#'Default method is "lm".
#'@param plottype To plot the diagnostic plot "Residuals vs Fitted", mention plottype as "RF", to plot
#'the "Theoretical Quantiles" vs "Standardised Residuals", mention "QQ", "SL" for "Standardised residuals
#'vs fitted Value", "Cook" for "Obs.No." vs Cook's distance", "RL" for "Standardised residuals" vs
#'"Leverage" and "CL" for "Leverage hii" vs "Cook's distance". Default is "RF".
#'@param title Mention the title based on the selection of plot. While implementing this function inside the
#'\code{apta.dplotall}, the title is decided automatically based on the row number of the combined replicates,
#'obtained after implementing the function \code{CombineReps}.
#'@return Diagnostic plots
#'@details The function \code{apta.dplot} can be implemented to plot the diagnostic plots of the
#'analysis of the data, based on different statistical models, including, linear model and loglinear model.
#'The function \code{apta.dplot} acts on the object of Class \code{aptakandt}, produced as a result after
#'implementing the function \code{CombineReps}. This function is also an auxilliary function to the
#'function \code{apta.dplotall}.So, the function can be implemented directly through the function
#'\code{apta.dplotall}. See also \code{\link[Aptakan]{ReadAptakan}}, \code{\link[AptaKan]{CombineReps}},
#'\code{\link[AptaKan]{apta.dplotall}} and \code{\link[ggplot2]{ggplot}}.
#'@author Navneet Phogat, Matthias Kohl, \email{Matthias.Kohl@@stamats.de}
#'@keywords aptakandt, ReadAptaKan, CombineReps, apta.dplot
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
#'##To get first plot
#'apta.dplot(cr.mean)  ##linear model
#'apta.dplot(cr.mean, method = "loglm")
#'
#'## To get second plot
#'apta.dplot(cr.mean, plottype = "QQ")
#'apta.dplot(cr.mean, method = "loglm", plottype = "QQ")
#'
#'## To get third plot
#'apta.dplot(cr.mean, plottype = "SL")
#'apta.dplot(cr.mean, method = "loglm", plottype = "SL")
#'
#'## To get fourth plot
#'apta.dplot(cr.mean, plottype = "Cook")
#'apta.dplot(cr.mean, method = "loglm", plottype = "Cook")
#'
#'## To get fifth plot
#'apta.dplot(cr.mean, plottype = "RL")
#'apta.dplot(cr.mean, method = "loglm", plottype = "RL")
#'
#'## To get sixth plot
#'apta.dplot(cr.mean, plottype = "CL")
#'apta.dplot(cr.mean, method = "loglm", plottype = "CL")
#'@export
setMethod("apta.dplot", signature = "aptakandt", definition =
            function (aptakandt, method = "lm", plottype = "RF", title = "Residuals vs Fitted"){
              crepdata <- as.data.frame(slot(aptakandt,"crepData"))
              conc <- as.data.frame(slot(aptakandt, "concentration"))
              data.new <- crepdata[1,]
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

              else {
                stop("Please provide the appropriate method out of lm, loglm, roblm and logroblm!")
              }

              if (plottype == "RF"){
                plot <- ggplot(model, aes(.fitted, .resid)) + geom_point()
                plot <- plot + stat_smooth(method="loess") +
                  geom_hline(yintercept=0, col="red", linetype="dashed")
                plot <- plot + xlab("Fitted values") + ylab("Residuals")
                plot <- plot + ggtitle(title)
              }

              else if (plottype == "QQ"){
                plot <- ggplot(model, aes(qqnorm(.stdresid)[[1]], .stdresid))+geom_point(na.rm = TRUE)
                plot <- plot + geom_abline(aes(qqline(.stdresid))) +
                  xlab("Theoretical Quantiles") + ylab("Standardised Residuals")
                plot <- plot + ggtitle(title)
              }

              else if (plottype == "SL"){
                plot <- ggplot(model, aes(.fitted, sqrt(abs(.stdresid)))) + geom_point(na.rm=TRUE)
                plot <- plot + stat_smooth(method="loess", na.rm = TRUE) + xlab("Fitted Value")
                plot <- plot + ylab(expression(sqrt("Standardised residuals")))
                plot <- plot + ggtitle(title)
              }

              else if (plottype == "Cook"){
                plot <- ggplot(model, aes(seq_along(.cooksd), .cooksd)) +
                  geom_bar(stat="identity", position="identity")
                plot <- plot + xlab("Obs. Number") + ylab("Cook's distance")
                plot <- plot + ggtitle(title)
              }

              else if (plottype == "RL"){
                plot <- ggplot(model, aes(.hat, .stdresid)) + geom_point(aes(size=.cooksd), na.rm=TRUE)
                plot <- plot + stat_smooth(method="loess", na.rm=TRUE)
                plot <- plot + xlab("Leverage") + ylab("Standardised Residuals")
                plot <- plot + ggtitle(title)
              }

              else if (plottype == "CL"){
                plot <- ggplot(model, aes(.hat, .cooksd))+geom_point(na.rm=TRUE)+stat_smooth(method="loess", na.rm=TRUE)
                plot <- plot + xlab("Leverage hii") + ylab("Cook's Distance")
                plot <- plot + ggtitle(title)
                plot <- plot + geom_abline(slope=seq(0,3,0.5), color="gray", linetype="dashed")

              }

              else {
                stop("Provide the argument plottype out of RF, QQ, SL, Cook, RL and CL!")
              }

              plot

            })
