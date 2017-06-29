#'@name apta.Kd
#'@aliases apta.Kd
#'@title To compute the dissociation constant and plot the fitting results
#'@description Function \code{apta.Kd} can be implemented on an object of class \code{"aptakandt"},
#'which is produced as an output from the function \code{CombineReps} and saves the results into
#'\code{"data.frame"} and also the plots. It is an auxialliary function to the function \code{apta.Kdall}
#'and can be implemented directly through the function \code{apta.Kdall}. See \code{\link[Aptakan]{ReadAptakan}}
#'and \code{\link[AptaKan]{CombineReps}} also.
#'@param aptakandt an object of class \code{"aptakandt"}, which is the output of function \code{CombineReps}.
#'@param plot To get the result in table form, mention "No". To get the plots mention "yes". Default is "No".
#'@param title The user defined character. But, while implementing it directly through the apta.Kdall, the
#'title will be selected automatically, based on the numbering of the plots.It will be the same row number
#'present in the results, obtained after implementing the \code{CombineReps} function.
#'@param method To implement the sigmoidal model, mention "sig", while implementing the non-sigmoidal model
#'implement the "non-sig". The default is "sig".
#'@param Kdstart The initial value to simulate the dissociation constant and it's curve. The default value is 1.
#'@param xlabel To label the x-axis of the plot.
#'@return an object of Class \code{data.frame} or plots.
#'@details Function \code{apta.Kd} computes the dissociation constant of the combined replicates
#'which are produced as a result after implementing the function \code{CombineReps}. The function
#'also plots the fitting curve of dissociation constant. The function \code{apta.kd} can be implemented
#'directly through the function \code{apta.Kdall}. More details are provided in the\code{AptaKan} package
#'vignette.
#'@author Navneet Phogat, Matthias Kohl, \email{Matthias.Kohl@@stamats.de}
#'@keywords aptakandt, ReadAptaKan, CombineReps, aptakan.conf
#'@examples
#'##See the examples of the function apta.Kdall. We recommend to implement the function directly thorugh
#'##apta.Kdall. Also, check the vignettes of the package AptaKan.
#'file1 <- system.file("exData", "aptakdnew.csv", package = "AptaKan")
#'read.datacsv <- ReadAptakan(file1)
#'## To combine the replicates on the basis of mean
#'## (replicates can also be combined on the basis of the median, but we
#'## recommend to combine the replicates on the basis of the mean, because that's
#'## the standard way to compute the dissociation constant)
#'kd.rep1.mean <- CombineReps(read.datacsv)
#'kd.rep1.mean ## To express all the data
#'slot(kd.rep1.mean,"crepData")  ##To express the combined replicates
#'kd.rep.median <- CombineReps(read.datacsv)
#'kd.rep.median ## To express all the data
#'slot(kd.rep.median,"crepData") ## To express the combined replicates
#'## To compute the dissociation constant by sigmoidal model
#'kd <- apta.Kd(kd.rep1.mean, kdstart = 1)
#'kd ## To express the results
#'kd.median <- apta.Kd(kd.rep.median, kdstart = 1)
#'kd.median
#'## To plot the fitting results of the sigmoidal model
#'apta.Kd(kd.rep1.mean, plot = "yes", kdstart = 1)
#'apta.Kd(kd.rep.median, plot = "yes", kdstart = 1)
#'## To compute the dissociation constant by non-sigmoidal model
#'kd.nonsig <- apta.Kd(kd.rep1.mean, method = "non-sig", kdstart = 1)
#'## To express the final results
#'kd.nonsig
#'kd.nonsig.median <- apta.Kd(kd.rep.median, method = "non-sig", kdstart = 1)
#'## To express the final results
#'kd.nonsig.median
#'## To plot the fitting results of the sigmoidal model
#'apta.Kd(kd.rep1.mean, plot = "yes", method = "non-sig")
#'apta.Kd(kd.rep.median, plot = "yes", method = "non-sig")
#'@export
setMethod("apta.Kd", signature = "aptakandt", definition =
            function(aptakandt, plot = "No",title = "Kd", method = "sig", kdstart = 1,
                     xlabel = "Concentration"){
              crepdata <- as.data.frame(slot(aptakandt,"crepData"))
              conc <- as.data.frame(slot(aptakandt, "concentration"))
              data.kd <- crepdata[1,] - crepdata[1,1]
              data.kd <- as.vector(data.kd)
              data.m <- as.matrix(rbind(conc,data.kd))
              x <- data.m[1,]
              y <- data.m[2,]
              if(is.na(y)||(is.na(x))){
                stop("Non-detects are present in fluorescence or concentration!")
              }
              else{
                if (method == "sig"){
                  nls.new <- nls(y ~ (max(y)*(x^2))/(Kd+x^2), start = list(Kd=kdstart))
                }
                else if (method == "non-sig") {
                  nls.new <- nls(y ~ (max(y)*x)/(Kd+x), start = list(Kd=kdstart))
                }
                else{
                  warning("Please mention sig or non-sig in method argument!")
                }
                y.predict <- predict(nls.new)
                if (plot == "No"){
                  Kds <- coef(summary(nls.new))
                  if (method == "sig"){
                    Kd <- sqrt(Kds[1])
                  }
                  else {
                    Kd <- Kds[1]
                  }
                  cor.kd <- cor(y,y.predict)
                  result.data <- as.data.frame(cbind(Kd, cor.kd))
                  names(result.data) <- c("Kd","Correlation")
                  result.data
                }
                else if (plot == "yes"){
                  plot.data <- as.data.frame(cbind(x,y,y.predict))
                  plot <- ggplot(plot.data, aes(x,y))+ geom_point(colour = "blue") + ylab("Fluorescence")+
                    xlab(xlabel)+ geom_line(aes(x,y.predict), color = "red", size = 2)+
                    ggtitle(title)
                  plot
                }
                else{
                  warning("Please mention either yes or no in plot argument!")
                }
              }
            })
