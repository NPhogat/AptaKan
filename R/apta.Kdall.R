#'@name apta.Kdall
#'@aliases apta.Kdall
#'@title To compute the dissociation constant and plot the fitting results
#'@description Function \code{apta.Kdall} can be implemented on an object of class \code{"aptakandt"},
#'which is produced as an output from the function \code{CombineReps} and saves the results into
#'class \code{"aptakandt"} and also the plots. The function \code{apta.Kdall} is based on the function
#'\code{apta.Kd}. See \code{\link[Aptakan]{ReadAptakan}}, \code{\link[AptaKan]{CombineReps}}
#'and \code{\link[AptaKan]{apta.Kd}} also.
#'@param aptakandt an object of class \code{"aptakandt"}, which is the output of function \code{CombineReps}.
#'@param plot To get the result in table form, mention "No". To get the plots mention "yes". Default is "No".
#'@param method To implement the sigmoidal model, mention "sig", while implementing the non-sigmoidal model
#'implement the "non-sig". The default is "sig".
#'@param Kdstart The initial value to simulate the dissociation constant and it's curve. The default
#'value is 1.
#'@param xlabel To label the x-axis of the plot.
#'@return an object of Class \code{data.frame} or plots.
#'@details Function \code{apta.Kdall} computes the dissociation constant in a single step of all
#'the combined replicates which are produced as a result after implementing the function
#'\code{CombineReps}. It also computes the correlation betweeen predicted and original data.
#'The function also plots the fitting curve of dissociation constant. The function \code{apta.kdall}
#'is based on the function \code{apta.Kd}. More details are provided in the \code{AptaKan} package
#'vignette.
#'@author Navneet Phogat, Matthias Kohl, \email{Matthias.Kohl@@stamats.de}
#'@keywords aptakandt, ReadAptaKan, CombineReps, apta.Kd, apta.Kdall
#'@examples
#'##To read the .csv file
#'file1 <- system.file("exData", "aptakdnew.csv", package = "AptaKan")
#'read.datacsv <- ReadAptakan(file1)
#'
#'## To express all the data
#'read.datacsv
#'
#'## To combine the replicates
#'kd.rep1.mean <- CombineReps(read.datacsv)
#'kd.rep1 ## To express all the data
#'slot(kd.rep1,"crepData")  ##To express the combined replicates
#'
#'## To compute the dissociation constant by sigmoidal model
#'kd <- apta.Kdall(kd.rep1, kdstart = 1)
#'
#'## To visualise the results of Kd and correlation between the original data and predicted
#'#data
#'
#'slot(kd,"KdData")
#'
#'## To plot the fitting results of the sigmoidal model
#'apta.Kdall(kd.rep1, plot = "yes", kdstart = 1)
#'
#'## To compute the dissociation constant by non-sigmoidal model
#'kd.nonsig <- apta.Kdall(kd.rep1, method = "non-sig", kdstart = 1)
#'
#'## To visualise the results of Kd and correlation between the original data and predicted
#'#data
#'
#'slot(kd.nonsig,"KdData")
#'
#'## To plot the fitting results of the sigmoidal model
#'apta.Kdall(kd.rep1, plot = "yes", method = "non-sig", kdstart = 1)
#'@export
setMethod("apta.Kdall", signature = "aptakandt", definition =
            function(aptakandt, plot = "No", method = "sig", kdstart = 1,
                     xlabel = "Concentration"){
              cr.rep <- slot(aptakandt,"crepData")
              row.cr <-unique(row.names(cr.rep))
              r.cr <- nrow(cr.rep)
              if (plot == "No"){
                d = data.frame(Kd=rep(0,r.cr), Correlation=rep(0,r.cr))
                for(i in 1:length(row.cr))
                {
                  cr.repnew <- cr.rep[i,]
                  a.crepdata <- as.data.frame(cr.repnew)
                  a2 <- new("aptakandt", crepData = a.crepdata, concentration =
                              as.data.frame(slot(aptakandt,"concentration")))
                  b <- apta.Kd(a2, plot = plot, method =  method, kdstart = kdstart, xlabel = xlabel)
                  d[i,] <-b
                }
                a2 <- new("aptakandt",crepData = a.crepdata, concentration =
                            as.data.frame(slot(aptakandt,"concentration")),
                          KdData = d)
                a2
              }
              else if (plot == "yes"){
                plots_list<- list()
                for(i in 1:length(row.cr))
                {
                  cr.repplot <- cr.rep[i,]
                  plot.crepdata <- as.data.frame(cr.repplot)
                  a2.plot <- new("aptakandt", crepData = plot.crepdata, concentration =
                                   as.data.frame(slot(aptakandt,"concentration")))
                  plots_list[[i]] <- apta.Kd(a2.plot, plot = plot, title = i,
                                             method = method, kdstart = kdstart)
                }
                do.call(grid.arrange,plots_list)
              }
            })
