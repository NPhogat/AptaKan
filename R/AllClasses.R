#'@title S4 Class aptakandt to contain the data
#'@slot initialData contains the initial data, except concentration, in the form of an object
#'of Class \code{data.frame}
#'@slot crepData contains the result of the function \code{CombineReps} in the form of an object
#'of Class \code{data.frame}
#'@slot concentration contains the concentration data in the form of an object of Class \code{data.frame}
#'@slot modelData contains the result of implementation of different statistical models of
#'analysis in the form of an object of Class \code{data.frame}
#'@slot confData contains the confidence intervals in the form of an object of Class \code{data.frame}
#'@slot KdData contains the results of dissociation constant in the form of an object of Class
#'\code{data.frame}
#'@exportClass
setClass("aptakandt", contains = "data.frame", representation(initialData = "data.frame",
                                                              crepData = "data.frame",
                                                              concentration = "data.frame",
                                                              modelData = "data.frame",
                                                              confData = "data.frame",
                                                              KdData = "data.frame"))

