#'@name ReadAptakan
#'@aliases ReadAptakan
#'@title To read the .csv as well as .txt file of experimental data
#'@description Function \code{ReadAptakan} reads in .csv as well as .txt files of experimental fluorescent
#'data and uses the data to populate an object of class \code{"aptakandt"}.
#'@param file the name of the file to read in.
#'@param type the type of the file out of ".csv" or ".txt" to read in. The default parameter is ".csv."
#'@return object of Class \code{"aptakandt"} with intialData and concentration slots.
#'@details Allows the user to read in experimental fluorescence data, arranged in a particular format.
#'More details are provided in the \code{AptaKan} package vignette.
#'@author Navneet Phogat, Matthias Kohl, \email{Matthias.Kohl@@stamats.de}
#'@keywords aptakandt, ReadAptakan
#'@examples
#'##To read the .csv file
#'file1 <- system.file("exData", "Aptakandata.csv", package = "AptaKan")
#'read.datacsv <- ReadAptakan(file1)
#'
#'## To express all the data
#'read.datacsv
#'
#'## To express the initial data (all the data read in, except concentration)
#'slot(read.datacsv,"initialData")
#'
#'## To express the concentration data (from the data read in)
#'slot(read.datacsv,"concentration")
#'
#'## To read the .txt file
#'file1 <- system.file("exData", "Aptakandata.txt", package = "AptaKan")
#'read.datatxt <- ReadAptakan(file1, type = ".txt")
#'
#'## To express the initial data (except concentration)
#'slot(read.datatxt, "initialData")
#'
#'## To express the concentration data
#'slot(read.datatxt, "concentration")
#'@export
ReadAptakan <- function(file, type = ".csv"){
  if (type == ".csv"){
    res1 <- read.csv(file, header = TRUE)
  }

  else if(type == ".txt"){
    res1 <- read.table(file, header = TRUE, fill = TRUE,skip = 0, sep = "\t", quote = "\"",comment.char = "")
  }
  else{
    warning('Select the type of file out of ".txt" and ".csv" !' )
  }

  res<-res1[-nrow(res1),]
  conc1 <- as.data.frame(res1[nrow(res1),])
  conc <- conc1[-c(1,2,ncol(res1))]
  row.names(conc) <- "Conc"
  res.data <- new("aptakandt", initialData = res, concentration = conc)
  res.data
}
