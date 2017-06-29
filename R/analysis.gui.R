#'@name analysis.gui
#'@aliases analysis.gui
#'@title Run the graphical user interface for analysis and computing the confidence interval
#'and concentration
#'@description Run the graphical user interface
#'@author Navneet Phogat, Matthias Kohl, \email{Matthias.Kohl@@stamats.de}
#'@examples
#'analysis.gui()
#'@export
analysis.gui <- function() {
  runApp(system.file("analysis.gui", package = "AptaKan"))
}
