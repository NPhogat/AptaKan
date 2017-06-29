#'@name Kd.gui
#'@aliases Kd.gui
#'@title Run the graphical user interface for computing the dissociation constant and
#'correlation
#'@description Run the graphical user interface
#'@author Navneet Phogat, Matthias Kohl, \email{Matthias.Kohl@@stamats.de}
#'@examples
#'Kd.gui()
#'@export
Kd.gui <- function() {
  runApp(system.file("Kd.gui", package = "AptaKan"))
}
