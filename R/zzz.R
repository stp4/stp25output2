#' Umlaute Entfernen
#'
#' Funktion `cleansing_umlaute()` entfernt stoerende Umlaute, unten stehende Liste ggf. erweitern.
#'
#' @param x string
#' @export
#'
cleansing_umlaute <- function(x, ...){
  x <- gsub("\u00e4","ae", x)
  x <- gsub("\u00fc","ue", x)
  x <- gsub("\u00f6","oe", x)
  x <- gsub("\u00dc","Ue", x)
  x <- gsub("\u00c4","Ae", x)
  x <- gsub("\u00d6","Oe", x)
  x <- gsub("\u00df","ss", x)
  x <- gsub(" ", "_", x)
  x
}

#' @importFrom utils browseURL citation read.table



