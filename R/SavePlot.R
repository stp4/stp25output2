#' SavePlot
#'
#' Speichern von Grafiken
#' Grafik werden  als PDF oder Windows-Metafile
#' speichern. Die Funktion Abb() beinhaltet den Pfad.
#'
#' @param caption TextAbbildung
#' @param w,h,res  Width, Height, an HTMLplot
#' default = 520, Breite der  Grafik  also zb w=8=dev.size("in")[2],
#' h=dev.size("in")[1] Hoehe der Grafik also zb h=8
#' res=72
#' @param filename Text
#' @param save_plot speichern als file = TRUE,
#' @param output nur wichtig bei html
#' @param out.type	 name of the output device: can
#' be "pdf", or "cairo"
#'
#' @return
#' @export
#'
#' @examples
#'
#' plot(1)
#' SavePlot("Einfache Grafik", w=4, h=4)
#'
#'
SavePlot <- function(caption = "",
                     w = dev.size("in")[1],
                     h = dev.size("in")[2],
                     filename = "",
                     save_plot = TRUE,
                     output =  which_output(),
                     res=72,
                     out.type="pdf"

                    ) {
  abb <- Abb(filename, caption)


  if (output == "html") {

    GraphFileName <- paste0(abb$GraphFileName, ".png")
    # resulution
    Width <- round(w * res)
    Height <- round(h * res)

    dev.print(
      device = png,
      file = file.path(dirname(HTMLGetFile()), GraphFileName),
      width = Width,
      height = Height,
      pointsize = 12,
      bg =  "white",
      res = res  # res liefert unerwartete Ergebnisse
    )


    HTML_default(
      paste0(
        "\n<div  class='center'>\n",
        "<figure style='font-family: verdana; font-size: 8pt; font-style: italic; padding: 8px; text-align: center'>\n",
        "<img src='", GraphFileName, "'",
       " alt='",abb$Name,"'",
        " style='width:",Width,"px;height:",Height,"px;'",
        "/>\n<br>\n",
        "<figcaption>", abb$HtmlCaption, "</figcaption>\n",
        "</figure>\n</div>"
      ))


 #  HTML_S(abb$GraphFileName)
    HTML_BR()
  }
  else if( output == "docx" ){
      save_plot<- FALSE
  }else{ NULL }




  if (save_plot) {
      abb$Name <- paste0(abb$Name, ".pdf")
      try(dev.copy2pdf(file = abb$Name,
                       width = w,
                       height = h,
                       out.type=out.type))

  }


invisible(abb$GraphFileName)
}



