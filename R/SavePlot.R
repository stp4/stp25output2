#' SavePlot
#'
#' Speichern von Grafiken
#' Grafik werden  als PDF oder Windows-Metafile
#' speichern. Die Funktion Abb() beinhaltet den Pfad.
#'
#' @param caption TextAbbildung
#' @param w,h,res  Width, Height, an HTMLplot w and h in Inches or Pixel
#' default = 520, Breite der  Grafik  also zb w=8=dev.size("in")[2],
#' h=dev.size("in")[1] Hoehe der Grafik also zb h=8
#' res=72
#' @param filename Text
#' @param save_plot  c("pdf", "esp", "png", "jpeg", "bmp") speichern als file
#' @param output nur wichtig bei html
#' @param out.type	 name of the output device: can
#' be "pdf", or "cairo" dev.copy2pdf
#'
#' @return file name
#' @export
#'
#' @importFrom grDevices bmp cairo_ps dev.copy2eps dev.copy2pdf dev.print dev.size jpeg png tiff
#' @examples
#'
#' \dontrun{
#'
#' plot(1)
#' # SavePlot("Einfache Grafik", w=4, h=4)
#'
#' #'  require(cowplot)
#' title <-
#'   cowplot::ggdraw() +
#'   cowplot::draw_label("FFQ DEGS 534", fontface = 'bold')
#'
#' cowplot::plot_grid(
#'   title, NULL,
#'   function(x) plot(1),
#'   function(x) plot(1:10),
#'   nrow = 2,
#'   #labels = c("", "", "C"),
#'   rel_heights = c(0.05, 1 )
#' )
#' #  ?cowplot::save_plot( )
#' #ggplot2::ggsave("test-23.pdf")
#' #ggplot2::ggsave("test-2.eps")
#'
#' }
#'
#' #save_my_plot <- function(p,
#' #                         w = 7,
#' #                         h = 3.5 ,
#' #                         filename = "A-Figure.jpeg",
#' #                         out.type = "cairo",
#' #                         path = getwd(),
#' #                         res= 72
#' #) {
#'#
#'  # p_new <-
#'  #   update(p, par.settings = stp25settings::bw_theme(farbe()))
#'  # filename <-   file.path(path, filename)
#'  # jpeg(filename,
#'  #      width = round(w * res),
#'  #      height = round(h * res),
#'  #      quality = 100,
#'  #      res = res,
#' #       type  = out.type
#'  # )
#'  # # wegen "cairo"
#' #  print(cowplot::plot_grid(p_new))
#'  # dev.off()
#'#
#'  # (p_new)
#' #}
#'
#'
#' # require(lattice)
#' # require(stp25output2)
#' # require(stp25settings)
#' # p<- barchart(yield ~ variety | site, data = barley,
#' #             groups = year, layout = c(1,6), stack = TRUE,
#' #             auto.key = list(space = "right"),
#' #             ylab = "Barley Yield (bushels/acre)",
#' #             scales = list(x = list(rot = 45)))
#' #
#' #   save_my_plot(p)
#'
#'
SavePlot <- function(caption = "",
                      w = grDevices::dev.size("in")[1],
                      h = grDevices::dev.size("in")[2],
                      filename = "",
                      save_plot = "pdf",  # c("pdf", "esp", "png", "jpeg", "bmp")
                      output =  which_output(),
                      res = 72,
                      out.type = "pdf") {

  # Pixels to Inches
  if (w > 20) w <- round(w / 96, 2)
  if (h > 20) h <- round(h / 96, 2)

  abb <- Abb(filename, caption)
  Width <- round(w * res)
  Height <- round(h * res)

  cat("\n w = ", w, ", h = ", h, ", res = ", res, "\n")

  if (output == "html") {
    GraphFileName <- paste0(abb$GraphFileName, ".png")
    # resulution
    # w und h in pixels
    grDevices::dev.print(
      device = grDevices::png,
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
        "<img src='",
        GraphFileName,
        "'",
        " alt='",
        abb$Name,
        "'",
        " style='width:",
        Width,
        "px;height:",
        Height,
        "px;'",
        "/>\n<br>\n",
        "<figcaption>",
        abb$HtmlCaption,
        "</figcaption>\n",
        "</figure>\n</div>"
      )
    )
    #  HTML_S(abb$GraphFileName)
    HTML_BR()
  }
  else if (output == "docx") {
    if (!file.exists(get_opt("fig_folder"))) {
         cat("\nDer Folder ", get_opt("fig_folder"), " existiet nicht!\n Diesen musst du selber erstellen.")
         #print("2 der folder existiert nicht!")
         save_plot <- "FALSE"
    }
  } else{
    NULL
  }


  if ("pdf" %in% save_plot) {
    if (filename == "")
      abb_Name <- paste0(abb$Name, ".pdf")
    else
      abb_Name <- paste0(get_opt("fig_folder"), filename, ".pdf")

    cat("\nPDF: ", abb_Name, "\n")

    try(grDevices::dev.copy2pdf(
      file = abb_Name,
      width = w,
      height = h,
      out.type = out.type
    ))
  }

  if ("eps" %in% save_plot) {
    if (filename == "")
      abb_Name <- paste0(abb$Name, ".eps")
    else
      abb_Name <- paste0(get_opt("fig_folder"), filename, ".eps")


    if (inherits(ggplot2::last_plot(), "ggplot")) {
      cat("\n ggplot2::ggsave: ", abb_Name, "\n")

      # save as png
      ggplot2::ggsave(gsub("\\.eps", "\\.png", abb_Name),
                      width = w,
                      height = h)

      if (out.type == "cairo")
        ggplot2::ggsave(abb_Name,
                        width = w,
                        height = h,
                        device = grDevices::cairo_ps)
      else
        ggplot2::ggsave(abb_Name,
                        width = w, height = h)
    }
    else{
      cat("\n dev.copy2eps: ", abb_Name, "\n")
      try(grDevices::dev.copy2eps(file = abb_Name,
                       width = w,
                       height = h))
    }

  }

  if ("jpeg" %in% save_plot) {
    if (filename == "")
      abb_Name <- paste0(abb$Name, ".jpeg")
    else
      abb_Name <-
        paste0(get_opt("fig_folder"), filename, ".jpeg")
    cat("\njpeg: ", abb_Name, "\n")
    #if( out.type !=  "cairo")  out.type <- "windows"

    grDevices::dev.print(
      device = grDevices::jpeg,
      file = abb_Name,
      width = Width,
      height = Height,
      pointsize = 12,
      bg =  "white",
      res = res
    )
  }

  if ("bmp" %in% save_plot) {
    if (filename == "")
      abb_Name <- paste0(abb$Name, ".bmp")
    else
      abb_Name <-
        paste0(get_opt("fig_folder"), filename, ".bmp")
    cat("\nbmpjpeg: ", abb_Name, "\n")
    #if( out.type !=  "cairo")  out.type <- "windows"

    grDevices::dev.print(
      device = grDevices::bmp,
      file = abb_Name,
      width = Width,
      height = Height,
      pointsize = 12,
      bg =  "white",
      res = res
    )
  }

  if ("png" %in% save_plot) {
    if (filename == "")
      abb_Name <- paste0(abb$Name, ".png")
    else
      abb_Name <- paste0(get_opt("fig_folder"), filename, ".png")
    cat("\npng: ", abb_Name, "\n")

    grDevices::dev.print(
      device = grDevices::png,
      file = abb_Name,
      width = Width,
      height = Height,
      pointsize = 12,
      bg =  "white",
      res = res  # res liefert unerwartete Ergebnisse
    )
  }

  if ("tiff" %in% save_plot) {
    if (filename == "")
      abb_Name <- paste0(abb$Name, ".tiff")
    else
      abb_Name <- paste0(get_opt("fig_folder"), filename, ".tiff")
    cat("\npng: ", abb_Name, "\n")

    grDevices::dev.print(
      device = grDevices::tiff,
      file = abb_Name,
      width = Width,
      height = Height,
      pointsize = 12,
      bg =  "white",
      res = res
    )
  }

  invisible(abb$GraphFileName)
}


#' @rdname SavePlot
#' @export
SaveGTplot <- function(x,
                       caption = "",
                       filename = "",
                       save_plot = "pdf",  # c("pdf", "esp", "png", "jpeg", "bmp")
                       output =  which_output(),
                       res = 72,
                       ...) {

  if(!inherits(x, "gt_tbl")) stop("Hier muss ein GT-Objekt übergeben werden!")
  abb <- Abb(filename, caption)
  GraphFileName <- paste0(abb$GraphFileName, ".png")
  file <- file.path(dirname(HTMLGetFile()), GraphFileName)

  if (output == "html") {
    # gtExtras::gtsave_extra(x,filename=file, zoom = 2 )
    gt::gtsave(x, filename=file, expand = 1, zoom = res/72 )

    HTML_default(
      paste0(
        "\n<div  class='center'>\n",
        "<figure style='font-family: verdana; font-size: 8pt; font-style: italic; padding: 8px; text-align: center'>\n",
        "<img src='",
        GraphFileName,
        "'",
        " alt='",
        abb$Name,
        "'",

        "/>\n<br>\n",
        "<figcaption>",
        abb$HtmlCaption,
        "</figcaption>\n",
        "</figure>\n</div>"
      )
    )
    HTML_BR()
  } # else if (output == "docx") {save_plot <- "FALSE"}
  else{
    return(x)
  }


  if ("pdf" %in% save_plot) {
    if (filename == "")
      abb_Name <- paste0(abb$Name, ".pdf")
    else
      abb_Name <- paste0(get_opt("fig_folder"), filename, ".pdf")

    cat("\nPDF: ", abb_Name, "\n")
    gt::gtsave(x, filename=abb_Name, expand = 1, zoom = res/72 )

  }

  if ("eps" %in% save_plot) {stop( "Das geht noch nicht!")}
  if ("jpeg" %in% save_plot) {stop( "Das geht noch nicht!") }
  if ("bmp" %in% save_plot) {stop( "Das geht noch nicht!") }
  if ("png" %in% save_plot) {stop( "Das geht noch nicht!") }
  if ("tiff" %in% save_plot) {stop( "Das geht noch nicht!") }

  (abb$GraphFileName)

}

#' @rdname SavePlot
#' @export
HTML_IMG <- function(file,
                     caption = file,
                     size = "50%",
                     output = which_output()) {
  if (output == "html") {
    HTML_default(
      paste0(
        "
<div  class='center'>
 <figure style='font-family: verdana; font-size: 8pt; font-style: italic; padding: 8px; text-align: center'>
   <img src='", file, "' style='width: ", size,"'/>
   <br>
   <figcaption>", caption, "</figcaption>
 </figure>
</div>
"
      )
    )
  }
  else
    " Bilder werden nur in HTML Angezeigt! "
}


