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
#' @param save_plot  c("pdf", "esp", "png", "jpeg", "bmp") speichern als file
#' @param output nur wichtig bei html
#' @param out.type	 name of the output device: can
#' be "pdf", or "cairo" dev.copy2pdf
#'
#' @return file name
#' @export
#'
#' @examples
#'
#' plot(1)
#' SavePlot("Einfache Grafik", w=4, h=4)
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
#' cowplot::save_plot( )
#' ggplot2::ggsave("test-23.pdf")
#' ggplot2::ggsave("test-2.eps")
#'
#'
#' save_my_plot <- function(p,
#' w = 7,
#' h = 3.5 ,
#' filename = "A-Figure.jpeg",
#' out.type = "cairo",
#' path = getwd(),
#' res= 72
#' ) {
#'
#'   p_new <-
#'     update(p, par.settings = stp25settings::bw_theme(farbe()))
#'   filename <-   file.path(path, filename)
#'   jpeg(filename,
#'        width = round(w * res),
#'        height = round(h * res),
#'        quality = 100,
#'        res = res,
#'        type  = out.type
#'   )
#'   # wegen "cairo"
#'   print(cowplot::plot_grid(p_new))
#'   dev.off()
#'
#'   (p_new)
#' }
#'
#'
#' require(lattice)
#' require(stp25output2)
#' require(stp25settings)
#' p<- barchart(yield ~ variety | site, data = barley,
#'              groups = year, layout = c(1,6), stack = TRUE,
#'              auto.key = list(space = "right"),
#'              ylab = "Barley Yield (bushels/acre)",
#'              scales = list(x = list(rot = 45)))
#'
#' save_my_plot(p)
#'
#'
#'
SavePlot <- function(caption = "",
                      w = dev.size("in")[1],
                      h = dev.size("in")[2],
                      filename = "",
                      save_plot = "pdf",  # c("pdf", "esp", "png", "jpeg", "bmp")
                      output =  which_output(),
                      res = 72,
                      out.type = "pdf") {
  abb <- Abb(filename, caption)
  Width <- round(w * res)
  Height <- round(h * res)


  if (output == "html") {
    GraphFileName <- paste0(abb$GraphFileName, ".png")
    #' resulution
    #' w und h in pixels


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
    save_plot <- "FALSE"
  } else{
    NULL
  }



  if ("pdf" %in% save_plot) {
    if (filename == "")
      abb_Name <- paste0(abb$Name, ".pdf")
    else
      abb_Name <- paste0(get_opt("fig_folder"), filename, ".pdf")

    cat("\nPDF: ", abb_Name, "\n")

    try(dev.copy2pdf(
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

      #' save as png
      ggplot2::ggsave(gsub("\\.eps", "\\.png", abb_Name),
                      width = w,
                      height = h)

      if (out.type == "cairo")
        ggplot2::ggsave(abb_Name,
                        width = w,
                        height = h,
                        device = cairo_ps)
      else
        ggplot2::ggsave(abb_Name,
                        width = w, height = h)
    }
    else{
      cat("\n dev.copy2eps: ", abb_Name, "\n")
      try(dev.copy2eps(file = abb_Name,
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

    dev.print(
      device = jpeg,
      file = abb_Name,
      width = Width,
      height = Height,
      pointsize = 12,
      bg =  "white",
      res = res  # res liefert unerwartete Ergebnisse
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

    dev.print(
      device = bmp,
      file = abb_Name,
      width = Width,
      height = Height,
      pointsize = 12,
      bg =  "white",
      res = res  # res liefert unerwartete Ergebnisse
    )
  }

  if ("png" %in% save_plot) {
    if (filename == "")
      abb_Name <- paste0(abb$Name, ".png")
    else
      abb_Name <- paste0(get_opt("fig_folder"), filename, ".png")
    cat("\npng: ", abb_Name, "\n")

    dev.print(
      device = png,
      file = abb_Name,
      width = Width,
      height = Height,
      pointsize = 12,
      bg =  "white",
      res = res  # res liefert unerwartete Ergebnisse
    )
  }

  invisible(abb$GraphFileName)
}




#' @rdname Tab_Abb
#' @export
Abb_Index <- function (x = NULL) {
  if (exists("Abb_Index", .step25Env))
    if (is.null(x)) {
      get("Abb_Index", .step25Env)
    }
  else{
    assign("Abb_Index", x, envir = .step25Env)
    x
  }
  else
    0
}


#' @rdname Tab_Abb
#' @export
#' @param filename  Name des Foles
#' @param folder   Speicherort
#'
Abb <- function (filename="",
                 caption="",
                 folder = get_opt("fig_folder")) {
  if (exists("Abb_Index", .step25Env))
    x <-  get("Abb_Index", .step25Env) + 1
  else x <- 1

  assign("Abb_Index", x, envir = .step25Env)

  list(
    HtmlCaption=paste0("Abb", x, ": ", caption),
    GraphFileName = paste0("Fig", x, filename),
    Name =paste0(folder, "Fig", x, filename)
  )
}





#' SavePlot_old <- function(caption = "",
#'                          w = dev.size("in")[1],
#'                          h = dev.size("in")[2],
#'                          filename = "",
#'                          save_plot = "pdf",
#'                          output =  which_output(),
#'                          res = 72,
#'                          out.type="pdf"
#'
#' ) {
#'   abb <- Abb(filename, caption)
#'
#'
#'
#'   if (output == "html") {
#'     GraphFileName <- paste0(abb$GraphFileName, ".png")
#'     #' resulution
#'     #' w und h in pixels
#'     Width <- round(w * res)
#'     Height <- round(h * res)
#'
#'     dev.print(
#'       device = png,
#'       file = file.path(dirname(HTMLGetFile()), GraphFileName),
#'       width = Width,
#'       height = Height,
#'       pointsize = 12,
#'       bg =  "white",
#'       res = res  # res liefert unerwartete Ergebnisse
#'     )
#'
#'     HTML_default(
#'       paste0(
#'         "\n<div  class='center'>\n",
#'         "<figure style='font-family: verdana; font-size: 8pt; font-style: italic; padding: 8px; text-align: center'>\n",
#'         "<img src='", GraphFileName, "'",
#'         " alt='",abb$Name,"'",
#'         " style='width:",Width,"px;height:",Height,"px;'",
#'         "/>\n<br>\n",
#'         "<figcaption>", abb$HtmlCaption, "</figcaption>\n",
#'         "</figure>\n</div>"
#'       ))
#'     #  HTML_S(abb$GraphFileName)
#'     HTML_BR()
#'   }
#'   else if( output == "docx" ){
#'     save_plot <- "FALSE"
#'   }else{ NULL }
#'
#'
#'
#'   if ("pdf" %in% save_plot) {
#'     if (filename == "")
#'       abb_Name <- paste0(abb$Name, ".pdf")
#'     else
#'       abb_Name <- paste0(get_opt("fig_folder"), filename, ".pdf")
#'
#'     cat("\nPDF: ",abb_Name,"\n")
#'
#'     try(dev.copy2pdf(
#'       file = abb_Name,
#'       width = w,
#'       height = h,
#'       out.type = out.type
#'     ))
#'
#'
#'
#'
#'   }
#'
#'   if ("eps" %in% save_plot) {
#'     if (filename == "")
#'       abb_Name <- paste0(abb$Name, ".eps")
#'     else
#'       abb_Name <- paste0(get_opt("fig_folder"), filename, ".eps")
#'
#'
#'     if (inherits(ggplot2::last_plot(), "ggplot")) {
#'       cat("\n ggplot2::ggsave: ", abb_Name, "\n")
#'
#'       #' save as png
#'       ggplot2::ggsave(gsub("\\.eps", "\\.png", abb_Name),
#'                       width = w,
#'                       height = h)
#'
#'       if (out.type == "cairo")
#'         ggplot2::ggsave(abb_Name,
#'                         width = w,
#'                         height = h,
#'                         device = cairo_ps)
#'       else
#'         ggplot2::ggsave(abb_Name,
#'                         width = w, height = h)
#'     }
#'     else{
#'       cat("\n dev.copy2eps: ", abb_Name, "\n")
#'       try(dev.copy2eps(
#'         file = abb_Name,
#'         width = w,
#'         height = h
#'       ))
#'     }
#'
#'   }
#'
#'   if ("jpg" %in% save_plot) {
#'
#'     if (filename == "")
#'       abb_Name <- paste0(abb$Name, ".jpg")
#'     else
#'       abb_Name <- paste0(get_opt("fig_folder"), filename, ".jpg")
#'
#'     cat("\njpg: ",abb_Name,"\n")
#'
#'     try(dev.copy2pdf(
#'       file = abb_Name,
#'       width = w,
#'       height = h,
#'       out.type = out.type
#'     ))
#'
#'     jpeg(filename,
#'          width = round(w * res),
#'          height = round(h * res),
#'          quality = 100,
#'          res = res,
#'          type  = out.type
#'     )
#'
#'     if( out.type ==  "cairo")
#'       print(cowplot::plot_grid(p_new))
#'     else print( )
#'     dev.off()
#'
#'
#'   }
#'
#'
#'
#'   invisible(abb$GraphFileName)
#' }
