#' @rdname Output
#' @description Output mit  knitr::kable Funftioniert nur wenn options(knitr.table.format = "latex")
#' gesetzt wird (das gilt nur fuer Pdf)
#' @importFrom kableExtra kable_styling add_header_above
#' @importFrom knitr kable
#' @export
#'
Output_kable <- function(x, ...) {
  UseMethod("Output_kable")
}

#' @rdname Output
#' @export
Output_kable.list <- function(x, ...) {
  cname <- names(x)
  for (i in  cname)
    Output_kable.default(x[[i]], ...)
}

#' @rdname Output
#'
#' @param booktabs,latex_options kable an Latex     latex_options = c("hold_position"),
#'  latex_options = "scale_down"
#'
#' @export
Output_kable.default <-
  function(x,
           caption = NULL,
           col.names = colnames(x),
           booktabs=TRUE,
           latex_options = c("hold_position"),
           note = NULL,
           output =  which_output(),
           ...) {


    caption <- Caption(caption, attr(x, "caption"), N = attr(x, "N"))
    note <- Note(note, attr(x, "note"))


    result_tbl_names <- stringr::str_split(col.names, "_")
    ebenen <- max(lengths(result_tbl_names), na.rm = TRUE)

    if (ebenen == 3) {
      #Fehler mit Name_name_SD abfangen
      result_tbl_names <- stringr::str_split(col.names, "_", 2)
      ebenen <- max(lengths(result_tbl_names), na.rm = TRUE)
    }





    dt <- cleanup_nbsp(x)

    if (ebenen == 1 & output == "markdown") {
      print(kableExtra::kable_styling(
        knitr::kable(
          dt,
          row.names = FALSE,
          col.names = col.names,
          booktabs = booktabs,
          caption = caption
        ),
        latex_options = latex_options
      ))

    }
    else if (ebenen == 2 & output == "markdown") {
      a1 <- sapply(result_tbl_names, "[", 1)
      a2 <- sapply(result_tbl_names, "[", 2)

      nas <- is.na(a2)
      a2[nas] <- a1[nas]
      a1[nas] <- ""
      header <- a2
      cgroup <-  rle(a1)$values
      n.cgroup <- rle(a1)$lengths


      header_above <-
        n.cgroup #ifelse(n.cgroup == 1, " ", n.cgroup)
      header_above[1] <- " "

      names(header_above) <-  gsub("&nbsp;", ' ', cgroup)

      print(kableExtra::add_header_above(
        kableExtra::kable_styling(
          knitr::kable(
            dt,
            row.names = FALSE,
            col.names = header,
            booktabs = booktabs,
            caption = caption,
            ...
          ),
          latex_options = latex_options
        ),
        header_above
      ))
    }
    else{
      cat("\n else \n")
      print(dt)
    }
    invisible(dt)
  }

