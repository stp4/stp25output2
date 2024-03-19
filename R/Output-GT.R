#' @rdname Output
#' @param split_header in GT Spaltenüberschrift spliten
#' @description Output mit gt() interne übergabe an gt_split_header()
#' @importFrom gt tab_caption tab_footnote
#' @export
#'
#' @examples
#'
#' require(gt)
#' # require(stp25output2)
#'
#' tab <- data.frame(
#'   Item = c("Aaa", "Bbb", "Ööö"),
#'   x_m = c(45.47, 26.14, 14.47),
#'   x_sd = c(1.43, 2.26, 5.42),
#'   y_m = c(35.42, 61.94, 24.43),
#'   y_sd = c(3.44, 5.25, 5.42)
#' )
#'
#' tab |>
#'   gt() |> Output()
#'
#' tab |>
#'   gt() |>
#'   as_raw_html() |>
#'   Output()
Output.gt_tbl <- function(x,
                          caption = NULL,
                          note = NULL,
                          split_header = TRUE,
                          na.rm = TRUE,
                          output = which_output(),
                          ...) {
  if (split_header)
    x <- gt_split_header(x, na.rm = na.rm)


  if (!is.null(caption)){
  #  if (caption == TRUE) caption <-   attr(x, "caption")
    x <- gt::tab_caption(x, caption = caption)
    }

  if (!is.null(note)){
   # if (note == TRUE) note <-   attr(x, "note")
    x <- gt::tab_footnote(x, footnote = note)
    }


  if (output == "html") {
    x |>
      gt::as_raw_html() |>
      Output()
    return(NULL)
  }
  else {
   return(x)
  }

}


#' @rdname Output
#' @export
Output.html <- function(x, ..., output =  which_output()) {
  x_s <- stringr::str_split(x, ">")[[1]]
  if (grepl("<div", x_s[1])) {
    x_s[[1]][1] <- "<div"
    if (output == "html") {
      HTML_BR()
      HTML_default(paste(x_s, collapse = "> "))
      HTML_BR()
    }
    else
      paste(x_s[1], "...")
  }
  else {
    HTML_default("<b>Unbekantes Format!</b>")
  }

}

#' @rdname Output
#' @description  gt_split_header: GT Tabel header spliten
#'
#' @param x gt-objekt
#' @param na.rm,missing_text  replace NA
#' @param ... nicht benutzt
#'
#' @importFrom gt tab_spanner cols_label sub_missing extract_body
#' @importFrom tidyselect starts_with
#'
#' @return gt_tbl, list
#' @export
gt_split_header <- function(x,
                            na.rm = TRUE,
                            missing_text = "",
                            ...) {
  xt <- gt::extract_body(x)
  if (!any(grepl("_", names(xt))))
    return(x)

  xt_h <- tbl_header(xt)

  for (i in xt_h$cgroup) {
    if (i == "")
      next
    x <-
      gt::tab_spanner(x, label = i, columns = tidyselect::starts_with(i))
  }

  names(xt_h$header) <- names(xt)
  x <- gt::cols_label(x, .list = xt_h$header)
  if (na.rm)
    x <- gt::sub_missing(x, missing_text = missing_text)

  x
}
