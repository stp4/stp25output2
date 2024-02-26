#' Output nach docx
#'
#' Geht nur direkt  und arbeitet mit flextable
#'
#' asis_output() function only works in top-level R expressions
#'
#' Wine Alternative ist die  gt-Bibilothek
#'
#' @param x data.frame
#' @param caption,note,output,split_header Ueberschrift usw
#' @param ... not used
#'
#' @return flextable
#' @export
#'
#' @importFrom flextable regulartable set_header_df merge_h merge_v theme_vanilla
#'
Output_word <- function(x,
                        caption = NULL,
                        note = NULL,
                        output =  "docx",
                        split_header=TRUE,
                        ...) {
  # callingFun = as.list(sys.call(-3))[[1]]
  # calledFun = as.list(sys.call())[[1]]
  # message(paste(callingFun, " is calling ", calledFun, sep=""))
  #
  caption <- Caption(caption, attr(x, "caption"), N = attr(x, "N"))
  note <- Note(note, attr(x, "note"))

  Text(caption)
  tbl <- tbl_header(x, split_header=split_header )
  x <- cleanup_nbsp(x)

  if (is.null(tbl$header_above)) {
    ft <- flextable::regulartable(x)
    ft <- flextable::theme_booktabs(ft)
    ft <- flextable::autofit(ft, add_w = 0, add_h = 0)
  }
  else {
    tbl$header_above2[1] <- tbl$header[1]
    typology <- data.frame(
      col_keys = names(x),
      what = tbl$header_above2,
      measure = tbl$header,
      stringsAsFactors = FALSE
    )
    ft <- flextable::regulartable(x)
    ft <- flextable::set_header_df(ft, mapping = typology, key = "col_keys")
    ft <- flextable::merge_h(ft, part = "header")
    ft <- flextable::merge_v(ft, part = "header")
    ft <- flextable::theme_booktabs(ft)
    ft <- flextable::autofit(ft, add_w = 0, add_h = 0)
    ft <- flextable::align(ft, align="center", part = "header")
  }

  ft
}



