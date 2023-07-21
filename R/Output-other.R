#' @rdname Output
#'
#' @export
#'
#' @examples
#' library(gtsummary)
#'  tbl_summary_ex1 <-
#' trial %>%
#'   select(age, grade, response) %>%
#'   tbl_summary() %>%
#'   Output()
#'
Output.gtsummary <- function(x,
                             caption = "",
                             note = "",
                             ...) {
  rst <-  gtsummary::as_tibble(x)
  names_rst <- trimws(gsub("[\\*\n\r\t]", "",   names(rst)))
  attr(rst, "N") <- x$N
  Output(rst,
         caption = caption,
         note = note,
         col.names = names_rst,
         ...)
}
