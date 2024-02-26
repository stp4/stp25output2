#' @rdname Output
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(gtsummary)
#'  tbl_summary_ex1 <-
#' trial |>
#'   select(age, grade, response) |>
#'   tbl_summary() |>
#'   Output()
#' }
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
         header = names_rst,
         ...)
}




#' @rdname Output
#'
#' @export
#'
#' @examples
#'  \dontrun{
#'
#' library(sjPlot)
#' library(sjmisc)
#' library(sjlabelled)
#'
#' data("efc")
#' efc <- as_factor(efc, c161sex, c172code)
#'
#' m1 <- lm(barthtot ~ c160age + c12hour + c161sex + c172code, data = efc)
#' m2 <- lm(neg_c_7 ~ c160age + c12hour + c161sex + e17age, data = efc)
#'
#' sjPlot::tab_model(m1, m2) |>
#'   Output("Side-by-side Regression Models")
#' }
#'
Output.sjTable <- function(x,
                           caption = NULL,
                           note = NULL,
                         #  col.names = NULL,
                           output = which_output(),
                           ...) {
  caption <- Caption(caption)
  Text(caption)
  if (output == "text")
    x
  else
    x$knitr  |> HTML_default()
}
