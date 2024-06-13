#' @param output to insight::display
#' @param digits to insight::display
#' @param ...
#'
#' @rdname Output
#' @description Output.parameters_model: of parameters::parameters Objekts
#' @importFrom insight display
#' @export
#'
#' @examples
#'
#' fit <- lm(breaks ~ wool * tension, data = warpbreaks)
#' parameters::parameters(fit) |> Output(select = -3)
#'
Output.parameters_model <- function(x,
                                    caption = NULL,
                                    note = NULL,
                                    output =  which_output(),
                                    digits = 1,
                                    ...) {
  out <- insight::display(x, format = output, digits = digits)

  if (is.null(caption))
    caption <- attr(x, "model_formula")

  if (is.null(note))
    note <- paste("Model: ",
                  attr(x, "model_class"),", ",
                  attr(x, "test_statistic"),
                  sep = "")

  Output(out[[1]], caption = caption, note = note, ...)
}
