#' Reporting the participant data
#'
#'
#' @param data A data frame.
#' @param caption Tabellen Überschrift
#' @param ..	Arguments passed to or from other methods.
#'
#' @examples
#'
#' data <- data.frame(
#'   "Age" = c(22, 23, 54, 21, 8, 42, 18, 32, 24, 27, 45),
#'   "Sex" = c("Intersex", "F", "F", "M", "M", "M", "F", "F", "F", "F", "F"),
#'   "Gender" = c("N", "W", "W", "M", "M", "M", "W", "W", "W", "W", "W"),
#'   "Race" = c(
#'     "Black", NA, "White", "Asian", "Black", "Arab", "Black",
#'     "White", "Asian", "Southeast Asian", "Mixed"
#'   )
#' )
#'
#' data<- rbind(data,data,data) %>% filter2(Gender!="N")
#'
#' report_data(data )
#'
#' @export
report_data <- function(data,
                        caption = "Excluded Participants",
                        ...) {
  if (is.null(attr(data, "filter")))
    Text("Participants: N =", nrow(data))
  else
    Output(attr(data, "filter"), caption = caption)
}



# data <- data.frame(
#   "Age" = cut(c(22, 23, 54, 21, 8, 42, 18, 32, 24, 27, 45),2),
#   "Sex" = factor(c("Intersex", "F", "F", "M", "M", "M", "F", "F", "F", "F", "F")),
#   "Gender" = factor(c("N", "W", "W", "M", "M", "M", "W", "W", "W", "W", "W")),
#   "Race" = c(
#     "Black", NA, "White", "Asian", "Black", "Arab", "Black",
#     "White", "Asian", "Southeast Asian", "Mixed"
#   )
# )
#
# data<- rbind(data,data,data)# |> filter2(Gender!="N")
#
# report_data(data )
