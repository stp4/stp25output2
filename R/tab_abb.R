#' Tabellen und Abbildungs Nummern
#'
#' Nummerierte Abbildung default Folder ist fig
#'
#' @name Tab_Abb
#'
#' @param caption  Name der Tabelle/Abbildung
#' @return Name plus Nummer
NULL

#' @rdname Tab_Abb
#' @export
#' @param caption
#'
Tab <- function (caption = "",
                 caption.nr.prefix = get_opt("table", "caption.nr.prefix")) {
  if (exists("Tab_Index", .step25Env))
    x <-  get("Tab_Index", .step25Env) + 1
  else
    x <- 1

  assign("Tab_Index", x, envir = .step25Env)
  if (is.null(caption.nr.prefix))
    paste0("Tab ", x, ": ", caption)
  else
    paste0("Tab ", caption.nr.prefix, x, ": ", caption)
}


#' @rdname Tab_Abb
#' @export
Tab_Index <- function (x = NULL)
{
  if (exists("Tab_Index", .step25Env)) {
    if (is.null(x)) {
      get("Tab_Index", .step25Env)
    }
    else{
      assign("Tab_Index", x, envir = .step25Env)
      x
    }
  }
  else
    0
}


#' @rdname Tab_Abb
#' @param atr in Caption: alternativer Text
#' @param include.n,include.tabel.number  wird über get_opt gesteuert
#' entweder direkt include.n oder indirekt mit caotion =TRUE/FALSE
#' @description  Ueberschrift aus stp-Objekt:
#' \dontrun{
#' Caption(caption, attr(x, "caption"))
#' oder Note(note, attr(x, "note")) mit
#' }
Caption <- function(caption = NULL,
                    atr = NULL,
                    N = NULL,
                    include.n =  get_opt("table", "include.n"),
                    include.tabel.number =  get_opt("table", "include.tabel.number")) {
#  cat("\n", class(caption), is.null(caption),"\n")
  # Optionen aufdröseln
  if (is.logical(get_opt("caption"))) {
    if (get_opt("caption"))
      include.n <- TRUE
    else
      include.tabel.number <- FALSE
  }
 # cat("\n", include.n, include.tabel.number, "\n")

  # Ueberschrift definieren
  if (!is.character(caption)) {
    if (is.character(atr)) caption <- atr
    else caption <- ""
  }

  if (include.n & !is.null(N))
    caption <- paste(caption, " (N=", N, ")", sep = "")


  if (include.tabel.number) Tab(caption)
  else caption
}


#' @rdname Tab_Abb
#' @param x Text
Note <- function(x = NULL,
                 atr = NULL) {
  if (is.null(x) & (!is.null(atr)))
    atr
  else if (!is.null(x))
    x
  else
    ""
}

