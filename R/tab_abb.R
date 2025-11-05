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
#' @param caption.nr.prefix was soll vorne stehen
#' @export
#'
Tab <- function (caption = "",
                 caption.nr.prefix = get_opt("table", "caption.nr.prefix")) {
  if (exists("Tab_Index", .step25Env)) {
    x <- get("Tab_Index", .step25Env)
    if (is.numeric(x)) {
      x <- x + 1
      assign("Tab_Index", x, envir = .step25Env)
    } else if (is.character(x) & nchar(x) == 1L) {
      x <-  tolower(x)
      x2 <- letters[[which(letters  %in% x) + 1]]
      assign("Tab_Index", x2, envir = .step25Env)


    }
    else{
      assign("Tab_Index", 0 , envir = .step25Env)
    }
  }
  else{
    x <- 1
    assign("Tab_Index", x, envir = .step25Env)
  }
  if (is.null(caption.nr.prefix))
    paste0("Tab ", x, ": ", caption)
  else
    paste0("Tab ", caption.nr.prefix, x, ": ", caption)
}


#' @rdname Tab_Abb
#' @param x numeric or character
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
#' @param N Stichprobe
#' @param include.n,include.tabel.number  wird über get_opt gesteuert
#' entweder direkt include.n oder indirekt mit caotion =TRUE/FALSE
#' @description  Ueberschrift aus stp-Objekt:
#' @examples
#' # example code
#'
#' # \dontrun{
#' # Caption(caption, attr(x, "caption"))
#' # oder Note(note, attr(x, "note")) mit
#' # }
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


#' @rdname Tab_Abb
#' @export
#'
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
Abb <- function (filename = "",
                 caption = "",
                 folder = get_opt("fig_folder")) {
  if (exists("Abb_Index", .step25Env))
    x <-  get("Abb_Index", .step25Env) + 1
  else
    x <- 1

  assign("Abb_Index", x, envir = .step25Env)

  list(
    HtmlCaption = paste0("Abb", x, ": ", caption),
    GraphFileName = paste0("Fig", x, filename),
    Name = paste0(folder, "Fig", x, filename)
  )
}
