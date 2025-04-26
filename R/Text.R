#' Text Ausgabe
#'
#' Ausgabe von \code{Text()} und Ueberschriften. \code{Head} ist dabei eine
#' Kopie von \code{Text} mit
#' dem Parameter \code{style = 2}
#' @param x Objekt dataframe, liste, vector
#' @param ... one or more R objects, to be converted to character vectors.
#' @param style 0 = p oder normaler Text 2 = h2 also Ueberschrift bei
#' Consolen-Ausgabe
#' @param char ist nicht zu Verwenden Text Trennzeichen bei Consolen-Ausgabe
#' @param output Abrage nach dem Ausgabeformat  which_output()
#' @return Vector
#' @name Text
#' @export
#' @examples
#'
#' \dontrun{
#' #library(stp25)
#' #Projekt("html")
#' Head("Ueberschrift") ## HR=2
#'
#' Text("hallo", "ich bin ein Test")
#' ###plot(sin, -pi, 2*pi,main="Sinus")
#' ###HTMLplot( Caption="Look at this curve!")
#' #End()
#' }
Text <- function(x, ...) {
  UseMethod("Text")
}


#' @rdname Text
#'
#' @export
Text.default<- function(x, ...){
  print(class(x))
}

#' @rdname Text
#'
#' @export
Text.character <- function(...,
                 style = 0,
                 char = "-",
                 output = which_output()) {

  report_html <- function(msg) {
    if (is.null(msg))
      msg <- ""
    if (is.vector(msg)) {
      msg <- unlist(msg)  #-Zeilenumbrueche
      msg <- gsub("\\n", "<BR>", msg)
    } else
      stop("msg must be of type vector")
    if (style == 0 | style == "p") {
      HTML_P(msg)
    }
    else{
      HTML_default(paste("\n <h", style, "> ", msg, "</h", style, ">\n", sep = ""))
      }
  }

  report_txt <- function(msg = NULL) {
    if (is.null(msg))
      msg <- ""
    if (is.vector(msg)) {
      msg <- unlist(msg)
      msg <- strsplit(msg, "\\n")  #-Zeilenumbr?che
      msg <- unlist(msg)
    } else {
      stop("msg must be of type vector")
    }

    char <- substr(char, 1, 1)
    underlined <- function(arg) {
      c(arg, paste(rep(char, max(nchar(
        msg
      ))), collapse = ""))
    }
    border <- function(arg) {
      n <- length(msg)
      ml <- max(nchar(msg))
      space <- paste(rep(" ", ml), collapse = "")
      line <- paste(rep(char, ml + 4), collapse = "")
      msg <-
        paste(msg, substr(rep(space, n), rep(1, n), ml - nchar(msg)), sep = "")
      c(line, paste(char, msg, char), line)
    }
    sfun <- list(underlined = underlined, border = border)


    if (is.numeric(style) &&
        length(style) == 1 && any(style == 1:length(sfun)))
      msg <-
      sfun[[style]](msg)
    else if (is.character(style) &&
             length(style) == 1 && !is.null(sfun[[style]]))
      msg <- sfun[[style]](msg)
    m <- matrix(msg, ncol = 1)
    colnames(m) <- ""
    rownames(m) <- rep("", length(msg))



    print.noquote(m)
  }

  msg <- paste0(...)
  if(grepl("\n#' ", msg))  msg <- gsub("\n#' ", " ", msg)



  if (output == "html") {
    report_html(msg)
  }
  else if (output == "markdown" | output == "markdown_html") {
    if (style > 0) {
      cat("\n") # Wegen spin
      cat(paste(rep("#", style), collapse = ""), msg)
      cat("\n")
    } else {
      cat("\n")
      cat(msg)
      cat("\n")
    }
  }
  else{
    report_txt(msg)
  }
}

#' @rdname Text
#' @param include.levels levels mit ausgeben default = FALSE
#'
#' @export
#' @examples
#' \dontrun{
#'
#' m <- data.frame(
#'   Item = factor(1:3, 1:3, c("A", "B", "C")),
#'   a = (1:3),
#'   b = (1:3) * 2,
#'   c = (1:3) * 3
#' )
#'
#' m <- stp25tools::Label(m, a = "Aplha", b = "Barbar", c = "Ciklyn")
#' Text(m)
#' }
Text.data.frame <- function(x,
                            ...,
                            style = 0,
                            char = "-",
                            include.levels = FALSE,
                            output =  which_output()) {
  dotts <- sapply(lazyeval::lazy_dots(...),
                  function(x)
                    as.character(x[1]))

  if (length(dotts) == 0) {
    dotts <- names(x)
  }

  if (!include.levels)
    Text(
      paste0(dotts, ": ",
                stp25tools::get_label(x[dotts])
             ),
         style = style,
         char = char,
      output=output)
  else{
    lvl <-   sapply(x[dotts], function(y) {
      if (is.numeric(y))
        "numeric"
      else if (is.factor(y))
        paste(levels(y), collapse = "|")
      else if (is.character(y)) {
        y<- factor(y)
        paste(levels(y), collapse = "|")

      }
      else
        "unknown"
    })
    Text(
      paste0(dotts, ": ",
             stp25tools::get_label(x[dotts]), ": ", lvl
             ),
      style = style,
      char = char,
      output=output
    )
  }

}


#' @rdname Text
#' @export
Head<- function( ...,
                 style=3,
                 char = "-"){
  Text(..., style = style, char = char)

}

#' @rdname Text
#' @export
Head1<- function( ... ){
  pagebreak()
  Text(..., style = 1, char = "-")
}

#' @rdname Text
#' @export
Head2<- function( ... ){
  pagebreak()
  Text(..., style = 2, char = "-")
}

#' @rdname Text
#' @export
Head3<- function( ... ){
  Text(..., style = 3, char = "-")
}

#' @rdname Text
#' @export
Head4<- function( ... ){
  Text(..., style = 4, char = "-")
}

#' @rdname Text
#' @export
Head5<- function( ... ){
  Text(..., style = 5, char = "-")
}

#' @rdname Text
#' @description  Anmerkung() ist ein blauer Text.
#' @param prafix Anmerkung
#' @param include.comment logical.
#' @export
Anmerkung <-
  function(...,
           prafix = "Anmerkung",
           include.comment = if (is.null(get_opt("include.comment")))
             TRUE
           else
             get_opt("include.comment")) {
    if (include.comment) {
      HTML_default(
        paste0(
          '<p style="color: #0000FF"><b>',
          prafix,
          ':</b> <br>
          ',
          paste0(...),
          "</p><br>"
        )
      )
    }
  }

#' @rdname Text
#' @param msg,name Name des kunden oder Nachricht
#' @description  Kunde() ist ein rot-brauner Text.
#' @export
#' @importFrom stringr str_split
Kunde <-
  function(x = "",
           msg = NULL,
           name = NULL,
           include.comment = if (is.null(get_opt("include.comment")))
             TRUE
           else
             get_opt("include.comment")) {
    if (include.comment) {
      if (is.null(name)) {
        name <- stringr::str_split(getwd(), "/"  , simplify = TRUE)
        name <- strtrim(gsub("[^::A-Z::]", "", name[length(name)]), 2)
      }

      if (is.null(msg))
        HTML_default(paste('<p style="color:#800000"> <b>', name, ': </b> ', x, '</p>'))
      else
        HTML_default(paste(
          '<p style="color:#800000"> <b>',
          name,
          x,
          ': </b>',
          msg,
          '</p>'
        ))
    }
  }

#' @rdname Text
#' @param ... namen der Librarys
#' @param .bibstyle an format
#' @param output html oder text
#' @return character
#' @export
#' @importFrom utils citation
#'
#' @examples
#' \dontrun{
#' citation_library(base,Hmisc,car,lattice)
#' }
citation_library <- function(...,
                             style = "text",
                             .bibstyle = NULL,
                             output="text") {
  libs <-
    sapply(lazyeval::lazy_dots(...), function(x)
      as.character(x[1]))

  res <-  format(citation(), style, .bibstyle)

  for (lib in libs) {
    x <- citation(lib)
    y <- format(x, style, .bibstyle)
    #  res <- append(res,  "\n  ")
    res <- append(res, y)
  }

  if(output=="text")
    gsub("[<>]", "", paste(res, collapse = "\n\n"))
  else if(output=="html")
    paste("<p>",
          paste( gsub("[<>]", "",res) , collapse = "</p> <p>"),
          "</p>")
  else res

}


#' @rdname Text
#' @description Zitat() ist eine Text  vom Type <blockquote>
#' @export
Zitat <- function(...)
  Text('<blockquote>', ..., "<br></blockquote>")


#' @rdname Text
#' @description Arbeitszeit() Tabelle zur Dokumentation der Arbeiszeit.
#' @param Lines in Arbeitszeit der Input-String
#' @param sep in Arbeitszeit das Input-String-Trennzeichen
#' @importFrom utils read.table
#' @export
Arbeitszeit <- function(Lines,
                        sep = "\\t",
                        ...) {
  zeit <- read.table(zz <- textConnection(gsub(sep, " ", Lines)),
                     header = TRUE)
  close(zz)

  names(zeit) <- c("Datum",  "Start",   "Ende",   "Task")
  zeit$strt <- strptime(zeit$Start, "%H:%M")
  zeit$end <- strptime(zeit$Ende, "%H:%M")
  zeit$Zeit_Stunden <-
    round(as.numeric(difftime(zeit$end, zeit$strt, units = "hours")), 2)
  zeit$Zeit_Summe <- cumsum(zeit$Zeit_Stunden)

  zeit$Pos <-  stringi::stri_extract_first_regex(zeit$Task, "[0-9]+")
  zeit$Task <- gsub("_", " ", zeit$Task)
  zeit$Task <- gsub("[0-9]+", "", zeit$Task)


  Output(zeit[, c("Datum",
                  "Start",
                  "Ende",
                  "Pos",
                  "Task",
                  "Zeit_Stunden",
                  "Zeit_Summe")], ...)
  invisible(zeit)
}


#' @rdname Text
#' @description Sonstige Texteingaben auszaehlen.
#' @export
#' @examples
#'
#' # Sonstige
#' DF <- data.frame(xa = 1:10 , b = letters[1:2], c = LETTERS[1:10]) |>
#'   stp25tools::Label(a = "Aplha", b = "Barbar", c = "Ciklyn")
#' Sonstige(DF, b, c)
#'
Sonstige <- function(x, ..., prefix = "Sonstige:") {
  if (is.vector(x))
    sonst_tabel_text(x, prefix)
  else{
    X <- stp25tools::prepare_data2(x, ...)
    for (i in seq_along(X$measure.vars)) {
      sonst_tabel_text(X$data[[i]], X$row_name[i])
    }
  }
}

sonst_tabel_text <- function(x, prefix) {
  tbl <- table(x)
  x <- names(tbl)
  n <- as.vector(tbl)
  if (any(n > 1)) {
    x <- paste0(x, "  [", n, "]")
  }
  Text(prefix,
       "\n",
       paste(
         "(",
         seq_along(x),
         ") ",
         x,
         sep = "",
         collapse = "\n"
       ))
}
