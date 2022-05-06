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
Tab <- function (caption = ""){

  if (exists("Tab_Index", .step25Env))
   x <-  get("Tab_Index", .step25Env) + 1
  else x <- 1

  assign("Tab_Index", x, envir = .step25Env)
  paste0("Tab ", x, ": ", caption)
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
#' @export
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
Abb <- function (filename="",
                 caption="",
                 folder = get_opt("fig_folder"))

{


  if (exists("Abb_Index", .step25Env))
    x <-  get("Abb_Index", .step25Env) + 1
  else x <- 1

  assign("Abb_Index", x, envir = .step25Env)



  list(
    HtmlCaption=paste0("Abb", x, ": ", caption),
    GraphFileName = paste0("Fig", x, filename),
    Name =paste0(folder, "Fig", x, filename)
  )
}




#' @rdname Tab_Abb
#' @param atr in Caption: alternativer Text
#' @description  Ueberschrift aus stp-Objekt: Caption(caption, attr(x, "caption"))
#' oder Note(note, attr(x, "note")) mit
Caption <- function(caption = NULL,
                    atr = NULL,
                    output =  which_output()) {
  if (is.null(caption) & (!is.null(atr)))
    caption <-  atr
  else if (is.null(caption))
    caption <- ""


  #if (output == "html" | output == "text" | output == "markdown_html")
    Tab(caption)
 # else
 #   caption
}

#' @rdname Tab_Abb
#' @param x Text
Note<- function(x=NULL,
                atr=NULL){
  if(is.null(x) & (!is.null(atr))) atr
  else if(!is.null(x)) x
  else ""
}

