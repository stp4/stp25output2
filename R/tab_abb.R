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
#' @param atr in Caption: alternativer Text
#' @description  Ueberschrift aus stp-Objekt:
#'
#' Caption(caption, attr(x, "caption"))
#' oder Note(note, attr(x, "note")) mit
#'
Caption <- function(caption = NULL,
                    atr = NULL,
                   # output =  which_output(),
                    N = NULL,
                    include.n = get_opt("caption"))
  {



  # print( list(
  #        caption=caption,
  #        atr = atr,
  #        output=output,
  #        n=N,
  #        include.n =include.n)
  #        )

  if( is.character(caption) ){
    if(!is.null(include.n) & !is.null(N))
      caption <- paste(caption, " (N=", N, ")", sep="")
  }
  else if( is.character( atr )) caption <-  atr
  else  if(!is.null(include.n) & !is.null(N))
         caption <- paste( "N=", N, sep="" )
  else caption <- ""

  #if (output == "html" | output == "text" | output == "markdown_html")
  Tab(caption)
}

#' @rdname Tab_Abb
#' @param x Text
Note<- function(x=NULL,
                atr=NULL){
  if(is.null(x) & (!is.null(atr))) atr
  else if(!is.null(x)) x
  else ""
}

