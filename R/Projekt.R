#' Projekt
#'
#' Ein R-File ruft die Interne Funktion Projekt_html() auf und ein Rmd-File Rmd_Start().
#'
#'
#' Start eines neuen Prpjekts
#' n.
#' @param myformat HTML, Spin, Knit, Rpres oder Text. Spin ist knitr wobei die Ausgabe der Tabellen mit html erfolgt
#' @param Projektname Bezeichnung des Projektes (gilt auch fuer die HTML Seite)
#' @param datum Datum zur Dokumentation
#' @name Projekt
#'
#' @return html-Pfad oder Projektname als Text
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#'  Projekt()
#'
#'
#'  # Auswertung
#'
#'
#'  End()
#'
#' }
#'
Projekt <- function(myformat = "",
                    Projektname = "Demo",
                    datum = date()
                    #,silent =TRUE
                  ) {
  cat("Projekt: ", Projektname, "\nWd: ", getwd(), "\n")
  set.seed(0815)
  path <- "test.R"

  if (!is.null(sys.calls())) {
    path <- tolower(as.character(sys.call(1))[2])
  }

  if(is.null(myformat)) {
    path <- "test.txt"
    myformat <- "text"
  }else{
    myformat <- tolower(myformat)
    }

  is_r_file <- grepl("\\.r$", path, perl = TRUE, ignore.case = TRUE)
  is_not_knit <- which_output() %in% c("text",	"html")


  if (is_r_file & is_not_knit) {
    if (myformat == "html") {
      HTML_Start(
        Projektname = Projektname,
        datum = datum
      )

     # if(!silent) cat("\n HTML_Start \n" )
    }
    else{
        projekt_settings(withprompt="> ")
    }

  } else   {
    Rmd_Start(
      myformat = if(myformat=="") which_output() else myformat,
      Projektname = Projektname,
      datum = datum
    )
  #  if(!silent) cat("\n Rmd_Start \n" )
  }


  # if(!silent){
  #
  #   cat("\nformat: ",myformat, " output:  ", which_output(),"\n", path )
  #   cat( "\nscript: ",  get_scriptpath() , "\n")
  # }

}


#' @rdname Projekt
#' @description Einstellungen fuer .Rmd files hier werden keine Folder erstellt
Rmd_Start <- function (myformat,
                                Projektname,
                                datum
                                ){
 # set_default_params(list(Tab_Index = 0, Abb_Index = 0))
  set_opt(output = myformat)
  invisible(
    paste(
      Projektname,
      "\n Datum: ",
      datum,
      ", Software: ",
      R.version.string ,
      ", Link: www.R-project.org/\nFile: ",
      get_scriptpath()
    )
  )
}


#' @description HTML_Start ist ein e Kopie von  R2HTML
#' @noRd
HTML_Start <- function (Projektname = "Demo",
                        datum) {
  output.dir = file.path(getwd(), get_opt("html_folder"))

  if (Projektname == "Demo")
    setwd("C:/Users/wpete/Dropbox/3_Forschung/R-Project")
  #  path.expand("~")
  set_opt(output = "html")
  #-- Fehler Abfangen
  if (options()$prompt[1] == "HTML> ") {
    options(prompt = "> ")
    return()
  }

  creat_folder(
    output.dir = output.dir,
    fig_folder = get_opt("fig_folder"),
    css.file = file.path(output.dir, "layout.css")
  )


  projekt_settings(
    withprompt = "HTML> ",
    extension = "html",
    file = file.path(
      output.dir,
      paste(
        stp25tools::cleansing_umlaute(Projektname),
        ".html",
        sep = ""
      )
    )
  )


  HTML_open(Projektname)

  HTML_P(Projektname)
  HTML_P(
    paste(
      "Datum: ",
      datum,
      ", Software: ",
      R.version.string,
      ", Link: www.R-project.org/"
    )
  )
  HTML_P(paste("File: ", paste0(
    gsub('C:/Users/wpete/Dropbox/', '../' ,  getwd()),
    "/",
    get_scriptpath()
  )))
}

#' @rdname Projekt
#' @description  \subsection{End}{
#' Zuruecksetzen der Einstellungen und Aufruf des Browsers browser = getOption("browser")}
#' @param browser Ie oder Chrome
#' @param output,file intern
#' @export
End <- function(browser = "iexplore",
                output = options()$prompt[1] == "HTML> ",
                file = HTMLGetFile(),
                ...) {
  if (output & !is.null(file)) {
    if (browser == "firefox")
      file <-  paste0("file:///", file)
    brwsr <-
      list(chrome = "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe",
           firefox = "C:/Program Files/Mozilla Firefox/firefox.exe",
           iexplore =  "C:/Program Files (x86)/Internet Explorer/iexplore.exe")
    browser <-  brwsr[[browser]]
    HTML_close()
    browseURL(file, browser = browser)
  }
  projekt_settings()
  #cat(format(Sys.time(), "%a %d %b %Y %H:%M:%S ") )
}





# HTML_End <- function(browser = "iexplore") {
#   file <-if(browser== "firefox") paste0("file:///", HTMLGetFile() )
#   else HTMLGetFile()
#   brwsr<- list(
#     chrome= "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe",
#     firefox= "C:/Program Files/Mozilla Firefox/firefox.exe",
#     iexplore=  "C:/Program Files (x86)/Internet Explorer/iexplore.exe")
#   browser<-  brwsr[[browser]]
#
#   HTML_close()
#   browseURL(file, browser = browser)
#
#
#   set_default_params(list(Tab_Index = 0, Abb_Index = 0, file.name.index = 0))
#   #  reset_lattice() # assign("old.par", par(no.readonly = TRUE), envir = .step25Env)
#   set_opt(output = "text")
#   options(prompt = "> ")
#
#   HTMLGetFile()
# }





#' @rdname Projekt
#' @export
Stop <- function() {
  End()
  stop("Nein das ist kein Fehler!\n", call. = FALSE)
}




#'  get_scriptpath
#'
#'  Ausfuehrendes File finden
#' Quelle: https://stackoverflow.com/questions/18000708/find-location-of-current-r-file
#'
#' @noRd
get_scriptpath <- function() {
  # location of script can depend on how it was invoked:
  # source() and knit() put it in sys.calls()
  path <- NULL

  if (!is.null(sys.calls())) {
    # get name of script - hope this is consisitent!
    path <- as.character(sys.call(1))[2]

    # make sure we got a file that ends in .R, .Rmd or .Rnw
    #Achtung grep sit falssch!!!!

    if (grepl("..+\\.[R|Rmd|Rnw]",
              path,
              perl = TRUE,
              ignore.case = TRUE))  {
      path <- strsplit(path, "/")[[1]]

      return(path[length(path)])
    } else {
    #  message("Obtained value for path does not end with .R, .Rmd or .Rnw: ",
     #         path)
    }
  } else{
    # Rscript and R -f put it in commandArgs
    args <- commandArgs(trailingOnly = FALSE)
  }
  return(path)
}





# set_default_params <- function(params) {
#   params <- as.list(params)
#   env    <-  .GlobalEnv
#   invisible(
#     lapply(names(params),
#            function(key) {
#              assign(key,
#                     params[[key]],
#                     envir = env, inherits = FALSE)}
#     ))
# }


#' Parameter fuer HTML
#'
#' @param withprompt,extension,file zu fixierende einstellungen
#' @noRd
projekt_settings <- function(
  withprompt = "> ",
  extension = "txt",
  file = NULL) {

  options(prompt = withprompt)
  set_opt(output = extension)

  assign("Tab_Index", 0, envir = .step25Env)
  assign("Abb_Index", 0, envir = .step25Env)
#  assign("file.name.index", 0, envir = .step25Env)
#  assign("old.par", par(no.readonly = TRUE), envir = .step25Env)
  assign(".HTML.file", file, .step25Env)

}


#' Kopie von R2HTML::HTMLGetFile
#' @noRd
.step25Env <- new.env(parent = emptyenv())


#' Kopie von R2HTML::HTMLGetFile
#' @noRd
HTMLGetFile <- function (){
  if (exists(".HTML.file", .step25Env))
    get(".HTML.file", .step25Env)
  else
    NULL
}


#' Folder erstellen
#' @noRd
creat_folder <- function(output.dir,
                         fig_folder,
                         css.file = NULL) {
  if (!dir.exists(output.dir)) {
    dir.create(
      output.dir,
      showWarnings = TRUE,
      recursive = FALSE,
      mode = "0777"
    )
    cat("output.dir: ", output.dir, "\n")
  }

  if (!dir.exists(fig_folder)) {
    dir.create(
      fig_folder,
      showWarnings = TRUE,
      recursive = FALSE,
      mode = "0777"
    )
    cat("fig_folder: ", fig_folder, "\n")
  }

  if (!is.null(css.file)) {
    if (!file.exists(css.file)) {
      cat(MyCss(), file = css.file)
      cat("css.file: ", css.file, "\n")
    }
  }
}



