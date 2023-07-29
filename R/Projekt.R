#' Projekt
#'
#' Ein R-File ruft die Interne Funktion Projekt_html() auf und ein Rmd-File Rmd_Start().
#'
#' Start eines neuen Prpjekts
#'
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
#'  # Auswertung
#'
#'  End()
#'
#' }
Projekt <- function(myformat = "",
                    Projektname = "Demo",
                    datum = date()
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
  }

}


#' @rdname Projekt
#' @description Einstellungen fuer .Rmd files hier
#' werden keine Folder erstellt
Rmd_Start <- function (myformat,
                                Projektname,
                                datum
                                ){
 # set_default_params(list(Tab_Index = 0, Abb_Index = 0))
  set_opt(output = myformat)
  html_tbl_css()

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

  if (Projektname == "Demo") {
    #setwd("C:/Users/wpete/Dropbox/3_Forschung/R-Project")

    demo_file <- path.expand("~/Spielwiese")
    if (!file.exists(demo_file)) {
      dir.create(
        demo_file,
        showWarnings = TRUE,
        recursive = FALSE,
        mode = "0777"
      )

    }
    setwd(demo_file)
    cat("\nDemo: ", demo_file, "\n")
  }
  #  path.expand("~")
  set_opt(output = "html")
  #-- Fehler Abfangen
  if (options()$prompt[1] == "HTML> ") {
    options(prompt = "> ")
    #  cat("\n Gab es vorhin einen Fehler?\n")
    cat("\n",
        crayon::bgMagenta('Gab es vorhin einen Fehler?'),
        "\n")
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

  html_tbl_css()

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
#'
#' Die Funktion geht nicht unter Linux/Apple
#'
#' @param browser Ie oder Chrome
#' @param output,file intern
#' @export
End <- function(browser = NA,
                output = options()$prompt[1] == "HTML> ",
                file = HTMLGetFile(),
                ...) {
  if (output & !is.null(file)) {
    if (browser %in% c("firefox", "meleon"))
      file <-  paste0("file:///", file)

    browser <-
      switch(
        browser,
        meleon    = "C:/Program Files (x86)/K-Meleon/k-meleon.exe",
        chrome    = "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe",
        firefox   = "C:/Program Files/Mozilla Firefox/firefox.exe",
        iexplore  = "C:/Program Files (x86)/Internet Explorer/iexplore.exe",
        word      = "C:/Program Files (x86)/Microsoft Office/Office14/WINWORD.EXE",
        getOption("browser")
      )


    HTML_close()
    browseURL(file, browser = browser)
  }
  projekt_settings()
  #cat(format(Sys.time(), "%a %d %b %Y %H:%M:%S ") )
}


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


html_tbl_css <- function() {
  # Ausgabe von Tabellen Formatieren
  # speichert in options("htmlTable.theme")
  htmlTable::setHtmlTableTheme(
    css.rgroup = "font-weight: normal; margin: 0; padding: 0;",
    css.rgroup.sep = "",

    # in html geht das mit den linien
    # aber die lassen sich nicht ins Word Kopieren
    # css.rgroup.sep = "border-top: 1px solid green;",


    css.tspanner = "font-weight: 900; text-align: left;",
    css.tspanner.sep = "border-top: 1px solid #BEBEBE;",
    css.total = "border-top: 1px solid #BEBEBE; font-weight: 900;",
    css.cell = "margin: 0; padding: 0;",
    css.cgroup = "margin: 0; padding: 0; vertical-align: middle;",
    css.header = "margin: 0; padding: 0; font-weight: 900; vertical-align: middle;",
    css.header.border_bottom = "border-bottom: 1px solid grey",
    css.class = "gmisc_table",
    css.table = "margin-top: 1em; margin-bottom: 1em;",
    spacer.celltype = "double_cell",
    spacer.css.cgroup.bottom.border = "1px solid white",
    spacer.content = "",
    spacer.css = "width: 2px;",
    # Positions
    pos.rowlabel = "bottom",
    pos.caption = "top"
  )
}
