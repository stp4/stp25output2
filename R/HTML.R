#' HTML Text Formatting Elements
#'
#' HTML_ schreibt direkt ins HTML-File
#' @param x Text
#' @param output HTML, Knit  oder Textfile (ist nicht gedacht zum Aendern)
#' @noRd
HTML_open <- function(title,   layout.css = "layout.css"){
  cat(
    paste0(
'<!DOCTYPE html>
<html>
<head>
  <meta charset="ISO-8859-1">
  <meta name="author" content="Wolfgang Peter">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">

<title>',  title, '</title>
<link rel=stylesheet href="', layout.css, '" type=text/css>
</head>
<body>

'),
    file = HTMLGetFile(), append = FALSE)

}

#' HTML-Fuss
#' @noRd
HTML_close <-
  function(d){
    cat(
      paste0('<p>',
             format(Sys.time(), "%a %d %b %Y %H:%M:%S "),
             '</p>\n</body>\n</html> '),
      file = HTMLGetFile(),  append = TRUE

    )
  }


#' @noRd
HTML_default <- function(x, output =  which_output()) {

  if (output == "html") {
    cat("\n", x, "\n",
        file = HTMLGetFile(),
        sep = "",
        append = TRUE)
  }
  else{
    cat(x)
  }

}



#'  HTML_P <p> Paragraph
#' @noRd
HTML_P <- function (x, output = which_output()){
  if (output == "html")
    HTML_default(paste0("\n<p>", x, "</p>\n"), output)
else
  HTML_default(x, output)
}



#'  HTML_I <i>	Defines italic text
#' @noRd
HTML_I <- function (x, output = which_output()){
  if (output == "html")
    HTML_default(paste0("<i>", x, "</i>"), output)
else if (output == "markdown")
  HTML_default(paste0("_", x, "_"), output)
else
  HTML_default(x, output)}


#'  HTML_B <b>	Defines bold text
#' @noRd
HTML_B <- function (x, output = which_output()){
  if (output == "html")
    HTML_default(paste0("<b>", x, "</b>"), output)
else if (output == "markdown")
  HTML_default(paste0("__", x, "__"), output)
else
  HTML_default(x, output)
}


#'  HTML_S <small>	Defines smaller text
#' @noRd
HTML_S <- function (x, output = which_output()){
  if (output == "html")
    HTML_default(paste0("\n<small>", x, "</small>\n"), output)
else
  HTML_default(x, output)
}


#'  HTML_HR <hr> Lienie
#' @noRd
HTML_HR <- function (x, output = which_output()){
  if (output == "html")
    HTML_default("<hr>",  output)
else if (output == "markdown")
  HTML_default("***", output)
else
  HTML_default("--------------------", output)
}

#'  HTML_DIF <dif> Formatierungsbereich
#' @noRd
HTML_DIF <- function (x, output = which_output()){
  if (output == "html")
    HTML_default(paste0("<dif>", x, "</dif>"), output)
else
  HTML_default(x, output)
}

#'  HTML_CENTER   zentrieren
#'
#'  align='center' entspricht nicht dem HTML-Standard aber beim Copy-Paste in Word ist das notwendig.
#'
#' @noRd
HTML_CENTER <- function (x, output = which_output()){
  if (output == "html")
    HTML_default(paste0("<div class='center' align='center'>", x, "</div>"), output)
else
  HTML_default(x, output)
}

#'  HTML_NULL wie  HTML_BR <br> Zeilenumbruch aber mit Text
#' @noRd
HTML_NULL <- function (x, output = which_output()){
  if (output == "html")
    HTML_default(paste0(x, "<br>"), output)
  else
  NULL}



#'  HTML_BR <br> Zeilenumbruch aber ohne Text
#' @noRd
HTML_BR <- function (x, output = which_output()){
  if (output == "html")
    HTML_default("<br>", output)
else
  NULL}



#' @description HTML_Start ist ein e Kopie von  R2HTML
#' @noRd
HTML_Start <- function (Projektname = "Demo",
                        datum = date(),
                        fig_folder = get_opt("fig_folder"),
                        html_folder = get_opt("html_folder"),

                        withprompt = "HTML> ",
                        .extension = "html",
                        .myDir = getwd(),
                        .filename = stp25tools::cleansing_umlaute(Projektname),
                        .output.dir = file.path(.myDir, html_folder),
                        .HTML.file  = file.path(.output.dir,
                                                paste(.filename, ".", .extension,
                                                      sep = "")),
                        .css.file = file.path(.output.dir, "layout.css")){
  if (Projektname == "Demo")
    setwd("C:/Users/wpete/Dropbox/3_Forschung/R-Project")
  #  path.expand("~")
  set_opt(output = "html")
  #-- Fehler Abfangen
  if (options()$prompt[1] == withprompt) {
    options(prompt = "> ")
    return()
  }

  creat_folder(.output.dir, fig_folder, .css.file)
  projekt_settings(withprompt, .extension, .HTML.file)

  HTML_open(Projektname, .HTML.file)

  HTML_P(Projektname)
  HTML_P(paste("Datum: ",   datum,
               ", Software: ", R.version.string,
               ", Link: www.R-project.org/"))
  HTML_P(paste("File: ",paste0(
    gsub('C:/Users/wpete/Dropbox/', '../' ,  getwd()),  "/",  get_scriptpath())) )



}









#' CSS erstellen
#' @noRd
MyCss <- function() {
  '
/*
* === MyCss layout.css MAJOR SECTION HEADING ===
*/


  body {
  background: #FFFFFF;
  color: #000000;
  font-family: Verdana, Arial, Helvetica, sans-serif;
  font-size: 10pt;
  font-weight: normal;
  line-height: normal;
  }

  H1 {
  font-family: Verdana, Arial, Helvetica, sans-serif;
  font-size: 18pt;
  font-style: normal;
  font-weight: bold;
  line-height: 25pt;
  color: #004080;
  }

  H2 {
  font-family: Verdana, Arial, Helvetica, sans-serif;
  font-size: 16pt;
  font-style: normal;
  font-weight: bold;
  line-height: 20pt;
  color: #004080;
  }

  H3 {
  font-family: Verdana, Arial, Helvetica, sans-serif;
  font-size: 14pt;
  font-style: normal;
  font-weight: bold;
  line-height: 16pt;
  color: #004080;
  }

  H4 {
  font-family: Verdana, Arial, Helvetica, sans-serif;
  font-size: 12pt;
  font-style: normal;
  font-weight: bold;
  color: #000000;
  line-height: 10pt;
  }

  H5 {
  font-family: Verdana, Arial, Helvetica, sans-serif;
  font-size: 10pt;
  font-style: normal;
  font-weight: bold;
  color: #000000;
  line-height: 10pt;
  }

  TABLE, TH, TD  {
  font-family: Arial, Helvetica,  Helvetica, sans-serif;
  font-size: 8pt;
  font-style: normal;
  line-height: normal;
  }

  LI {
  font-family: "Times New Roman", Times, serif;
  font-size: 10pt;
  }

  A {
  font-family: "Times New Roman", Times, serif;
  font-size: 10pt;
  text-decoration: none;
  }

  P{
  font-family: "Times New Roman", Times, serif;
  font-style: normal;
  font-size: 10pt;
  }


  .class{
  margin-left: auto;
  margin-right: auto;
  }


  .gmisc_table{
  margin-left: auto;
  margin-right: auto;
  }

  '
}




#
#Copie von
#R2HTML::HTMLStop
#@noRd
#
# HTMLStop2 <- function ()
# {
#   invisible(removeTaskCallback("HTML"))
#   .HTMLTmpEnv <- get(".HTMLTmpEnv", envir = .HTMLEnv)
#   options(prompt = get("oldprompt", envir = .HTMLTmpEnv))
#   .tmp = get(".HTML.file", envir = .HTMLTmpEnv)
#   HTMLEndFile(file = get(".HTML.file", envir = .HTMLTmpEnv))
#   rm(".HTMLTmpEnv", envir = .HTMLEnv)
#   invisible(return(.tmp))
# }


