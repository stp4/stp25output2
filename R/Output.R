#' Ausgabe von HTML, Knit oder Text
#'
#' HTML :  htmlTable::htmlTable
#'
#' markdown: knitr::kable -> kableExtra::kable_styling
#'
#' Text: knitr::kable
#'
#' Word: knitr::kable(x, format = "pandoc")
#'
#' Output_word() flextable::regulartable
#'
#'
#' @name Output
#' @param x Objekt liste oder Dataframe
#' @param output FALSE = NULL,
#' @param ... weitere Einstellungen
#' @return HTML oder Textausgabe
#' @export
#'
Output <- function(x, ...) {
  UseMethod("Output")
}


#' @rdname Output
#' @export
Output.character <- function(x, ...){ Text(x) }

#' @rdname Output
#' @param x dataframe
#' @param caption,note  Ueberschrift Fussnote
#' @param output welcher output, text, html, markdown
#' @param css.table,css.cell,align  htmlTable
#'  padding-left: .5em; padding-right: .2em;
#' @param booktabs,latex_options an kableExtra
#' @param linesep linesep = ""  linesep = c("", "", "\\midrule")
#' @param rgroup,n.rgroup an htmlTable
#' @param ... nicht benutzt
#'
#' @return nix
#' @export
#'
#' @examples
#'
#' df1 <- data.frame(
#' term = c("A", "B", "C", "D"),
#' n = c(23, 14, 56, 2),
#' m = c("4.7 (2.4)", "4.1 (2.3)", "8.9 (3.6)", NA)
#' )
#'
#'
#' df2 <- data.frame(
#'   term = c("A", "B", "C", "D"),
#'   G1_k_n = c(23, 14, 56, 2),
#'   G1_k_m = c("4.7 (2.4)", "4.1 (2.3)", "8.9 (3.6)", NA),
#'   G2_n = c(33, 35, 78, 21),
#'   G2_m = c("4.9 (2.7)", "4.7 (2.5)", "4.1 (5.6)", "4.2 (5.6)")
#'   )
#' #+ df-default ,  results='asis'
#' df1 %>% Output()
#'
#' #+ df-false ,  results='asis'
#' df1 %>% Output()
#'
#'
#' #+ df-html
#' df1 %>% Output(output="html")
#'
#' #+ df-text ,  results='asis'
#' df1 %>% Output(output="text")
#'
#' #+ df-word ,  results='asis'
#' df1 %>% Output(output="word")
#'
#'
#' #+ df-mark
#' df1 %>% Output(output="markdown")
#'
#' # df1 %>%  Output(linesep = c("", "", "\\midrule"))
#'
Output.data.frame <-
  function(x,
           caption = NULL,
           note = NULL,
           output =  which_output(),
           split_header = TRUE,
           css.table = 'padding-left: .5em; padding-right: .2em;',
           css.cell = 'padding-left: .5em; padding-right: .2em;',
           booktabs = TRUE,
           latex_options = c("hold_position"),
           linesep = "",
           align = "l",
           rgroup = attr(x, "rgroup", TRUE),
           n.rgroup =  attr(x, "n.rgroup", TRUE),
         #  cgroup = NULL,
         #  n.cgroup = NULL,
           ...) {
    if (nrow(x) == 0)
      return(NULL)
    tbl <- tbl_header(x, split_header=split_header)

    if (output == "docx") {
      # In spin-word geht Word.doc  nicht weil die Ausgabe nicht an knit_print weitergegeben wird.
      return(Output_word(x, caption, note, output, split_header))
    }

    if (output == "text") {
      caption <- Caption(caption, attr(x, "caption"))
      note <- Note(note, attr(x, "note"))
      if (!is.null(tbl$header_above))
        names(x) <-
        ifelse(tbl$header_above2 == "",
               tbl$header,
               paste0(tbl$header_above2, "_", tbl$header))
      else{
        names(x) <- tbl$header
      }
      cat("\n", caption, "\n")
      print(data.frame(x))
      if (is.character(note))
        cat("\n", note, "\n\n")
    }
    else if (output == "html" | output == "markdown_html") {
      caption <- Caption(caption, attr(x, "caption"))
      note <- Note(note, attr(x, "note"))

      tbl$header <-  gsub(" +", '&nbsp;', tbl$header)
      tbl$cgroup <-  gsub(" +", '&nbsp;', tbl$cgroup)

      if (is.null(tbl$header_above)) {
        res <- htmlTable::htmlTable(
          insert_nbsp(x),
          caption = caption,
          header = tbl$header,
          rnames = FALSE,
          rgroup = rgroup,
          n.rgroup = n.rgroup,
          align = align,
          tfoot = note,
          escape.html = FALSE,
          css.table = css.table,
          css.cell = css.table,
          ...
        )
      }
      else{
        res <- htmlTable::htmlTable(
          insert_nbsp(x),
          caption = caption,
          header = tbl$header,
          rnames = FALSE,
          rgroup = rgroup,
          n.rgroup = n.rgroup,
          cgroup = tbl$cgroup,
          n.cgroup = tbl$n.cgroup,
          align = align,
          tfoot = note,
          escape.html = FALSE,
          css.table = css.table,
          css.cell = css.table,
          ...
        )
      }

      # Copy-Paste problem mit Word
      res <- gsub("grey;", "black;", res)

      if (output == "html") {
        HTML_BR()
        HTML_CENTER(res)
        HTML_BR()
      } else{
        print(res)
      }
    }
    else if (output == "latex") {
      x <- cleanup_nbsp(x)

      if (is.null(tbl$header_above)) {
        print(kableExtra::kable_styling(
          knitr::kable(
            x,
            format = "latex",
            #  wegen kableExtra in stp25output ( Null ist aber die Default Einstellung)
            #  die Caption wird bei Null oberhalb der Tabelle angebracht bei
            #  Latex am Rand (Margin)
            row.names = FALSE,
            col.names = tbl$header,
            booktabs = booktabs,
            caption = caption,
            linesep = linesep
          ),
          latex_options = latex_options
        ))
      }
      else {
        print(
          kableExtra::add_header_above(
            kableExtra::kable_styling(
              knitr::kable(
                x,
                format = "latex",
                row.names = FALSE,
                col.names = tbl$header,
                booktabs = booktabs,
                caption = caption,
                linesep = linesep
              ),
              latex_options = latex_options
            ),
            tbl$header_above
          )
        )
      }

      if (is.character(note))
        Text(note)
    }
    else{
      x <- cleanup_nbsp(x)

      if (!is.null(tbl$header_above))
        tbl$header <-
          ifelse(tbl$header_above2 == "",
                 tbl$header,
                 paste0(tbl$header_above2, " / ", tbl$header))

      print(
        knitr::kable(
          x,
          row.names = FALSE,
          col.names = tbl$header,
          booktabs = booktabs,
          caption = caption,
          format = output,
          linesep = linesep
        )
      )
      if (is.character(note))
        cat("\n", note, "\n\n")
    }

    invisible(x)
  }


#' @rdname Output
#' @description Output.iste: einzeln in einen data.frame transformieren
#' @export
#'
Output.list <- function(x,
                        caption = NULL,
                        note = NULL,
                        output =  which_output(),
                        ...) {
  if (!is.null(caption))
    Head5(caption)
  if (!is.null(note))
    Head5(note)
  if (output == "docx") {
    if (length(x) > 1)
      Text("Weitere Tabellen:", names(x)[-1])
    x <- x[[1]]
    return(Output_word(x, ...))
  } else {
    res <- list()
    for (i in 1:length(x)) {
      if (inherits(x[[i]], "list")) {
        print(x[[i]])
        stop("Recrusive Liste!")
      }
      res[[i]] <- Output(x[[i]], output = output, ...)
    }
    invisible(x)
  }
}


#' Header aufbereiten
#'
#' Interne Funktion
#' @param x data.frame
#' @param col.names colnames(x)
#' @param split_header  spllit header
#'
#' @return list(header,header_above,cgroup,n.cgroup,header_above2,)
#' @noRd
tbl_header <-
  function(x,
           col_names = colnames(x),
           split_header = TRUE,
           split_heder.chr = "_") {

    header <- col_names
    header_above <- NULL
    cgroup <- NULL
    n.cgroup <- NULL
    a1 <- a2 <- NULL

    if (split_header)
    {
      result_tbl_names <- stringr::str_split(col_names, split_heder.chr)
      ebenen <- max(lengths(result_tbl_names), na.rm = TRUE)

      if (ebenen == 3) {
        #Fehler mit Name_name_SD abfangen
        result_tbl_names <-
          stringr::str_split(col_names, split_heder.chr , 2)
        ebenen <- max(lengths(result_tbl_names), na.rm = TRUE)
      }

      if (ebenen == 2) {
        a1 <- sapply(result_tbl_names, "[", 1)
        a2 <- sapply(result_tbl_names, "[", 2)

        nas <- is.na(a2)
        a2[nas] <- a1[nas]
        a1[nas] <- ""
        header <- a2
        cgroup <-  rle(a1)$values
        n.cgroup <- rle(a1)$lengths
        header_above <- n.cgroup
        # add_header_above zero-length variable macht probleme daher
        names(header_above) <-
          ifelse(nchar(cgroup) == 0, " ", cgroup)
      }
    }

    list(
      header = header,
      header_above = header_above,
      cgroup = cgroup,
      n.cgroup = n.cgroup,
      header_above2 = a1
    )
  }

#' Leehrzeiche auffüllen
#'
#' Interne Funktion Html &nbsp; ergaenzen
#' @param x data.frame
#'
#' @return data.frame
#'
insert_nbsp <- function(x) {
  data.frame(plyr::llply(x, function(x) {
    if (is.character(x))
      gsub(" +", '&nbsp;', x)
    else
      x
  }))
}

#' nbsp entfernen
#'
#' Interne Funktion Html &nbsp; erntfernen
#' @param x data.frame
#'
#' @return data.frame
#' @noRd
cleanup_nbsp <- function(x) {
  data.frame(plyr::llply(x, function(strg) {
    if (is.character(strg) | is.factor(strg)) {
      strg <- gsub("&nbsp;", ' ', strg)
      strg[is.na(strg)] <- ""
      strg
    }
    else
      strg
  }))
}


#' @rdname Output
#' @export
#'
Output.default <- function(x, ...) {
  rslt <- stp25tools::fix_to_df(x)
  if (is.data.frame(rslt))
    Output.data.frame(rslt, ...)
  else {
    warning("Unbekanter Objekt-Typ: ", class(x)[1])
    x
  }

}


