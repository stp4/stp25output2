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
Output2 <- function(x,
                    ...,
                    note = NULL,
                    header = NULL,
                    output =  which_output()) {
  Output(
    x,
    caption = paste(...),
    note = note,
    header = header,
    output =  output
  )
}


#' @rdname Output
#' @export
#' @examples
#'
#'  Output("
#' | # | Modellname | Formel (R) | Grafik | Hypothesen |
#'   |---|-----------|-----------|--------|-----------|
#'   | **M-A** | Biomarker → MRS (univariat) | `d.MRS ~ d.cort` / `~ d.ammp8` / `~ d.pyri` | Scatterplot mit Regressionslinie, facettiert nach Biomarker | H1a, H1b, H1c |
#'   | **M-B** | Hormone → aMMP-8 | `d.ammp8 ~ d.fsh + d.estr` | Scatterplot-Matrix (ggpairs) | H1f |
#'   | **M-C** | Hormone → MRS-Subskalen | `d.MRS.vasomotorisch ~ d.fsh + d.estr` | Heatmap: Korrelation MRS-Subskalen × Hormonparameter | H1g |
#'   | **M-D** | Cortisol → aMMP-8 | `d.ammp8 ~ d.cort` | Scatterplot + Smooth | H1d |
#'   | **M-E** | aMMP-8 → Pyridinium | `d.pyri ~ d.ammp8` | Scatterplot + Smooth | H1e |
#'   | **M-F** | Kausale Kette (sequentiell) | Drei verkettete lm()-Schritte: Cortisol → aMMP-8 → Pyridinium → MRS | Pfaddiagramm (ggraph / semPlot) | H1a–H1e |
#'   | **M-G** | Treatment-Moderation | `d.MRS ~ d.ammp8 * treatment` | Grouped Scatterplot, getrennte Regressionslinien | Übergeordnete H1 |
#'   ")
#'
Output.character <-
  function(x, ...) {
    pipes <-
      sum(lengths(gregexpr("\\|", x)))

    if (pipes < 4)
      Text(x)
    else {
      x <- gsub("\n\\s*[-|\\s]+\n", "\n", x)
      x_1 <- gsub("[\n\t ]", "", x)
      start_end <- (all(c(substr(x_1, 1, 1), substr(x_1, nchar(x_1), nchar(x_1))) == "|"))

      x <-  readr::read_delim(
        file = I(x),
        delim = "|",
        trim_ws = TRUE,
        # strip.white = TRUE,
        # bereinigt Leerzeichen
        show_col_types = FALSE
      )
      if (start_end)
        x <- x[-c(1, ncol(x))]

      Output(x, ...)

    }
  }



#' @rdname Output
#' @param x dataframe
#' @param caption,note  Ueberschrift Fussnote
#' @param output welcher output, text, html, markdown
#' @param header,col.names new name for the header row in the tables
#' @param select,col,row Selecting columns or rows
#' @param na.rm NA replace by ""
#' @param wrap wrap_result Insert line breaks in the tables set_opt(table = list(wrap = TRUE, wrap_result = TRUE))
#' @param css.table,css.cell,align  htmlTable
#'  padding-left: .5em; padding-right: .2em;
#' @param booktabs,latex_options an kableExtra
#' @param linesep linesep = ""  linesep = c("", "", "\\midrule")
#' @param rgroup,n.rgroup an htmlTable rgroup = c("A", "B", "C")
#' n.rgroup wie oft kommt A b und C vor.
#' Die Position kann über   rgroup = c("A"=1, "B"=3, "C" =6 ) eingestellt werden
#' @param ... nicht benutzt
#'
#' @return input
#' @importFrom dplyr mutate across where
#' @importFrom tidyr replace_na
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
#' df1 |> Output()
#'
#' #+ df-false ,  results='asis'
#' df1 |> Output()
#'
#'
#' #+ df-html
#' df1 |> Output(output="html")
#'
#' #+ df-text ,  results='asis'
#' df1 |> Output(output="text")
#'
#' #+ df-word ,  results='asis'
#'  # df1 |> Output(output="word")
#'
#'
#' #+ df-mark
#' df1 |> Output(output="markdown")
#'
#' # df1 |>  Output(linesep = c("", "", "\\midrule"))
#'
Output.data.frame <-
  function(x,
           caption = NULL,
           note = NULL,
           col.names=NULL,

           select = col,
           col = NULL,
           row = NULL,
           wrap = get_opt("table", "wrap"),
           wrap_column = TRUE,
           wrap_result = get_opt("table", "wrap_result"),
           na.rm=TRUE,

           output =  which_output(),
           header = col.names,

           split_header = TRUE,
           css.table = 'padding-left: .5em; padding-right: .2em;',
           css.cell = 'padding-left: .5em; padding-right: .2em;',
           booktabs = TRUE,
           latex_options = c("hold_position"),
           linesep = "",
           align = "l",
           rgroup = attr(x, "rgroup", TRUE),
           n.rgroup =  attr(x, "n.rgroup", TRUE),
           ...) {

 #   cat("\n   in Output \n")
    if (nrow(x) == 0)
      return(NULL)

  if(!is.null(select)) x <- x[select]
  if(!is.null(row)) x <- x[row,]

  if (!is.null(wrap)) {
    if (is.logical(wrap))
      if (wrap)
        wrap <- 30
  }

 # if (!is.null(wrap_column)) {
    if (is.logical(wrap_column))
      if (wrap_column) wrap_column <- 12
 # }



  if (na.rm)
    x <- mutate(x,
                across(where(is.character), ~ replace_na(., "")))


    if (output == "docx") {
      # In spin-word geht Word.doc  nicht weil die Ausgabe nicht an knit_print weitergegeben wird.
      return(
        Output_word(x,
                    caption, note, output,
                    split_header, wrap, wrap_result)
        )
    }

    caption <- Caption(caption, attr(x, "caption"), N = attr(x, "N"))
    note    <- Note(note, attr(x, "note"))

    attr(x, "caption") <- caption
    attr(x, "note") <- note

    if (!is.null(header)) {
      #cat("\nin header\n")
      if (ncol(x) > length(header)) {
        #cat("Header lenght is wrong! (to short) \n")
        header <- append(header, names(x)[-seq_along(header)])
      } else if (ncol(x) < length(header)) {
        #cat("Header lenght is wrong! (to long) \n")
        header <- header[seq_along(x)]
      }
    }

     tbl <- tbl_header(x, header, split_header)
     if(!is.null(rgroup)) {
       if (!is.null(names(rgroup))) {
         n.rgroup <- as.vector(rgroup)
         rgroup <- names(rgroup)
         if (n.rgroup[1L] != 1) {
           rgroup <- c("", rgroup)
           n.rgroup <- c(1, n.rgroup)
         }
         n.rgroup <-  c(diff(n.rgroup - 1))
       }
     }

    if (output == "text") {
      #caption <- Caption(caption, attr(x, "caption"))
      #note <- Note(note, attr(x, "note"))
      if (!is.null(tbl$header_above))
        names(x) <-
        ifelse(tbl$header_above2 == "",
               tbl$header,
               paste0(tbl$header_above2, "_", tbl$header))
      else{
        names(x) <- tbl$header
      }
      if(is.numeric(wrap)) {
        x[[1]] <-
          wrap_character(x[[1]],
                                  width = wrap,
                                  sep =  "\n")
        }

      cat("\n", caption, "\n")
      print(data.frame(x))
      if (is.character(note))
        cat("\n", note, "\n\n")
    }
    else if (output == "html" | output == "markdown_html") {
      #caption <- Caption(caption, attr(x, "caption"))
      #note <- Note(note, attr(x, "note"))
      if (is.numeric(wrap)) {
        # neu programmiert aber unten habe ich noch nichts geaendert!!
        if (grepl("Item", names(x)[1]  ))
          x[[1]] <- wrap_character(x[[1]], width = wrap, sep = "<br>" )
      }

      # Nicht Getestet!!! eventuell Fehler
      if (!is.null(wrap_result)) {
        x[-1] <-
          stp25tools2::dapply2(x[-1], function(y) {
            y <- gsub(" \\(", "<br>\n(", y)
            y <- gsub(", p", "<br>\np", y)
            gsub("[()]", "", y)
          })
      }


      # Nicht Getestet!!! eventuell Fehler
      if (is.numeric(wrap_column)) {
        if (is.null(tbl$header_above))
          tbl$header <-
            wrap_character(tbl$header, width = wrap_column, sep = "<br>")
        else
          tbl$cgroup <-
            wrap_character(tbl$cgroup, width = wrap_column, sep = "<br>")
      }

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
#' @description Output_split data.frame anhand einer Spalte in mehere Tabellen Aufspliten.
#'
#' by ist entweder Character oder Formula
#' @export
#'
Output_split <- function(x, by, caption = "", ...) {
  if (inherits(by, "formula"))
    by <- all.vars(by)
  f <- x[by]
  pos_by <- which(names(x) %in% by)
  x_list <- split(x[-pos_by], f)
  for (i in seq_along(x_list)) {
    Output(x_list[[i]], caption = paste(caption, names(x_list)[i]), ...)
  }

  invisible(x)
}


#' @rdname Output
#' @description Output.iste: einzeln in einen data.frame transformieren
#' Wenn in einer liste ein recrusive Liste kommt wird diese mit invisible
#' zurückgegeben (das sind oft Daten)
#' @export
#'
Output.list <- function(x,
                        caption = NULL,
                        note = NULL,
                        output =  which_output(),
                        ...) {
  if (!is.null(caption)) Head5(caption)
  if (!is.null(note)) Head5(note)

  if (output == "docx") {
    if (length(x) > 1)
      Text("Weitere Tabellen:", names(x)[-1])
    x <- x[[1]]
    return(Output_word(x, ...))
  }
  else {
    res <- list()
    rtrn <- NULL
    for (i in 1:length(x)) {
      if (inherits(x[[i]], "list")) {
        if (is.data.frame(x[[i]])) print(x[[i]])
        else rtrn <- x[[i]]

       #print(names()
       # warning("Recrusive Liste!")
      }
      if (is.data.frame(x[[i]]))
      res[[i]] <- Output(x[[i]], output = output, ...)
    }
    if(is.null(rtrn)) invisible(rtrn)
    else invisible(x)
  }
}


#' Header aufbereiten
#'
#' Interne Funktion
#' @param x data.frame
#' @param split_header  spllit header
#'
#' @return list(header,header_above,cgroup,n.cgroup,header_above2,)
#' @noRd
tbl_header <-
  function(x,
           header = NULL,
           split_header = TRUE,
           split_heder.chr = "_") {

    header <- if( is.null(header)) colnames(x)
    else if(length(header)==ncol(x) ) header
    else colnames(x)

    header_above <- NULL
    cgroup <- NULL
    n.cgroup <- NULL
    a1 <- a2 <- NULL

    if (split_header) {
      result_tbl_names <- stringr::str_split(header, split_heder.chr)
      ebenen <- max(lengths(result_tbl_names), na.rm = TRUE)

      if (ebenen == 3) {
        #Fehler mit Name_name_SD abfangen
        result_tbl_names <-
          stringr::str_split(header, split_heder.chr , 2)
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
  rslt <- stp25tools2::fix_to_df(x)

  print(rslt)
  if (is.data.frame(rslt))
    Output.data.frame(rslt, ...)
  else {
    warning("Unbekanter Objekt-Typ: ", class(x)[1])
    x
  }
}


wrap_character <- function(x,
                           width = 25,  sep =  "\n",
                         #  max.lines = NULL,
                          # max.lines.char = " ...",
                           ...) {
  # removes whitespace from start and end of string
  # x <- stringr::str_squish(x)

  # # Truncate a string to maximum width
  # if (!is.null(max.lines)) {
  #   x <- sapply(x, function(z) {
  #     stringr::str_trunc(z,
  #                        width =  max.lines,
  #                        side = "right",
  #                        ellipsis = max.lines.char)
  #   })
  # }

  # um die einrückung bei factoren zu erhalten
  sep <- unlist(
    lapply(x[[1]],
           function(i){
             if (substr(i, 1, 5) == "&nbsp")
               paste0(sep,"&nbsp; &nbsp; ")
             else
               sep
           }
    ))

  if (is.numeric(width)) {
    mapply(function(txt, s)
      paste(strwrap(txt, width = width), collapse = s),
      x, sep,
      USE.NAMES = FALSE)
  }
}


