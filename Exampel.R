#' ---
#' title: Statistische Auswertung
#' author: Wolfgang Peter
#' output:
#'   word_document
#' keep_tex: yes
#' lang: de-DE
#' ---


#+ setup, include=FALSE
require(knitr)
knitr::opts_chunk$set(echo = TRUE, warnings=FALSE)
#flextable::set_flextable_defaults(fonts_ignore=TRUE)


require(stp25output2)
require(stp25stat2)
Projekt("html")





#+ results='asis', echo=FALSE

which_output()
Head1("Überschrift 1" )
Head2("Überschrift 2")
Head3("Überschrift 3")
Head4("Überschrift 4")
Text("Beispiel, wie ein Projekt aufgebaut sein muss, um Tabellen und Grafiken auszugeben.

     ÄÜÖäöü?")

Text("h1")



tab<- data.frame(
  Item = c("A", "B", "Ö"),
  x_m = c(45.47, 256.14, 14.47),
  x_sd = c(1.43, 25.264, 5.423),
  y_m = c(35.42, 261.94, 24.43),
  y_sd = c(3.44, 25.25, 5.42)

  )

#+
 (tab )


#+ results='asis'
Output(tab ,caption = "Tabellen über Output + split_header")
Output(tab ,caption = "Tabellen über Output + note + split_header=false", split_header = FALSE, note="Kommentar")





#+ fig.cap='Health', fig.height=2.70, fig.width=3, echo=FALSE
plot(1:10)
SavePlot("Grafik  Influence of feed given to cows on the fatty acid pattern of milk
(g/100 g milk fat) (adapted acc. to [31])", w = 5, h = 4)


#+ results='asis'
lm1 <- lm(breaks ~ wool + tension, data = warpbreaks)
lm2 <- lm(breaks ~ wool * tension, data = warpbreaks)

rslt_reg_table <- Tbll_reg(lm1, lm2)

rslt_reg_table |> Output("Regressionstabelle mit Output")



Head3("GT-Bibliothek")
Text("Beispiel für die Ausgabe einer Tabelle mit gt(). Die GT-Bibliothek erlaubt verschiedene Formatierungen, die vor allem unter Quarto von Vorteil sind.")

require(gt)

rslt_reg_table |>
  gt()


which_output()
rslt_reg_table |>
  gt() |>
  tab_footnote(
    footnote = "Fussnote die weiter Information zu der zweiten Spalte liefert.",
    locations = cells_column_labels(columns = 2      )
  ) |>
  Output(caption="Beispiel für die Ausgabe einer Tabelle mit gt()")

#' das macht keinen sinn
#'
# slt_reg_table |>
#   gt() |> extract_body ( ) |> Output()




End()
