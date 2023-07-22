
<!-- README.md is generated from README.Rmd. Please edit that file -->

# stp25output2

Die Scripten dienen der Erstellung von Reports als HTML aber auch von
PDF ueber knit. Zum Teil handelt es sich um modifizerte Funktionen von
R2HTML, texreg und htmlTable.

## Overview

- Initialisiert von neuen Projekten
  - `Projekt()` und `End()` Initialisiert ein neues Projekt und aufruf
    der HTML-Seite mit Hilfe von R2HTML.
- Text und Tabellen-Formatierung
  - `Output` Erstellt Tabellen
  - `Text`, `Head` und `Head1 ...` Schreibt HTML Text.
- Hilfsfunktionen
  - `SavePlot` Speichert Bilder in den Arbeitsfolder.

## Output formats

| File | Type            | Parameter       | which_output() |
|------|-----------------|-----------------|----------------|
| r    |                 |                 | text           |
| r    | Projekt(“html”) |                 | html           |
| r    | spin            | html_document   | markdown_html  |
| r    | spin            | pdf_document    | latex          |
| r    | spin            | word_document   | docx           |
| Rmd  |                 | html_document   | markdown_html  |
| Rmd  |                 | pdf_document    | latex          |
| Rmd  |                 | word_document   | docx           |
| Rmd  |                 | beamer          | latex          |
| Rmd  |                 | github_document | markdown_html  |

## Projekt

Initialisieren eines Projektes.

``` r
# devtools::install_github("stp4/stp25output")
 
Projekt("md", "Beispiel Projekt",
        datum = date()
        )
#> Projekt:  Beispiel Projekt 
#> Wd:  C:/Users/wpete/Dropbox/3_Forschung/R-Project/stp25output2

#stp25settings
set_opt(prozent=list(digits=c(1,0), style=2))

warpbreaks2 <- Label(warpbreaks,
                     breaks =   "The number of breaks",
                     wool   =   "The type of wool",
                     tension    =   "The level of tension")
 

warpbreaks2 %>%
  Tbll_desc(breaks + tension ~ wool)
#> # A tibble: 6 × 3
#>   Item                          A               B             
#> * <chr>                         <chr>           <chr>         
#> 1 "(N) "                        "27"            "27"          
#> 2 "The number of breaks (mean)" "31.04 (15.85)" "25.26 (9.30)"
#> 3 "The level of tension "       ""              ""            
#> 4 "    L"                       "9 (33%)"       "9 (33%)"     
#> 5 "    M"                       "9 (33%)"       "9 (33%)"     
#> 6 "    H"                       "9 (33%)"       "9 (33%)"

End()
```

### Usage

data.frame()

``` r
set_opt(caption =TRUE)
which_output()
```

\[1\] “markdown_html”

``` r
 
dat<- data.frame(
term = factor(c("A", "B", "C", "D")),
n = c(23, 14, 56, 2),
m = c("4.7 (2.4)", "4.1 (2.3)", "8.9 (3.6)", NA)
) 
attr(dat, "N") <- 56

dat  %>% Output("data.frame")
```

<table class="gmisc_table" style="border-collapse: collapse; padding-left: .5em; padding-right: .2em;">
<thead>
<tr>
<td colspan="3" style="text-align: left;">
Tab 1: data.frame (N=56)
</td>
</tr>
<tr>
<th style="margin: 0; padding: 0; font-weight: 900; vertical-align: middle; border-bottom: 1px solid black; border-top: 2px solid black; text-align: center;">
term
</th>
<th style="margin: 0; padding: 0; font-weight: 900; vertical-align: middle; border-bottom: 1px solid black; border-top: 2px solid black; text-align: center;">
n
</th>
<th style="margin: 0; padding: 0; font-weight: 900; vertical-align: middle; border-bottom: 1px solid black; border-top: 2px solid black; text-align: center;">
m
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
A
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
23
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
4.7 (2.4)
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
B
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
14
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
4.1 (2.3)
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
C
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
56
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
8.9 (3.6)
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid black; text-align: left;">
D
</td>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid black; text-align: left;">
2
</td>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid black; text-align: left;">
</td>
</tr>
</tbody>
<tfoot>
<tr>
<td colspan="3">
</td>
</tr>
</tfoot>
</table>

``` r

Text("Hallo")
#> Hallo
Text(dat, term, n, m,  include.levels=TRUE)
#> term: term: A|B|C|D n: n: numeric m: m: 4.1 (2.3)|4.7 (2.4)|8.9 (3.6)
```

``` r

# matrix(c("a1","a2",3,4,5,6),
#                 nrow=2, byrow=TRUE,
#                 dimnames=list(gender=c("M", "F"),
#                               party=c( "Dem", "Ind", "Rep")))  #%>% Output("matrix()")

as.table(matrix(c("a1","a2",3,4,5,6),
                 nrow=2, byrow=TRUE,
                 dimnames=list(gender=c("M", "F"),
                               party=c( "Dem", "Ind", "Rep")))) %>% Output("as.table()")
```

<table class="gmisc_table" style="border-collapse: collapse; padding-left: .5em; padding-right: .2em;">
<thead>
<tr>
<td colspan="6" style="text-align: left;">
Tab 2: as.table()
</td>
</tr>
<tr>
<th colspan="1" style="font-weight: 900; border-top: 2px solid black; text-align: center; margin: 0; padding: 0; vertical-align: middle;">
</th>
<th style="width: 2px; border-bottom: 1px solid white; border-top: 2px solid black;" colspan="1">
</th>
<th style="width: 2px; border-bottom: 1px solid white; border-top: 2px solid black;" colspan="1">
</th>
<th colspan="3" style="font-weight: 900; border-bottom: 1px solid black; border-top: 2px solid black; text-align: center; margin: 0; padding: 0; vertical-align: middle;">
party
</th>
</tr>
<tr>
<th style="margin: 0; padding: 0; font-weight: 900; vertical-align: middle; border-bottom: 1px solid black; text-align: center;">
gender
</th>
<th style="width: 2px; margin: 0; padding: 0; font-weight: 900; vertical-align: middle; border-bottom: 1px solid black; text-align: center;" colspan="1">
</th>
<th style="width: 2px; margin: 0; padding: 0; font-weight: 900; vertical-align: middle; border-bottom: 1px solid black; text-align: center;" colspan="1">
</th>
<th style="margin: 0; padding: 0; font-weight: 900; vertical-align: middle; border-bottom: 1px solid black; text-align: center;">
Dem
</th>
<th style="margin: 0; padding: 0; font-weight: 900; vertical-align: middle; border-bottom: 1px solid black; text-align: center;">
Ind
</th>
<th style="margin: 0; padding: 0; font-weight: 900; vertical-align: middle; border-bottom: 1px solid black; text-align: center;">
Rep
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
M
</td>
<td style="width: 2px; padding-left: .5em; padding-right: .2em; text-align: left;" colspan="1">
</td>
<td style="width: 2px; padding-left: .5em; padding-right: .2em; text-align: left;" colspan="1">
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
a1
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
a2
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
3
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid black; text-align: left;">
F
</td>
<td style="width: 2px; padding-left: .5em; padding-right: .2em; border-bottom: 2px solid black; text-align: left;" colspan="1">
</td>
<td style="width: 2px; padding-left: .5em; padding-right: .2em; border-bottom: 2px solid black; text-align: left;" colspan="1">
</td>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid black; text-align: left;">
4
</td>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid black; text-align: left;">
5
</td>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid black; text-align: left;">
6
</td>
</tr>
</tbody>
<tfoot>
<tr>
<td colspan="6">
</td>
</tr>
</tfoot>
</table>

``` r
 
warpbreaks2 %>%
  Tbll_desc(breaks + tension + wool) %>% Output()
```

<table class="gmisc_table" style="border-collapse: collapse; padding-left: .5em; padding-right: .2em;">
<thead>
<tr>
<td colspan="2" style="text-align: left;">
Tab 3: Summary (N=54) (N=54)
</td>
</tr>
<tr>
<th style="margin: 0; padding: 0; font-weight: 900; vertical-align: middle; border-bottom: 1px solid black; border-top: 2px solid black; text-align: center;">
Item
</th>
<th style="margin: 0; padding: 0; font-weight: 900; vertical-align: middle; border-bottom: 1px solid black; border-top: 2px solid black; text-align: center;">
m
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
(N) 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
54
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
The number of breaks (mean)
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
28.15 (13.20)
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
The level of tension 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 L
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
18 (33%)
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 M
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
18 (33%)
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 H
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
18 (33%)
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
The type of wool 
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
 A
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
27 (50.0%)
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid black; text-align: left;">
 B
</td>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid black; text-align: left;">
27 (50.0%)
</td>
</tr>
</tbody>
<tfoot>
<tr>
<td colspan="2">
</td>
</tr>
</tfoot>
</table>

### Verschiedene Szenarien

``` r
#' default, results='markup'
df1 %>% Output()
#> <table class='gmisc_table' style='border-collapse: collapse; padding-left: .5em; padding-right: .2em;' >
#> <thead>
#> <tr><td colspan='3' style='text-align: left;'>
#> Tab 4: Demo Ueberschrift (N=99)</td></tr>
#> <tr>
#> <th style='margin: 0; padding: 0; font-weight: 900; vertical-align: middle; border-bottom: 1px solid black; border-top: 2px solid black; text-align: center;'>term</th>
#> <th style='margin: 0; padding: 0; font-weight: 900; vertical-align: middle; border-bottom: 1px solid black; border-top: 2px solid black; text-align: center;'>n</th>
#> <th style='margin: 0; padding: 0; font-weight: 900; vertical-align: middle; border-bottom: 1px solid black; border-top: 2px solid black; text-align: center;'>m</th>
#> </tr>
#> </thead>
#> <tbody>
#> <tr>
#> <td style='padding-left: .5em; padding-right: .2em; text-align: left;'>A</td>
#> <td style='padding-left: .5em; padding-right: .2em; text-align: left;'>23</td>
#> <td style='padding-left: .5em; padding-right: .2em; text-align: left;'>4.7&nbsp;(2.4)</td>
#> </tr>
#> <tr>
#> <td style='padding-left: .5em; padding-right: .2em; text-align: left;'>B</td>
#> <td style='padding-left: .5em; padding-right: .2em; text-align: left;'>14</td>
#> <td style='padding-left: .5em; padding-right: .2em; text-align: left;'>4.1&nbsp;(2.3)</td>
#> </tr>
#> <tr>
#> <td style='padding-left: .5em; padding-right: .2em; text-align: left;'>C</td>
#> <td style='padding-left: .5em; padding-right: .2em; text-align: left;'>56</td>
#> <td style='padding-left: .5em; padding-right: .2em; text-align: left;'>8.9&nbsp;(3.6)</td>
#> </tr>
#> <tr>
#> <td style='padding-left: .5em; padding-right: .2em; border-bottom: 2px solid black; text-align: left;'>D</td>
#> <td style='padding-left: .5em; padding-right: .2em; border-bottom: 2px solid black; text-align: left;'>2</td>
#> <td style='padding-left: .5em; padding-right: .2em; border-bottom: 2px solid black; text-align: left;'></td>
#> </tr>
#> </tbody>
#> <tfoot><tr><td colspan='3'>
#> Note: Anmerkung</td></tr></tfoot>
#> </table>
```

``` r
#' default, results='asis'
df1 %>% Output()
```

<table class="gmisc_table" style="border-collapse: collapse; padding-left: .5em; padding-right: .2em;">
<thead>
<tr>
<td colspan="3" style="text-align: left;">
Tab 5: Demo Ueberschrift (N=99)
</td>
</tr>
<tr>
<th style="margin: 0; padding: 0; font-weight: 900; vertical-align: middle; border-bottom: 1px solid black; border-top: 2px solid black; text-align: center;">
term
</th>
<th style="margin: 0; padding: 0; font-weight: 900; vertical-align: middle; border-bottom: 1px solid black; border-top: 2px solid black; text-align: center;">
n
</th>
<th style="margin: 0; padding: 0; font-weight: 900; vertical-align: middle; border-bottom: 1px solid black; border-top: 2px solid black; text-align: center;">
m
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
A
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
23
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
4.7 (2.4)
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
B
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
14
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
4.1 (2.3)
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
C
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
56
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
8.9 (3.6)
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid black; text-align: left;">
D
</td>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid black; text-align: left;">
2
</td>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid black; text-align: left;">
</td>
</tr>
</tbody>
<tfoot>
<tr>
<td colspan="3">
Note: Anmerkung
</td>
</tr>
</tfoot>
</table>

``` r
#' results='markup'
df1 %>% Output(output="text")
#> 
#>  Tab 6: Demo Ueberschrift (N=99) 
#>   term  n         m
#> 1    A 23 4.7 (2.4)
#> 2    B 14 4.1 (2.3)
#> 3    C 56 8.9 (3.6)
#> 4    D  2      <NA>
#> 
#>  Note: Anmerkung
```

``` r
#' results='asis'
df1 %>% Output(output="markdown")
```

| term |   n | m         |
|:-----|----:|:----------|
| A    |  23 | 4.7 (2.4) |
| B    |  14 | 4.1 (2.3) |
| C    |  56 | 8.9 (3.6) |
| D    |   2 |           |

Tab 7: Demo Ueberschrift (N=99)

Note: Anmerkung

## xtable()

Ist eine Package zum Erstellen von HTML und latex. Convert an R object
to an xtable object, which can then be printed as a LaTeX or HTML table

``` r
 
require(xtable)
#> Loading required package: xtable
data(tli)
## Demonstrate aov
fm1 <- aov(tlimth ~ sex + ethnicty + grade + disadvg, data = tli)
fm1.table <- xtable(fm1)

Output(fm1.table)
```

<table class="gmisc_table" style="border-collapse: collapse; padding-left: .5em; padding-right: .2em;">
<thead>
<tr>
<td colspan="5" style="text-align: left;">
Tab 8:
</td>
</tr>
<tr>
<th style="margin: 0; padding: 0; font-weight: 900; vertical-align: middle; border-bottom: 1px solid black; border-top: 2px solid black; text-align: center;">
Df
</th>
<th style="margin: 0; padding: 0; font-weight: 900; vertical-align: middle; border-bottom: 1px solid black; border-top: 2px solid black; text-align: center;">
Sum Sq
</th>
<th style="margin: 0; padding: 0; font-weight: 900; vertical-align: middle; border-bottom: 1px solid black; border-top: 2px solid black; text-align: center;">
Mean Sq
</th>
<th style="margin: 0; padding: 0; font-weight: 900; vertical-align: middle; border-bottom: 1px solid black; border-top: 2px solid black; text-align: center;">
F value
</th>
<th style="margin: 0; padding: 0; font-weight: 900; vertical-align: middle; border-bottom: 1px solid black; border-top: 2px solid black; text-align: center;">
Pr(\>F)
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
1
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
75.3725490196085
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
75.3725490196085
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
0.375191177841786
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
0.541683003365518
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
3
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
2572.14917617822
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
857.383058726072
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
4.26790076558696
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
0.00718305207307176
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
1
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
36.307404193483
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
36.307404193483
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
0.180731817099443
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
0.671727134425381
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
1
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
59.3033811724377
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
59.3033811724377
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
0.295201710987628
</td>
<td style="padding-left: .5em; padding-right: .2em; text-align: left;">
0.588206240205116
</td>
</tr>
<tr>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid black; text-align: left;">
93
</td>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid black; text-align: left;">
18682.8674894362
</td>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid black; text-align: left;">
200.891048273508
</td>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid black; text-align: left;">
</td>
<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid black; text-align: left;">
</td>
</tr>
</tbody>
<tfoot>
<tr>
<td colspan="5">
</td>
</tr>
</tfoot>
</table>

``` r

fm2 <- lm(tlimth ~ sex*ethnicty, data = tli)
fm2b <- lm(tlimth ~ ethnicty, data = tli)

#Output(xtable(anova(fm2b, fm2)), output="md")
```

## knitr::kable

``` r
knitr::kable(
  df1, row.names = FALSE,
  format = "pandoc" 
)
```

| term |   n | m         |
|:-----|----:|:----------|
| A    |  23 | 4.7 (2.4) |
| B    |  14 | 4.1 (2.3) |
| C    |  56 | 8.9 (3.6) |
| D    |   2 | NA        |

``` r

knitr::kable(
  df1, row.names = FALSE,
  format = "markdown" 
)
```

| term |   n | m         |
|:-----|----:|:----------|
| A    |  23 | 4.7 (2.4) |
| B    |  14 | 4.1 (2.3) |
| C    |  56 | 8.9 (3.6) |
| D    |   2 | NA        |

``` r
knitr::kable(
  df1, row.names = FALSE,
  format = "pandoc" 
)
```

| term |   n | m         |
|:-----|----:|:----------|
| A    |  23 | 4.7 (2.4) |
| B    |  14 | 4.1 (2.3) |
| C    |  56 | 8.9 (3.6) |
| D    |   2 | NA        |

## Grafik settings

``` r
 
set.seed(2)
n <- 20 * 3 * 2
DF <- data.frame(
  n = runif(n, min = 1, max = 5),
  e = runif(n, min = 1, max = 5),
  o = runif(n, min = 1, max = 5),
  g = runif(n, min = 1, max = 5),
  a = runif(n, min = 1, max = 5),
  treatment = gl(3, n / 3, labels = c("UG1", "UG2", "KG"))[sample.int(n)],
  sex = gl(2, n / 2, labels = c("male", "female"))
)
```

``` r
#set_lattice()
bwplot(e ~ treatment, DF)
```

![](README-fig-default-1.png)<!-- -->

``` r

#set_lattice_ggplot()
bwplot(e ~ treatment, DF)
```

![](README-fig-ggplot-1.png)<!-- -->

``` r
#set_lattice_bw()
bwplot(e ~ treatment, DF)
```

![](README-fig-bw-1.png)<!-- -->

## A few methods for making tables in rmarkdown

Quelle: <https://gist.github.com/benmarwick/8ad99f35d5e4caa06492>

<https://github.com/yihui/printr>
<https://github.com/jalapic/simpletable>
<https://github.com/renkun-ken/formattable>

``` r
my_data <- head(iris)
names(my_data) <- c(letters[1:ncol(iris)])
```

``` r
library("knitr")
kable(my_data)
```

<table>
<thead>
<tr>
<th style="text-align:right;">
a
</th>
<th style="text-align:right;">
b
</th>
<th style="text-align:right;">
c
</th>
<th style="text-align:right;">
d
</th>
<th style="text-align:left;">
e
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
5.1
</td>
<td style="text-align:right;">
3.5
</td>
<td style="text-align:right;">
1.4
</td>
<td style="text-align:right;">
0.2
</td>
<td style="text-align:left;">
setosa
</td>
</tr>
<tr>
<td style="text-align:right;">
4.9
</td>
<td style="text-align:right;">
3.0
</td>
<td style="text-align:right;">
1.4
</td>
<td style="text-align:right;">
0.2
</td>
<td style="text-align:left;">
setosa
</td>
</tr>
<tr>
<td style="text-align:right;">
4.7
</td>
<td style="text-align:right;">
3.2
</td>
<td style="text-align:right;">
1.3
</td>
<td style="text-align:right;">
0.2
</td>
<td style="text-align:left;">
setosa
</td>
</tr>
<tr>
<td style="text-align:right;">
4.6
</td>
<td style="text-align:right;">
3.1
</td>
<td style="text-align:right;">
1.5
</td>
<td style="text-align:right;">
0.2
</td>
<td style="text-align:left;">
setosa
</td>
</tr>
<tr>
<td style="text-align:right;">
5.0
</td>
<td style="text-align:right;">
3.6
</td>
<td style="text-align:right;">
1.4
</td>
<td style="text-align:right;">
0.2
</td>
<td style="text-align:left;">
setosa
</td>
</tr>
<tr>
<td style="text-align:right;">
5.4
</td>
<td style="text-align:right;">
3.9
</td>
<td style="text-align:right;">
1.7
</td>
<td style="text-align:right;">
0.4
</td>
<td style="text-align:left;">
setosa
</td>
</tr>
</tbody>
</table>

``` r
library("xtable")
print(xtable(my_data), type = "html", include.rownames=FALSE, html.table.attributes=list("border='0' cellpadding='5' "))
```

<!-- html table generated in R 4.2.3 by xtable 1.8-4 package -->
<!-- Sat Jul 22 12:00:43 2023 -->
<table border="0" cellpadding="5">
<tr>
<th>
a
</th>
<th>
b
</th>
<th>
c
</th>
<th>
d
</th>
<th>
e
</th>
</tr>
<tr>
<td align="right">
5.10
</td>
<td align="right">
3.50
</td>
<td align="right">
1.40
</td>
<td align="right">
0.20
</td>
<td>
setosa
</td>
</tr>
<tr>
<td align="right">
4.90
</td>
<td align="right">
3.00
</td>
<td align="right">
1.40
</td>
<td align="right">
0.20
</td>
<td>
setosa
</td>
</tr>
<tr>
<td align="right">
4.70
</td>
<td align="right">
3.20
</td>
<td align="right">
1.30
</td>
<td align="right">
0.20
</td>
<td>
setosa
</td>
</tr>
<tr>
<td align="right">
4.60
</td>
<td align="right">
3.10
</td>
<td align="right">
1.50
</td>
<td align="right">
0.20
</td>
<td>
setosa
</td>
</tr>
<tr>
<td align="right">
5.00
</td>
<td align="right">
3.60
</td>
<td align="right">
1.40
</td>
<td align="right">
0.20
</td>
<td>
setosa
</td>
</tr>
<tr>
<td align="right">
5.40
</td>
<td align="right">
3.90
</td>
<td align="right">
1.70
</td>
<td align="right">
0.40
</td>
<td>
setosa
</td>
</tr>
</table>

``` r
library(xtable)
print(xtable(my_data), type = 'html')
```

<!-- html table generated in R 4.2.3 by xtable 1.8-4 package -->
<!-- Sat Jul 22 12:00:43 2023 -->
<table border="1">
<tr>
<th>
</th>
<th>
a
</th>
<th>
b
</th>
<th>
c
</th>
<th>
d
</th>
<th>
e
</th>
</tr>
<tr>
<td align="right">
1
</td>
<td align="right">
5.10
</td>
<td align="right">
3.50
</td>
<td align="right">
1.40
</td>
<td align="right">
0.20
</td>
<td>
setosa
</td>
</tr>
<tr>
<td align="right">
2
</td>
<td align="right">
4.90
</td>
<td align="right">
3.00
</td>
<td align="right">
1.40
</td>
<td align="right">
0.20
</td>
<td>
setosa
</td>
</tr>
<tr>
<td align="right">
3
</td>
<td align="right">
4.70
</td>
<td align="right">
3.20
</td>
<td align="right">
1.30
</td>
<td align="right">
0.20
</td>
<td>
setosa
</td>
</tr>
<tr>
<td align="right">
4
</td>
<td align="right">
4.60
</td>
<td align="right">
3.10
</td>
<td align="right">
1.50
</td>
<td align="right">
0.20
</td>
<td>
setosa
</td>
</tr>
<tr>
<td align="right">
5
</td>
<td align="right">
5.00
</td>
<td align="right">
3.60
</td>
<td align="right">
1.40
</td>
<td align="right">
0.20
</td>
<td>
setosa
</td>
</tr>
<tr>
<td align="right">
6
</td>
<td align="right">
5.40
</td>
<td align="right">
3.90
</td>
<td align="right">
1.70
</td>
<td align="right">
0.40
</td>
<td>
setosa
</td>
</tr>
</table>

``` r
library(xtable)
print(xtable(my_data), type = 'html', html.table.attributes = '')
```

<!-- html table generated in R 4.2.3 by xtable 1.8-4 package -->
<!-- Sat Jul 22 12:00:43 2023 -->
<table>
<tr>
<th>
</th>
<th>
a
</th>
<th>
b
</th>
<th>
c
</th>
<th>
d
</th>
<th>
e
</th>
</tr>
<tr>
<td align="right">
1
</td>
<td align="right">
5.10
</td>
<td align="right">
3.50
</td>
<td align="right">
1.40
</td>
<td align="right">
0.20
</td>
<td>
setosa
</td>
</tr>
<tr>
<td align="right">
2
</td>
<td align="right">
4.90
</td>
<td align="right">
3.00
</td>
<td align="right">
1.40
</td>
<td align="right">
0.20
</td>
<td>
setosa
</td>
</tr>
<tr>
<td align="right">
3
</td>
<td align="right">
4.70
</td>
<td align="right">
3.20
</td>
<td align="right">
1.30
</td>
<td align="right">
0.20
</td>
<td>
setosa
</td>
</tr>
<tr>
<td align="right">
4
</td>
<td align="right">
4.60
</td>
<td align="right">
3.10
</td>
<td align="right">
1.50
</td>
<td align="right">
0.20
</td>
<td>
setosa
</td>
</tr>
<tr>
<td align="right">
5
</td>
<td align="right">
5.00
</td>
<td align="right">
3.60
</td>
<td align="right">
1.40
</td>
<td align="right">
0.20
</td>
<td>
setosa
</td>
</tr>
<tr>
<td align="right">
6
</td>
<td align="right">
5.40
</td>
<td align="right">
3.90
</td>
<td align="right">
1.70
</td>
<td align="right">
0.40
</td>
<td>
setosa
</td>
</tr>
</table>

``` r
library("pander")
pandoc.table(my_data)
```

|  a  |  b  |  c  |  d  |   e    |
|:---:|:---:|:---:|:---:|:------:|
| 5.1 | 3.5 | 1.4 | 0.2 | setosa |
| 4.9 |  3  | 1.4 | 0.2 | setosa |
| 4.7 | 3.2 | 1.3 | 0.2 | setosa |
| 4.6 | 3.1 | 1.5 | 0.2 | setosa |
|  5  | 3.6 | 1.4 | 0.2 | setosa |
| 5.4 | 3.9 | 1.7 | 0.4 | setosa |

``` r
library("pander")
pandoc.table(my_data, split.cells = 5)
```

|  a  |  b  |  c  |  d  |   e    |
|:---:|:---:|:---:|:---:|:------:|
| 5.1 | 3.5 | 1.4 | 0.2 | setosa |
| 4.9 |  3  | 1.4 | 0.2 | setosa |
| 4.7 | 3.2 | 1.3 | 0.2 | setosa |
| 4.6 | 3.1 | 1.5 | 0.2 | setosa |
|  5  | 3.6 | 1.4 | 0.2 | setosa |
| 5.4 | 3.9 | 1.7 | 0.4 | setosa |

``` r
pander::panderOptions('table.split.table', 350)
pander::pandoc.table(my_data, style="rmarkdown")
```

|  a  |  b  |  c  |  d  |   e    |
|:---:|:---:|:---:|:---:|:------:|
| 5.1 | 3.5 | 1.4 | 0.2 | setosa |
| 4.9 |  3  | 1.4 | 0.2 | setosa |
| 4.7 | 3.2 | 1.3 | 0.2 | setosa |
| 4.6 | 3.1 | 1.5 | 0.2 | setosa |
|  5  | 3.6 | 1.4 | 0.2 | setosa |
| 5.4 | 3.9 | 1.7 | 0.4 | setosa |

    library("ascii")
    print(ascii(my_data), type = 'pandoc')

``` r
library("htmlTable")
htmlTable(my_data, col.rgroup = c("none", "#F7F7F7"))
```

<table class="gmisc_table" style="border-collapse: collapse; margin-top: 1em; margin-bottom: 1em;">
<thead>
<tr>
<th style="border-bottom: 1px solid grey; border-top: 2px solid grey;">
</th>
<th style="margin: 0; padding: 0; font-weight: 900; vertical-align: middle; border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">
a
</th>
<th style="margin: 0; padding: 0; font-weight: 900; vertical-align: middle; border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">
b
</th>
<th style="margin: 0; padding: 0; font-weight: 900; vertical-align: middle; border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">
c
</th>
<th style="margin: 0; padding: 0; font-weight: 900; vertical-align: middle; border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">
d
</th>
<th style="margin: 0; padding: 0; font-weight: 900; vertical-align: middle; border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">
e
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="margin: 0; padding: 0; text-align: left;">
1
</td>
<td style="margin: 0; padding: 0; text-align: center;">
5.1
</td>
<td style="margin: 0; padding: 0; text-align: center;">
3.5
</td>
<td style="margin: 0; padding: 0; text-align: center;">
1.4
</td>
<td style="margin: 0; padding: 0; text-align: center;">
0.2
</td>
<td style="margin: 0; padding: 0; text-align: center;">
setosa
</td>
</tr>
<tr style="background-color: #f7f7f7;">
<td style="margin: 0; padding: 0; background-color: #f7f7f7; text-align: left;">
2
</td>
<td style="margin: 0; padding: 0; background-color: #f7f7f7; text-align: center;">
4.9
</td>
<td style="margin: 0; padding: 0; background-color: #f7f7f7; text-align: center;">
3
</td>
<td style="margin: 0; padding: 0; background-color: #f7f7f7; text-align: center;">
1.4
</td>
<td style="margin: 0; padding: 0; background-color: #f7f7f7; text-align: center;">
0.2
</td>
<td style="margin: 0; padding: 0; background-color: #f7f7f7; text-align: center;">
setosa
</td>
</tr>
<tr>
<td style="margin: 0; padding: 0; text-align: left;">
3
</td>
<td style="margin: 0; padding: 0; text-align: center;">
4.7
</td>
<td style="margin: 0; padding: 0; text-align: center;">
3.2
</td>
<td style="margin: 0; padding: 0; text-align: center;">
1.3
</td>
<td style="margin: 0; padding: 0; text-align: center;">
0.2
</td>
<td style="margin: 0; padding: 0; text-align: center;">
setosa
</td>
</tr>
<tr style="background-color: #f7f7f7;">
<td style="margin: 0; padding: 0; background-color: #f7f7f7; text-align: left;">
4
</td>
<td style="margin: 0; padding: 0; background-color: #f7f7f7; text-align: center;">
4.6
</td>
<td style="margin: 0; padding: 0; background-color: #f7f7f7; text-align: center;">
3.1
</td>
<td style="margin: 0; padding: 0; background-color: #f7f7f7; text-align: center;">
1.5
</td>
<td style="margin: 0; padding: 0; background-color: #f7f7f7; text-align: center;">
0.2
</td>
<td style="margin: 0; padding: 0; background-color: #f7f7f7; text-align: center;">
setosa
</td>
</tr>
<tr>
<td style="margin: 0; padding: 0; text-align: left;">
5
</td>
<td style="margin: 0; padding: 0; text-align: center;">
5
</td>
<td style="margin: 0; padding: 0; text-align: center;">
3.6
</td>
<td style="margin: 0; padding: 0; text-align: center;">
1.4
</td>
<td style="margin: 0; padding: 0; text-align: center;">
0.2
</td>
<td style="margin: 0; padding: 0; text-align: center;">
setosa
</td>
</tr>
<tr style="background-color: #f7f7f7;">
<td style="margin: 0; padding: 0; background-color: #f7f7f7; border-bottom: 2px solid grey; text-align: left;">
6
</td>
<td style="margin: 0; padding: 0; background-color: #f7f7f7; border-bottom: 2px solid grey; text-align: center;">
5.4
</td>
<td style="margin: 0; padding: 0; background-color: #f7f7f7; border-bottom: 2px solid grey; text-align: center;">
3.9
</td>
<td style="margin: 0; padding: 0; background-color: #f7f7f7; border-bottom: 2px solid grey; text-align: center;">
1.7
</td>
<td style="margin: 0; padding: 0; background-color: #f7f7f7; border-bottom: 2px solid grey; text-align: center;">
0.4
</td>
<td style="margin: 0; padding: 0; background-color: #f7f7f7; border-bottom: 2px solid grey; text-align: center;">
setosa
</td>
</tr>
</tbody>
</table>

``` r
library(hwriter)
hwrite(my_data, border=0)
```

\[1\] “
<table border="\&quot;0\&quot;">
<tr>
<td>
</td>
<td>
a
</td>
<td>
b
</td>
<td>
c
</td>
<td>
d
</td>
<td>
e
</td>
</tr>
<tr>
<td>
1
</td>
<td>
5.1
</td>
<td>
3.5
</td>
<td>
1.4
</td>
<td>
0.2
</td>
<td>
setosa
</td>
</tr>
<tr>
<td>
2
</td>
<td>
4.9
</td>
<td>
3.0
</td>
<td>
1.4
</td>
<td>
0.2
</td>
<td>
setosa
</td>
</tr>
<tr>
<td>
3
</td>
<td>
4.7
</td>
<td>
3.2
</td>
<td>
1.3
</td>
<td>
0.2
</td>
<td>
setosa
</td>
</tr>
<tr>
<td>
4
</td>
<td>
4.6
</td>
<td>
3.1
</td>
<td>
1.5
</td>
<td>
0.2
</td>
<td>
setosa
</td>
</tr>
<tr>
<td>
5
</td>
<td>
5.0
</td>
<td>
3.6
</td>
<td>
1.4
</td>
<td>
0.2
</td>
<td>
setosa
</td>
</tr>
<tr>
<td>
6
</td>
<td>
5.4
</td>
<td>
3.9
</td>
<td>
1.7
</td>
<td>
0.4
</td>
<td>
setosa
</td>
</tr>
</table>

”

This one is the most useful, and has a nice guide to customisation here:
<http://www.ebi.ac.uk/~gpau/hwriter/>

``` r
library(hwriter)
cat(
  hwrite(
    my_data,
    border = 0,
    center = TRUE,
    table.frame = 'void',
    width = '300px',
    table.style = 'padding: 50px',
    row.names = FALSE,
    row.style = list('font-weight:bold')
  )
)
```

<center>
<table frame="void" style="padding: 50px" border="0" width="300px">
<tr>
<td style="font-weight:bold">
a
</td>
<td style="font-weight:bold">
b
</td>
<td style="font-weight:bold">
c
</td>
<td style="font-weight:bold">
d
</td>
<td style="font-weight:bold">
e
</td>
</tr>
<tr>
<td>
5.1
</td>
<td>
3.5
</td>
<td>
1.4
</td>
<td>
0.2
</td>
<td>
setosa
</td>
</tr>
<tr>
<td>
4.9
</td>
<td>
3.0
</td>
<td>
1.4
</td>
<td>
0.2
</td>
<td>
setosa
</td>
</tr>
<tr>
<td>
4.7
</td>
<td>
3.2
</td>
<td>
1.3
</td>
<td>
0.2
</td>
<td>
setosa
</td>
</tr>
<tr>
<td>
4.6
</td>
<td>
3.1
</td>
<td>
1.5
</td>
<td>
0.2
</td>
<td>
setosa
</td>
</tr>
<tr>
<td>
5.0
</td>
<td>
3.6
</td>
<td>
1.4
</td>
<td>
0.2
</td>
<td>
setosa
</td>
</tr>
<tr>
<td>
5.4
</td>
<td>
3.9
</td>
<td>
1.7
</td>
<td>
0.4
</td>
<td>
setosa
</td>
</tr>
</table>
</center>
