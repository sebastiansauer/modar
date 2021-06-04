## ----echo = FALSE-------------------------
knitr::include_graphics("images/komm/Kommunizieren-crop.pdf")


## Lernziele:

## 
## - Anforderungen an ein gutes Werkzeug zur Textverarbeitung benennen können

## - Die Grundlagen der Markdown-Syntax kennen

## - Wissen, was YAML ist

## - Die Ablaufschritte der Texterstellung mit RMarkdown kennen

## - Tabellen mit Markdown erzeugen können

## - Zitationen und Referenzen im Text mit Markdown erstellen können

## 

## ----libs-rmd-----------------------------
library(knitr)
library(tidyverse)
library(xtable)
library(stargazer)
library(kableExtra)
library(formattable)
library(apaTables)
data(stats_test, package = "pradadata")


## ----libs-hiden-rmd, echo = FALSE, eval = FALSE----
## library(yart)
## library(papaja)
## library(komaletter)
## library(pradadata)


## ----rmd-flow, echo = FALSE, fig.cap = "Die zwei Arbeitsschritte bei der Konvertierung von RMarkdown-Dokumenten"----

knitr::include_graphics("images/komm/rmd_flow-crop.pdf")


## RMarkdown unterstützt drei wesentliche Ausgabeformate: HTML-, Word- und PDF-Dokumente; aber auch andere Format sind möglich. Um PDF-Dokumente erstellen zu können, muss Latex auf Ihrem Computer installiert sein. Wenn Sie eine Rmd-Datei in ein anderes Auszeichnungsformat (wie HTML) übersetzen, so wird (in der Voreinstellung) der Ablageort dieser Datei als Arbeitsverzeichnis verwendet, unabhängig davon, auf welchen Ordner Ihr Arbeitsverzeichnis gesetzt ist.

## 

## ----md-steps, echo = FALSE, fig.cap = "Die Arbeitsschritte mit RMarkdown", out.width = "100%"----
knitr::include_graphics("images/komm/Rmarkdown_flow.pdf")


## ----echo = FALSE, comment = ""-----------
cat(htmltools::includeText("includes/Rmd_demo.Rmd"))


## Übrigens bedeutet ein Zeilenumbruch in einer Rmd- oder md-Datei *keinen* Zeilenumbruch im Ausgabe-Dokument. Der einfache Zeilenumbruch dient primär der Übersichtlichkeit in der Textdatei. Eine Faustregel besagt, dass eine Textzeile nicht länger als 70 bis 80 Zeichen sein sollte, damit sie gut zu lesen ist. Möchten Sie im fertigen Dokument einen (sichtbaren) Zeilenumbruch einfügen, so fügen Sie in Ihrem Markdown-Dokument eine Leerzeile mit der Returntaste ein, bevor Sie Enter drücken.

## 

## ---- echo = FALSE, comment = ""----------
cat(readr::read_file("includes/markdown.Rmd"))


## -----------------------------------------
mtcars %>%
  slice(1:3)


## ----kable--------------------------------
mtcars %>%
  slice(1:3) %>%
  kable(caption = "Eine Tabelle mit kable aus knitr")


## ----lm1xtable-no-eval, eval = FALSE------
## lm1 <- lm(score ~ interest + study_time + interest:study_time,
##           data = stats_test)
## print(xtable(lm1), type = "html")  # oder: type = "latex"


## ----lm1xtable, echo = FALSE, result = "asis"----
lm1 <- lm(score ~ interest + study_time + interest:study_time,
          data = stats_test)
if (knitr:::is_html_output()) print(xtable(lm1), type = "html")
if (knitr:::is_html_output()) print(xtable(lm1), type = "latex")


## ----eval = FALSE-------------------------
## options(knitr.table.format = "html")  # oder knitr.table.format = "latex"


## ----kableExtra-output, echo = FALSE------
if (knitr:::is_html_output()) options(knitr.table.format = "html")
if (knitr:::is_html_output())options(knitr.table.format = "latex")


## ----eval = FALSE-------------------------
## kable(head(stats_test), "html") %>%
##   kable_styling(bootstrap_options = "striped", full_width = F)


## ----eval = FALSE-------------------------
## kable(head(stats_test), "html") %>%
##   kable_styling(bootstrap_options = "striped", full_width = F) %>%
##    column_spec(7, bold = T) %>%
##    row_spec(3:4, bold = T, color = "white", background = "red")


## ----kablextra-demo1, fig.cap = "Eine HTML-Tabelle mit kableExtra", echo = FALSE----
knitr::include_graphics("images/komm/kableextra_demo1.png")


## ----df-stats, include = FALSE------------
df <- stats_test %>%
  select(interest, study_time, score, bestanden) %>%
  na.omit() %>%
  arrange(bestanden) %>%
  filter(row_number() %in% c(1,2,3, 1629, 1630, 1631))


## ----eval = FALSE-------------------------
## df  %>%
##   mutate(bestanden = ifelse(bestanden == "nein",
##                             cell_spec(bestanden, "html",
##                                       color = "red", bold = T),
##                             cell_spec(bestanden, "html",
##                                       color = "green", italic = T)),
##          score = color_bar("lightblue")(score),
##          interest = color_tile("orange", "green")(interest)
##   ) %>%
##   kable("html", escape = F) %>%
##   kable_styling("hover", full_width = F) %>%
##   column_spec(3, width = "3cm")


## Nimm den Datensatz `df` UND DANN

## mutiere (hier: formatiere) die Variable `bestanden` in Abhängigkeit von ihrem Wert in rote oder grüne Schrift.

## mutiere (hier: formatiere) `score` mit einem "Farbbalken".

## mutiere (hier: formatiere) `interest` mit einem farbigen Hintergrund UND DANN

## Erstelle die Tabelle in HTML, wobei HTML-Steuerzeichen als solche erkannt werden sollen UND DANN

## formatiere im Stil "hover" und nicht seitenbreit UND DANN

## spezifiziere die Breite von Spalte 3 auf 3 cm.

## 

## ----kablextra-demo2, fig.cap = "Eine HTML-Tabelle mit kableExtra und formattable", echo = FALSE, out.width = "50%"----
knitr::include_graphics("images/komm/kableextra_demo2.png")


## Geben Sie für die Literatur- und die CSL-Datei keinen Pfad an, so geht R davon aus, dass die Dateien im Verzeichnis der jeweiligen Rmd-Datei liegen.

## 

## ----yart-demo, echo = FALSE, fig.cap = "Ein Screenshot von PDF-Dokumenten mit der yart-Vorlage"----
knitr::include_graphics("images/komm/yart_screenshot.png")


## 1. Schreibt man Dokumente in Markdown, so können diese in andere Markup-Formate wie HTML oder Latex übersetzt werden; RStudio stellt diese Funktionalität via Pandoc zur Verfügung. (In dieser und in den folgenden Fragen ist stets die Markdown-Version von RStudio gemeint.)

## 1. RMarkdown-Dokumente sind keine Textdateien.

## 1. Pandoc ist Software, mit der man Markdown-Texte in andere Markup-Sprachen übersetzen kann.

## 1. Das R-Paket `knitr` "strickt" Markdown und andere Auszeichnungssprachen in ein Dokument zusammen.

## 1. Markdown-Dateien dürfen nicht mit einem YAML-Teil beginnen.

## 6. Möchte man eine Zeile als Überschrift der Ebene 1 formatieren, so könnte man schreiben "#Überschrift".

## 7. Möchte man R-Syntax in eine Markdown-Datei integrieren, so kennzeichnet man den Beginn eines R-Blocks mit ````{r}`.

## 8. Auszeichnungen dieser Art "[WeisOis2017]" kennzeichnen eine Zitation in Markdown.

## 9. Im Moment unterstützt Markdown nur einen Zitationsstil.

## 10. Die Markdown-Vorlage `papaja` ist nützlich zur Gestaltung von optisch aufwändigen Briefen.

## 
## 
## 
