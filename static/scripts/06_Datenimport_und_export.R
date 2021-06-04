## Lernziele:

## 
## - Wissen, auf welchen Wegen man Daten in R hineinbekommt

## - Wissen, was eine CSV-Datei ist

## - Wissen, was UTF-8 bedeutet

## - Wissen, mit welchen Datenformaten man häufig in R arbeitet

## - Erläutern können, was R unter dem "working directory" versteht

## - Daten aus R exportieren können

## 

## -----------------------------------------
library(tidyverse)


## ----data-import-RStudio, echo = FALSE, out.width = "50%", fig.cap = "Daten einlesen (importieren) mit RStudio"----
knitr::include_graphics("images/tidy/import_RStudio2.png")


## Daten (CSV, Excel ...)  können Sie *nicht* öffnen über *File > Open File ...* Dieser Weg ist Skript-Dateien und R-Daten-Objekten vorbehalten.

## 

## ----load-extra---------------------------
data(profiles, package = "okcupiddata")


## ----date-deutsch-no-eval, eval = FALSE----
## daten_deutsch <- read_delim("daten_deutsch.csv",
##                             delim = ";",
##                             locale = locale(decimal_mark = ","))
## 
## daten_deutsch <- read_csv2("daten_deutsch.csv")


## ----rstudio-delimiter, echo = FALSE, fig.cap = "Trennzeichen einer CSV-Datei in RStudio einstellen"----
knitr::include_graphics("images/tidy/delimiter.png")


## ----prada-stats-test-download, eval = FALSE----
## prada_stats_test_url <-
##   paste0("https://raw.github.com/",  # Domain, Webseitenname
##          "sebastiansauer/",  # Nutzer
##          "Praxis_der_Datenanalyse/",  # Projekt/Repositorium
##          "master/",  # Variante
##          "data/stats_test.csv")  # Ordner und Dateinamen
## 
## stats_test <- read_csv(prada_stats_test_url)
## 


## ----echo = FALSE-------------------------
data(stats_test, package = "pradadata")


## 
## Der Befehl `paste0` "klebt" (to paste) mehrere Textabschnitte zusammen; die null dabei heißt, dass zwischen den Textabschnitten nichts steht; die Textabschnitte werden nahtlos zusammengeklebt. Wir hätten die ganze URL auch in einen Textschnipsel packen können. So ist es nur ein bisschen übersichtlicher.

## 

## ----eval = TRUE--------------------------
stats_test_normaler_df <- as.data.frame(stats_test)
str(stats_test_normaler_df[, c("self_eval", "score")])  # Output: df
str(stats_test_normaler_df[, "self_eval"])  # output: Vektor!


## ----class-tibble-------------------------
class(stats_test)
class(data.frame(stats_test))
attr(stats_test, "class")


## Speichern Sie R-Textdateien wie Skripte stets mit UTF-8-Kodierung ab.

## 

## ----write-csv, eval = FALSE--------------
## write_csv(name_der_tabelle, "Dateiname.csv")
## 


## 1. In CSV-Dateien dürfen Spalten *nie* durch Komma getrennt sein.

## 2. RStudio bietet die Möglichkeit, CSV-Dateien per Klick zu exportieren.

## 2. RStudio bietet  die Möglichkeit, CSV-Dateien per Klick zu importieren.

## 2. "Deutsche" CSV-Dateien verwenden als Spalten-Trennzeichen einen Strichpunkt.

## 2. R stellt fehlende Werte mit einem Fragezeichen `?` dar.

## 6. Um Excel-Dateien zu importieren, kann man den Befehl `read_csv()` verwenden.

## 7. Im Standard nutzen CSV-Dateien ein Semikolon als Spaltentrennzeichen.

## 8. Daten im Rda-Format sind keine Textdateien.

## 9. Tibbles sind auch Dataframes.

## 10. Mit `write_csv()` exportiert man einen Dataframe in eine CSV-Datei.

## 

## ----eval = FALSE-------------------------
## # nur die ersten paar Zeilen zeigen, daher mit head()
## as_tibble(head(profiles))  # tibble
## as.data.frame(head(profiles))  # normaler Dataframe

