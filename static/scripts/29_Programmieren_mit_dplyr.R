## Lernziele:

## 
## - Ein Grundverständnis von *non-standard evaluation* (NSE) erwerben.

## - Wissen, wie man Funktionen für dplyr schreibt.

## - Funktionen, die NSE verwenden, erkennen.

## - Wissen, was man unter Zitieren (Quotation) versteht.

## 

## ----libs-nse-----------------------------
library(rlang)
library(pryr)
library(tidyverse)
library(gridExtra)

data(extra, package = "pradadata")


## ----easy-extra---------------------------
extra %>%
  drop_na(i01) %>%
  summarise(median(i01),
            IQR(i01))


## ----eval = FALSE-------------------------
## extra %>%
##   stats(i01)


## ----stats-wrong--------------------------
stats_ohno <- function(df, spalte){
  df %>%
    summarise(median(spalte, na.rm = TRUE),
              IQR(spalte, na.rm = TRUE))
}


## ----error = TRUE-------------------------
stats_ohno(extra, i01)


## -----------------------------------------
print("Hallo, R")


## ----error=TRUE---------------------------
print(Hallo)


## -----------------------------------------
library(ggplot2)


## ----nse-test, results = "hide"-----------
data(Affairs, package = "AER")
select(Affairs, affairs: age, education, rating, children)


## Wie erkennt man, ob eine Funktion NSE verwendet? Angenommen, Sie fragen sich, ob `library(ggplot2)` ein Funktionsaufruf ist, der NSE verwendet. Um das zu überprüfen, evaluieren Sie den Ausdruck `ggplot2` (geben Sie den Ausdruck in R ein). Wird ein Objekt erkannt, dann ist es vermutlich eine reguläre Evaluation, eine "normale" Funktion. Wird kein Objekt gefunden, so wurde vermutlich NSE verwendet.

## 

## ----nse-test2-no-eval, eval = FALSE------
## data("Affairs", package = "AER")
## select("Affairs", "Affairs$affairs", "Affairs$age", "Affairs$education",
##        "Affairs$rating", "Affairs$children")


## Ein Ausdruck ist ein Objekt, das R-Code enthält. Erstellt man einen Ausdruck, so spricht man von Zitieren.

## 

## ----kuchen-backen1, error = TRUE---------
ruehrkuchen <- eier + salz + milch + mehl


## ----zutaten------------------------------
eier <- 120
salz <- 1
milch <- 100
mehl <- 80


## ----kuchen-backen2-----------------------
ruehrkuchen <- eier + salz + milch + mehl
ruehrkuchen


## Zitieren bedeutet, den Ausdruck (den R-Code als Objekt) zu speichern -- nicht das Ergebnis des Ausdrucks.

## 

## ----rename-quo---------------------------
rezept <- quo


## ----ruehrkuchen-rezept-------------------
ruehrkuchen_rezept <- rezept(eier + salz + milch + mehl)
ruehrkuchen_rezept


## ----ruehrkuchen2-------------------------
backen <- eval_tidy

ruehrkuchen2 <- backen(ruehrkuchen_rezept)
ruehrkuchen2


## ----zuckerguss1, error = TRUE------------
zuckerguss_rezept <- rezept(butter + zitrone + zucker)
zuckerguss_rezept

zuckerguss <- backen(zuckerguss_rezept)


## ----zuckerguss-backen--------------------
butter <- 130
zitrone <- 20
aepfel <- 70
zucker <- 110

zuckerguss <- backen(zuckerguss_rezept)


## ----kuchen-zucker-error, error = TRUE----
kuchen <- backen(ruehrkuchen_rezept + aepfel + zuckerguss_rezept)


## ----oma----------------------------------
oma <- qq_show


## ----oma-show-----------------------------
oma(backen(ruehrkuchen_rezept + aepfel + zuckerguss_rezept))


## ----oma-zucker---------------------------
oma(!!zuckerguss_rezept)


## ----backen3------------------------------
kuchen_gesamtrezept <- rezept(!!ruehrkuchen_rezept + aepfel + !!zuckerguss_rezept)
kuchen_gesamtrezept


## ----kuchen-yeah--------------------------
kuchen <- backen(kuchen_gesamtrezept)
kuchen


## ----stats1-------------------------------
my_median <- function(df, col){
  col_q = enquo(col)
  df %>%
    summarise(median(!!(col_q), na.rm = TRUE))
}
my_median(extra, i01)


## ----stats1-quo, error=TRUE---------------
debug(my_median_quo)
my_median_quo <- function(df, col){
  col_q = quo(col)
  df %>%
    summarise(median(!!(col_q), na.rm = TRUE))
}
my_median_quo(extra, i01)


## ----quo-quo------------------------------
quo(quo(aepfel))


## ----quo-bang-bang------------------------
quo(!!quo(aepfel))


## ----bang-bang-fun-dplyr------------------
my_median <- function(df, col){
  col_q = enquo(col)
  df %>%
    summarise(md_spalte = median(!!(col_q),
                                 na.rm = TRUE))
}

my_median(extra, i01)


## ----new-eq-operator----------------------
my_median <- function(df, col){
  col_q = enquo(col)
  col_name_md_q = paste0(quo_name(col_q),"_md")
  df %>%
    summarise(!!(col_name_md_q) := median(!!(col_q),
                                          na.rm = TRUE))
}

my_median(extra, i01)


## ----quo-name-quo-------------------------
string <- "Das ist ein Text"
string2 <- quo_name(quo(string))
string2


## ----my-median-test-----------------------
extra %>%
  group_by(sex) %>%
  my_median(extra_mean)


## ----p-nse, echo = FALSE, eval = FALSE, fig.cap = "Der Unterschied zwischen Ausführen und Zitieren"----
## knitr::include_graphics("images/Programmieren/nse_schema_de-crop.pdf")


## ----nse-fun1-----------------------------
modus <- function(df, col){
  col_q <- enquo(col)
  df %>%
  count(!!col_q, sort = TRUE) %>%
  slice(1) %>%
  pull(1)
}

extra %>%
  summarise(modus_i1 = modus(., i01))


## Definiere `modus()` als Funktion mit den Parametern `df` und `col`

## Zitiere `col` und weise das Ergebnis `col_q` zu

## Nimm `df` UND DANN

## zähle die Ausprägungen von der evaluierten Variablen `col_q` und sortiere das Ergebnis UND DANN

## schneide die 1. Zeile heraus UND DANN

## ziehe die erste Spalte als Vektor heraus.

## 

## ----stats-grouped------------------------
my_median_grouped <- function(df, col, ...){
  col_q <- enquo(col)
  groups_q <- enquos(...)

  df %>%
    group_by(!!!groups_q) %>%
    summarise(md = median(!!(col_q), na.rm = TRUE))
}


extra %>%
  my_median_grouped(i01, sex, smoker)


## ----count-prop---------------------------
count_prop <- function(df, ...){
  groups_q <- enquos(...)

  df %>%
    count(!!!groups_q) %>%
    mutate(prop = round(n / sum(n), 2))
}

extra %>%
  count_prop(sex, smoker)


## ----gg-nse-p1----------------------------
gg_fun <- function(data, col)
{
  col_q <- enquo(col)
  p <- ggplot(data,
              aes_q(x = col_q))
  p + geom_histogram()
}
p1 <- gg_fun(extra, extra_mean)


## ----gg-fun2------------------------------
gg_fun2 <- function(data, col, group)
{
  p <- ggplot(data,
              aes_q(x = enquo(col))) +
    facet_wrap(as.formula(enquo(group))) +
    geom_histogram()
}

p2 <- gg_fun2(extra, extra_mean, sex)


## ----gg-nse, fig.cap = "ggplot-Diagramme erzeugt mit NSE", echo = FALSE----
grid.arrange(p1, p2, nrow = 1)


## 1. Leider funktionieren die dplyr-Befehle nicht, wenn man sie programmiert, d.h. innerhalb von Funktionen verwendet.

## 1. dplyr-Befehle arbeiten nach dem Prinzip der Nonstandard Evaluation, was immer das ist.

## 3. Sei `ggplot2 <- "dplyr"`; dann startet der Aufruf `library(ggplot2)` *dplyr*, nicht `ggplot2`.

## 4. `quo(eier+schmalz)` wird nicht zu einem Fehler führen, wenn die Objekte `eier` und `schmalz` unbekannt sind.

## 5. Unter einer Quosure versteht man einen (zitierten) Ausdruck, der sich die zugehörige Umgebung merkt.

## 6. Mit dem Bang-Bang-Operator kann man den Namen eines "Rezepts" in seine "Zutaten" übersetzen.

## 7. Wenn wir Oma fragen `oma(!!zuckerguss_rezept)`, antwortet sie `^butter + zitrone + zucker`.

## 8. Das Hütchen (Circumflex) in `^butter + zitrone + zucker` zeigt an, dass es sich bei der Ausgabe um eine Zitierung handelt.

## 9. In Funktionen muss man anstelle von `enquo()` stets `quo()` verwenden.

## 10. Möchte man einen Ausdruck in einen String umwandeln, so kann man `quo_name(expr)` verwenden.

## 
