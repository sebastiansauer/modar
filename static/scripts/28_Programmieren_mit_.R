## Lernziele:

## 
## - Wissen, wie man in R eine Funktion schreibt

## - Das Iterationskonzept mit `purrr::map()` in den Grundzügen verstehen

## - Grundideen des defensiven Programmierens kennen

## 

## ----libs-progr---------------------------
library(tidyverse)
library(magrittr)
library(gridExtra)

data(extra, package = "pradadata")
data(Affairs, package = "AER")


## ----p-fun-def, fig.cap = "Funktionen definieren: Die Funktion als Gefäß für andere Befehle", echo = FALSE, out.width="50%"----
knitr::include_graphics("images/Programmieren/Funs_def-crop.pdf")


## ----abfolge------------------------------
x <- c(1, 2, 3)

x %>%
  subtract(mean(x)) %>%
  raise_to_power(2) %>%
  sum() %>%
  divide_by(length(x)) %>%
  sqrt()


## Nimm den Vektor `x` UND DANN

## ziehe von jedem Element von `x` den Mittelwert von `x` ab UND DANN

## nimm jedes Element hoch 2 (quadriere es) UND DANN

## bilde die Summe der Elemente UND DANN

## teile durch die Anzahl der Elemente von `x`  UND DANN

## ziehe von dieser Zahl die Wurzel.

## 

## ----eval = FALSE-------------------------
## sd2 <- function(eingabevektor){
##   # hier unsere Rechenschritte, der "Körper" der Funktion
## }


## ----abfolge-fun--------------------------
sd2 <- function(eingabevektor){
eingabevektor %>%
  subtract(mean(eingabevektor)) %>%
  raise_to_power(2) %>%
  sum() %>%
  divide_by(length(eingabevektor)) %>%
  sqrt()
}


## -----------------------------------------
sd2(x)


## Der Aufruf des Namens einer Funktion $f$ führt eine Reihe von anderen Funktionen aus (nämlich die, die im Körper von $f$ angeführt sind). Die von $f$ aufgerufenen Funktionen können parametrisiert sein: so können die Werte der Argumente von $f$ an die aufgerufenen Funktionen weitergegeben werden. Das, was $f$ erledigt, hängt also von Ihren Parameter(werten) ab.

## 

## ----prefix-funtion-----------------------
`+`(1, 1)


## ----strange-fun--------------------------
sd2 <- function(eingabevektor){
eingabevektor %>%
  `-`(mean(eingabevektor)) %>%
  `^`(2) %>%
  sum() %>%
  `/`(length(eingabevektor)) %>%
  sqrt() -> ausgabe
  return(ausgabe)

}
sd2_von_x <- sd2(x)
str(sd2_von_x)


## ----p-wdh, echo = FALSE, fig.cap = "Die Funktion f wird auf jedes Element von E angewendet"----
knitr::include_graphics("images/Programmieren/wdh-crop.pdf")


## -----------------------------------------
x <- 1:3
y <- 4:6
x+y


## ----for-loop-----------------------------
z <- vector(mode = "numeric", length = length(x))
for(i in seq_along(x)) {
      z[i] <- x[i] + y[i]
}
z


## -----------------------------------------
extra %>%
  summarise_at(vars(i01:i02r), funs(max, median, sd, IQR), na.rm = TRUE)


## ----favstats-----------------------------
extra %>%
  select(i01, i02r) %>%
  map(mosaic::favstats)


## -----------------------------------------
extra %>%
  select(i01:i02r) %>%
  map(mean, na.rm = TRUE)


## Nimm den Datensatz `extra` UND DANN

## wähle Spalten `i01` bis `i02r` aus UND DANN

## wende den Befehl `mean` auf alle diese Spalten an.

## 

## ----results = "hide"---------------------
extra %>%
  select(i01:i10) %>%
  map(~mean(., na.rm = TRUE))


## -----------------------------------------
extra %>%
  select(i01:i02r) %>%
  map_dbl(~mean(., na.rm = TRUE))


## Jedes gewählte Element von `extra` (allgemeiner: der vorausgehenden Liste) wird an die Funktion angegeben, die bei `map()` als Argument angeführt ist.

## 

## ----write-csvs---------------------------
1:3 %>%
  map(~filter(extra, row_number() == .)) %>%
  map(~select(., 1:3)) %>%
  imap(~write_csv(., path = paste0("dummy/df", .y, ".csv")))


## ----csv-names----------------------------
1:3 %>%
  map(~filter(extra, row_number() == .)) %>%
  map(~select(., 1:3)) %>%
  imap(~paste0("dummy/df", .y, ".csv"))


## ----eval = FALSE-------------------------
## df1 <- read_csv("dummy/df1.csv")
## df2 <- read_csv("dummy/df2.csv")
## df3 <- read_csv("dummy/df3.csv")


## -----------------------------------------
df_filenames <- dir(path = "dummy", pattern = "*.csv")
str(df_filenames)


## ----eval = FALSE-------------------------
## df_filenames %>%
##   map(read_csv)


## ----df-filenames-------------------------
df_filenames %>%
  map_df(~read_csv(file = paste0("dummy/", .))) -> df
df


## -----------------------------------------
extra %>%
  select(i01, i02r, i03, n_facebook_friends, sex) %>%
  split(.$sex) %>%
  map(~lm(n_facebook_friends ~ i01 + i02r + i03, data = .)) -> extra_models


## ----map-r-sq-----------------------------
extra_models %>%
  map(summary) %>%
  map_dbl("r.squared")


## -----------------------------------------
Affairs %>%
select_if(is.numeric) %>% head


## -----------------------------------------
Affairs %>%
  select_if(is.numeric) %>%
  map(~t.test(. ~ Affairs$gender)) %>%
  map_dbl("p.value")


## Nimm die Tabelle `Affairs` UND DANN

## wähle alle numerischen Spalten UND DANN

## wende auf alle Spalten einen *t*-Test an, wobei die Gruppierungsvariable `Affairs$gender` ist UND DANN

## wende die Funktion "extrahiere das Element mit dem Namen `p.value`" an; das Ergebnis soll vom Typ 'reelle Zahl' (double) sein.

## 

## ----lapply-demo, results = "hide"--------
lapply(Affairs[c("affairs", "age", "yearsmarried")],
       function(x) t.test(x ~ Affairs$gender))


## ----p-map-ttest, fig.cap = "Multiple t-Tests und deren p-Werte"----
Affairs %>%
  select_if(is.numeric) %>%
  map_df(~t.test(. ~ Affairs$gender)$p.value) %>%
  gather() %>%
  mutate(signif = ifelse(value < .05, "significant", "ns")) %>%
  ggplot(aes(x = reorder(key, value), y = value)) +
  geom_point(aes(color = signif, shape = signif)) +
  coord_flip() +
  labs(x = "Untersuchte Variablen",
       y = "p-Wert")


## ----map-pvalue---------------------------
Affairs %>%
  select_if(is.numeric) %>%
  map(~t.test(. ~ Affairs$gender)) %>%
  keep(~.$p.value < .05) %>%
  map("p.value")


## ----blinke, eval = FALSE-----------------
## abbiegen <- function(richtung, krass = FALSE){
##   schulterblick(richtung)
##   blinke(richtung)
##   zustand <- lenke(richtung)
##   return(zustand)
## }


## ----p-simple-code, fig.cap = "Den Code zu vereinfachen, hilft, Fehler zu finden", out.width = "100%"----
extra %>%
  select(extra_mean) %>%
  drop_na(extra_mean) %>%
  ggplot +
  aes(x = extra_mean, fill = "sex") +
  geom_histogram() +
  labs(title = "Was für ein Histogramm",
       subtitle = "Aber was ist mit den Farben?") +
  theme(legend.position  = "bottom") -> p1


extra %>%
  ggplot() +
  aes(x = extra_mean, color = sex) +
  geom_histogram() -> p2

grid.arrange(p1, p2, nrow = 1)


## ----stop-if-not-demo, eval = FALSE-------
## stopifnot(is.numeric(extra$sex))
## stopifnot(is.numeric(extra$n_party))
## 
## stopifnot(sum(is.na(extra$n_party)) == 0)
## sum(is.na(extra$n_party))


## 1. In Funktionen darf die Pfeife (aus dem Paket `magrittr`) nicht verwendet werden.

## 1. Eine Funktion kann über Argumente (Parameter) verfügen.

## 3. Eine Funktion muss über Argumente (Parameter) verfügen.

## 4. `+(1, 1)` schreibt man gewöhnlich als `1+1`.

## 5. Die Funktion `sd2()` liefert identische Wert wie `sd()`.

## 6. Wenn `x <- 1:3; y <- 4:6`, dann gibt `z <- x + y` zurück: `5 7 9`.

## 7. Sei `s <- c("Hallo", "R", "wie", "geht's")`; führt man `paste0(s, "!")` aus, wird man sehen, dass `paste0()` vektoriell rechnet.

## 8. Mit `summarise_at()` kann man mehrere Spalten eines Dataframes zu einer oder mehreren Statistiken zusammenfassen.

## 9. Mit `map()` kann man mehrere Elemente eines Vektors einer Funktion zuführen; die Funktion wird auf jedes Element angewendet.

## 10. Der Funktion `map()` dürfen auch Dataframes eingegeben werden; in dem Fall wird die zugeordnete Funktion auf jede Spalte angewendet.

## 

## -----------------------------------------

s <- c("Hallo", "R", "wie", "geht's")

paste0(s, "!")


