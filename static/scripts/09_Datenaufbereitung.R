## Lernziele:

## 
## - Typische Probleme der Datenaufbereitung kennen und bearbeiten können

## - Grundlegende Herangehensweisen im Umgang mit fehlenden Werte kennen

## - Datenanomalien aufspüren können

## - Daten umformen können (z.B. partitionieren)

## - Wissen, was man unter der Normalform eines Dataframes versteht

## 

## ----libs-typische-probleme---------------
library(corrr)  
library(car)  
library(pradadata)
library(tidyverse)
library(mice)
library(VIM)
library(skimr)
library(ggcorrplot)
library(sjmisc)

data(extra, package = "pradadata")
data(stats_test, package = "pradadata")



## ----libs-hiddeen-datenauf, echo = FALSE----
library(viridis)


## -----------------------------------------
stats_test %>%
  pull(score) %>%
  is.na() %>% sum()


## -----------------------------------------
# Ursprüngliche Anzahl an Fällen (Zeilen)
nrow(stats_test)

# Nach Umwandlung in neuen Dataframe
stats_test %>%
   drop_na() -> stats_test_na_omit
nrow(stats_test_na_omit)


## Bei mit der Pfeife verketteten Befehlen darf man für Funktionen die runden Klammern weglassen, wenn man keinen Parameter schreibt. Also ist `drop_na` (ohne Klammern) erlaubt bei `dplyr`, wo es eigentlich `drop_na()` heißen müsste. Sie dürfen die Klammern natürlich schreiben, aber Sie müssen nicht. Mit Klammern ist deutlicher, dass es sich um eine Funktion handelt, nicht um ein Datenobjekt; das erkennt man u.U. nur an den runden Klammern. Also schreiben Sie Funktionen besser mit Klammern.

## 

## ----demo-complete-cases, results = "hide"----
stats_test %>%
   filter(!complete.cases(.)) %>%
  head()


## 
## Man beachte, dass der Punkt `.` für den Datensatz steht, wie er vom letzten Schritt weitergegeben wurde. Innerhalb einer dplyr-Befehlskette können wir den Datensatz, wie er im letzten Schritt beschaffen war, stets mit `.` ansprechen; ganz praktisch, weil schnell zu tippen. Natürlich könnten wir diesen Datensatz jetzt als neues Objekt speichern und damit weiterarbeiten. Das Ausrufezeichen `!` steht für logisches "Nicht". Mit `head` bekommt man nur die ersten paar Fälle (6 im Standard) angezeigt, was für einen Überblick oft reicht.

## 

## Nimm den Datensatz `stats_test` UND DANN

## filtere die nicht-kompletten Fälle.

## 

## ----count-nas-stats-test-----------------
stats_test %>%
  mutate(self_eval_NA = is.na(self_eval)) %>%
  summarise(self_eval_NA_sum = sum(self_eval_NA))


## Nimm die Tabelle `stats_test` UND DANN

## berechne eine Spalte `self_eval_NA` in der steht, ob bei `self_eval` ein `NA` steht UND DANN

## zähle die `NA`s zusammen.

## 

## ----extra-count--------------------------
extra %>%
  select(i01:i10) %>%
  skim_to_wide() %>%
  select(missing)


## ----extra-count2, echo = FALSE-----------
extra %>%
  select(i01:i10) %>%
  skim_to_wide() %>%
  pull(missing) %>% as.integer %>% sum -> extra_count_na


## Eine "lange Pfeife", also eine lange Sequenz verbundener R-Befehle, kann manchmal kompliziert sein. Wenn Sie unsicher sind, was ein bestimmter Befehl auslöst, dann markieren Sie nur eine oder eine Auswahl an Zeilen. Führen Sie diesen Teil aus und arbeiten Sie sich auf diese Weise vor.

## 

## ----i-am-complete------------------------
extra %>%
  drop_na(code, i01:i10)


## -----------------------------------------
stats_test %>%
  select(-date_time, row_number) %>%
  tidyr::replace_na(replace = list(interest = mean(.$interest, na.rm = TRUE)))


## ----replace-na-stats-test, eval = FALSE----
## interest_avg <- mean(stats_test$interest, na.rm = TRUE)
## stats_test %>%
##   mutate(interest = replace(x = .$interest,
##                             list = is.na(.$interest),
##                             values = interest_avg)) -> stats_test


## ----vim, fig.cap = "Histogramm fehlender Werte (links); Muster fehlender Werte (rechts)", out.width = "80%"----
items <- extra %>%
  select(i01:i10)

# Erstellt Diagramme zur Inspektion fehlender Werte:
aggr(items, col = c('grey80','red'),
     numbers = TRUE,
     sortVars = TRUE,
     labels = names(data))


## -----------------------------------------
extra %>%
  select(i01:i10) %>%
  mice(m = 1,
       printFlag = FALSE) -> items_imputed


## ----eval = FALSE-------------------------
## stats_test$study_time[1] <- -99
## head(stats_test)
## 
## stats_test %>%
##   mutate_if(is_numeric, na_if, -99) %>% head


## ----no-dublicates------------------------
stats_test %>%
  mutate(is_duplicate = duplicated(date_time)) %>%
  filter(!is_duplicate) %>%
  nrow


## ----stats-test-head, eval = FALSE--------
## stats_test %>%
##   dplyr::count(interest, sort = TRUE)
## 
## stats_test %>%
##   dplyr::count(interest, sort = TRUE) %>%
##   ggplot(aes(x = factor(interest), y = n)) +
##   geom_point()


## ----p-skewed, echo = FALSE, fig.cap = "Anomalien identifizieren", out.width = "100%"----
extra %>%
  select(i01:i08) %>%
  gather() %>%
  ggplot() +
  aes(x = value) +
  geom_bar() +
  facet_wrap(~key, ncol = 4) -> p1

extra %>%
  select(i01:i08) %>%
  gather() %>%
  ggplot() +
  aes(x = key) +
  theme(legend.position = "bottom") +
  scale_fill_viridis(discrete = TRUE) +
  geom_bar(position = "fill", aes(fill = factor(value))) +
  guides(fill = guide_legend(keywidth = 0.3,
                                           keyheight = 0.1)) +
  labs(x = "Item",
       y = "Anteil",
       fill = "Antwort") -> p2

gridExtra::grid.arrange(p1, p2, nrow = 1)


## ----stats-test-replace, eval = FALSE-----
## stats_test %>%
##   mutate(score_bereinigt = replace(.$score,
##                                    .$score < .5,
##                                    NA)) -> stats_test


## ----fig-ausreisser, fig.cap = "Ausreißer identifizieren; links: Histogramm; rechts: Boxplot", echo = FALSE, out.width = "100%"----
p1 <- qplot(x = score, data = stats_test)
p2 <- qplot(x = "score", data = stats_test, y = score, geom = "boxplot")

gridExtra::grid.arrange(p1, p2, nrow = 1)


## ----def-extreme-cases--------------------
stats_test %>%
  mutate(score_z = (score - mean(score, na.rm = TRUE)) /
           sd(score, na.rm = TRUE))  %>%
  mutate(is_extreme = if_else(abs(score_z) > 3, TRUE, FALSE)) %>%
  count(is_extreme)


## ----extra-correlate----------------------
extra %>%
  dplyr::select(i01:i10) %>% # Wähle die Variablen von i1 bis i10 aus
  corrr::correlate() -> km   # Korrelationsmatrix berechnen

km %>%
  select(1:3)  # der Übersichtlichkeit halber nur ein Auszug


## ----p-corr1------------------------------
p_corr1 <- km %>%
  shave() %>% # Oberes Dreieck ist redundant, wird "abrasiert"
  rplot(colors = c("firebrick", "blue4"))  # Korrelationsplot


## ----p-corr2------------------------------
extra %>%
  dplyr::select(i01:i10) %>%
  drop_na() %>%
  cor() %>%
  ggcorrplot() -> p_corr2


## ----fig-corrr, out.width="100%", echo = FALSE, fig.cap = "Varianten von Korrelationsplots"----
gridExtra::grid.arrange(p_corr1, p_corr2, nrow = 1)


## ----quasi-konst--------------------------
IQR(extra$n_facebook_friends, na.rm = TRUE)  # keine Konstante
n_distinct(extra$sex)  # es scheint 3 Geschlechter zu geben...


## ----fig-norm-check, echo = FALSE, fig.cap = "Visuelles Prüfen der Normalverteilung", fig.asp = .4, out.width = "100%"----
extra %>%
  ggplot() +
  aes(x = extra_mean) +
  geom_histogram(bins = 10) -> p1

extra %>%
  ggplot() +
  aes(x = extra_mean) +
  geom_density() -> p2

extra %>%
  ggplot() +
  aes(sample = extra_mean) +
  stat_qq() -> p3

gridExtra::grid.arrange(p1, p2, p3, nrow = 1)


## ----tidy1, fig.cap = "Schematische Darstellung eines Dataframes in Normalform", echo = FALSE, out.width = "100%"----
knitr::include_graphics("images/tidy/tidy-1.png")


## ----lang-breit, echo = FALSE, fig.cap = "`Tidy` Dataframes. Links: langes vs. breites Format; rechts: eine Abbildung auf Basis einer Normalform-Tabelle", out.width = "100%"----
imgs <- c("images/tidy/breit_lang.png",
          "images/tidy/gather_spread-crop.png")
#prada::comb2pngs(imgs)
knitr::include_graphics("images/tidy/tidy-dataframes-crop.pdf")



## ----eval = FALSE-------------------------
## library(tidyr)
## df_lang <- gather(df_breit, key = "Quartal", value = "Umsatz")
## 
## df_breit <- spread(df_lang, Quartal, Umsatz)


## ----eval = FALSE-------------------------
## df_lang <- gather(df_breit, key = "Quartal", value = "Umsatz", -ID)


## ----percentiles-normal, echo = FALSE, fig.cap = "Flächenanteile und z-Werte bei der Normalverteilung", fig.asp = .25----
mean.1 <-0
sd.1 <- 1
zstart <- 2
zend <- 3
zcritical <- 1.65

my_col <- "#00998a"

x <- seq(from = mean.1 - 3*sd.1, to = mean.1 + 3*sd.1, by = .01)


MyDF <- data.frame(x = x, y = dnorm(x, mean = mean.1, sd = sd.1))

shade_curve <- function(MyDF, zstart, zend, fill = "red", alpha = .5){
  geom_area(data = subset(MyDF, x >= mean.1 + zstart*sd.1
                          & x < mean.1 + zend * sd.1),
            aes(y=y), fill = fill, color = NA, alpha = alpha)
}


p1a <- ggplot(MyDF, aes(x = x, y = y)) + geom_line() +
  shade_curve(MyDF = MyDF, zstart = -1, zend = 1, fill = my_col, alpha = .3) +
  shade_curve(MyDF = MyDF, zstart = 1, zend = 2, fill = my_col, alpha = .5) +
  shade_curve(MyDF = MyDF, zstart = -2, zend = -1, fill = my_col, alpha = .5) +
  shade_curve(MyDF = MyDF, zstart = 2, zend = 6, fill = my_col, alpha = .7) +
  shade_curve(MyDF = MyDF, zstart = -3, zend = -2, fill = my_col, alpha = .7) +
  scale_x_continuous(breaks = -3:3) +
  scale_y_continuous(breaks = NULL) +
  theme_classic() +
  ylab("") + xlab("")

MyDF %>%
  mutate(y_cdf = cumsum(y)) -> MyDF

MyDF %>%
  filter(x %in% c(-3, -2.58, -2, -1.65, -1, -.5, 0, .5, 1, 1.65, 2, 2.58, 3)) -> MyDF_filtered

p1a + geom_text(data = MyDF_filtered,
                aes(x = x, y = y + .1, label = paste(round(y_cdf, 0),"%")),
                check_overlap = TRUE) +
  geom_segment(data = MyDF_filtered,
               aes(x = x, xend = x, y = 0, yend = y), linetype = "dashed")


## ----z-stand------------------------------
extra %>%
  dplyr::select(i01, i02r) %>%  
  scale() %>%  # z-standardisieren
  as_data_frame() %>%  # von Matrix zurück zu Dataframe
  head()  # nur die ersten paar Zeilen abdrucken


## ----eval = FALSE-------------------------
## extra %>%
##   mutate_if(is.numeric, funs("z" = scale)) %>%
##   glimpse


## ----rename-stats-test, eval = FALSE------
## stats_test %>%
##   rename(Punkte = score) -> stats_test


## Übrigens kann man den gleichen Effekt mit `dplyr::select()` auch erreichen; genauer den fast gleichen. Probieren Sie mal `rename(stats_test, Punkte = score)` und vergleichen Sie das Ergebnis.

## 

## -----------------------------------------
stats_test %>%
  mutate(interest_chr = as.character(interest)) -> stats_test


## ----eval = FALSE-------------------------
## stats_test %>%
##   ggplot(aes(x = interest, fill = as.numeric(bestanden))) +
##   geom_bar() +
##   labs(title = "Keine Füllfarben. Traurig.")


## ----feistersack1-------------------------
Ergebnis <- c(1, 1, 1, 2, 3)
Ergebnis <- factor(Ergebnis, labels = c("bestanden",
                                        "durchgefallen",
                                        "nicht abgegeben"))
Ergebnis


## ----feistersack2-------------------------
Ergebnis <- c(1, 1, 1, 2, 3)
Ergebnis <- factor(Ergebnis, labels = c("10", "20", "30"))
Ergebnis


## ----feistersack3-------------------------
as.numeric(Ergebnis)


## ----feistersack4-------------------------
Ergebnis %>% as.character() %>% as.numeric()


## ----umkodieren, echo = FALSE, fig.cap = "Links: Umkodieren; rechts: Binnen.", out.width = "50%"----
knitr::include_graphics("images/typ_prob/umkodieren-crop.pdf")


## ----recode-stats-test1, results = "hide"----
stats_test %>%
  mutate(study_binned = car::recode(.$study_time,
"5 = 'sehr viel'; 2:4 = 'mittel'; 1 = 'wenig'",
as.factor = TRUE)) -> stats_test

stats_test %>%
  mutate(study_binned = car::recode(.$study_time,
"5 = 'sehr viel'; 2:4 = 'mittel'; 1 = 'wenig'",
as.factor = FALSE)) -> stats_test

stats_test %>%
  mutate(score_binned = car::recode(.$score,
"40:38 = 'sehr gut'; 37:35 = 'gut'; else = 'Weiterlernen'",
as.factor = TRUE)) -> stats_test

stats_test %>% select(score_binned, study_binned) %>% head


## Wann braucht man einen Punkt `.` in einer dplyr-Pfeifenkette? Man braucht ihn nur dann, wenn man innerhalb der Pfeifenkette Nicht-dplyr-Befehle verwendet wie `mean()` oder `recode()`. Diese Befehle wissen nicht, dass sie gerade in einer Pfeifenkette stehen, daher müssen wir ihnen den Namen der Tabelle explizit mitteilen. Genau genommen steht der Punkt für die Tabelle, so wie sie aus dem letzten Pfeifenschritt herausgekommen ist (z.B. schon mit einigen Zeilen weggefiltert).

## 
## 

## ----recode-stats-test2-------------------
stats_test %>%
  mutate(no_interest = car::recode(.$interest,
"1 = 6; 2 = 5; 3 = 4; 4 = 3; 5 = 2; 6 = 1; else = NA")) -> stats_test

glimpse(stats_test$no_interest)


## ----eval = FALSE-------------------------
## dplyr::count(stats_test, interest)
## dplyr::count(stats_test, no_interest)


## ----recode-stats-test3-------------------
stats_test %>%
  mutate(bestanden =
           car::recode(.$score,
                       "0:0.9 = 'durchgefallen';
                       else = 'bestanden'")) -> stats_test_schnaggeldi


## ----recode-stats-test4-------------------
stats_test %>%
  mutate(bestanden = score > 24) -> stats_test

head(stats_test$bestanden)


## ----recode-stats-test5-------------------
stats_test$Grenzfall <- stats_test$score == 24/40

dplyr::count(stats_test, Grenzfall)


## ----durchpfeifen-demo-lgl-var, eval = FALSE----
## stats_test <-
## stats_test %>%
##   mutate(Grenzfall = score == 24/40)


## ----demo-cut-interval--------------------
temp <- cut_interval(x = stats_test$score, n = 3)
levels(temp)


## ----demo-cut-number----------------------
score_gruppen <- cut_number(stats_test$score, n = 2)
str(score_gruppen)


## ----demo-cut-pur-------------------------
stats_test %>%
  mutate(punkte_gruppe = cut(stats_test$score,
         breaks = c(-Inf, .5, .6, .7, .8, .9),
         labels = c("5", "4", "3", "2", "1"))) -> stats_test

dplyr::count(stats_test, punkte_gruppe)


## ----demo-rowmeans, eval = TRUE-----------
items %>%
  row_means(n = .9) %>% head(2)


## ----demo-rowwise-dplyr-------------------
select(items, i01, i02r, i03) %>%
  rowwise() %>%
  mutate(extra_md = median(c(i01, i02r, i03)),
         max_row = max(c(i01, i02r, i03)))


## Wähle aus `items` folgende drei Items UND DANN

## führe den folgenden Befehl zeilenweise aus UND DANN

## berechne den Median und das Maximum über die angegebenen Spalten.

## 

## 1. Zufälliges Fehlen von Daten (MAR) kann man salopp veranschaulichen mit "Der Hund hat die Daten aufgefressen".

## 2. Auf fehlende Werte kann man z.B. mit `is.na()` prüfen.

## 3. Bei der Frage, ob man fehlende Werte löschen oder ersetzen sollte, gilt die Regel: Wenn mehr als 5% der Werte fehlen, so muss man auf jeden Fall ersetzen.

## 4. Unter multipler Imputation versteht man das mehrfache Prüfen einer Transformation.

## 5. *z*-Werte haben einen Mittelwert von 1.

## 6. Um vor unliebsamen Überraschungen geschützt zu sein, sollte man Faktorvariablen zuerst in Text- und dann erst in numerische Variablen umwandeln.

## 7. Kategoriale Variablen können mit `cut` in numerische umgewandelt werden.

## 8. Man sollte sich hüten, Vektoren in Skalare umzuwandeln.

## 9. Der R-Befehl `cor` berechnet Korrelationstabellen; in der Voreinstellung werden fehlende Werte fallweise ignoriert.

## 10. Deskriptive Statistik beinhaltet das Zusammenfassen von Vektoren zu Skalaren als zentralen Teil.

## 2. In einem Dataframe in Normalform steht in jeder Zeile eine Beobachtung.

## 2. In einem Dataframe in Normalform steht in jeder Spalte eine Variable.

## 
