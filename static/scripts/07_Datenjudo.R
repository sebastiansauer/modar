## ----fig-datenjudo, echo = FALSE----------
knitr::include_graphics("images/Datenjudo/Aufbereiten.png")


## Lernziele:

## 
## - Die zentralen Ideen der Datenanalyse mit dplyr verstehen

## - Typische Probleme der Datenanalyse schildern können

## - Zentrale `dplyr`-Befehle anwenden können

## - `dplyr`-Befehle kombinieren können

## - Die Pfeife anwenden können

## - Dataframes zusammenführen (joinen) können

## 

## -----------------------------------------
library(mosaic)
library(tidyverse)
library(stringr)   
library(car)  

data(stats_test, package = "pradadata")
data(flights, package = "nycflights13")
data(stats_test, package = "pradadata")
data(profiles, package = "okcupiddata")
data(airlines, package = "nycflights13")


## ----echo = FALSE-------------------------
library(methods)  # wird aus irgendeinem Grund nicht automatisch geladen, obwohl built-in
library(magrittr)


## ----bausteine, echo = FALSE, fig.cap = "Lego-Prinzip: Zerlege eine komplexe Struktur in einfache Bausteine"----
knitr::include_graphics("images/Datenjudo/Bausteine_dplyr-crop.pdf")


## ----durchpfeifen-allgemein, echo = FALSE, fig.cap = "Durchpfeifen: Ein Dataframe wird von Operation zu Operation weitergereicht"----
knitr::include_graphics("images/Datenjudo/durchpfeifen_allgemein_crop.png")


## ----fig-filter, echo = FALSE, fig.cap = "Zeilen filtern", out.width = "50%"----
knitr::include_graphics("images/Datenjudo/filter.pdf")


## Merke: Die Funktion `filter()` filtert Zeilen aus einem Dataframe. Die Zeilen, die zum Filterkriterium passen, bleiben im Datensatz.

## 

## ----filter-ex, eval = FALSE--------------
## df_frauen <- filter(profiles, sex == "f")  # nur die Frauen
## df_alt <- filter(profiles, age > 70)  # nur die alten Menschen
## # nur die alten Frauen, d.h. UND-Verknüpfung:
## df_alte_frauen <- filter(profiles, age > 70, sex == "f")
## # zwischen (between) 35 und 60:
## df_mittelalt <- filter(profiles, between(age, 35, 60))
## # liefert alle Personen, die Nicht-Raucher *oder* Nicht-Trinker sind:
## df_nosmoke_nodrinks <- filter(profiles,
##                               smokes == "no" | drinks == "not at all")


## ----eval = FALSE-------------------------
## # dplyr:
## filter(profiles, age > 70, sex == "f", drugs == "sometimes")
## 
## # base-R:
## profiles[profiles$age > 70 & profiles$sex == "f" &
##            profiles$drugs == "sometimes", ]


## Manche Befehle wie `filter()` haben einen Allerweltsnamen; gut möglich, dass ein Befehl mit gleichem Namen in einem anderen (geladenen) Paket existiert. Das kann dann zu Verwirrung führen -- und kryptischen Fehlern. Im Zweifel den Namen des richtigen Pakets ergänzen, und zwar zum Beispiel so: `dplyr::filter(...)`.

## 

## ----eval = FALSE-------------------------
## filter(profiles, body_type %in% c("a little extra", "average"))


## ----eval = FALSE-------------------------
## filter(profiles, str_detect(pets, "cats"))


## ----eval = FALSE-------------------------
## profiles_keine_nas <- drop_na(profiles)


## ----eval = FALSE-------------------------
## profiles_keine_nas2 <- drop_na(profiles, income, sex)
## filter(profiles_keine_nas2)


## 1. `filter()` filtert Spalten.

## 1. `filter()` ist eine Funktion aus dem Paket `dplyr`.

## 1. `filter()` erwartet als ersten Parameter das Filterkriterium.

## 1. `filter()` lässt nur ein Filterkriterium zu.

## 1. Möchte man aus dem Datensatz `profiles` (`okcupiddata`) die Frauen filtern, so ist folgende Syntax korrekt: `filter(profiles, sex == "f")`.

## 1. `filter(profiles, between(age, 35, 60))` filtert die mittelalten Frauen (zwischen 35 und 60)

## 

## ----fig-select, echo = FALSE, fig.cap = "Spalten auswählen", out.width = "50%"----
knitr::include_graphics("images/Datenjudo/select-crop.pdf")


## Merke: Die Funktion `select()` wählt Spalten aus einem Dataframe aus.

## 

## ----eval = FALSE-------------------------
## select(stats_test, score)  # Spalte "score" auswählen
## select(stats_test, score, study_time)
## # Spalten "score" und "study_time" auswählen
## 
## select(stats_test, score:study_time) # dito
## select(stats_test, 5:6)  # Spalten 5 bis 6 auswählen


## ----select-one-of, eval = FALSE----------
## vars <- c("score", "study_time")
## select(stats_test, one_of(vars))


## 1. `select()` wählt *Zeilen* aus.

## 1. `select()` ist eine Funktion aus dem Paket `knitr`.

## 1. Möchte man zwei Spalten auswählen, so ist folgende Syntax prinzipiell korrekt: `select(df, spalte1, spalte2)`.

## 1. Möchte man Spalten 1 bis 10 auswählen, so ist folgende Syntax prinzipiell korrekt: `select(df, spalte1:spalte10)`.

## 1. Mit `select()` können Spalten nur bei ihrem Namen, aber nicht bei ihrer Nummer aufgerufen werden.

## 

## ----arrange-demo, eval = FALSE-----------
## 
## arrange(stats_test, score) # *schlechteste* Noten zuerst
## arrange(stats_test, -score) # *beste* Noten zuerst
## arrange(stats_test, interest, score)  # zwei Sortierkriterien
## 


## ----arrange-demo2, echo = FALSE----------
stats_test %>%
  select(-c(row_number, date_time)) %>%
  arrange(-score) %>%
  head(2)
arrange(stats_test, score)  %>% head(2) # liefert die *schlechtesten* Noten zurück




## Merke: Die Funktion `arrange()` sortiert die Zeilen eines Dataframes.

## 

## ----fig-arrange, echo = FALSE, fig.cap = "Spalten sortieren"----
knitr::include_graphics("images/Datenjudo/arrange-crop.pdf")


## 1. `arrange()` arrangiert Spalten.

## 1. `arrange()` sortiert im Standard absteigend.

## 1. `arrange()` lässt nur ein Sortierkriterium zu.

## 1. `arrange()` kann numerische Werte, aber nicht Zeichenketten sortieren.

## 1. `top_n(5)` liefert immer fünf Werte zurück.

## 

## Gruppieren bedeutet, einen Datensatz anhand einer diskreten Variablen (z.B. Geschlecht) so aufzuteilen, dass Teil-Datensätze entstehen -- pro Gruppe ein Teil-Datensatz (z.B. ein Datensatz, in dem nur Männer enthalten sind, und einer, in dem nur Frauen enthalten sind).

## 

## ----fig-groupby, echo = FALSE, fig.cap = "Datensätze nach Subgruppen aufteilen"----
knitr::include_graphics("images/Datenjudo/group_by-crop.pdf")


## ----demo-groupby-no-eval, eval = FALSE----
## test_gruppiert <- group_by(stats_test, interest)
## test_gruppiert


## ----demo-groupby, eval = TRUE------------
test_gruppiert <- group_by(stats_test, interest)
select(test_gruppiert, study_time, interest, score) %>% head(4)


## ----sac, echo = FALSE, fig.cap = "Schematische Darstellung 'Gruppieren -- Zusammenfassen -- Kombinieren'"----

knitr::include_graphics("images/Datenjudo/sac-crop.pdf")



## 1. Mit `group_by()` gruppiert man einen Datensatz.

## 1. `group_by()` lässt nur ein Gruppierungskriterium zu.

## 1. Die Gruppierung durch `group_by()` wird nur von Funktionen aus `dplyr` erkannt.

## 1. `group_by()` kann sinnvoll mit `summarise()` kombiniert werden.

## 
## 

## Merke: Mit `group_by()` teilt man einen Datensatz in Gruppen ein, entsprechend den Werten einer oder mehrerer Spalten.

## 

## ----fig-summarise, echo = FALSE, fig.cap = "Spalten zu einer Zahl zusammenfassen", out.width = "50%"----
knitr::include_graphics("images/Datenjudo/summarise-crop.pdf")


## -----------------------------------------
summarise(stats_test, mean(score))


## -----------------------------------------
test_gruppiert <- group_by(stats_test, interest)
summarise(test_gruppiert, mean(score, na.rm = TRUE))


## -----------------------------------------
test_gruppiert <- group_by(stats_test, interest)
summarise(test_gruppiert, mw_pro_gruppe = mean(score, na.rm = TRUE))


## Merke: Mit `summarise()` kann man eine Spalte eines Dataframes zu einem Wert zusammenfassen.

## 

## -----------------------------------------
summarise(stats_test, n())
summarise(test_gruppiert, n())
nrow(stats_test)


## -----------------------------------------
dplyr::count(stats_test, interest)


## ----eval = FALSE-------------------------
## dplyr::count(stats_test, study_time, sort = TRUE)
## dplyr::count(stats_test, interest, study_time)


## ----fig-count, echo = FALSE, fig.cap = "Sinnbild für count()", out.width = "50%"----

knitr::include_graphics("images/Datenjudo/count-crop.pdf")


## Merke: n und count zählen die Anzahl der Zeilen, d.h. die Anzahl der Fälle.

## 

## 1. Mit `count()` kann man Zeilen zählen.

## 1. `count()` ist ähnlich (oder identisch) wie eine Kombination von `group_by()` und `n()`.

## 1. Mit `count(()` kann man nur eine Gruppe beim Zählen berücksichtigen.

## 1. `count)` darf nicht bei nominalskalierten Variablen verwendet werden.

## 1. `n()` nimmt einen Vektor als Eingabe und liefert einen Skalar zurück.

## 

## ----cecie-une-pipe, echo = FALSE, out.width = "50%", fig.cap = "Das ist keine Pfeife"----
knitr::include_graphics("images/Datenjudo/800px-Pipa_savinelli.jpg")


## ----fig-durchpfeifen, echo = FALSE, out.width  = "80%", fig.cap = 'Das "Durchpeifen"'----
knitr::include_graphics("images/Datenjudo/durchpfeifen-crop.pdf")


## Die sog. "Pfeife" (pipe\index{Pfeife}: `%>%`) in Anspielung an das berühmte Bild von René Magritte verkettet Befehle hintereinander. Das ist praktisch, da es die Syntax vereinfacht. Tipp: In RStudio gibt es einen Shortcut für die Pfeife: Strg-Shift-M (auf allen Betriebssystemen).

## 

## ----eval = FALSE-------------------------
## filter(summarise(group_by(filter(stats_test,  # oh je
##        !is.na(score)), interest), mw = mean(score)), mw > 30)


## -----------------------------------------
stats_test %>%   # oh ja
  filter(!is.na(score)) %>%
  group_by(interest) %>%
  summarise(mw = mean(score)) %>%
  filter(mw > 30)


## Nimm die Tabelle "stats_test" UND DANN

## filtere alle nicht-fehlenden Werte UND DANN

## gruppiere die verbleibenden Werte nach "interest" UND DANN

## bilde den Mittelwert (pro Gruppe) für "score" UND DANN

## liefere nur die Werte größer als 30 zurück.

## 

## 
## Wenn Sie mit der Pfeife Befehle verketten, sind nur Befehle erlaubt, die einen Datensatz als Eingabe verlangen und einen Datensatz ausgeben. Nur die letzte Funktion einer Pfeifenkette muss keinen Dataframe zurückliefern, da die Pfeife beendet ist. In der Praxis wird häufig als letztes Glied einer Pfeifenkette eine Funktion gesetzt, die ein Diagramm ausgibt (s. Kapitel \@ref(datavis)). So eine "Plotfunktion" liefert in der Regel keinen Dataframe zurück; wenn sie das letzte Glied der Pfeifenkette ist, ist das aber egal. Pfeifen-Aficionados finden im Paket `magrittr`, aus dem die Pfeife stammt, weitere Pfeifen-Varianten.

## 

## ----eval = FALSE-------------------------
## df %>%
##   mutate(neue_spalte = spalte1 + spalte2)


## Nimm die Tabelle "df" UND DANN

## bilde eine neue Spalte mit dem Namen `neue_spalte`,

## die sich berechnet als Summe von `spalte1` und `spalte2`.

## 

## -----------------------------------------
stats_test %>%
  select(bestanden, interest, score) %>%
  mutate(Streber = score > 38) %>%
  head()


## ----fig-mutate, echo = FALSE, fig.cap = "Sinnbild für mutate", out.width = "50%"----
knitr::include_graphics("images/Datenjudo/mutate-crop.pdf")


## Betrachten Sie das Sinnbild von `mutate()`: Die Idee ist, eine Spalte umzuwandeln nach dem Motto: "Nimm eine Spalte, mach was damit und liefere die neue Spalte zurück." Die Spalte (und damit jeder einzelne Wert in der Spalte) wird *verändert* ('mutiert', daher `mutate()`). Man kann auch sagen, die Spalte wird *transformiert*.

## 

## ----ungetuem, eval = FALSE---------------
## bestanden_gruppen <-
##   filter(
##     summarise(
##       group_by(filter(select(stats_test,
##                              -c(row_number, date_time)),
##                       bestanden == "ja"), interest),
##       Punkte = mean(score), n = n()))


## ----ungetuem-easy, eval = FALSE----------
## stats_test %>%
##   select(-row_number, -date_time) %>%
##   filter(bestanden == "ja") %>%
##   group_by(interest) %>%
##   summarise(Punkte = mean(score),
##             n = n())


## Nimm den Datensatz `stats_test` UND DANN

## Wähle daraus die Spalte `score` UND DANN

## Berechne den Mittelwert der Spalte UND DANN

## ziehe vom Mittelwert die Spalte ab UND DANN

## quadriere die einzelnen Differenzen UND DANN

## bilde davon den Mittelwert.

## 

## ----eval = FALSE-------------------------
## stats_test %>%
##   select(score) %>%
##   mutate(score_delta = score - mean(.$score)) %>%
##   mutate(score_delta_squared = score_delta^2) %>%
##   summarise(score_var = mean(score_delta_squared)) %>%
##   summarise(sqrt(score_var))


## ----suffix-if----------------------------
stats_test %>%
  summarise_if(is.numeric, mean, na.rm = TRUE)


## Nimm die Tabelle "stats_test" UND DANN

## fasse eine jede Spalte zusammen WENN

## die Spalte numerisch ist ACH JA

## fasse diese Spalten mithilfe des Mittelwerts zusammen.

## 

## ----suffix-all---------------------------
stats_test %>%
  summarise_all(funs(is.na(.) %>% sum))


## Nimm die Tabelle "stats_test" UND DANN

## fasse jede Spalte zusammen und zwar

## mit der Funktion 'funs' und zwar

## mit dem Ergebnis dieser zwei Schritte: (Finde alle NAs und summiere sie).

## 

## -----------------------------------------
stats_test %>%
  select(-date_time) %>%
  summarise_at(.vars = vars(study_time, self_eval),
               .funs = max,
               na.rm = TRUE) %>%
  head(3)


## -----------------------------------------
stats_test %>%
  drop_na() %>%
  mutate_at(.vars = vars(study_time, self_eval, interest),
            .funs = funs(prop =  ./max(.))) %>%
  select(contains("_prop"))


## Der Punkt `.` kann hier übersetzt werden mit "alle gewählten Spalten von `stats_test`". Generell bezeichnet der Punkt den Datensatz, so wie er aus dem letzten Pfeifenschritt hervorging.

## 

## -----------------------------------------
flights %>%
  select(carrier) %>%
  head(3)


## -----------------------------------------
head(airlines, 3)


## -----------------------------------------
flights %>%
  inner_join(airlines, by = "carrier") -> flights_joined

head(flights_joined$name)


## Bei einem *inner join* werden nur Zeilen behalten, die in beiden Tabellen vorkommen. Die resultierende Tabelle ist also nie länger, höchstens kürzer als eine oder beide Ausgangstabellen.

## 

## ----join-start, echo = FALSE, fig.cap = "Diese zwei Tabellen sollen zusammengeführt werden", out.width = "50%"----
knitr::include_graphics("images/Datenjudo/join_start-crop.pdf")



## ----join-types, echo = FALSE, fig.cap = "Einige Arten von Joins"----
knitr::include_graphics("images/Datenjudo/join_types-crop.pdf")



## 2. Möchte man mit `dplyr`  eine Spalte zu einem Wert zusammenfassen, so nutzt man die Funktion `mutate()`.

## 3. Die Befehlskette "Datensatz nehmen -> Spalten auswählen -> Filtern -> Gruppieren -> Mittelwert berechnen" könnte man mit `dplyr` mit Pseudo-Syntax so darstellen: `df %>% select(col1, col2)

##  %>% filter(cond == TRUE) %>% group_by(my_groups) %>% summarise(mean(score))`.

## 4. Mit der Funktion `na.rm(df)` kann man alle Zeilen aus `df` entfernen, die fehlende Werte aufweisen.

## 5. Möchte man Tabellen "nebeneinander kleben", also die gemeinsamen Zeilen zweier Tabellen zusammenführen, so bietet sich der Befehl `join` (in einigen Varianten) an. Mit `left_join()` werden nur Zeilen behalten, die im *zweiten* Datensatz liegen (sofern sie eine Entsprechung im anderen Datensatz haben).

## 6. Möchte man mehrere Spalten zu ihrem Mittelwert zusammenfassen, so ist dieser Befehl hilfreich: `summarise_at()`.

## 7. Möchte man von allen numerischen Variablen den Mittelwert wissen, so ist dies in dplyr mit `summarise_if(is.numeric, mean, na.rm = TRUE)` umsetzbar.

## 8. Der Befehl `tidyverse_packages(include_self = TRUE)` liefert eine Aufstellung aller Pakete des Tidyverse.

## 
