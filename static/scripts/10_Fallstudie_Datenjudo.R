## Lernziele:

## 
## - Daten sicher aufbereiten können

## - Deskriptive Statistiken nach Bedarf berechnen können

## - Typische Fragestellungen von einfachen Situationen der angewandten Datenanalyse kennen

## - Grundlegende Funktionen von `dplyr` anwenden können

## - Das Konzept der Pfeife in einem echten Datensatz anwenden können

## - Auch mit relativ großen Daten sicher hantieren können

## 

## ----libs-flights-------------------------
library(tidyverse)
library(viridis)
library(GGally)
library(corrr)

data(flights, package = "nycflights13")


## -----------------------------------------
flights %>%
  arrange(arr_delay)


## -----------------------------------------
flights %>%
  arrange(arr_delay) %>%
  select(arr_delay, carrier, month, day, dep_time, flight, dest)


## ----max-arr-delay-stats------------------
flights %>%
  arrange(-arr_delay) %>%
  select(arr_delay, carrier, month, day, dep_time, tailnum, flight, dest) %>%
  filter(row_number() < 11)


## ----max-arr-delay-stats-per-carrier------
flights %>%
  arrange(-arr_delay) %>%
  select(arr_delay, carrier, month, day, dep_time, tailnum, flight, dest) %>%
  group_by(carrier) %>%
  filter(row_number() < 4)


## ----max-arr-delay-stats-per-carrier2-----
flights %>%
  arrange(-arr_delay) %>%
  select(arr_delay, carrier, month, day, dep_time, tailnum, flight, dest) %>%
  group_by(carrier) %>%
  filter(row_number() < 4) %>%
  arrange(carrier)


## ----mean-arr-delay-----------------------
flights %>%
  select(arr_delay, carrier, month, day, dep_time, tailnum, flight, dest) %>%
  group_by(carrier) %>%
  summarise(delay_mean = mean(arr_delay, na.rm = TRUE)) %>%
  arrange(-delay_mean) %>%
  head


## ----cor-overall--------------------------
cor(flights$arr_delay, flights$distance, use = "complete.obs")


## ----warnings = FALSE---------------------
flights %>%
  group_by(carrier) %>%
  summarise(cor_delay_dist = cor(arr_delay, distance, use = "complete.obs")) %>%
  filter(abs(cor_delay_dist) > .2)


## -----------------------------------------
flights %>%
  select(-year) %>%
  select_if(is.numeric) %>%
  correlate() %>%
  focus(arr_delay) %>%
  filter(abs(arr_delay) > .2)


## ----max-arr-delay-stats-bar-plot-no-eval, eval = FALSE----
## flights %>%
##   arrange(-arr_delay) %>%
##   select(arr_delay, carrier, month, day, dep_time, tailnum, flight, dest) %>%
##   filter(between(row_number(),1, 10)) %>%
##   ggplot() +
##   aes(x = reorder(tailnum, -arr_delay), y = arr_delay, fill = arr_delay) +
##   geom_col() +
##   scale_fill_viridis(direction = -1)


## Nimm den Datensatz `flights` UND DANN

## sortiere absteigend nach der Spalte `arr_delay` UND DANN

## wähle die mir genehmen Spalten aus UND DANN

## filtere die Zeilen zwischen (`between`) 1 und 10 UND DANN

## male ein Bild mit ggplot UND DANN

## definiere die Achsen und Füllfarbe (die `aes`thetik) wobei

## die Flugzeuge (`tailnum`) nach `arr_delay` sortiert sein sollen UND DANN

## male das Balken-Geom UND DANN

## fülle die Balken mit dem Farbschema des Pakets `viridis` wobei

##   die Farben in umgekehrter Reihenfolge gezeigt werden sollen. Fertig.

## 

## ----max-arr-delay-stats-bar-plot, eval = TRUE, echo = FALSE, fig.cap = "Verspätung von Flügen", out.width = "100%", cache = FALSE----
library(tidyverse)
library(viridis)

p_delay1 <- flights %>%
  arrange(-arr_delay) %>%
  select(arr_delay, carrier, month, day, dep_time, tailnum, flight, dest) %>%
  filter(between(row_number(),1, 10)) %>%
  ggplot() +
  aes(x = reorder(tailnum, -arr_delay), y = arr_delay, fill = arr_delay) +
  geom_col() +
  coord_flip() +
  scale_fill_viridis(direction = -1) +
  labs(x = "Flugzeug",
       y = "Verspätung",
       title = "Top-10 der verspäteten Flugzeuge")  +
  theme(title = element_text(size = rel(0.5)),
        axis.text = element_text(size = rel(0.5)))

p_delay2 <- flights %>%
  select(arr_delay, carrier, month, day, dep_time, tailnum, flight, dest) %>%
  group_by(carrier) %>%
  summarise(delay_mean = mean(arr_delay, na.rm = TRUE)) %>%
  arrange(-delay_mean) %>%
  filter(between(row_number(),1, 10)) %>%
  ggplot() +
  aes(x = reorder(carrier, -delay_mean), y = delay_mean) +
  geom_point() +
  labs(x = "Fluggesellschaft",
       y = "Verspätung",
       fill = "Verspätung",
       title = "Top-10 der Verspätungs-Airlines")   +
  theme(title = element_text(size = rel(0.5)),
        axis.text = element_text(size = rel(0.5)))

gridExtra::grid.arrange(p_delay1, p_delay2, nrow = 1)


## -----------------------------------------
flights %>%
  select(arr_delay, carrier, month, day, dep_time, tailnum, flight, dest) %>%
  group_by(carrier) %>%
  summarise(delay_md = median(arr_delay, na.rm = TRUE),
            delay_mean = mean(arr_delay, na.rm = TRUE),
            delay_sd = sd(arr_delay, na.rm = TRUE),
            delay_iqr_lower = quantile(arr_delay, na.rm = TRUE, probs = .25),
            delay_iqr_upper = quantile(arr_delay, na.rm = TRUE, probs = .75),
            delay_count = n()) %>%
  ungroup() -> flights_summary


## ----mean-arr-delay-plot2-no-eval, eval = FALSE----
## flights_summary %>%
##   ggplot() +
##   aes(x = reorder(carrier, -delay_mean), y = delay_mean) +
##   geom_point() +
##   labs(title = "Verspätung nach Fluggesellschaft",
##        x = "Fluggesellschaft",
##        y = "Verspätung") +
##   coord_flip() +
##   theme(title = element_text(size = rel(0.7))) -> p_delay2


## ----mean-arr-delay-plot3-----------------
p_delay3 <- flights_summary %>%
  ggplot() +
  aes(x = reorder(carrier, -delay_md),
      y = delay_md) +
  geom_point(aes(size = delay_count,
                 color = delay_count)) +
  geom_errorbar(aes(ymin = delay_iqr_lower,
                    ymax = delay_iqr_upper),
                color = "grey60") +
  labs(title = "Verspätung nach Fluggesellschaft",
       x = "Fluggesellschaft",
       y = "Median der Verspätung",
       caption = paste0("Größe und Farbe der Punkte",
                        "spiegeln die Anzahl der Flüge wider")) +
  theme(title = element_text(size = rel(0.7)))


## ----ggpairs-flights, fig.cap = "Streudiagramm-Matrix", fig.asp = .7, out.width="100%"----
flights %>%
  select(arr_delay, air_time, distance, dep_time, carrier) %>%
  filter(carrier %in% c("F9", "AS")) %>%
  ggpairs(aes(color = carrier, fill = carrier))


## -----------------------------------------
praise::praise()


## 1. Möchte man einen Dataframe sortieren, so kann man anstelle von `arrange()` auch `count(..., sort = TRUE)` verwenden.

## 2. Möchte man eine Spalte mit der Häufigkeit einer Gruppe `group` an einen Dataframe anhängen, so kann man den Befehl `add_count(group)` verwenden, wie die Hilfeseite von `count()` zeigt.

## 3. Möchte man mehrere Spalten mit `select()` auswählen und stehen die Spalten im Dataframe nebeneinander, so kann man praktischerweise bei select schreiben: `select(df, spalte1:spalte10)`.

## 4. Der Befehl `filter(row_number() < 4)` hat den gleichen Effekt, unabhängig davon, ob zuvor eine Gruppierung (mit `group_by()`) durchgeführt wurde.

## 5. Möchte man nur Flüge der deutschen Lufthansa filtern, so kann man diese Syntax verwenden: `filter(flights, str_detect(carrier, "LHH"))`.

## 6. Die Lufthasa startete laut unserem Datensatz im Jahr 2013 mehr als einmal aus New York.

## 7. Möchte man `cor()` nahebringen, dass fehlende Werte ignoriert werden sollen, so ist das Argument `rm.na = TRUE` zu verwenden.

## 8. Möchte man mit `ggplot` ein Balkendiagramm nach absteigenden Balken sortieren, so ist dieser Befehl korrekt: `aes(x = reorder(tailnum, -arr_delay))`. Dabei steht das Nummernschild auf der X-Achse; die Balken werden absteigend nach Verpätung sortiert.

## 9. Der Befehl `summarise()` erlaubt nicht nur Zusammenfassungen zu einem einzelnen Wert, sondern es dürfen auch längere Vektoren zurückgegeben werden.

## 10. Berechnet man die Korrelation von Flugdauer und Zieldistanz, so würde die Korrelation sinken, wenn man nur Flüge mit ähnlicher Flugdauer berücksichtigte (die Varianz der Flugdauer also deutlich eingeschränkt wäre).

## 
## 
