## ----step-rahmen, echo = FALSE, fig.cap = "Der Rahmen als Bestandteil der Datenanalyse"----
stopifnot(file.exists("images/Rahmen/Rahmen-crop.png"))
knitr::include_graphics("images/Rahmen/Rahmen-crop.png")


## Lernziele:

## 
## - Wissen, was Statistik ist, bzw. einige Aspekte einer Definition kennen

## - Statistik zu Data Science und anderen verwandten Begriffen abgrenzen können

## - Grundkonzepte wie Daten, Variable und Beobachtung definieren können

## - Die Begriffe *Signal* und *Rauschen* in Verbindung bringen können

## - Die Wissensgebiete der Datenanalyse aufzählen und erläutern können

## 

## ----p-desk-vs-inf, echo = FALSE, fig.cap  ="Sinnbild für die Deskriptiv- und die Inferenzstatistik"----
knitr::include_graphics("images/Rahmen/desk_vs_inf-crop.png")


## Aufgabe der deskriptiven Statistik ist es primär, Daten prägnant

## zusammenzufassen. Aufgabe der Inferenzstatistik ist es, zu prüfen, ob Daten

## einer Stichprobe auf eine Grundgesamtheit verallgemeinert werden können.

## 

## ----signal-noise, echo = FALSE, fig.cap = "Muster und Rauschen", out.width = "100%"----
data(wo_men, package = "pradadata")
library(tidyverse)

wo_men %>%
  drop_na(sex, height) %>%
  filter(height > 150, height < 220) -> df


p1 <- wo_men %>%
  drop_na(sex, height) %>%
  filter(height > 150, height < 220) %>%
  ggplot(aes(x = sex, y = height)) +
  geom_jitter(width = .2, alpha = .7) +
  ylim(150, 220) +
  theme_gray() -> p1


df %>%
  ggplot(aes(x = sex, y = height)) +
  stat_summary(fun.y = "mean", geom = "point", size = 10,
               color = "red", alpha = .5, shape = 15) +
  stat_summary(fun.y = "mean", geom = "line", group = 1,
               color = "red", alpha = .5) +
  geom_jitter(width = .2, alpha = .7) +
  ylim(150, 220) +
  theme_gray() -> p2


gridExtra::grid.arrange(p1, p2, nrow = 1)




## ----signal-noise2, echo = FALSE, out.width = "100%", fig.cap = "Das Ziehen von Stichproben  birgt Zufall"----

# sample
set.seed(123456)
sample_m <- sample(df$row_number[df$sex == "man"], size = 1)
sample_f <- sample(df$row_number[df$sex == "woman"], size = 1)

df %>%
  filter(row_number %in% c(sample_m, sample_f)) %>%
  ggplot(aes(x = sex, y = height)) +
  stat_summary(fun.y = "mean", geom = "point", size = 10,
               color = "red", alpha = .5, shape = 15) +
  stat_summary(fun.y = "mean", geom = "line", group = 1,
               color = "red", alpha = .5) +
  geom_point(alpha = .7) +
  ylim(150, 220) +
  theme_gray() -> p3


set.seed(123456)
sample_m2 <- sample(df$row_number[df$sex == "man"], size = 5)
sample_f2 <- sample(df$row_number[df$sex == "woman"], size = 5)

df %>%
  filter(row_number %in% c(sample_m2, sample_f2)) %>%
  ggplot(aes(x = sex, y = height)) +
  stat_summary(fun.y = "mean", geom = "point", size = 10,
               color = "red", alpha = .5, shape = 15) +
  stat_summary(fun.y = "mean", geom = "line", group = 1,
               color = "red", alpha = .5) +
  geom_jitter(width = .2, alpha = .7) +
  ylim(150, 220) +
  theme_gray() -> p4


gridExtra::grid.arrange(p3, p4, nrow = 1)



## 1. Eine gängige Unterteilung der Statistik erfolgt in die drei Bereiche deskriptiv, inferierend und explikativ.

## 2. Aufgabe der Inferenzstatistik ist es, Daten prägnant zusammenzufassen.

## 1. Zu den Wissensgebieten der Datenanalyse zählen Wissen um Wahrscheinlichkeit, Kausalität, stochastische Modelle, Normalverteilung.

## 1. Den Begriff *Daten* kann man definieren als *Information ohne Kontext*.

## 1. Wesentliches Merkmal von Tabellen, wie in diesem Text verstanden, ist eine Organisation aus Zeilen und Spalten; eine rechteckige Struktur ist nicht nötig.

## 1. Unter einer Beobachtungseinheit versteht man den Gegenstand, der in den Zeilen einer Tabelle auftauchen soll.

## 1. Wissenschaft kann man als zweistufigen Prozess verstehen: Signale erkennen und Daten erklären.

## 1. Wissenschaftliche Theorien beziehen sich auf Daten, nicht auf Phänomene.

## 1. Experimentieren ist die Kunst, Rauschen vor der Messung zu verringern.

## 1. Kleinere Stichproben bergen mehr Raum für Zufall als große.

## 
