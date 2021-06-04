## ----libs-hallo-silent, echo = FALSE------
library(tidyverse)
library(gridExtra)
library(magrittr)


## Lernziele:

## 
## - Die Geschichte von R in kurzer Form kennen

## - Vor- und Nachteile der Verwendung von R unterscheiden

## - Vor- und Nachteile von Skriptsprachen wie R einem "Klickprogramm" wie Excel gegenüberstellen können

## 

## ----load-pckg-data, echo = FALSE---------
pkg_data <- readRDS("data/r_pckgs.RDS")
n_pcgs <- sum(pkg_data$New_Pkg, na.rm = TRUE)



## ----caret-lookup, echo = FALSE-----------
caret::modelLookup() %>% pull(model) %>% unique %>% length -> n_mod_caret


## ----p-tenthousand, echo = FALSE, fig.cap = "Anzahl (neuer) R-Pakete auf CRAN", out.width = "100%", fig.asp = 0.5----


p1 <- ggplot(pkg_data, aes(x=Month, y=cum_results)) +
  geom_line() +
  labs(x = "Zeit",
       y = "Anzahl R-Pakete") +
  geom_hline(yintercept = 10000, color = "grey40", linetype = "dashed") +
  scale_y_continuous(breaks = c(2500, 5000, 7500, 10000)) +
  theme(axis.text = element_text(size = rel(0.5)))


p2 <- ggplot(pkg_data, aes(x=Month, y=New_Pkg)) +
  geom_line() +
  geom_smooth(se = FALSE) +
  labs(x = "Zeit",
       y = "Anzahl neuer R-Pakete") +
  theme(axis.text = element_text(size = rel(0.5)))



p3 <- ggplot(pkg_data, aes(x=Month, y=New_Pkg)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_log10(breaks = c(.1, 1, 10, 100)) +
  labs(x = "Zeit",
       y = "Anzahl neuer R-Pakete") +
  coord_cartesian(ylim = c(10, 200))  +
  theme(axis.text = element_text(size = rel(0.5)))

grid.arrange(p1, p2, p3, nrow = 1)



## ----p-elegant, echo = FALSE, fig.cap = "Beispiele für Datenvisualisierung mit R", out.width = "100%"----

# imgs <- c(
#   "images/hallo/circlize.png",
#   "images/hallo/nuts.png"
# )
# prada::comb2pngs(imgs)

knitr::include_graphics("images/hallo/p-elegant.png")



## ----p-downside-r, echo = FALSE, fig.cap = "Schwierigkeiten mit R"----

efun <- function(x) {2.2^x}
my_log <- function(x) {log(x) * 30 + 5}

ggplot(data = data_frame(x = c(1, 6)), aes(x = x)) +
  stat_function(fun = efun, color = "red") +
  stat_function(fun = my_log, color = "blue") +
  labs(x = "Komplexität des Problems",
       y = "Aufwand") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  annotate(geom = "text", x = 4, y = 5, label = "Excel", color = "red", size = 10) +
  annotate(geom = "text", x = 2, y = 45, label = "R", color = "blue", size = 10) -> p1


p1




## 1. R ist ein Dialekt der Programmiersprache Q.

## 2. R wurde zu Beginn kommerziell vertrieben.

## 3. R ist für Windows, Mac OS und Linux verfügbar.

## 4. Alternativen zu R sind Python, Julia und Excel.

## 5. Es gibt ca. 1000 R-Pakete auf CRAN.

## 6. Excel vermengt Syntax und Daten; R nicht.

## 7. Trennung von Syntax und Daten verbessert (potenziell) die Reproduzierbarkeit einer Analyse.

## 8. Etwas zugespitzt könnte man formulieren, dass in R komplexe Sachen einfach, einfache Aufgaben aber komplex sind.

## 9. CRAN ist ein Ort bzw. eine Webseite, auf der man R kaufen kann.

## 10. Python ist eine Programmiersprache, die für die Datenanalyse nicht geeignet ist.

## 
