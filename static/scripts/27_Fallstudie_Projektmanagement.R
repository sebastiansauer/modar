## ----fig-datenjudo-proc, echo = FALSE-----
knitr::include_graphics("images/Rahmen/Rahmen-crop.pdf")


## Lernziele:

## 
## - Eine Definition von Populismus berichten und diskutieren können

## - Die Eckpunkte eines größeren Datenanalyse-Projekts kennen

## - Grundlegende Methoden des Projektmanagements größerer Datenanalyse-Projekte kennen

## - Grundlagen der Versionierung mit Git erläutern können

## 

## ----libs-polit-twitter-silent, echo = FALSE----
library(huxtable)
library(tidyverse)


## ----sentiws, echo = FALSE----------------
library(sentiws, package = "pradadata")

sentiws %>%
  select(-4) %>%
  group_by(neg_pos) %>%
  top_n(3, wt = abs(value)) %>%
  knitr::kable(caption = "Ausschnitt aus dem SentiWS-Emotionslexikon",
booktabs = TRUE)


## ----accounts-anz, echo = FALSE, fig.cap = "Anzahl von Accounts und Tweets pro Tag", out.width = "100%"----
# imgs <- c("images/Twitter/p_accounts_per_party.pdf",
#           "images/Twitter/p_tweets_day_party_md.pdf")‹

knitr::include_graphics("images/Twitter/accounts-tweets-crop.pdf")


## ----polit-tweets-daten-aggregation-dt, echo = FALSE, fig.cap = "Berechnung der Populismuswerte", out.width = "100%", cache = FALSE----
knitr::include_graphics("images/Twitter/polit_tweets_daten_aggregation_dt-crop.pdf")



## ----pop-sum, echo = FALSE, fig.cap = "Populismuswerte der Parteien", out.width = "100%"----
knitr::include_graphics("images/Twitter/pop-sum.png")


## ----polit-indicators, echo = FALSE, fig.cap = "Populismus-Indikatoren deutscher Parteien", out.width = "100%"----
knitr::include_graphics("images/Twitter/p_party_pop_scores_details_no_trump_point_limited_grey.pdf")


## ----p-party-pop-scores-details-no-trump-point-limited-grey, eval = FALSE, out.height="100%"----
## polits_df_long %>%
##   ggplot +
##   aes(x = Indikator, y = z_Wert,) +
##   geom_point(color = "grey80", position = "jitter") +
##   facet_wrap(~party, nrow = 1) +
##   scale_color_manual(values = party_pal) +
##   scale_x_discrete(labels = pop_vars_de) +
##   scale_y_continuous(limits = c(-1.5,1.5), breaks = c(-1,0,1)) +
##   theme(legend.position = "none") +
##   coord_flip() +
##   labs(caption =
##        "dargestellt sind Mediane über alle Tweets aller Politiker",
##        y = "*z*-Wert",
##        color = "Partei") +
##   geom_point(data = party_pop_scores_md_long, aes(color = party), size = 4) +
##   geom_line(data = party_pop_scores_md_long, aes(color = party), group = 1)


## ----eval = FALSE-------------------------
## # Einige Erläuterung und Hinweise finden sich in Readme.md
## load_data(compute_all = TRUE)
## prepare_data()
## train_models(all = TRUE)
## test_models(all = TRUE)
## plot_figs(format = "PDF")
## print_tables(format = "LateX")


## ----eval = FALSE-------------------------
## prepare_data <- function(input_df, save_to_disk = FALSE){
##   # komplizierte Syntax hier
##   if (save_to_disk = TRUE) save(ausgabe_objekt,
##                                 file = "data/data_prepared.Rda")
##   return(ausgabe_objekt)
## }


## ----eval = FALSE-------------------------
## train_models(input_df, read_from_disk = FALSE, save_to_disk = FALSE){
##   if (read_from_disk = TRUE) read("data/data_prepared.Rmd")
##   # berechne wilde Modelle
##   return(models)
## }


## ----uebergabe-objekte, echo = FALSE, fig.cap = "Übergabe von Datenobjekten zwischen Funktionen und zwischen Dateien", cache = FALSE----
knitr::include_graphics("images/Twitter/uebergabe_objekte-crop.pdf")



## 1. Der Grad der Emotionalität eines Textes wird in der Studie dieses Kapitels als ein Indikator für Populismus definiert.

## 1. Poppers Buch "Logik der Forschung" skizziert eine Theorie der Urhorde, auf die sich die hier vorliegende Konzeption von Populismus bezieht.

## 3. Für jeden Populismus-Indikator wurde pro Person der Mittelwert berechnet.

## 4. Um die Indikatoren vergleichbar zu machen, wurden sie auf den Bereich von null bis eins standardisiert.

## 5. Ein Projektverzeichnis für Datenanalyse sollte einen Ordner enthalten, der sowohl Rohdaten als auch erzeugte Datenobjekte enthält.

## 6. Ein *Make-File*  ist eine Datei, die eine Abfolge von Befehlen ausführt, oft, um ein (Analyse-)Projekt in Gänze auszuführen.

## 7. Pfade sollten (in R) nie relativ, sondern stets absolut angegeben werden.

## 8. Das Nachverfolgen von Änderungen mit der Software Git basiert primär auf Textdateien.

## 9. RStudio bietet eine Integration zentraler Git-Befehle.

## 10. Ein Emotionslexikon, im Sinne der vorliegenden Studie, umfasst eine Anzahl von Wörtern, denen jeweils ein oder mehrere Emotionswerte zugeordnet werden.

## 
