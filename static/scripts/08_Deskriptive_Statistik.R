## Lernziele:

## 
## - Die grundlegende Idee der deskriptiven Statistik erläutern können

## - Die beiden zentralen Arten von Kennwerten der deskriptiven Statistik kennen

## - Typische Lage- und Streuungsmaße benennen und erläutern können

## - Deskriptive Statistiken mit `mosaic` und `dplyr` berechnen können

## 

## ----libs-desk----------------------------
library(mosaic)
library(tidyverse)
library(skimr)
library(lsr)
library(corrr)
library(GGally)
library(sjmisc)

data(stats_test, package = "pradadata")


## ----libs-desk-hidden, echo = FALSE-------
library(mvtnorm)


## ----skalenniveaus, echo = FALSE----------

d <- read_csv("includes/skalenniveaus.csv")

knitr::kable(d, caption = "Typische Skalenniveaus", booktabs = TRUE)


## Aufgabe der deskriptiven Statistik ist es primär, Daten prägnant

## zusammenzufassen. Die univariate deskriptive Statistik hat zwei Hauptbereiche: Lagemaße und Streuungsmaße.

## 

## ----delta-plot, echo = FALSE, fig.cap = "Abweichungsbalken und Abweichungsquadrate", fig.asp= .5, out.width="100%"----
library(magrittr)
d <- read_csv("includes/noten.csv")


d %<>%
  mutate(delta_abs = abs(delta),
         pos = ifelse(delta > 0, "positiv", "negativ"),
         delta_sq = delta^2)

d %>%
  ggplot(aes(x = id, y = note)) +
  geom_hline(yintercept = mean(d$note), linetype = "dashed") +
  geom_segment(aes(y = mean(d$note),
                   yend = note,
                   x = id,
                   xend = id,
                   linetype = pos)
               ) +
  geom_point(size = 5) +
  labs(linetype = "Richtung der Abweichung") +
  theme(legend.position =  c(.7, .85)) +
  annotate(geom = "label",
           x = 0,
           hjust = 0,
           y = mean(d$note), label = paste0("MW = ", round(mean(d$note), 2))) +
  scale_y_continuous(limits = c(1, 4)) +
  scale_x_continuous(breaks = 1:4) -> p_mean_deltas
#p_mean_deltas



d %>%   
  ggplot(aes(x = id, y = note)) +
  geom_hline(yintercept = mean(d$note), linetype = "dashed") +
  geom_segment(aes(y = mean(d$note),
                   yend = note,
                   x = id,
                   xend = id,
                   linetype = pos)) +
    annotate(geom = "label",
           x = 0,
           hjust = 0,
           y = mean(d$note), label = paste0("MW = ", round(mean(d$note), 2))) +
  geom_rect(aes(ymin = note_avg2,
                ymax = note2,
                xmin = id,
                xmax = id+delta_abs),
            fill = "firebrick",
            alpha = .5) +
  geom_point(size = 5) +
    labs(linetype = "Richtung der Abweichung") +
  theme(legend.position =  "bottom") +
  scale_y_continuous(limits = c(1, 5)) +
  scale_x_continuous(breaks = 1:4) -> p_mean_deltas_sq
p_mean_deltas_sq




## ----quartile, echo = FALSE, out.width = "100%", fig.cap = "Verdeutlichung der Quartile"----
df2 <- data_frame(id = 1:100,
                  groesse = seq(from = 150, to = 210, length.out = 100))

#quantile(x = df2$groesse)

df3 <- data_frame(id = 1:3,
                  label = c("Q1", "Q2", "Q3"),
                  yval = c(165, 180, 195),
                  xval = c(25, 50, 75))

df2 %>%
  mutate(quartile = cut_number(groesse, 4)) %>%
  ggplot(aes(y = groesse, x= id)) +
  geom_col(aes(fill = quartile)) +
  geom_vline(xintercept = c(25, 50, 75)) +
  geom_label(data = df3,
             aes(x = xval,
                 y = yval,
                 label = label),
             fontface = "bold") +
  labs(x = "Nummer der Person",
       y = "Größe",
       fill = "Viertel")



## ----eval = FALSE-------------------------
## verfahren(zielvariable ~ gruppierungsvariable,
##           data = meine_tabelle)


## Berechne das statistische Verfahren mit dem Namen `verfahren`,

## wobei die Zielvariable `y` heißt und die Gruppierungsvariable

## bzw. die zweite relevante Variable `x` ist. Der Datensatz,

## in dem sich diese Variablen finden, heißt `meine_tabelle`.

## 

## ----favstatss-stats-test-----------------
favstats(interest ~ bestanden, na.rm = TRUE, data = stats_test)


## ----summarise-test1----------------------
stats_test <- drop_na(stats_test, score)
summarise(stats_test,
          mean(score), sd(score), aad(score))  # 'aad' aus Paket 'lsr'


## Viele R-Befehle der deskriptiven Statistik sind im Standard so eingestellt, dass sie `NA` zurückliefern, falls es in den Daten fehlende Werte gibt. Das ist einerseits informativ, andererseits aber oft unnötig. Mit dem Parameter `na.rm = TRUE` kann man dieses Verhalten abstellen.

## 
## Tipp: Mit dem Befehl `df <- drop_na(df)` entfernen Sie alle fehlenden Werte aus `df`. Der Befehl stammt aus `tidyr`, welches automatisch geladen wird, wenn Sie `library(tidyverse)` ausführen. Mit `drop_na()` kann man angeben, aus welchen Spalten die fehlenden Werte gelöscht werden sollen. Das ist praktisch, wenn es eine Spalte mit fehlenden Werten gibt, die aber für die vorliegende Analyse nicht so wichtig ist.

## 

## ----desctable4, eval = FALSE-------------
## stats_test %>%
##   filter(!is.na(bestanden)) %>%
##   group_by(bestanden) %>%
##   skim_to_wide()
## 
## dplyr::group_by(iris) %>% skim() -> df


## ----eval = FALSE-------------------------
## favstats(~score, data = stats_test)
## inspect(stats_test)


## -----------------------------------------
tally(~bestanden, data = stats_test, format = "proportion")


## -----------------------------------------
stats_test %>%
  drop_na() %>%
  filter(interest %in% c(1, 6)) %>%  # nur die wenig und stark Interessierten
  tally(bestanden ~ interest, data = ., format = "proportion")


## -----------------------------------------
stats_test %>%
  drop_na() %>%
  filter(interest %in% c(1, 6)) %>%  # nur die wenig und stark Interessierten
  diffprop(bestanden ~ interest, data = ., format = "proportion")


## ----sjmisc-frq-no-eval, eval = FALSE-----
## stats_test %>%
##   group_by(bestanden) %>%
##   frq(study_time)


## -----------------------------------------
stats_test %>%
  flat_table(bestanden, study_time, margin = "row")


## -----------------------------------------
stats_test %>%
  count(interest) %>%
  mutate(interest_prop = n / sum(n))


## ----example-desc-stats-dplyr-------------
stats_test %>%
  drop_na(interest, bestanden) %>%
  mutate(bestanden = score > .7) %>%
  group_by(interest, bestanden) %>%
  summarise(n = n()) %>%
  mutate(interest_prop = n / sum(n)) %>%
  head()


## ----eval = FALSE-------------------------
## stats_test %>%
##   count(interest, bestanden) %>%
##   mutate(interest_prop = n / sum(n))


## ----eval = TRUE--------------------------
stats_test %>%
  count(interest, bestanden) %>%
  group_by(interest) %>%
  mutate(interest_prop = n / sum(n)) %>%
  head(2)


## ----sim-rho, echo = FALSE, out.width="100%", fig.asp=.5, fig.cap = "Beispiele für Korrelationskoeffizienten verschiedener Stärke und Richtung"----
correlation <- c(-0.99, 0, -.4, .80)
n_sim <- 100

values <- NULL
for(i in 1:length(correlation)){
  rho <- correlation[i]
  sigma <- matrix(c(5, rho * sqrt(50), rho * sqrt(50), 10), 2, 2)
  sim <- rmvnorm(
    n = n_sim,
    mean = c(20,40),
    sigma = sigma
    ) %>%
    as_data_frame() %>%
    mutate(correlation = round(rho,2))

  values <- bind_rows(values, sim)
}

ggplot(data = values, mapping = aes(V1, V2)) +
  geom_point() +
  facet_wrap(~ correlation, nrow = 1) +
  labs(x = "", y = "") +
  geom_smooth(method = "lm", se = FALSE) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  )
#detach("package:mvtnorm", unload=TRUE)


## ----rho-delta-rect, fig.cap="Sinnbild zur Berechnung der Korrelation", echo = FALSE, fig.asp = .618----
data("wo_men", package = "pradadata")

set.seed(42)
wo_men %>%
  drop_na() %>%
  select(height, shoe_size) %>%
  filter(between(height, 150, 220)) %>%
  sample_n(30) -> wo_men_sample

wo_men_sample %>%
  ggplot(aes(x = height, y = shoe_size)) +
  geom_hline(yintercept = mean(wo_men_sample$shoe_size), linetype = "dashed") +
  geom_vline(xintercept =  mean(wo_men_sample$height), linetype = "dashed") +
  geom_label(x = mean(wo_men_sample$height),
             y = 47,
             label = "MW") +
  geom_label(y = mean(wo_men_sample$shoe_size),
             x = 200,
             label = "MW") +
  geom_rect(xmin = mean(wo_men_sample$height),
            xmax = wo_men_sample$height[20],
            ymin = mean(wo_men_sample$shoe_size),
            ymax = wo_men_sample$shoe_size[20],
            alpha = .3, fill = "grey60") +
     geom_point() +
  geom_point(x = wo_men_sample$height[20],
             y = wo_men_sample$shoe_size[20],
             size = 4,
             shape = 17,
             color = "firebrick") +
  annotate(geom = "segment",
           x = mean(wo_men_sample$height),
           xend = wo_men_sample$height[20],
           y = wo_men_sample$shoe_size[20],
           yend = wo_men_sample$shoe_size[20],
           color = "firebrick") +
  annotate(geom = "segment",
           x = wo_men_sample$height[20],
           xend = wo_men_sample$height[20],
           y = mean(wo_men_sample$shoe_size),
           yend = wo_men_sample$shoe_size[20],
           color = "firebrick") +
  annotate(geom = "text",
          x =  mean(wo_men_sample$height)+ ((wo_men_sample$height[20] - mean(wo_men_sample$height)) / 2),
          y = wo_men_sample$shoe_size[20],
          label = "dx[i]",
          parse = TRUE,
          vjust = 0) +
  annotate(geom = "text",
           x = wo_men_sample$height[20],
           y = mean(wo_men_sample$shoe_size) + ((wo_men_sample$shoe_size[20] - mean(wo_men_sample$shoe_size)) / 2),
           label = "dy[i]",
           parse = TRUE,
           hjust = -0.1,
           vjust = -0.1) +
labs(x = "Körpergröße",
       y = "Schuhgröße",
       caption = "MW: Mittelwert")



## ----cor-demo1----------------------------
stats_test %>%
  select(study_time, self_eval, score) %>%
  cor()


## ----cor-demo2----------------------------
stats_test %>%
  select(study_time, self_eval, score) %>%
  drop_na() %>%
  cor(method = "spearman")


## ----correlate-demo-----------------------
stats_test %>%
  select(study_time, self_eval, score) %>%
  correlate(method = "kendall")


## ----rplot-demo, fig.cap = "Eine Korrelationstabelle visualisieren", eval = FALSE----
## stats_test %>%
##   select(study_time, self_eval, score, interest) %>%
##   correlate() %>%
##   shave() %>%
##   rplot()


## 
## 1. Die "normale" Korrelation (nach Pearson) setzt ein metrisches Skalenniveau voraus.

## 2. `cor()` führt in der Voreinstellung die Korrelation nach Pearson aus.

## 3. Liegen fehlende Werte vor, so liefert `cor()` in der Voreinstellung `NA` zurück.

## 4. Möchte man trotz fehlender Werte einen Wert für die Korrelation ausgegeben bekommen, so fügt man bei `cor()` das Argument `na.rm = TRUE` hinzu.

## 5. Statistiken, die mehrere Vektoren zu einer Zahl zusammenfassen, bezeichnet man als multivariat.

## 6. Die Varianz ist definiert als Quadratwurzel des mittleren Abweichungsquadrats.

## 7. Die Abweichungen zum Mittelwert summieren sich zu 1 auf.

## 8. Der Unterschied in zwei Anteilen ist ein Beispiel für eine bivariate Statistik.

## 9. Für positive Korrelationen zweier Variablen X und Y gilt: Je größer X, desto kleiner Y.

## 10. Ist die Korrelation null, so ist die Trend- bzw. Regressionsgerade parallel zur X-Achse.

## 
