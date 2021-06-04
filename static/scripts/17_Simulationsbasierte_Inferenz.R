## ----include=FALSE, cache=FALSE-----------
set.seed(1014)
options(digits = 3)

knitr::opts_chunk$set(
  comment = "#>",
  collapse = TRUE,
  message = FALSE,
  warning = FALSE,
  cache = TRUE,
  out.width = "70%",
  fig.align = 'center',
  fig.width = 6,
  fig.asp =  0.4,  #0.618,  # 1 / phi
  fig.show = "hold",
  size = "tiny"
)

script <- TRUE

options(max.print = 20,
        dplyr.print_max = 7,
        dplyr.print_min = 3)


library(methods)  # sometimes not loaded, although built-in, appears like a bug


## Lernziele:

## 
## - Simulationskonzepte anwenden können, um inferenzstatistische Schlüsse zu ziehen

## - Wissen, was man unter *Bootstrapping*  versteht

## - Den Unterschied zwischen einer Bootstrap-Stichprobenverteilung und einer "normalen" Verteilung erläutern können

## - Den *p*-Wert im Rahmen von Simulationskonzepten erläutern können

## - Nullhypothesen testen im Rahmen von Simulationskonzepten

## 

## ----silent-load-libs, echo = FALSE-------
library(boot)
library(latex2exp)


## -----------------------------------------
library(tidyverse)
library(mosaic)  
library(bootES)
library(mosaic)

data(flights, package = "nycflights13")


## ----flights-sample-----------------------
set.seed(1234567)
flights_sample <- flights %>%
  drop_na() %>%
  sample_n(size = 30)


## ----flights-sample-summarise-all---------
flights_sample %>%
  select(arr_delay) %>%
  summarise_all(funs(min, max, median, mean, sd, IQR))


## ----flights-summarise-all----------------
flights %>%
  select(arr_delay) %>%
  drop_na() %>%
  summarise_all(funs(min, max, median, mean, sd, IQR))


## Einige Statistiken eignen sich also nicht oder weniger als andere, um auf die Population zu schließen. Hier hat der Median am besten abgeschnitten; der Maximalwert am schlechtesten. Das ist ein typischer Befund.

## 

## -----------------------------------------
flights_sample %>%
  select(arr_delay) %>%
  mutate(delay_ranking = percent_rank(arr_delay)) %>%
  arrange(-delay_ranking)


## -----------------------------------------
quantile(~arr_delay, probs = .95,
         data = flights, na.rm = TRUE)


## ----simu-df, echo = FALSE----------------
#  dauert lange:
set.seed(42)
simu_df <- do(100000) * mean(rflip(100))


## ----simu-vs-analyse, echo = FALSE, fig.cap = "Sinnbild für klassische und simulationsbasierte Inferenztechniken", out.width="100%", fig.asp=.5----
p <- 0.5
q <- 1 - p
n <- 100


mue <- n*p
se <- sqrt(n*p*q)

hits <- 60


lo2 <- 30
hi2 <- 70


nv_tex <- "$f(x|\\mu,\\sigma^2) = \\frac{1}{\\sqrt{2\\pi\\sigma^2}}e^{-\\frac{(x-\\mu)^2}{2\\sigma^2}}$"


nv_expr <- TeX(nv_tex)



labx <- "Anzahl Treffer bei 100 fairen Münzwürfen"

p_klassisch <- ggplot(NULL, aes(c(lo2,hi2))) +
  labs(title = "Berechne das Integral der Fläche unter der Kurve",
       #   caption = "Analytischer Weg",
       x = labx,
       y = "Wahrscheinlichkeit",
       parse = TRUE) +
  stat_function(fun = dnorm, geom = "area", fill = "grey40", args = list(mean = mue, sd = se), xlim = c(lo2, hi2)) +
  stat_function(fun = dnorm, geom = "area", args = list(mean = mue, sd = se), fill = "firebrick", xlim = c(hits, hi2)) +
  geom_vline(xintercept = hits, linetype = "dashed") +
  annotate(geom = "label", x = hits, y= 0, label = hits, size = 4) +
  annotate(geom = "label", x = hi2/2, y= 0.05, label =TeX(nv_tex), size = 4, hjust=0) +
  theme(line = element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_text(size = rel(0.5)),
        title = element_text(size = rel(0.5)),
        legend.text=element_text(size=6))






simu_df %>%
  mutate(extreme = ifelse(mean >= 60, "Extreme Stichproben", "common")) -> simu_df


p_simuliert <- simu_df %>%
  ggplot(aes(x = mean)) +
  geom_histogram(bins = 15) +
  geom_histogram(data = filter(simu_df, extreme == "Extreme Stichproben"), aes(fill = extreme), bins = 15) +
  labs(x = labx,
      # title = "Wirf viele Male 10 faire Münzen; zähle die Anzahl der Treffer",
       title = "Führe das Experiment häufig aus; zähle die Treffer",
   #    caption = "Simulationsweg",
       fill = "",
       y = "relative Häufigkeit") +
  geom_vline(xintercept = hits, linetype = "dashed") +
  annotate(geom = "label", x = hits, y= 0, label = hits, size = 4) +
  theme(line = element_blank(),
        axis.text.y = element_blank(),
        legend.position = c(1,1),
        legend.justification = c(1,1),
        legend.title = element_text(size = rel(0.5)),
        title = element_text(size = rel(0.5)),
        legend.text=element_text(size=4))



gridExtra::grid.arrange(p_klassisch, p_simuliert, nrow = 1)


## Wie wir gesehen haben, ist keineswegs sicher, dass eine Statistik dem Parameter (d.h. dem analogen Kennwert in der Population) ähnlich ist. Wir brauchen also einen Kennwert, der uns sagt, *wie ähnlich* eine Statistik einem Parameter wohl ist. Das leisten der *Standardfehler*\index{Standardfehler} und die *Stichprobenverteilung*\index{Stichprobenverteilung}.

## 

## ----sample-fun1--------------------------
flights_nona <- drop_na(flights, arr_delay)
n_replics <- 1000
sample_size <- 30
set.seed(42)  # Zufallszahlen fixieren zur Reproduzierbarkeit

viele_stipros30 <- do(n_replics) * mean(sample(flights_nona$arr_delay,
                                                   size = sample_size))
head(viele_stipros30$mean)
sd(viele_stipros30$mean)  


## ----abw-m-30, echo = FALSE---------------
abw_m_30 <- abs(mean(viele_stipros30$mean) - mean(flights_nona$arr_delay)) %>% round(2)


## ----sample-fun100, echo = FALSE----------
set.seed(42)  # Zufallszahlen fixieren zur Reproduzierbarkeit

viele_stipros100 <- do(n_replics) * mean(sample(flights_nona$arr_delay,
                                                size = 100))
viele_stipros5 <- do(n_replics) * mean(sample(flights_nona$arr_delay,
                                              size = 5))


## ----viele-stipros-sds, echo = FALSE------
viele_stipros_sds <-
  data_frame(
    `n = 005` = sd(viele_stipros5$mean),
    `n = 030` = sd(viele_stipros30$mean),
    `n = 100` = sd(viele_stipros100$mean)
  ) %>%
  gather() %>%
  rename(sample_size = key,
         sd_avg = value) %>%
  mutate(arr_delay_mean = c(mean(viele_stipros5$mean),
                      mean(viele_stipros30$mean),
                      mean(viele_stipros100$mean)),
         y = 0)


## ----stipro-vert-delay, fig.cap = "Die grobe Stichprobenverteilung des Mittelwerts der Verspätungen", echo = FALSE, out.width = "100%", fig.asp = .5----
data_frame(n_005 = viele_stipros5$mean,
           n_030 = viele_stipros30$mean,
           n_100 = viele_stipros100$mean) %>%
  gather(key = sample_size, value = arr_delay_mean) %>%
  mutate(sample_size = factor(sample_size,
                              levels = c("n_005", "n_030", "n_100"),
                              labels = c("n = 005", "n = 030", "n = 100"))) %>%
  ggplot() +
  aes(x = arr_delay_mean) +
  geom_histogram(aes(y = ..density..),
                 alpha = .8) +
  facet_wrap(~sample_size) +
  geom_density() +
  labs(caption = "Der horizontale Balken zeigt den Standardfehler (SE)." ) +
  coord_cartesian(xlim = c(-30, 100)) +
  geom_segment(data = viele_stipros_sds,
                 aes(x = arr_delay_mean - sd_avg,
                     xend = arr_delay_mean + sd_avg),
                     y = 0.01, yend = 0.01,
               color = "red",
               size = 4) +
  geom_label(data = viele_stipros_sds,
             aes(label = paste0("SE = ",round(sd_avg, 0))),
             y = 0.08,
             x = 80,
             hjust = 1)


## Der Standardfehler (SE) zeigt, wie sehr sich Stichprobenkennwerte ähneln, wenn man viele Stichproben zieht. Er wird oft als Maß für die Schätzgenauigkeit des Mittelwerts verstanden.

## Der *SE* ist nicht identisch mit der Streuung (*SD*) in der Population. Grob gesagt will der *SE* die Genauigkeit der Schätzung angeben. Je kleiner der *SE*, desto präziser die Schätzung (kleinerer Schätzbereich).

## 

## ----ci-boot------------------------------
delay_avg <- mean(viele_stipros100$mean, na.rm = TRUE) %>% round(2)
delay_se <- sd(viele_stipros100$mean, na.rm = TRUE) %>% round(2)
(UG <- delay_avg - 2 * delay_se)  # untere Grenze
(OG <- delay_avg + 2 * delay_se)  # obere Grenze


## ----sample-fun-quantil-------------------
set.seed(42)
n_replics <- 1000

viele_stipros30_q <- do(n_replics) * quantile(sample(flights_nona$arr_delay,
                                                      size = 30),
                                               probs = .95)
mean(viele_stipros30_q$X95.)
sd(viele_stipros30_q$X95.)
quantile(~arr_delay, data = flights_nona, probs = 0.95)


## Lege die Zufallszahlen fest.

## Tue `n_replics` Mal das Folgende:

## Berechne das 95%-Quantil für eine Stichprobe an Verspätungswerten.

## Das Ergebnis soll als Objekt `viele_stipros3` gespeichert werden.

## 

## ----q-95-stats, echo = FALSE-------------
q_95 <- quantile(~arr_delay, data = flights_nona, probs = 0.95) %>% unname() %>% round(2)

q95_m <- round(mean(viele_stipros30_q$X95.),2)

delay30_avg <- mean(viele_stipros30$mean, na.rm = TRUE) %>% round(2)
delay30_se <- sd(viele_stipros30$mean, na.rm = TRUE) %>% round(2)

abw_q95_30 <- abs(q_95 - q95_m) %>% round(2)


## ----echte-kleine-stichprobe--------------
set.seed(42)
echte_kleine_stichprobe <- sample(flights_nona$arr_delay,
                                  size = 3, replace = FALSE)
echte_kleine_stichprobe


## ----bootstrap-sample---------------------
set.seed(42)
bootstrap_sample <- sample(echte_kleine_stichprobe,
                           size = 3, replace = TRUE)
bootstrap_sample


## ----echte-stipro-------------------------
set.seed(42)
sample_n <- 100
echte_stichprobe <- sample(flights_nona$arr_delay, size = sample_n)
viele_bootstrap_samples <- do(1000) * mean(resample(echte_stichprobe))
sd(viele_bootstrap_samples$mean)


## -----------------------------------------
favstats(viele_stipros100$mean)  


## Für einigermaßen große Stichproben ist der Bootstrap -- bzw. der Bootstrap-Standardfehler -- ein guter Schätzer für den "echten" Standardfehler [@efron1994introduction]. Damit bietet das "Bootstrapping" eine gute Möglichkeit, die Schätzgenauigkeit zu quantifizieren.

## 

## -----------------------------------------
quantile(viele_bootstrap_samples$mean, p = c(.025, .975))


## ----p-bootstrap-distrib-arrdelay, echo  = FALSE----

lower <- quantile(viele_bootstrap_samples$mean, p = c(.025, .975))[1] %>% round(0)
upper <- quantile(viele_bootstrap_samples$mean, p = c(.025, .975))[2] %>% round(0)

data_frame(xbar = viele_bootstrap_samples$mean) %>%
  ggplot +
  aes(x = xbar) +
  geom_histogram(fill = "grey60") +
  geom_vline(xintercept = qdata(viele_bootstrap_samples$mean, p = .025)[2],
             color = "firebrick", linetype = "dashed") +
  geom_vline(xintercept = qdata(viele_bootstrap_samples$mean, p = .975)[2],
             color = "firebrick", linetype = "dashed") +
  theme(plot.subtitle = element_text(size = rel(.7))) +
  labs(x = "Mittlere Verspätung",
       title = paste0("95%-KI: [", lower, ",", upper, "]"),
      caption = "Die vertikalen Linien geben das 95%-KI an.") -> p_boot


## Bootstrapping ist ein Verfahren, um die Genauigkeit einer Schätzung zu quantifizieren, zum Beispiel in Form eines Konfidenzintervalls. Bootstrapping beruht auf wiederholtem Ziehen mit Zurücklegen aus der Stichprobe; dadurch wird eine Verteilung erzeugt, die eine Annäherung an die tatsächliche Stichprobenverteilung darstellt. Der Bootstrap sollte nicht verwendet werden, wenn die Stichprobe als unrepräsentativ für die Population eingeschätzt wird bzw. wenn sie klein ist. Laut @mooney1993bootstrapping sollte gelten $n>50$.

## 

## ----ex-bootES----------------------------
bootES(echte_stichprobe)


## ----nhst-modell, echo = FALSE, fig.cap = "Nullhypothesen auf Signifikanz testen", out.width="50%"----

knitr::include_graphics("images/inferenz/inferenz_modell-crop.pdf")



## ----viele-stipros-h0---------------------
set.seed(42)
viele_stipros_H0<- do(n_replics) * mean(rnorm(n = sample_n, mean = 0,
                                              sd = sd(echte_stichprobe)))
head(viele_stipros_H0)
favstats(viele_stipros_H0$mean)  


## ----p-simu-boot-h0, echo = FALSE, fig.cap = "Simulation einer Stichproben-Verteilung", out.width="100%"----
data_frame(xbar0 = viele_stipros_H0$mean) %>%
  mutate(more_extreme = xbar0 > mean(echte_stichprobe)) %>%
  filter(more_extreme) -> extreme_data

my_lab <- paste0("bar(x) == ", mean(echte_stichprobe))

p.value <- prop(viele_stipros_H0$mean >= mean(echte_stichprobe), format = "proportion")


data_frame(xbar0 = viele_stipros_H0$mean) %>%
  ggplot +
  aes(x = xbar0) +
  geom_histogram(fill = "grey60") +
  geom_histogram(data = extreme_data, fill = "firebrick") +
  geom_vline(xintercept = mean(echte_stichprobe)) +
  labs(x = TeX("$\\bar{x}_0$"),
       title = paste0("p-Wert: ", round(p.value, 3)),
       caption = paste0("Der Anteil der roten Fläche entspricht dem p-Wert.")) +
  theme(plot.subtitle = element_text(size = rel(.7))) +
  #annotate(geom = "text", x = 15, y = 70, label = paste0("p = ", round(p.value, 3)), color = "firebrick", size =6) +
    geom_label(x = mean(echte_stichprobe), y = 40, label = my_lab, parse = TRUE) -> p_H0_distrib

gridExtra::grid.arrange(p_boot, p_H0_distrib, ncol = 2)




## ----ex-lsg-04----------------------------
prop(viele_stipros_H0$mean >= mean(echte_stichprobe), format = "proportion")


## 1. Unter einer Stichprobenverteilung versteht man die Verteilung von Stichproben aus einer definierten Hypothese, aber nicht aus der Nullhypothese.

## 2. Unter einer Teststatistik versteht man eine Statistik, die man in einem Inferenztest untersucht.

## 3. Die Nullhypothese heißt Nullhypothese, weil sie verworfen, also "nullifiziert" werden soll.

## 4. Beim Bootstrap werden Stichproben ohne Zurücklegen gezogen.

## 5. Das 95%-Quantil einer Stichprobe ist besser geeignet als das 50%-Quantil, um den Wert in der Population zu schätzen.

## 6. Bei größeren Stichproben wird die Schätzung eines Populationsparameters genauer (unter sonst gleichen Umständen).

## 7. Der Standardfehler zeigt, wie sehr sich Stichprobenkennwerte ähneln, wenn man viele Stichproben zieht.

## 8. Eine gängige Interpretation des Standardfehlers ist, dass ein größerer Standardfehler mit größerer Präzision der Parameterschätzung einhergeht.

## 9. Der Bereich, der sich zwei Einheiten des Standardfehlers um den Mittelwert herum erstreckt, wird bei einer Normalverteilung auch als 95%-Konfidenzintervall bezeichnet.

## 10. Der Standardfehler ist eine Standardabweichung -- die Standardabweichung der Stichprobenverteilung.

## 
## 

## ----score-boot, eval = FALSE-------------
## data("stats_test", package = "pradadata")
## set.seed(42)
## 
## n_replics <- 1000
## stats_test %>%
##   drop_na(score) -> d
## 
## boot_verteilung <- do(n_replics)* mean(~score, data = resample(d))
## 
## qplot(data = boot_verteilung,
##      x = mean)


## ----boot-cor, eval = TRUE----------------
set.seed(42)
flights_sample100 <- drop_na(flights) %>% sample_n(100)
boot_cor <- do(10000) * cor(arr_delay ~ distance, data =
                              resample(flights_sample100))
boot_cor %>%
  ggplot(aes(x = cor)) +
  geom_histogram()

quantile(~cor, data = boot_cor, prob = c(.05, .5, .95))


## ----boot-diffmean-lga-jfk----------------
set.seed(42)
flights_sample100 %>%
  filter(origin %in% c("JFK", "LGA")) -> flights_sample_JFK_LGA

boot_diffmean <- do(1000) * diffmean(arr_delay ~ origin,
                                     data = resample(flights_sample_JFK_LGA))
quantile(~diffmean, data = boot_diffmean,prob = c(.05, .5, .95))  


## -----------------------------------------
diffmean(arr_delay ~ origin,
         data = filter(flights, origin %in% c("JFK", "LGA")),
         na.rm = TRUE)

