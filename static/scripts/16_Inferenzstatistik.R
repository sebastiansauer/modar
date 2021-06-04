## Lernziele:

## 
## - Den Unterschied zwischen Stichprobe und Grundgesamtheit verstehen

## - Die Idee des statistischen Inferierens erläutern können

## - Den *p*-Wert erläutern können

## - Den *p*-Wert kritisieren können

## - Alternativen zum *p*-Wert kennen

## - Inferenzstatistische Verfahren für häufige Fragestellungen kennen

## - Gängige Effektstärken kennen und inferenzstatistischen Testverfahren zuordnen können

## 

## ----libs-inferenz------------------------
library(pwr)  
library(compute.es)  
library(tidyverse)  
library(broom)  
library(BayesFactor)  
library(mosaic)  
library(boot)
library(bootES)
library(mosaic)

data(flights, package = "nycflights13")
data(extra, package = "pradadata")
data(wo_men, package = "pradadata")


## ----include = FALSE----------------------

comb2plots <- function(imgs, bottom_text = NULL){
  library(png)
  library(grid)
  library(gridExtra)
  img1 <-  rasterGrob(as.raster(readPNG(imgs[1])), interpolate = FALSE)
  img2 <-  rasterGrob(as.raster(readPNG(imgs[2])), interpolate = FALSE)
  grid.arrange(img1, img2, ncol = 2, bottom = bottom_text)
}




## ----silent-load-libs, echo = FALSE-------
library(boot)
library(latex2exp)


## ----p-de, echo = FALSE, fig.cap = "Stichproben vs. Vollerhebung"----

imgs <- c("images/inferenz/p_de4.png", "images/inferenz/p_de2.png")

comb2plots(imgs, bottom_text = "Links: Stichproben; Rechts: Vollerhebung")



## Die Methoden der Inferenzstatistik beziehen sich auf Zufallsstichproben. Hat man keine Zufallsstichprobe, so erübrigt sich ein Inferenzschluss auf eine Population.

## 

## ----sir-fisher, echo = FALSE, fig.cap = "Der größte Statistiker des 20. Jahrhunderts (p < .05)", out.width = "20%", fig.align = "center"----
stopifnot(file.exists("images/inferenz/RAFischer.jpg"))
knitr::include_graphics("images/inferenz/RAFischer.jpg")


## Der *p*-Wert ist eine Zahl zwischen 0 und 1, die aussagt, wie gut die Daten zur Nullhypothese passen, wie plausibel die Daten für die Hypothese sind. Häufig wird 5% als Grenzwert herangezogen; das ist reine Konvention. *p*-Werte kleiner als 5% zeigen, dass die Daten unter der getesteten Hypothese unplausibel sind, und führen zur *Verwerfung* der Hypothese. So ein Ergebnis nennt man *statistisch signifikant*\index{Signifikanz}. Andernfalls wird die Hypothese beibehalten; das Ergebnis ist nicht signifikant.

## 

## Der *p*-Wert -- P(T|H) -- gibt die Wahrscheinlichkeit $P$ unserer Teststatistik $T$ an (und noch extremere Ausprägungen von $T$), unter der Annahme, dass die getestete Hypothese $H$ wahr ist (und alle sonstigen Annahmen und wenn wir den Versuch unendlich oft wiederholen würden, unter identischen Bedingungen und ansonsten zufällig).

## 
## 

## ----simu-muenz, fig.cap = "Simulation von Würfen einer fairen Münze", echo = FALSE----
set.seed(42)  # Zufallszahlen fixieren
sample_size <- 1000
muenz_10 <-  rbinom(n = sample_size, size = 10, prob = .5) / 10
muenz_5 <-  rbinom(n = sample_size, size = 5, prob = .5) / 5
muenz_30 <- rbinom(n = sample_size, size = 30, prob = .5) / 30

muenz_df <- data_frame(muenz_10 = muenz_10,
                       muenz_5 = muenz_5,
                       muenz_30 = muenz_30)  %>%
  gather(key = Anzahl_Wuerfe, value = Trefferquote) %>%
  separate(Anzahl_Wuerfe, sep = "_", into = c("dummy", "Anzahl_Wuerfe")) %>%
  select(-dummy)


muenz_df %>%
  filter(Anzahl_Wuerfe == 10) %>%
  count(Trefferquote) %>%
  mutate(Trefferquote_cum = cumsum(n)/1000,
         p_values = lag(1 - Trefferquote_cum),
         likelihood = n / sample_size,
         sig = if_else(Trefferquote >= .8, 1, 0)) -> muenz_df2

muenz_df2 %>%
  filter(p_values < 0.05) %>%
  summarise(threshold = min(Trefferquote)) %>%
  pull(threshold) -> threshold

muenz_df2 %>%
  filter(Trefferquote >= .8) %>%
  summarise(p_value_8_of_10 = sum(p_values)) -> p_value

muenz_df2 %>%
  ggplot(aes(x = Trefferquote, y = n)) +
  geom_vline(xintercept = threshold, linetype = "dashed") +
  geom_col(aes(fill = factor(sig))) +
  geom_label(aes(label = round(likelihood, 2)),
             nudge_y = 10)  +
  scale_x_continuous(breaks = seq(0,1, by= .1)) +
  scale_fill_manual(values = c("grey60", "firebrick")) +
  scale_y_continuous(limits = c(0, 300)) +
  labs(title = paste0("p-Wert von q=8/10: ", round(p_value, 2)),
       x = "Trefferquote q",
       caption = paste0("Bereich der Verwerfung (p<.05) beginnt bei q = ", round(threshold, 2))) +
  theme(legend.position = "none") -> p_simu1
p_simu1

muenz_df %>%
  filter(Anzahl_Wuerfe != 10) %>%
  mutate(Anzahl_Wuerfe = factor(Anzahl_Wuerfe, levels = c(5, 30))) %>%
  ggplot(aes(x = Trefferquote)) +
  facet_wrap(~Anzahl_Wuerfe) +
  geom_vline(xintercept = .8, linetype = "dashed", color = "firebrick") +
  geom_histogram() +
  scale_x_continuous(breaks = c(0, .2, .4, .6, .8, 1)) +
  scale_y_continuous(limits = c(0, 400), name = "n") -> p_simu2


## ----simu-se2, fig.cap = "Simulation des Münzversuchs mit n=5 und n=30", out.width = "100%", echo = FALSE----
p_simu2


## Der Anteil der simulierten Stichproben, die mindestens so extrem sind wie das echte (empirische) Stichprobenergebnis, summiert sich zum *p*-Wert. Die Streuung der Stichprobenverteilung ist maßgeblich zur Berechnung des *p*-Werts, daher hat sie einen eigenen Namen: *Standardfehler*.

## 

## ----norm-hyps, fig.cap = "Ungerichtete vs. gerichtete Hypothesen", out.width="100%", echo = FALSE----
ggplot(NULL, aes(c(-3,3))) +
   theme(line = element_blank(),
         axis.text = element_blank(),
         axis.title = element_blank()) +
  labs(title = "A",
       caption = 'Gerichtet, "kleiner als"') +
  geom_area(stat = "function", fun = dnorm, fill = "grey60", xlim = c(-3, 3)) +
  geom_area(stat = "function", fun = dnorm, fill = "firebrick", xlim = c(-3, -1.65)) -> p_norm1

ggplot(NULL, aes(c(-3,3))) +
    theme(line = element_blank(),
         axis.text = element_blank(),
         axis.title = element_blank()) +
  labs(title = "B",
       caption = 'Gerichtet, "größer als"') +
  geom_area(stat = "function", fun = dnorm, fill = "grey60", xlim = c(-3, 3)) +
  geom_area(stat = "function", fun = dnorm, fill = "firebrick", xlim = c(+1.65, 3)) -> p_norm2

ggplot(NULL, aes(c(-3,3))) +
     theme(line = element_blank(),
         axis.text = element_blank(),
         axis.title = element_blank()) +
  labs(title = "C",
       caption = "Ungerichtet") +
  geom_area(stat = "function", fun = dnorm, fill = "grey60", xlim = c(-3, 3)) +
  geom_area(stat = "function", fun = dnorm, fill = "firebrick", xlim = c(-3, -2)) +
  geom_area(stat = "function", fun = dnorm, fill = "firebrick", xlim = c(+2, 3)) -> p_norm3

gridExtra::grid.arrange(p_norm1, p_norm2, p_norm3, nrow = 1)



## ----moslems-terroristen, echo = FALSE, fig.cap = "Mann und Papst zu sein, ist nicht das Gleiche."----
knitr::include_graphics("images/inferenz/maenner_papst-crop.pdf")


## ----einfluss-pwert, echo = FALSE, fig.cap = "Zwei Haupteinflüsse auf den p-Wert", out.width = "40%"----

knitr::include_graphics("images/inferenz/einfluss_pwert-crop.pdf")


## Ein *p*-Wert sollte nicht das zentrale Kriterium zur Entscheidung über eine Hypothese sein. Inferenztests sollten mit anderen Kennwerten wie Effektstärken und Vorhersagegüte angereichert werden (oder von ihnen ersetzt werden), um über die Annahme oder Ablehnung einer Hypothese zu entscheiden.

## 

## ----xchisq-test-mosaic-------------------
extra$viel_saeufer <- extra$n_hangover > 10
xchisq.test(sex ~ viel_saeufer, data = extra)


## -----------------------------------------
saeufer_tab <- table(x = extra$sex, extra$viel_saeufer)


## ----xchisq-test2, eval = FALSE-----------
## xchisq.test(saeufer_tab)


## ----t-test-test--------------------------
t.test(extra_mean ~ sex, data = extra, alternative = "less")


## ----aov-extra-mean-----------------------
aov(extra_mean ~ clients_freq, data = extra) %>% tidy()
aov(extra_mean ~ clients_freq, data = extra) %>% glance()


## ----eval = FALSE-------------------------
## aov(extra_mean ~ clients_freq + sex + sex:clients_freq, data = extra) %>% tidy()


## ----cor-test-----------------------------
cor.test(extra$n_facebook_friends, extra$extra_mean)


## -----------------------------------------
extra_cor_test <- cor.test(extra$n_facebook_friends, extra$extra_mean)
str(extra_cor_test)


## ----lm-test------------------------------
lm(n_hangover ~ n_party, data = extra) %>% tidy()
lm(n_hangover ~ n_party, data = extra) %>% glance()


## Sehr kleine oder sehr große Zahlen gibt R in der Exponentialschreibweise an, z.B. `3.615622e-22`. Möchte man in die Dezimalschreibweise konvertieren, so hilft dieser Befehl: `format(3.615622e-22, scientific = FALSE)`. Faustregel: Wenn eine 3 oder eine größere Zahl im (negativen) Exponenten steht, so ist das Ergebnis statistisch signifikant; z.B. `2e-03` $= 0.002$.

## 

## ----wilcox-test--------------------------
wilcox.test(extra_mean ~ sex, data = extra)


## ----kruska-wallis-test-------------------
extra %>%
  kruskal.test(extra_mean ~ factor(sex), data = .)


## ----shapiro-test-------------------------
shapiro.test(extra$extra_mean)


## ----glm-test-----------------------------
glm(viel_saeufer ~ extra_mean, data = extra, family = "binomial") %>% tidy()


## ----warning = FALSE, eval = FALSE--------
## cor.test(extra$extra_mean, extra$n_facebook_friends, method = "spearman")


## ----ci-t-test----------------------------
wo_men %>%
  filter(sex == "man") %>%
  select(height) %>%
  drop_na() %>%
  pull(height) %>%
  t.test(.)


## Das 95%-Konfidenzintervall ist ein Bereich, so dass bei unendlich häufiger Stichprobenziehung der wahre Wert in 95% der Intervalle beinhaltet wäre.

## 

## ----ci-simu2, fig.cap = "Simulation eines Konfidenzintervalls", echo = FALSE, eval = TRUE, out.width="100%"----
# 100 Stichproben simulieren:
set.seed(42)
height_avg <- do(100) * mean(rnorm(n = 20, mean = 183, sd = 8))
height_sd <- do(100) * sd(rnorm(n = 20, mean = 183, sd = 8))

height <- height_avg %>%
  bind_cols(height_sd) %>%
  add_column(id = 1:100) %>%
  rename(height_avg = mean,
         height_sd = sd)

# Einen Dataframe mit 95%-CI erstellen:  
height %>%  
  mutate(height_sd = sd(height_avg),
         lower = height_avg - 2*height_sd,
         upper = height_avg + 2*height_sd) %>%
  mutate(hit = if_else((183 > lower) & (183 < upper), 1, 0)) -> height


# 100 CIs plotten:
height %>%
  ggplot() +
  aes(x = id) +
  geom_hline(yintercept = 183, linetype = "dashed") +
  geom_errorbar(aes(ymin = height_avg - 2*height_sd,
                    ymax = height_avg + 2*height_sd,
                    color = as.factor(hit),
                    linetype = as.factor(hit))) +
  labs(y = "Größe",
       title = "Die meisten Konfidenzintervalle beinhalten hier den wahren Wert",
       caption = "Horizontale Linie: Wahrer Wert (183cm) der Größe in der Population") +
  theme(legend.position = "none") +
  scale_color_manual(values = c("firebrick", "grey40"))


## ----effectsizes, echo = FALSE------------

df <- read_csv("includes/effectsizes.csv")

knitr::kable(df, caption = "Überblick über gängige Effektstärkemaße",
             escape = FALSE,
           booktabs = T)



## ----eval = FALSE-------------------------
## cohen.ES(test = c("p", "t", "r", "anov", "chisq", "f2"),
##     size = c("small", "medium", "large"))
## 


## ----effsize-functions, echo = FALSE, cache = FALSE----

df <- read.csv("includes/effsize_ueberblick.csv")

knitr::kable(df, caption = "R-Befehle für gängige Effektstärkemaße",
             escape = FALSE,
           booktabs = T)


## Die Power eines Tests gibt an, wie groß die Wahrscheinlichkeit ist, einen Effekt zu finden, wenn wirklich einer da ist. Anders gesagt: Die Power gibt die Wahrscheinlichkeit an, die $H_0$ berechtigterweise zu verwerfen. Die Power ist abhängig von der Effektgröße, von der Stichprobengröße und dem Signifikanzniveau $\alpha$.

## 

## ----bayes, echo = FALSE, fig.cap = "Die zwei Stufen der Bayes-Statistik in einem einfachen Beispiel"----

knitr::include_graphics("images/inferenz/bayes-crop.pdf")



## Die Bayes-Statistik liefert die Wahrscheinlichkeit $p$ einer Hypothese $H$ im Lichte einer Teststatistik $T$ (d.h. ein gewisses Stichprobenergebnis): $p(H|T)$. Damit gibt die Bayes-Statistik die Antwort, die sich die meisten Anwender wünschen.

## 

## -----------------------------------------

extra %>%
  group_by(sex) %>%
  summarise(mean(extra_mean, na.rm = TRUE))

extra %>%
  filter(sex %in% c("Mann", "Frau")) %>%
  mutate(sex = factor(sex)) %>%
  as.data.frame %>%  # 'ttestBF' verkraftet nur althergebrachte data.frames!
  ttestBF(formula = extra_mean ~ sex,
        data = .)  -> extra_BF

# 'formula' muss explizit hingeschrieben sein,
# sonst droht Fehlermeldung

# Hier kommt der Bayes-Faktor:
extra_BF@bayesFactor



## 1. Der *p*-Wert gibt die Wahrscheinlichkeit der $H_0$ an unter der Annahme der Daten.

## 1. p(D|H) = p(H|D)

## 1. Der *p*-Wert sagt, wie gut die Daten zur Nullhypothese passen.

## 1. Bei sehr großen Stichproben werden nur sehr große Effekte signifikant.

## 5. Egal wie klein die Effektstärke ist, es existiert eine Stichprobengröße, die diesen Effekt beliebig signifikant werden lässt -- solange die Effektstärke größer null ist.

## 1. Wenn der *p*-Wert kleiner als 5% ist, dann ist meine Hypothese (H1) höchstwahrscheinlich richtig.

## 1. Wenn der *p*-Wert größer als 5% ist, dann ist das ein Beleg für die  $H_0$.

## 1. Der *p*-Wert basiert auf der Idee, dass man ein Experiment unendlich oft wiederholt; und das unter zufälligen, aber ansonsten komplett gleichen Bedingungen.

## 1. Das 95%-Konfidenzintervall ist der Bereich, in den der Parameter in 95% der Fälle fallen würde bei sehr häufiger Wiederholung des Versuchs.

## 1. Der Vorhersagewert ist definiert als p(H|D).

## 

## ----eval = FALSE-------------------------
## aov_hangover <- aov(n_hangover ~ sex + extra_mean, data = extra)
## broom::glance(aov_hangover)
## 
## lm_hangover <- lm(n_hangover ~ sex + extra_mean, data = extra)
## broom::glance(lm_hangover)


## ----chisq-or, eval = FALSE---------------
## data(titanic_train, package = "titanic")
## 
## titanic_train %>%
##   select(Survived, Pclass) %>%
##   filter(Pclass %in% c(1,3)) -> d
## 
## tally(Survived ~ Pclass, data = d, format = "prop")
## 
## OR <- (.63/.37) / (.24/.76)
## OR


## ----ci-se-no-eval, eval = FALSE----------
## # SE (d.h. Histogramm der Stichproben) plotten:
## height %>%
##   ggplot() +
##   aes(x = height_avg) +
##   geom_histogram(aes(y = ..density..)) +
##   geom_density()


## ----ci-simu-no-eval, echo = TRUE, eval = FALSE----
## # 100 Stichproben simulieren:
## height_avg <- do(100) * mean(rnorm(n = 20, mean = 183, sd = 8))
## height_sd <- do(100) * sd(rnorm(n = 20, mean = 183, sd = 8))
## 
## height <- height_avg %>%
##   bind_cols(height_sd) %>%
##   add_column(id = 1:100) %>%
##   rename(height_avg = mean,
##          height_sd = sd)
## 
## # Eine Dataframe mit 95%-CI erstellen:
## height %>%
##   mutate(height_sd = sd(height_avg),
##          lower = height_avg - 2*height_sd,
##          upper = height_avg + 2*height_sd) %>%
##   mutate(hit = if_else((183 > lower) & (183 < upper), 1, 0)) -> height
## 
## 
## # 100 CIs plotten:
## height %>%
##   ggplot() +
##   aes(x = id) +
##   geom_hline(yintercept = 183, linetype = "dashed") +
##   geom_errorbar(aes(ymin = height_avg - 2*height_sd,
##                     ymax = height_avg + 2*height_sd,
##                     color = as.factor(hit),
##                     linetype = as.factor(hit))) +
##   labs(y = "Größe",
##        title = "Die meisten Konfidenzintervalle beinhalten hier den wahren Wert",
##        caption = "Horizontale Linie: Wahrer Wert (183cm) der Größe in der Population") +
##   theme(legend.position = "none") +
##   scale_color_manual(values = c("firebrick", "grey40"))


## ----ex-inf-simu, eval = FALSE------------
## set.seed(42)  # Zufallszahlen fixieren
## sample_size <- 1000
## muenz_10 <-  rbinom(n = sample_size, size = 10, prob = .5) / 10
## muenz_5 <-  rbinom(n = sample_size, size = 5, prob = .5) / 5
## muenz_30 <- rbinom(n = sample_size, size = 30, prob = .5) / 30
## 
## muenz_df <- data_frame(muenz_10 = muenz_10,
##                        muenz_5 = muenz_5,
##                        muenz_30 = muenz_30) %>%
##   gather(key = Anzahl_Wuerfe, value = Trefferquote) %>%
##   separate(Anzahl_Wuerfe, sep = "_", into = c("dummy", "Anzahl_Wuerfe")) %>%
##   select(-dummy) %>%
##   filter(Anzahl_Wuerfe == 10) %>%
##   count(Trefferquote) %>%
##   mutate(Trefferquote_cum = cumsum(n)/1000,
##          p_values = lag(1 - Trefferquote_cum),
##          likelihood = n / sample_size,
##          sig = if_else(Trefferquote >= .8, 1, 0)) -> muenz_df
## 
## muenz_df %>%
##   filter(p_values < 0.05) %>%
##   summarise(threshold = min(Trefferquote)) %>%
##   pull(threshold) -> threshold
## 
## muenz_df %>%
##   filter(Trefferquote >= .8) %>%
##   summarise(p_value_8_of_10 = sum(p_values)) -> p_value
## 
## muenz_df %>%
##   ggplot(aes(x = Trefferquote, y = n)) +
##   geom_vline(xintercept = threshold, linetype = "dashed") +
##   geom_col(aes(fill = factor(sig))) +
##   geom_label(aes(label = round(likelihood, 2)))  +
##   scale_x_continuous(breaks = seq(0,1, by= .1)) +
##   scale_fill_manual(values = c("grey60", "firebrick")) +
##   scale_y_continuous(limits = c(0, 250)) +
##   labs(title = paste0("p-Wert von 8/10 Treffer: ", round(p_value, 2)),
##        x = "Trefferquote q",
##        caption = paste0("Bereich der Verwerfung (p<.05) beginnt bei q = ",
##                         round(threshold, 2))) +
##   theme(legend.position = "none") -> p_simu1
## p_simu1

