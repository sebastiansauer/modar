## Lernziele:

## 
## - Praktische Erfahrung mit angewandter Datenanalyse sammeln

## - Typische Analysen des geleiteten Modellierens anwenden

## - Lineare Modelle berechnen an einem echten Datensatz

## - Die typischen Schritte einer Datenanalyse durchlaufen

## 

## ----libs-affairs-------------------------
library(tidyverse)  
library(broom)
library(corrplot)  
library(compute.es)  
library(viridis)
library(mosaic)

data(titanic_train, package = "titanic")


## ----count-titanic------------------------
c1 <- dplyr::count(titanic_train, Pclass)
c1


## Achtung -- Namenskollision! Sowohl im Paket `mosaic` als auch im Paket `dplyr` gibt es einen Befehl `count()`. Für `select()` gilt Ähnliches -- und für eine Reihe anderer Befehle auch. Das arme R weiß nicht, welchen von beiden wir meinen, und entscheidet sich im Zweifel für den falschen. Da hilft es, zu sagen, aus welchem Paket wir den Befehl beziehen wollen. Das macht der Operator `::`.

## 

## ----count-titanic2-----------------------
c3 <- dplyr::count(titanic_train, Pclass, Survived)
c3


## ----titanic1, fig.cap = "Überlebensraten auf der Titanic in Abhängigkeit von der Passagierklasse", out.width = "100%", echo = FALSE----
ggplot(c3, aes(x = factor(Pclass),
               y = n,
               fill = factor(Survived))) +
         geom_col(position = "fill") +
  theme(legend.position = "bottom") +
  scale_fill_viridis(discrete = TRUE) -> p1

qplot(x = factor(Pclass),
      y = n,
      size = 5,
      color = factor(Survived),
      shape = factor(Survived),
      data = c3) +
  theme(legend.position = "bottom") -> p2



gridExtra::grid.arrange(p1, p2, nrow = 1)


## ----titanic-chi--------------------------
xchisq.test(Survived ~ Pclass, data = titanic_train)


## ----t2-filter----------------------------
t2 <- filter(titanic_train, Pclass != 2)  # "!=" heißt "nicht"


## ----t2-filter-2--------------------------
t2 <- filter(titanic_train, Pclass == 1 | Pclass == 3)  # "|" heißt "oder"


## ----count-c4-----------------------------
(c4 <- dplyr::count(t2, Pclass))


## ----t2-chi, echo = FALSE-----------------
t2_chi <- xchisq.test(Survived ~ Pclass, data = t2)


## ----eval = TRUE, results = "hide"--------
t2_or <- compute.es::chies(chi.sq = 96, n = 711)


## ----titanic1-no-eval, eval = FALSE-------
## qplot(x = factor(Pclass),
##       y = n,
##       color = factor(Survived),
##       shape = factor(Survived),
##       data = c3) -> p1
## 
## ggplot(c3, aes(x = factor(Pclass),
##                y = n,
##                fill = factor(Survived))) +
##          geom_col(position = "fill") +
##   theme(legend.position = "bottom") +
##   scale_fill_viridis(discrete = TRUE) -> p2


## ----titanic4-links-no-eval, eval = FALSE----
## c3 %>%
##   group_by(Pclass) %>%
##   mutate(prop = n / sum(n)) -> c3_grouped
## 
## c3_grouped %>%
##   ggplot +
##   aes(x = factor(Pclass), y = factor(Survived), fill = prop) +
##   geom_tile() #+ scale_fill_viridis()


## ----titanic4-rechts-no-eval, eval = FALSE----
## c3 %>%
##   group_by(Pclass) %>%
##   mutate(prop = n / sum(n)) %>%
##   ungroup() %>%
##   filter(Survived == 1) %>%
##   mutate(Survived_diff = prop - lag(prop)) %>%
##   replace_na(list(Survived_diff = 0)) %>%
##   ggplot(aes(x = factor(Pclass), y = Survived_diff)) +
##   geom_point()


## ----titanic4, echo = FALSE, fig.cap = "Überlebenshäufigkeiten anhand eines Fliesendiagramms dargestellt", outwidth = "100%"----
c3 %>%
  group_by(Pclass) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot +
  aes(x = factor(Pclass), y = factor(Survived), fill = prop) +
  geom_tile() +
  scale_fill_viridis() -> p3

c3 %>%
  group_by(Pclass) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  filter(Survived == 1) %>%
  mutate(Survived_diff = prop - lag(prop)) %>%
  replace_na(list(Survived_diff = 0)) %>%
  ggplot(aes(x = factor(Pclass), y = Survived_diff)) +
  geom_point() -> p4

gridExtra::grid.arrange(p3, p4, nrow = 1)


## Nimm den Datensatz `c3` UND DANN

## Gruppiere nach Passagierklassen UND DANN

## berechne für jede Klasse die Überlebenswahrscheinlichkeit UND DANN

## löse die Gruppierung wieder auf, die folgenden Befehle (`mutate`) sollen *keine* Gruppierung berücksichtigen UND DANN

## filtere nur die Werte für Überleben, nicht für Sterben, da das Diagramm nur Überlebenswahrscheinlichkeit darstellen soll UND DANN

## berechne den Unterschied zwischen einer Klasse und der vorherigen (also z.B. 2 vs. 1) UND DANN

## Ersetze `NA` in der Spalte `Survived_diff` durch 0 UND DANN

## plotte das Diagramm.

## 

## -----------------------------------------
x <- c(10, 20, 30)
lag(x)


## ----glm-titanic--------------------------
glm_titanic <- glm(data = titanic_train,
                   formula = Survived ~ Pclass,
                   family = "binomial")
coef(glm_titanic)
exp(coef(glm_titanic))

titanic_train$pred_prob <- predict(glm_titanic, type = "response")


## ----fig-titanic, echo = FALSE, fig.cap = "Logistische Regression zur Überlebensrate nach Passagierklasse"----
titanic_train %>%
  dplyr::select(Pclass, Survived, pred_prob) %>%
  ggplot() +
  aes(x = Pclass, y = Survived) +
  geom_jitter(width = .1, alpha = .3) +
  stat_smooth(aes(y = Survived, x = Pclass), method="glm", method.args=list(family="binomial")) +
  scale_x_continuous(breaks = c(1,2, 3)) +
  scale_y_continuous(breaks = c(0, .2, .4, .6, .8, 1)) +
  labs(y = "p")


## ----glm-titanic-koeffs, echo = FALSE-----
b0 <- unname(round(exp(coef(glm_titanic))[1], 2))
b1 <- unname(round(exp(coef(glm_titanic))[2], 2))

b0b1 <- round(b0*b1, 2)


## ----predict-glm_titanic-titanic----------
predict(glm_titanic, newdata = data.frame(Pclass = 1), type = "response")
predict(glm_titanic, newdata = data.frame(Pclass = 2), type = "response")
predict(glm_titanic, newdata = data.frame(Pclass = 3), type = "response")


## -----------------------------------------
tally(Survived ~ Pclass, data = titanic_train,
         format = "proportion")


## -----------------------------------------
titanic_train %>%
  dplyr::select(Survived, Pclass) %>%
  group_by(Pclass, Survived) %>%
  summarise(n = n() ) %>%
  mutate(Anteil = n / sum(n)) -> c5
c5


## Nimm den Datensatz "titanic_train" UND DANN

## Filtere nur die 1. und die 3. Klasse heraus UND DANN

## wähle nur die Spalten "Survived" und "Pclass" UND DANN

## gruppiere nach "Pclass" und "Survived" UND DANN

## zähle die Häufigkeiten für jede dieser Gruppen aus UND DANN

## berechne den Anteil an Überlebenden bzw. Nicht-Überlebenden

## für jede der beiden Passagierklassen.

## 

## 1. Um die Überlebenschancen zu berechnen, hätte man anstelle von `count()` auch `mosaic::tally()` oder `sjmisc::frq()` verwenden können.

## 2. Ist diese Syntax korrekt `sjmisc::frq(titanic_train, Pclass)`, um die Überlebenschancen zu berechnen?

## 3. Die Analyse bivariater Häufigkeitsverteilungen wird häufig als Analyse von Kontingenztabellen bezeichnet.

## 4. Balkendiagramme sollten nie zur Darstellung von Häufigkeiten verwendet werden.

## 5. Balkendiagramme sollten nie zur Darstellung von Mittelwerten verwendet werden.

## 6. $\chi^2$-Quadrat-Tests werden nicht nur Analyse nominaler Zusammenhänge verwendet.

## 7. Ein Effektstärkemaß für nominale Zusammenhänge ist Odds Ratio.

## 8. Ein Odds Ratio von 0.1 zeigt einen gleich starken Zusammenhang an wie ein Odds Ratio von 10.

## 9. Ein Odds Ratio von 1.0 zeig keinen Zusammenhang an.

## 10. Eine logistische Regression darf nur binäre (dichotome) Prädiktoren beinhalten.

## 
## 
