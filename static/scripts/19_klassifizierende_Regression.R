## Lernziele:

## 
## - Die Idee der logistischen Regression verstehen

## - Die Koeffizienten der logistischen Regression interpretieren können

## - Die Modellgüte einer logistischen Regression einschätzen können

## - Klassifikatorische Kennzahlen kennen und beurteilen können

## 

## ----libs-logist-regr---------------------
library(SDMTools)  
library(pROC)  
library(tidyverse)
library(BaylorEdPsych)  
library(broom)  

data(stats_test, package = "pradadata")


## ----libs-hidden19, echo = FALSE----------
library(kableExtra)


## ----logist-regr1-------------------------
stats_test %>%
  drop_na() %>%  #fehlende Werte entfernen
  mutate(bestanden_num = if_else(bestanden == "ja", 1, 0)) -> stats_test


## -----------------------------------------
lm1 <- lm(bestanden_num ~ study_time, data = stats_test)
tidy(lm1)


## ----fig-logist-regr2, echo = FALSE, fig.cap = "Regressionsgerade für das Bestehen-Modell"----
ggplot(stats_test) +
  aes(x = study_time, y = bestanden_num) +
  geom_jitter(alpha = .5) +
  geom_abline(slope = lm1$coefficients[2], intercept = lm1$coefficients[1], color = "red")


## ----logist-curve, echo = FALSE-----------
# eta-Werte von -10 bis +10 erzeugen
x <- seq(-10,10,by = 0.1)
# y-Werte mit logistischer Funktion berechnen
y <- 1/(1+exp(-x))        # exp() ist die e-Funktion

data_frame(
  x = x,
  y = y) %>%
ggplot() +
  aes(x = x, y = y) +
  geom_line(color = "#00998a") -> p_logist



## ----glm1-study-time----------------------
glm1 <- glm(bestanden_num ~ study_time,
            family = "binomial",
            data = stats_test)


##  Die logistische Regression in R wünscht, dass das Kriterium eine binäre Variable sei (Stufen `0` und `1`), eine Faktor-Variable oder eine logische Variable (Stufen `TRUE` und `FALSE`) sei;

## eine String-Variable (Stufen `ja` und `nein`) wird *nicht* goutiert.

## Wenn Sie eine Faktor-Variable verwenden, so wird die erste Stufe als `0` und die zweite Stufe als `1` interpretiert (`1` ist das zu modellierende Ereignis).

## Wenn nicht anderweitig vorgegeben, sortiert R die Faktorstufen alphanumerisch.

## Mit `levels(stats_test$bestanden_fct)` können Sie die Reihenfolge der Faktorstufen auslesen. So können Sie eine neue Faktorstufe als Referenzstufe (d.h. erste Faktorstufe) bestimmen: `relevel(stats_test$bestanden_fct, "nein") -> stats_test$bestanden_fct`.

## 

## ----aktien-plot, echo = FALSE, out.width = "100%", fig.cap = "Die logistische Regression beschreibt eine 's-förmige' Kurve"----
stats_test %>%
  mutate(pred = stats::predict(glm1,
                              data = data.frame(bestanden_num = stats_test$bestanden_num),
                               type = "response")) %>%
  ggplot() +
  aes(x = study_time) +
  geom_jitter(aes(y = bestanden_num), alpha = .5) +
  geom_line(aes(y = pred), color = "red", size = 2) -> p_study_bestanden

gridExtra::grid.arrange(p_logist, p_study_bestanden, nrow = 1)



## -----------------------------------------
glm1_tidy <- tidy(glm1)
glm1_tidy


## Die Koeffizienten der logistischen Regression dürfen *nicht* so interpretiert werden wie bei der "normalen" (linearen) Regression.

## Aber es gilt nach wie vor: Ist der Einfluss eines Prädiktors $X$ positiv,

## so steigt der Wert des Kriteriums $Y$ (z.B. die Wahrscheinlichkeit für Bestehen),

## wenn der Prädiktorwert steigt. Analoges gilt für ein Einflussgewicht mit negativem Vorzeichen.

## Ist das Einflussgewicht null, so hat der Prädiktor keinen Einfluss.

## In ähnlicher Weise gilt: Ist der Wert des Achsenabschnitts $b_0$ positiv ($b_0>0$),

## so ist die Wahrscheinlichkeit von $Y$ (z.B. Bestehen bzw. für das zu modellierende Ereignis) größer als 50% bei $x=0$.

## 

## -----------------------------------------
predict(glm1, newdata = data.frame(study_time = 3))


## -----------------------------------------
exp(glm1_tidy$estimate[2])


## -----------------------------------------
(wskt <- 2 / (2+1))


## -----------------------------------------
predict(glm1, newdata = data.frame(study_time = 3), type = "response")


## ----interessiert-logist------------------
stats_test$interessiert <- stats_test$interest > 3


## ----glm2---------------------------------
glm2 <- glm(bestanden_num ~ interessiert,
               family = "binomial",
               data = stats_test)
tidy(glm2)


## ----glm2-predict-------------------------
predict(glm2, newdata = data.frame(interessiert = FALSE),
        type = "response")
predict(glm2, newdata = data.frame(interessiert = TRUE),
        type = "response")


## ----glm3-one-factor-predictor------------
glm3 <- glm(bestanden_num ~ factor(interest), data = stats_test)
tidy(glm3) %>% head(3)


## ----glm4-many-predictors-----------------
glm4 <- glm(bestanden_num ~ study_time + interessiert + self_eval,
            family = binomial,
            data = stats_test)
tidy(glm4) %>% str()



## -----------------------------------------
b_glm4 <- round(broom::tidy(glm4)$p.value[3], 3)


## -----------------------------------------
glance(glm3)


##  Ein Test, der alle positiven Fälle korrekt als positiv klassifiziert, muss deshalb noch lange nicht alle negativen Fälle als negativ klassifizieren. Die Richtig-Positiv-Rate und die Richtig-Negativ-Rate können unterschiedlich sein.

## 

## ----class-stats, echo = FALSE------------
df <- readr::read_csv("includes/class_results.csv")

knitr::kable(df, caption = "Vier Arten von Ergebnissen von Klassifikationen",
             booktabs = T)



## ----confmatrix-glm3----------------------
(cm <- confusion.matrix(stats_test$bestanden_num,
                        glm4$fitted.values))


## ----diag-stats, echo = FALSE-------------

df <- readr::read_csv("includes/diag_stats.csv")

knitr::kable(df, caption = "Geläufige Kennwerte der Klassifikation.",
             booktabs = T,
             format = "latex") %>% 
  kable_styling() %>% 
  add_footnote("Anmerkungen: F: Falsch. R: Richtig. P: Positiv. N: Negativ")


## -----------------------------------------
(cm <- confusion.matrix(stats_test$bestanden_num,
                        glm4$fitted.values))
SDMTools::sensitivity(cm)
SDMTools::specificity(cm)


## ----glm4-pred1---------------------------
glm4_pred <- predict(glm4, newdata = stats_test)
head(glm4_pred)


## ----glm4-pred2---------------------------
stats_test$glm4_pred <- predict(glm4, newdata = stats_test, type = "response")
head(glm4_pred)


## ----glm4-pred-class----------------------
stats_test$glm_pred_class <- ifelse(stats_test$glm4_pred < .5, "Bad", "Good")
head(stats_test$glm_pred_class)
count(stats_test, glm_pred_class)


## ----glm4-pred-class2---------------------
stats_test$glm_pred_class <- "Good"
stats_test$glm_pred_class[stats_test$glm4_pred < .5] <- "Bad"
head(stats_test$glm_pred_class)
count(stats_test, glm_pred_class)


## Ein Test ist dann gut, wenn er für alle möglichen Schwellenwerte insgesamt wenig Fehler produziert.

## 

## ----lets-roc-----------------------------
lets_roc <- roc(response = stats_test$bestanden_num,
                predictor = glm4$fitted.values)


## ----roc-stats-no-eval, eval = FALSE------
## plot(lets_roc, main = "ROC-Kurve von glm4")


## ----roc1roc3, eval = FALSE---------------
## lets_roc1 <- roc(response = stats_test$bestanden_num,
##                 predictor = glm1$fitted.values)
## #plot(lets_roc1, main = "ROC-Kurve von glm1")
## 
## lets_roc3 <- roc(response = stats_test$bestanden_num,
##                 predictor = glm3$fitted.values)
## #plot(lets_roc3, main = "ROC-Kurve von glm3")


## ----roc-stats, fig.cap = "Eine ROC-Kurve", echo = FALSE, out.width = "100%", fig.asp = .3, echo = FALSE----

knitr::include_graphics("images/regr/rocs_glms-crop.pdf")



## ----example-rocs, echo = FALSE, fig.cap = "Beispiel für eine sehr gute (A), gute (B) und schlechte (C) Klassifikation", out.width = "100%", fig.asp = .3----

library(plotROC)
library(gridExtra)
D.ex <- rbinom(200, size = 1, prob = .5)
M1 <- rnorm(200, mean = D.ex, sd = .3)
M2 <- rnorm(200, mean = D.ex, sd = 1.5)
M3 <- rnorm(200, mean = D.ex, sd = 10)


test <- data.frame(D = D.ex, D.str = c("Healthy", "Ill")[D.ex + 1],
                   M1 = M1, M2 = M2, stringsAsFactors = FALSE)


p1 <- ggplot(test, aes(d = D, m = M1)) + geom_roc(labels = FALSE) + style_roc() + ggtitle("A")
p2 <- ggplot(test, aes(d = D, m = M2)) + geom_roc(labels = FALSE) + style_roc() + ggtitle("B")
p3 <- ggplot(test, aes(d = D, m = M3)) + geom_roc(labels = FALSE) + style_roc() + ggtitle("C")

grid.arrange(p1, p2, p3, nrow = 1)



## ----coords-lets-roc----------------------
coords(lets_roc, "best")


## 
## 1. Die logistische Regression ist eine Regression für dichotome Kriterien.

## 1. Unter einer ~~Olive~~Ogive versteht man eine "s-förmige" Kurve.

## 3. Berechnet man eine "normale" (OLS-)Regression bei einem dichotomen Kriterium, so kann man Wahrscheinlichkeiten < 0 oder > 1 erhalten, was unlogisch ist.

## 4. Ein Logit ist definiert als der Einfluss eines Prädiktors in der logistischen Regression. Der Koeffizient berechnet sich als Logarithmus des Wettquotienten.

## 5. Das AIC ein Gütemaß, welches man bei der logistischen Regression generell vermeidet.

## 6. Eine Klassifikation kann vier Arten von Ergebnissen bringen -- gemessen an der Richtigkeit des Ergebnisses.

## 7. Der positive Vorhersagewert ist definiert als der Anteil aller richtig-positiven Klassifikationen an allen als positiv klassifizierten Objekten.

## 8. Man möchte eine einfache logistische Regression (d.h. einen Prädiktor) visualisieren. Stellt man auf der X-Achse den Prädiktor und auf der Y-Achse die Logits des Kriteriums dar, so wird eine Gerade abgebildet werden, wenn man die Vorhersagewerte einzeichnet.

## 9. Cohens Kappa > 1 ist ein Indiz für eine gute Vorhersage.

## 10. Je größer die Fläche unter der ROC-Kurve, desto schlechter ist die Vorhersage des Modells.

## 

## ----exp-coef-glm1-2----------------------
wskt1 <- predict(glm1, data.frame(study_time = 1), type = "response")
wskt2 <- predict(glm1, data.frame(study_time = 2), type = "response")


wskt2 - wskt1


## ----predict-glm1-------------------------
# mit dem vollständigen Modell berechnet
predict(glm1, data.frame(study_time = 1),
        type = "response")

predict(glm1, data.frame(study_time = 5),
        type = "response")


## ----glm4-no-eval, eval = FALSE-----------
## stats_test %>%
##   mutate(bestanden_fct = factor(bestanden)) -> stats_test
## glm4_fct <- glm(bestanden_fct ~ study_time + interessiert + self_eval,
##             family = binomial,
##             data = stats_test)
## broom::tidy(glm4_fct)

