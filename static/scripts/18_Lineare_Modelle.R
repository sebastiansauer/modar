## Lernziele:

## 
## - Wissen, was man unter linearen Modellen bzw. linearer Regression versteht

## - Die Annahmen der Regression überprüfen können

## - Regression mit kategorialen Prädiktoren durchführen können

## - Die Modellgüte bei der Regression bestimmen können

## - Interaktionen erkennen und ihre Stärke einschätzen können

## - Die Relevanz von Prädiktoren einschätzen können

## 

## ----libs-regr----------------------------
library(caret)  
library(tidyverse)  
library(gridExtra)  
library(modelr)  
library(broom)
library(mosaic)

data(stats_test, package = "pradadata")


## ----libs-regr-hidden---------------------
library(ggrepel)


## ----bsp-regression, fig.cap = "Beispiel für eine Regression"----
stats_test %>%
  ggplot() +
  aes(x = study_time, y = score) +
  geom_jitter(alpha = .3) +
  geom_smooth(method = "lm")


## ----bsp-regression-lm, echo = FALSE------
lm_bsp1 <- lm(score ~ study_time, data = stats_test) %>% tidy


## Nimm `stats_test` UND DANN

## starte ein neues Diagramm mit ggplot UND DANN

## definiere das Diagramm (X-Achse, Y-Achse) UND DANN

## zeichne das Geom "jitter" (verwackeltes Punktediagramm) UND DANN

## zeichne eine Glättungslinie vom Typ "lineares Modell" (`lm`) ein.

## 

## ----bsp-regression2, echo = FALSE, fig.cap = "Zwei weitere Beispiele für Regressionen", fig.asp = .3, out.width = "100%"----

stats_test %>%
  ggplot() +
  aes(x = self_eval, y = score) +
  geom_jitter(alpha = .2) +
  geom_smooth(method = "lm", se = FALSE) -> p1


stats_test %>%
   ggplot +
  aes(x = interest, y = score) +
  geom_jitter(alpha = .2) +
  geom_smooth(method = "lm", se = FALSE) -> p2


gridExtra::grid.arrange(p1, p2, ncol = 2)




## ----lm1-stats-test-----------------------
lm1 <- lm(score ~ study_time, data = stats_test)
tidy(lm1)


## ----eval = FALSE-------------------------
## lm(kriterium ~ praediktor, data = meine_datentabelle)


## ----eval = FALSE-------------------------
## mein_lm <- lm(kriterium ~ praediktor, data = meine_datentabelle)
## tidy(mein_lm)


## ----eval = FALSE-------------------------
## lm(kriterium ~ praediktor, data = meine_datentabelle) %>% tidy()


## Je kürzer die "Abweichungslinien", desto besser die Vorhersage.

## 

## ----eval = FALSE-------------------------
## RMSE <- sqrt(mean((df$pred - df$obs)^2))


## ---- eval = FALSE------------------------
## caret::postResample(pred = df$predicted, obs = df$y_werte)


## ----eval = FALSE-------------------------
## R2 <- 1 - sum((df$pred - df$obs)^2) / sum((mean(df$obs) - df$obs)^2)


## ----rsquared-svg, echo = FALSE, fig.cap = "Sinnbild für den Determinationskoeffizienten. (Quelle: Orzetto 2010"----

knitr::include_graphics("images/regr/Coefficient_of_Determination.pdf")



## Verwendet man die Korrelation ($r$) oder $R^2$ als Gütekriterium, so sollte man sich über folgenden Punkt klar sein: Ändert man die Skalierung der Variablen, ändert sich die Korrelation *nicht*; das gilt auch für $R^2$. Beide Koeffizienten zielen allein auf das *Muster* der Zusammenhänge ab -- nicht die Größe der Abstände. Aber häufig ist die Größe der Abstände zwischen beobachteten und vorhergesagten Werten das, was uns interessiert. In dem Fall wäre der *(R)MSE* vorzuziehen. Auch perfekte Korrelation heißt nicht, dass vorhergesagte und beobachtete Werte identisch sind.

## 

## -----------------------------------------
mein_lm <- lm(score ~ study_time, data = stats_test)
tidy(mein_lm)


## ----unname-lm2, echo = FALSE-------------
mein_lm_coeff2 <- mein_lm$coefficients[2] %>% unname
mein_lm_coeff1 <- mein_lm$coefficients[1] %>% unname


## Der Einfluss des Prädiktors `study_time` steht in der Spalte 'estimate' und der Zeile `study_time`. Er wird als "estimate" bezeichnet, weil er zugleich der Schätzwert ist für den "wahren" Wert des Regressionsgewichts in der Population (der Parameter). Der Kriteriumswert, wenn der Prädiktor Null ist, steht in der Zeile '(Intercept)', ebenfalls in der Spalte `estimate`. Auf Deutsch spricht man hier vom *Achsenabschnitt*\index{Achsenabschnitt, Intercept}. Der Standardfehler `std.error` schätzt die Genauigkeit des Parameters (des Regressionsgewichts bzw. des Achsenabschnitts). Ob der Parameter signifikant von null verschieden ist, gibt die Spalte mit dem *p*-Wert an.

## 

## ----stats-test-scatter2, eval = FALSE, fig.cap = "Streudiagramm von Lernzeit und Klausurerfolg", echo = FALSE----
## ggplot(data = stats_test) +
##   aes(y = score, x = study_time) +
##   geom_jitter(alpha = .3) +
##   geom_smooth(method = "lm")


## ----predict-mein-lm----------------------
predict(mein_lm, data.frame(study_time = 4))


## ----glacne-mein-lm-----------------------
glance(mein_lm)


## ----mse-mein-lm--------------------------
MSE <- mean(mein_lm$residuals^2)
MSE
sqrt(MSE)


## ----p-erklaeren, echo = FALSE, fig.cap = "Verdeutlichung, was 'Erklären' im Rahmen eines Regressionsmodells bedeutet", out.width = "100%", fig.asp = .3----

stats_test %>%
  mutate(delta = (score - mean(stats_test$score, na.rm = TRUE))^2) %>%
  add_residuals(mein_lm) %>%
  mutate(resid = resid^2) %>%
  select(delta, resid) %>%
  gather(key = "Modell", value = "Abweichung") %>%
  mutate(Modell = dplyr::recode(Modell,
                                delta = "Nullmodell",
                                resid = "Regressionsmodell")) -> stats_test_regr


stats_test_regr %>%
  ggplot() +
  aes(x = Abweichung, fill = Modell) +
  geom_density(alpha = .7) +
  theme(legend.position = c( .7, .5)) +
  scale_fill_viridis_d(name = "Modell",
                      labels = c("Nullmodell", "Regression")) +
  coord_cartesian(xlim = c(0, 0.2)) -> p_regr1


stats_test_regr %>%
  ggplot() +
  aes(x = Modell, y = Abweichung) +
  geom_boxplot() +
  ylim(0, 0.2) -> p_regr2



gridExtra::grid.arrange(p_regr1, p_regr2, nrow = 1)





## ----echo = FALSE, eval = FALSE-----------
## knitr::include_graphics(paste0(path_to_images,"regr/p_delta_regr.pdf"))
## 


## ----resid-distrib, fig.cap = "Prüfung von Annahmen des Regressionsmodells", out.width = "100%"----
stats_test %>%
  add_residuals(mein_lm) %>%
  ggplot +
  aes(x = resid) +
  geom_histogram() -> p_assumption1

stats_test %>%
  add_predictions(mein_lm) %>%
  add_residuals(mein_lm) %>%
  ggplot() +
  aes(y = resid, x = pred) +
  geom_jitter(alpha = .2, width = .005) +
  geom_boxplot(aes(group = pred), alpha = .7) -> p_assumption2

gridExtra::grid.arrange(p_assumption1, p_assumption2, nrow = 1)



## -----------------------------------------
stats_test %>%
  add_predictions(mein_lm) %>%
  select(pred) %>%
  head


## -----------------------------------------
stats_test$interessiert <- stats_test$interest > 3


## Dichotomisieren von quantitativen Variablen wird von Statistikern mit Exkommunikation bestraft (hab ich gehört): Es verschleudert grundlos Information. Wir dichotomisieren hier nur zu Demonstrationszwecken.

## 

## ----score-interesse----------------------
stats_test %>%
  drop_na(score, interessiert) %>%
  group_by(interessiert) %>%
  summarise(score = mean(score)) -> score_interesse

score_interesse


## ----score-interesse-diff, echo = FALSE----
diff(score_interesse$score) -> diff_interesse


## ----fig-interessierte, fig.cap = "Der Unterschied im Klausurerfolg zwischen Interessierten und Nicht-Interessierten"----
stats_test %>%
  drop_na() %>%
  ggplot() +
  aes(x = interessiert, y = score) +
  geom_boxplot(width = .1) +
  geom_jitter(width = .1, alpha = .1) +
  geom_point(data = score_interesse,
             color = "red",
             size = 5,
             shape = 17) +
  geom_line(data = score_interesse,
            group = 1,
            color = "red")


## ----lm2----------------------------------
lm2 <- lm(score ~ interessiert, data = stats_test)
tidy(lm2)
glance(lm2)


## ----lm3----------------------------------
lm3 <- lm(score ~ study_time + interessiert, data = stats_test)
tidy(lm3)


## ----no-interakt, echo = FALSE, fig.cap = "Eine multivariate Analyse (hier ohne Interaktionseffekte) fördert Einsichten zu Tage, die bei einfacheren Analysen verborgen bleiben", fig.asp = 1, out.width = "100%"----

library(viridis)

ni_ypos <- predict(lm3, newdata = data.frame(study_time = 2, interessiert = FALSE))
i_ypos <- predict(lm3, newdata = data.frame(study_time = 5, interessiert = TRUE))


tidy(lm3)


df1 <- data_frame(
  interessiert = c(T, F),
  study_time = c(5, 2),
  score = c(i_ypos, ni_ypos),
  hjust = c(1, 0),
  label = c("interessiert", "nicht interessiert")
)

stats_test %>%
  #drop_na(interessiert) %>%
  ggplot +
  aes(x=study_time, y = score, color = interessiert) +
  # geom_boxplot(aes(x = factor(study_time))) +
  geom_jitter(width = .2, alpha = .1, color = "grey20") +
  #facet_wrap(~interessiert) +
  geom_abline(intercept = tidy(lm3)$estimate[1], slope = tidy(lm3)$estimate[2],
              color = "#440154ff",
              size = 2) +
  geom_abline(intercept = tidy(lm3)$estimate[1] + tidy(lm3)$estimate[3], slope = tidy(lm3)$estimate[2],
              color = "#fde725ff",
              size = 2) +
  #geom_smooth(method = "lm", se = FALSE) +
  #geom_abline(data = df1, aes(slope = slope, intercept = intercept,
                              #color = interessiert)) +
  #guides(color = FALSE) +
  #scale_color_viridis(discrete = TRUE) +
  labs(title = "A") +
  geom_label(data = df1,
                   aes(label = label,
                       x = study_time,
                       hjust = hjust),
                   #segment.color = NA,
                  # hjust = 0,
                   #nudge_x = 3,
                   color = c("#fde725ff", "#440154ff")) +
  theme(legend.position = "none") +
  coord_cartesian(ylim = c(.6, .9)) -> p_mult_regr1

df2 <- data_frame(
  slope = rep(-.33, 5),
  intercept = rep(c(24), 5),
  study_time = 1:5)


p_mult_regr2 <- stats_test %>%
  select(interessiert, study_time, score) %>%
  na.omit %>%
  filter(study_time %in% c(1,5)) %>%
  ggplot +
  aes(x=interessiert, y = score, color = interessiert) +
  geom_jitter(width = .1, alpha = .8) +
  facet_wrap(~study_time) +
  # geom_abline(data = df2, mapping = aes(slope = slope, intercept = intercept, color = study_time)) +
  scale_color_viridis(discrete = TRUE) +
  guides(color = FALSE) +
  labs(title = "B",
       caption = "Nur Werte 1 und 5 von study_time sind dargestellt. Die Rauten symbolisieren den Median.") +
  stat_summary(fun.y = "median", shape = 17, fun.args = list(na.rm = TRUE), size = 6, geom = "point", color = "grey40") +
  stat_summary(fun.y = "median", shape = 17, fun.args = list(na.rm = TRUE), size = 1, geom = "line", color = "grey40", group = 1)


gridExtra::grid.arrange(p_mult_regr1, p_mult_regr2, nrow = 2,
                        bottom = "keine Interaktion")


## Die multivariate Analyse zeigt ein anderes Bild, ein genaueres Bild als die einfachere Analyse mit weniger Prädiktoren. So kann ein Sachverhalt, der für den  Datensatz als Ganzes gilt, in seinen Subgruppen anders sein.

## Beachten Sie, dass sich die Werte (und Vorzeichen) eines Prädiktors ändern können, wenn sich die Gleichung des Regressionsmodells ändert; wenn also Prädiktoren hinzukommen oder entfernt werden.

## 

## Von einem Interaktionseffekt spricht man, wenn die Wirkung eines Prädiktors (auf das Kriterium) abhängig ist von einem anderen Prädiktor. Bildlich gesprochen: Wenn die Regressionsgeraden nicht parallel sind. Umgangssprachlich liegt ein Interaktionseffekt dann vor, wenn die Antwort auf die Frage "Hat $X$ einen Einfluss auf $Y$?" lautet: "Kommt drauf an." Und zwar auf den Wert einer dritten Variablen, $Z$.

## 

## -----------------------------------------
lm4 <- lm(score ~ interessiert + study_time + interessiert:study_time,
          data = stats_test)
tidy(lm4)


## ----my-coef, echo = FALSE----------------
my_coef <- round(tidy(lm4)$estimate[4], 2)


## ----interakt-stats-test, echo = FALSE, fig.cap = "Eine Regressionsanalyse mit Interaktionseffekten", fig.asp = .5, out.width = "100%"----

stats_test_nona <- drop_na(stats_test)

df1 <- data_frame(
  interessiert = c(F, T),
  slope = c(2.44, 2.44-.32),
  intercept = c(23.6, 23.6+.66)
)


stats_test_nona %>%
  split(.$interessiert) %>%
  map_dfr(~data_frame(score = lm(score ~ study_time , data = .) %>% predict(newdata = data.frame(study_time = 5)))) %>%
  mutate(interessiert = c(FALSE, TRUE),
         study_time = 5,
         label = c("nicht interessiert", "interessiert")) -> labels_df_old


ni_ypos_lm4 <- predict(lm4, newdata = data.frame(study_time = 5, interessiert = FALSE))
i_ypos_lm4 <- predict(lm4, newdata = data.frame(study_time = 5, interessiert = TRUE))

labels_df <- data_frame(
  interessiert = c(T, F),
  study_time = c(5, 5),
  score = c(i_ypos_lm4, ni_ypos_lm4),
  label = c("interessiert", "nicht interessiert")
)

#
# lm(score ~ study_time + interessiert, data = stats_test_nona) %>% predict(newdata = data.frame(study_time = 5, interessiert = TRUE))
# lm(score ~ study_time + interessiert, data = stats_test_nona) %>% predict(newdata = data.frame(study_time = 5, interessiert = FALSE))
#

pinterakt1 <- stats_test_nona %>%
  ggplot() +
  aes(x=study_time, y = score, color = interessiert) +
  geom_jitter(width = .2, alpha = .1, color = "grey20") +
  geom_smooth(method = "lm", se = FALSE) +
  geom_label(data = labels_df,
             aes(label = label)) +
  labs(title = "A") +
  coord_cartesian(xlim = c(0, 6), ylim = c(0.6, 1), expand = TRUE) +
  theme(legend.position = "none")



df2 <- stats_test %>%
  data_grid(
    study_time = 1:5,
    interessiert = c(T, F),
  ) %>%
  add_predictions(lm4) %>%
  rename(score = pred)




pinterakt2 <- stats_test %>%
  na.omit %>%
  ggplot +
  aes(x=interessiert, group = study_time, y = score) +
  geom_jitter(width = .1, alpha = .1) +
  geom_line(data = df2, aes(y = score, color = factor(study_time))) +
  theme(legend.position = "none") +
  labs(title = "B",
       caption = 'Jede Linie entspricht einer Stufe von "study_time"') +
  coord_cartesian(ylim = c(.5, 1)) +
  geom_label(data = filter(df2, interessiert == TRUE),
             aes(label = study_time),
             x = 2)


gridExtra::grid.arrange(pinterakt1, pinterakt2, nrow = 1,
                        bottom = "Mit Interaktion")


## ----center-study-time--------------------
stats_test %>%
  mutate(study_time_centered = study_time -
           mean(study_time, na.rm = TRUE)) -> stats_test



## -----------------------------------------

lm5 <- lm(score ~ interessiert + study_time_centered +
            interessiert:study_time,
          data = stats_test)
tidy(lm5)


## -----------------------------------------
summary(lm2)$r.squared
summary(lm3)$r.squared
delta <- summary(lm3)$r.squared - summary(lm2)$r.squared
delta


## ----beta-stand---------------------------
QuantPsyc::lm.beta(lm1)

stats_test %>%
  mutate_at(c("study_time", "score"), funs(z = scale(.))) -> stats_test

lm1_z <- lm(score_z ~ study_time_z, data = stats_test)
tidy(lm1_z)


## ----lm-overfitting1----------------------
stats_test %>%
  select(score, study_time) %>%
  drop_na %>%
  mutate_if(is_integer, as.numeric) -> stats_test_lm4

lm4 <- lm(score ~ ., data = stats_test_lm4)
glance(lm4)


## ----lm-overfitting2----------------------
lm4_train <- stats_test_lm4 %>%
  sample_frac(.8, replace = FALSE)  # Stichprobe von 80%, ohne Zurücklegen

lm4_test <- stats_test_lm4 %>%
  anti_join(lm4_train)  # Alle Zeilen von "df", die nicht in "train" vorkommen

lm_train <- lm(score ~ study_time, data = lm4_train)


## ----lm-overfitting-predict---------------
lm_predict <- predict(lm_train, newdata = lm4_test)


## Speichere unter dem Namen `lm2_predict` das Ergebnis folgender Berechnung:

## Mache eine Vorhersage ("to predict") anhand des Modells `lm_train`,

## wobei frische Daten, d.h. die Test-Stichprobe (`newdata = lm4_test`) verwendet werden sollen.

## 

## ----R2-postresample----------------------
caret::postResample(pred = lm_predict, obs = lm4_test$score)



##  Die Modellgüte in der Test-Stichprobe ist meist schlechter als in der Trainings-Stichprobe. Das warnt uns vor Befunden, die naiv nur die Werte aus der Trainings-Stichprobe berichten. Bei flexibleren Modellen ist der Unterschied tendenziell stärker als bei einfachen Modellen wie der linearen Regression.

## 

## ----lm4-boot-b1-no-eval, eval = TRUE-----
set.seed(42)
lm4_boot_b <- do(10000) * lm(score ~ study_time,
                             data = mosaic::resample(stats_test_nona))

lm4_boot_b %>%
  select(Intercept, study_time) %>%
  gather(key = coefficient, value = value) %>%
  ggplot(aes(x = value)) +
  geom_histogram() +
  facet_wrap(~coefficient, scales = "free_x") -> p_lm_boot1
p_lm_boot1


## ----lm4-boot-ci, echo = FALSE, fig.cap = 'Konfidenzintervalle für die Modellparameter eines Regressionsmodells', out.width="70%"----
knitr::include_graphics("images/regr/p_lm_boot1.pdf")


## -----------------------------------------
quantile(~Intercept, data = lm4_boot_b, prob = c(.025, .5, .975))
quantile(~study_time, data = lm4_boot_b, prob = c(.025, .5, .975))

confint(mein_lm)


## ----lm4-boot-b2, fig.cap = "Visualisierung der 1000 via Bootstrap simulierten Regressionsgeraden"----
lm4_boot_b %>%
  #head() %>%
  #select(Intercept, study_time) %>%
  #gather(key = coefficient, value = value) %>%
  ggplot(aes(x = value)) +
  geom_abline(aes(slope = study_time, intercept = Intercept),
              alpha = .01) +
  scale_x_continuous(limits = c(1, 5)) +
  scale_y_continuous(limits = c(.6, .9))


## -----------------------------------------
quantile(~r.squared, data = lm4_boot_b, prob = c(.055, .5, .945))


## 1. X-Wert: Kriterium; Y-Wert: Prädiktor.

## 2. Der Y-Wert in der einfachen Regression wird berechnet als Achsenabschnitt plus *x*-mal die Geradensteigung.

## 3. $R^2$ liefert einen *relativen* Vorhersagefehler  (relativ im Sinne eines Anteils) und *MSE* einen *absoluten*.

## 4. Unter 'Ordinary Least Squares' versteht man eine abschätzige Haltung gegenüber Statistik.

## 5. Zu den Annahmen der Regression gehört die Normalverteilung der Kriteriumswerte.

## 6. Die Regression darf nicht bei kategorialen Prädiktoren verwendet werden.

## 7. Mehrere bivariate Regressionsanalysen (1 Prädiktor, 1 Kriterium) sind einer multivariaten Regression i.d.R. vorzuziehen (unter sonst gleichen Bedingungen).

## 8. Interaktionen erkennt man daran, dass die Regressionsgeraden *nicht* parallel sind.

## 9. Im Standard von `lm()` werden Interaktionseffekte berechnet.

## 10. Stellen Sie sich vor, Sie haben in zwei linearen Modellen jeweils den Einfluss eines Prädiktors berechnet; also für jeden der beiden Prädiktoren eine einfache Regression berechnet. Jetzt vergleichen Sie deren Einflussgewichte mit den beiden Einflussgewichten, die sich aus einer multiplen Regression ergeben, wenn also beide Prädiktoren zugleich in ein lineares Modell eingegeben werden. Dann ist es möglich, dass sich die Einflussgewichte aus der einfachen Regression von den Einflussgewichten unterscheiden, die sich aus der multiplen Regression ergeben (wenn sich sonst nichts ändert).

## 
