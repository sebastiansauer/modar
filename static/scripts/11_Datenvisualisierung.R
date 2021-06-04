## ----plot-vis, echo = FALSE---------------
knitr::include_graphics("images/visualisieren/Visualisieren-crop.pdf")


## Lernziele:

## 
## - An einem Beispiel erläutern können, warum/wann ein Bild mehr sagt als 1000 Worte

## - Häufige Arten von Diagrammen erstellen können

## - Diagramme bestimmten Zwecken zuordnen können

## - Die Syntax von `ggplot2` in Grundzügen beherrschen

## - Beispiele für optisch ansprechende und weniger ansprechende Diagramme aufzeigen können

## 

## ----libs-visualisieren-------------------
library(tidyverse)  
library(mosaic)

data(flights, package = "nycflights13")
data(movies, package = "ggplot2movies")
data(profiles, package = "okcupiddata")
data(stats_test, package = "pradadata")
data(wo_men, package = "pradadata")


## ----libs-hidden-vis, echo = FALSE--------
library(gridExtra)


## ----anscombe-compute, echo = FALSE-------
data(anscombe)
options(max.print = 20)

anscombe %>%
  summarise_all(funs(mean, sd))

anscombe %>%
  summarise(cor1 = cor(x1, y1),
            cor2 = cor(x2, y2),
            cor3 = cor(x3, y3),
            cor4 = cor(x4, y4))


## ----fig-anscombe, echo = FALSE, fig.cap = "Das Anscombe-Quartett"----
knitr::include_graphics("images/visualisieren/anscombe-crop.pdf")


## ----ggplot-demo, echo = FALSE, fig.cap = "Beispiele für Diagramme mit ggplot2", out.width = "100%"----

knitr::include_graphics("images/visualisieren/ggplot-demo.png")


## ----fig-anatomie, echo = FALSE, fig.cap = "Anatomie eines Diagramms"----
knitr::include_graphics("images/visualisieren/anatomie_diagramm_crop.pdf")


## ----ggplots-scales, echo = FALSE, fig.cap = "Sinnbild der Zuordnung von Spalten zu Attributen eines Diagramms"----
knitr::include_graphics("images/visualisieren/ggplot_scales-crop.pdf")


## 1. Welche Variable steht auf der X-Achse?

## 2. Welche Variable steht auf der Y-Achse?

## 3. Was wird gemalt? Linien, Boxplots, Punkte?

## 4. Wie heißt der Datensatz, aus dem die Daten gezogen werden?

## 

## ----figmovies-prep, message = FALSE, echo = FALSE----
p_mov1 <- qplot(x = year,
      y = budget,
      geom = "point",
      data = movies)

p_mov2 <- qplot(x = factor(year),
      y = budget,
      geom = "boxplot",
      data = movies)



## ----figmovies, echo = FALSE, fig.cap = "Mittleres Budget pro Jahr", eval = TRUE, out.width = "100%"----
gridExtra::grid.arrange(p_mov1, p_mov2, nrow = 1)



## `qplot`: Erstelle schnell (q wie quick in `qplot`) mal einen Plot (engl. "plot": Diagramm).

## `x`: Der X-Achse soll die Variable "year" zugeordnet werden.

## `y`: Der Y-Achse soll die Variable "budget" zugeordnet werden.

## `geom`: ("geometriches Objekt") Gemalt werden sollen Punkte, und zwar

## pro Beobachtung (hier: Film) ein Punkt; nicht etwa Linien oder Boxplots.

## `data`: Als Datensatz bitte `movies` verwenden.

## 

## ----p-mov-no-eval-boxplot, eval = FALSE----
## qplot(x = factor(year),
##       y = budget,
##       geom = "boxplot",
##       data = movies)


## ----movies-mutate-jahrzehnt--------------
movies %>%  
  mutate(Jahrzehnt = year / 10) %>%
  mutate(Jahrzehnt = trunc(Jahrzehnt)) %>%  # trunkieren, abrunden
  mutate(Jahrzehnt = Jahrzehnt * 10) %>%
  mutate(Jahrzehnt = factor(Jahrzehnt)) -> movies


## ----fig-movies-jahrzehnt, fig.cap = "Film-Budgets über die die Jahrzehnte"----
qplot(x = Jahrzehnt,
      y = budget,
      geom = "boxplot",
      group = Jahrzehnt,
      data = movies) + geom_smooth(aes(group = 1), se = FALSE)


## ----qplot-blaupause, eval = FALSE--------
## qplot (x = X_Achse,
##        y = Y_Achse,
##        data = mein_dataframe,
##        geom = "ein_geom")


## ----fig-movies-jahrzehnt-mosaic, fig.cap = "Ein ggplot-Diagramm erstellt mit mosaic", eval = FALSE----
## gf_boxplot(budget ~ factor(Jahrzehnt), data = movies)


## ----eval = FALSE-------------------------
## gf_XXX(y_achse ~ x_achse, data = meine_tabelle)


## ----movie-budget-jahrzehnt-mosaic, eval = FALSE----
## gf_boxplot(budget ~ factor(Jahrzehnt), data = movies) %>%
##   gf_point(stat = "summary", color = "red", shape = 17)


## ----fig-budget-movies-no-eval, eval = FALSE----
## qplot(x = budget, data = movies)


## ----fig-budget-movies_mov3_mov4, echo = FALSE----
p_mov3 <- qplot(x = budget, data = movies)
p_mov4 <- qplot(x = budget, geom = "density", data = movies)



## ----fig-budget-movies, fig.cap = "Verteilung des Budgets von Filmen mit einem Histogramm", eval = TRUE, echo = FALSE, out.width = "100%"----
gridExtra::grid.arrange(p_mov3, p_mov4, nrow = 1)



## Was heißt das kleine 'e', das man bei wissenschaftlichen Zahlen hin und wieder sieht (wie im Diagramm \@ref(fig:fig-budget-movies))?

## 
## Zum Beispiel: `5.0e+07`. Das $e$ sagt, wie viele Stellen im Exponenten (zur Basis 10) stehen: hier $10^{07}$. Eine große Zahl -- eine $1$ gefolgt von *sieben* Nullen: 10000000. Die schöne Zahl soll noch mit 5 multipliziert werden: also 50000000. Bei so vielen Nullern kann man schon mal ein Flimmern vor den Augen bekommen. Daher ist die "wissenschaftliche" Notation ganz praktisch, wenn die Zahlen sehr groß (oder sehr klein) werden. Sehr kleine Zahlen werden mit dieser Notation so dargestellt: `5.0e-07` heißt $\frac{1}{10^7}$. Eine Zahl sehr nahe bei Null. Das Minuszeichen zeigt hier, dass wir den Kehrwert von `5.0e+07` nehmen sollen.

## 

## ----p1, echo = FALSE---------------------
p_budget_rating <- qplot(x = budget, y = rating, data = movies)


## ----dots2--------------------------------
p_budget_rating_lm <- p_budget_rating + geom_smooth(method = "lm")


## ----p-dots, echo = FALSE, fig.cap = "Visualisierung zweier metrischer Variablen", out.width = "100%"----
p_hex <- ggplot(movies) +
          aes(x = year, y = budget) +
          geom_hex()
gridExtra::grid.arrange(p_budget_rating_lm, p_hex, nrow = 1)


## ----eval = FALSE-------------------------
## movies %>%
##   ggplot() +
##   aes(x = budget, y = rating) +
##   geom_point() +
##   geom_smooth(method = "lm")


## Innerhalb einer ggplot-Syntax dürfen Sie die Pfeife nicht verwenden; trennen Sie die einzelnen Teilbefehle mit einem Pluszeichen.

## 

## ----flights-hexbin, eval = FALSE---------
## p_hex <- ggplot(movies) +
##           aes(x = year, y = budget) +
##           geom_hex()


## ----movies-facets, fig.cap = "Facettierung eines Diagramms", out.width = "100%"----
movies %>%
  filter(Jahrzehnt %in% c("1990", "2000")) %>%
  ggplot(aes(x = budget, y = rating, color = Jahrzehnt)) +
  geom_point(alpha = .5) +
  facet_wrap(~Jahrzehnt) +
  geom_smooth() +
  geom_rug() +
  theme(legend.position = "none")


## ----bar1-no-eval, eval = FALSE-----------
## stats_test %>% drop_na(interest, score) -> stats_test
## p_bar1 <- qplot(x = bestanden, data = stats_test)
## p_bar1


## ----pbar1, echo = FALSE, out.width = "100%", fig.cap = "Visualisierung von Häufigkeiten mit Balkendiagrammen"----
stats_test %>% drop_na(interest, score) -> stats_test
p_bar1 <- qplot(x = bestanden, data = stats_test)

p_bar2 <- qplot(x = bestanden, fill = factor(interest),
                data = filter(stats_test, interest %in% c(1, 6)))
gridExtra::grid.arrange(p_bar1, p_bar2, nrow = 1)


## ----pbar2-no-eval, eval = FALSE----------
## p_bar2 <- qplot(x = bestanden, fill = factor(interest),
##                 data = stats_test)
## p_bar2


## ----pbar3-no-eval, eval = FALSE----------
## p_bar3 <- stats_test %>%
##   filter(interest == 1 | interest == 6) %>%
##   mutate(interest = factor(interest)) %>%
##   ggplot() +
##   aes(x = bestanden, fill = interest) +
##   geom_bar(position = "fill")  +
##   labs(fill = "Interesse",
##        y = "Anzahl")
## p_bar3
## 
## mosaicplot(interest ~bestanden, data = stats_test,
##            main = NULL)


## ----p-bar, echo = FALSE, fig.cap = "Varianten von Balkendiagramme für Häufigkeiten", out.width = "100%", eval = FALSE----
## gridExtra::grid.arrange(p_bar1,
##                         p_bar2 + labs(fill = "Interesse"),
##                         p_bar3,
##                         nrow = 1)


## ----pbar3, echo = FALSE, fig.cap = "Balkendiagramme, um Anteile darzustellen", out.width = "100%"----
# #file.exists("images/visualisieren/mosaic_statstest.png")
# imgs <- c("images/visualisieren/prop_plot.png",
#           "images/visualisieren/mosaic_statstest.png")
# prada::comb2pngs(imgs)

knitr::include_graphics("images/visualisieren/pbar3.png")


## `stats_test`:  Hey R, nimm den Datensatz `stats_test` UND DANN

## `ggplot()` : Hey R, male ein Diagramm von Typ `ggplo`t (mit dem Datensatz aus dem vorherigen Pfeifen-Schritt, d.h. aus der vorherigen Zeile, also `stats_test`)!

## `filter`: wir wollen nur Zeilen (Studenten), für die gilt `interest == 1` oder `interest == 6`. Der horizontale Strich heißt 'oder'.

## `+`:  Das Pluszeichen grenzt die Teile eines ggplot-Befehls voneinander ab.

## `aes`:  von "aethetics", also welche Variablen des Datensatzes den sichtbaren Aspekten (v.a. Achsen, Farben) zugeordnet werden.

## `x`: Der X-Achse (Achtung, `x` wird klein geschrieben hier) wird die Variable `bestanden` zugeordnet.

## `y`: gibt es nicht??? Wenn in einem ggplot-Diagramm *keine* Y-Achse definiert wird, wird ggplot automatisch ein Histogramm bzw. ein Balkendiagramm erstellen. Bei diesen Arten von Diagrammen steht auf der Y-Achse keine eigene Variable, sondern meist die Häufigkeit des entsprechenden X-Werts (oder eine Funktion der Häufigkeit, wie relative Häufigkeit).

## `fill` Das Diagramm (die Balken) sollen so gefüllt werden, dass sich die Häufigkeit der Werte von `interest` darin widerspiegelt.

## `geom_XYZ`: Als "Geom" soll ein Balken ("bar") gezeichnet werden.  Ein Geom ist in ggplot2 das zu zeichnende Objekt, also ein Boxplot, ein Balken, Punkte, Linien etc. Entsprechend wird das gewünschte Geom mit `geom_bar`, `geom_boxplot`, `geom_point` etc. gewählt.

## `position = fill`: `position_fill` will sagen, dass die Balken alle eine Höhe von 100% (1) haben, d.h. gleich hoch sind. Die Balken zeigen also nur die Anteile der Werte der `fill`-Variablen.

## 

##  Je unterschiedlicher die "Füllhöhe", desto stärker sind die Variablen (X-Achse vs. Füllfarbe) voneinander abhängig (d.h. desto stärker der Zusammenhang).

## 

## ----nom-vis-no-eval, eval = FALSE--------
## profiles %>%
##   dplyr::count(drinks, body_type) %>%
##   ggplot() +
##   aes(x = drinks, y = body_type, fill = n) +
##   geom_tile() +
##   theme(axis.text.x = element_text(angle = 90))


## ----nom-vis, fig.cap = "Visualisierung des Zusammenhangs nominaler Variablen", out.width = "100%", echo = FALSE----
data(profiles, package = "okcupiddata")
profiles %>%
  dplyr::count(drinks, body_type) %>%
  ggplot() +
  aes(x = drinks, y = body_type, fill = n) +
  geom_tile() +
  theme(axis.text.x = element_text(angle = 90)) -> p_nom1
p_nom1


## ----include = FALSE, eval = FALSE--------
## library(ggmosaic)
## profiles %>%
##   #dplyr::count(drinks, body_type) %>%
##   ggplot() +
##   geom_mosaic(aes(weight = drinks, x = product(body_type) ))
## 
## data(NHANES)
## ggplot(data = NHANES) +
##    geom_mosaic(aes(weight = Weight, x = product(SleepHrsNight, AgeDecade), fill=factor(SleepHrsNight)), na.rm=TRUE)


## Nimm den Datensatz "profiles" UND DANN

## Zähle die Kombinationen von "drinks" und "body_type" UND DANN

## Erstelle ein ggplot-Plot UND DANN

## Weise der X-Achse "drinks" zu, der Y-Achse "body_type" und der Füllfarbe "n" UND DANN

## Male Fliesen UND DANN

## Passe das Thema so an, dass der Winkel für Text der X-Achse auf 90 Grad steht.

## 

## -----------------------------------------
profiles %>%
  group_by(sex) %>%
  summarise(income_m = mean(income, na.rm = TRUE),
            income_sd = sd(income, na.rm = TRUE)) -> profiles_income_summary
profiles_income_summary


## ----profiles-income-summary-qplot-no-eval, eval = FALSE----
## profiles_income_summary %>%
##   qplot(x = sex, y = income, data = ., color = sex)


## ----profiles-income-summary-qplot, eval = TRUE, echo = FALSE, out.width = "100%", fig.cap = "Zusammenfassungen zeigen"----
profiles_income_summary %>%
  qplot(x = sex, y = income_m, data = ., size = I(5), color = sex) -> p1

profiles_income_summary %>%
  ggplot() +
  aes(x = sex, y = income_m) +
  geom_errorbar(aes(ymin = income_m - income_sd,
                    ymax = income_m + income_sd),
                width = 0.05) +
  geom_point(aes(color = sex), size = 5) +
  labs(caption = "Fehlerbalken zeigen die SD",
       x = "",
       y = "Mittleres Einkommen") -> p2

gridExtra::grid.arrange(p1, p2, nrow = 1)




## ----eval = FALSE-------------------------
## profiles_income_summary %>%
##   ggplot() +
##   aes(x = sex, y = income_m) +
##   geom_errorbar(aes(ymin = income_m - income_sd,
##                     ymax = income_m + income_sd)) +
##   geom_point(aes(color = sex),
##              size = 5,
##             show.legend = FALSE) +
##   labs(caption = "Fehlerbalken zeigen die SD",
##        x = "",
##        y = "Mittleres Einkommen")


## Nimm den Datensatz `profiles_income_summary` UND DANN

## 
## erstelle ein `ggplot`-Diagramm UND DANN

## 
## weise der X-Achse `sex` und der Y-Achse `income_m` zu UND DANN

## 
## zeichne einen Fehlerbalken UND DANN

## 
## zeichne Punkte, die nach Geschlecht gefärbt und von der Größe 5 sind.

## 

## Beachten Sie die unterschiedlichen Skalierungen der Y-Achsen in den zwei Teilbildern von Abbildung \@ref(fig:profiles-income-summary-qplot). Der numerische Wert des Unterschieds von Frauen und Männer hinsichtlich Gehalt ist jedes Mal gleich. Die Achsenskalierung suggeriert jedoch Gegenteiliges: Im einen Teilbild wirkt der Unterschied groß, im anderen klein.

## 

## ----eval = FALSE-------------------------
## qplot(x = sex,
##       y = income,
##       data = profiles,
##       geom = "violin")  # oder "boxplot""
## 
## profiles %>%
##   filter(between(income, 100, 100000)) %>%
##   ggplot(aes(x = sex, y = income)) +
##   geom_violin() +
##   geom_point(data = profiles_income_summary, aes(color = sex,
##                                                  y = income_m),
##              size = 5)


## ----profiles-details, echo = FALSE, fig.cap = "Informationsreiche Darstellung des Einkommens"----
qplot(x = sex,
      y = income,
      data = profiles,
      geom = "violin") -> p3

profiles %>%
  filter(between(income, 100, 100000)) %>%
  ggplot(aes(x = sex, y = income)) +
  geom_violin() +
  geom_point(data = profiles_income_summary, aes(color = sex,
                                                 y = income_m),
             size = 5)


## ----diagrammtypen, echo = FALSE----------

df <- read_csv("includes/Diagrammtypen.csv")


# library(pander)
# pander::cache.off()
# panderOptions("table.alignment.default", "left")
# pander::pander(df, caption = "Häufige Diagrammtypen")
knitr::kable(df, caption = "Häufige Diagrammtypen",
booktabs = T)





## ----fig-diagrammtypen, echo = FALSE, fig.cap = "Überblick über häufige Diagrammtypen", fig.asp = 1, out.width = "100%", cache = TRUE----
p0 <- ggplot2::ggplot(data = stats_test) +
  ggplot2::theme_minimal() +
  ggplot2::theme(text = element_text(size = 6),
        title = element_text(size = 8),
        legend.position = "none")

p1 <- p0 + aes(x = score) + geom_histogram() +
  labs(title = "Histogramm")


p2 <- p0 + aes(x = score) + geom_density() +
  labs(title = "Histogramm")


p3 <- p0 + aes(x = score, y = self_eval) +
  geom_point() +
  labs(title = "Streu")

p4 <- p0 + aes(x = score, y = self_eval, fill = "bestanden") +
  geom_smooth() +
  geom_point(color = "grey80") +
  labs(title = "Smooth")



p5 <-p0 + aes(x = bestanden) +
  geom_bar() +
  labs(title = "Balken (Rohwerte)")


p6 <- stats_test %>%
  mutate(interessiert = stats_test$interest > 3) %>%
  count(bestanden, interessiert) %>%
  ggplot() +
  aes(x = bestanden, y = n, fill = interessiert) +
  geom_col(position = "fill") +
  theme_minimal() +
  theme(text = element_text(size = 6),
        title = element_text(size = 8),
        legend.position = "none") +
  labs(title = "Balken (Anteile)")




p7 <- stats_test %>%
  mutate(interessiert = stats_test$interest > 3) %>%
  group_by(bestanden, interessiert) %>%
  summarise(score_median_gruppe = median(score)) %>%
  ggplot(aes( x = bestanden, y = score_median_gruppe,
              color = interessiert,
              shape = interessiert),
         size = 6) +
  geom_point(alpha = .7) +
  labs(title = "Punkte (Mittelwerte)") +
  theme_minimal() +
  theme(text = element_text(size = 6),
        title = element_text(size = 8),
        legend.position = "none")

p8 <- p0 + aes(x = bestanden, y = score) +
  geom_boxplot() +
  labs(title = "stats_test")


p9 <- p0 +  aes(x = interest, y = score) +
  stat_summary(fun.y = "mean", geom = "line") +
  labs(title = "Linie")



grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, nrow = 3)



## ----eval = FALSE-------------------------
## movies %>%
##   filter(Jahrzehnt %in% c("1990", "2000")) %>%
##   ggplot(aes(x = budget, y = rating, color = Jahrzehnt)) +
##   geom_point(data = select(movies, -Jahrzehnt), color = "grey80") +
##   geom_point(alpha = .5) +
##   facet_wrap(~Jahrzehnt) +
##   geom_smooth() +
##   #geom_rug() +
##   scale_color_viridis(discrete = TRUE)


## ----eval = FALSE-------------------------
## qplot(x = score, data = stats_test)  # identisch zu
## qplot(x = score, data = stats_test, geom = "bar")
## 


## ----eval = FALSE-------------------------
## qplot(x = score, data = stats_test)  # identisch zu
## qplot(x = score, data = stats_test, geom = "histogram")


## ----eval = FALSE-------------------------
## qplot(x = score, y = self-eval, data = stats_test)  # identisch zu
## qplot(x = score, y=  self-eval, data = stats_test, geom = "point")


## ----eval = FALSE-------------------------
## #oh no:
## qplot(x = rating, y = affairs, geom = "boxplot", data = Affairs)
## 
## #oh yes:
## qplot(x = factor(rating), y = affairs, geom = "boxplot", data = Affairs)
## 
## #oh yes:
## qplot(x = gender, y = affairs, geom = "boxplot", data = Affairs)


## ----eval = FALSE-------------------------
## qplot(data = stats_test, x = bestanden, y = interest, geom = "boxplot")
## # ist identisch zu
## ggplot(data = stats_test) +
##   aes(x = bestanden, y = interest) +
##   geom_boxplot()


## ----learn-for-prada-ex-no-eval, eval = FALSE----
## #data(stats_test, package = "pradadata")
## 
## 
## stats_test %>%
##   filter(study_time %in% 1:5) %>%
##   select(bestanden, study_time, score) %>%
##   na.omit() %>%
##   ggplot(aes(x = factor(study_time), y = score)) +
##   geom_jitter(aes(color = bestanden), alpha = .56) +
##   geom_boxplot(alpha = .7) +
##   geom_smooth(aes(group = 1), method = "lm", se = FALSE) +
##   theme(legend.position = "bottom") +
##   labs(y = "Prozent richtiger Lösungen",
##        x = "Lernaufwand",
##        title = "Mehr Lernen, bessere Noten",
##        subtitle = "Der Zusammenhang von Lernzeit und Klausurerfolg",
##        caption = paste0("n = ", nrow(stats_test), " Studenten"))


## ----wo-men2------------------------------
wo_men %>%
  drop_na() %>%
  filter(between(height, 150, 210)) %>%
  filter(between(shoe_size, 35, 48)) -> wo_men2


## -----------------------------------------
wo_men2 %>%
  group_by(sex) %>%
  summarise(height = mean(height)) -> wo_men3


## ----eval = FALSE-------------------------
## wo_men2 %>%
##   ggplot() +
##   aes(x = sex, y = height) +
##   stat_summary(fun.data = "mean_cl_normal",
##                geom = "errorbar",
##                color = "grey40") +
##   geom_jitter(color = "grey80") +
##   geom_point(data = wo_men3, color = "red", size = 8) +
##   labs(x = "Geschlecht",
##        y = "Größe",
##        caption = "Fehlerbalken zeigen das 95%-KI des Mittelwerts")


## -----------------------------------------
Hmisc::smean.cl.normal(wo_men2$height)


## ----p1p2p3, echo = TRUE, out.width="100%", eval = FALSE----
## p1 <-
##   ggplot(wo_men2) +
##   aes(x = height, y = shoe_size) +
##   geom_point() +
##   labs(title = "geom_point")
## 
## p2 <-
##   ggplot(wo_men2) +
##   aes(x = height, y = shoe_size) +
##   geom_jitter() +
##   labs(title = "geom_jitter -\nverwackelt")
## 
## p3 <-
##   ggplot(wo_men2) +
##   aes(x = height, y = shoe_size) +
##   geom_smooth() +
##   labs(title = "geom_smooth")
## 
## p4 <-
##   ggplot(wo_men2) +
##   aes(x = height, y = shoe_size) +
##   geom_bin2d(bins = 10) +
##   labs(title = "geom_bin2d")
## 
## grid.arrange(p1, p2, p3, p4, ncol = 2)


## ----p1p2, echo = TRUE, out.width="100%", fig.asp = .7, eval = FALSE----
## p1 <-
## ggplot(wo_men2) +
##   aes(x = sex, y = height) +
##   geom_boxplot() +
##   labs(title = "geom_boxplot")
## 
## p2 <-
## ggplot(wo_men2) +
##   aes(x = sex, y = height) +
##   geom_violin() +
##   labs(title = "geom_violin")
## 
## p3 <-
##   ggplot(wo_men3, aes(x = sex, y = height)) +
##   geom_point(size = 7, aes(shape = sex), color = "firebrick", alpha = .7) +
##   geom_line(group = 1) +
##   geom_point(data = wo_men2, aes(x = sex)) +
##   labs(title = "Mit zusammengefasstem Datensatz") +
##   theme(legend.position = c(1, 1),
##         legend.justification = c(1, 1))
## 
## grid.arrange(p1, p2, p3, ncol = 1)


## ----fig-diagrammtypen-ex, eval = FALSE----
## p0 <- ggplot(data = stats_test) +
##   theme_minimal() +
##   theme(text = element_text(size = 6),
##         title = element_text(size = 8),
##         legend.position = "none")
## 
## p1 <- p0 + aes(x = score) + geom_histogram() +
##   labs(title = "Histogramm")
## 
## p2 <- p0 + aes(x = score) + geom_density() +
##   labs(title = "Histogramm")
## 
## p3 <- p0 + aes(x = score, y = self_eval) +
##   geom_point() +
##   labs(title = "Streu")
## 
## p4 <- p0 + aes(x = score, y = self_eval, fill = "bestanden") +
##   geom_smooth() +
##   geom_point(color = "grey80") +
##   labs(title = "Smooth")
## 
## p5 <-p0 + aes(x = bestanden) +
##   geom_bar() +
##   labs(title = "Balken (Rohwerte)")
## 
## p6 <- stats_test %>%
##   mutate(interessiert = stats_test$interest > 3) %>%
##   count(bestanden, interessiert) %>%
##   ggplot() +
##   aes(x = bestanden, y = n, fill = interessiert) +
##   geom_col(position = "fill") +
##   theme_minimal() +
##   theme(text = element_text(size = 6),
##         title = element_text(size = 8),
##         legend.position = "none") +
##   labs(title = "Balken (Anteile)")
## 
## p7 <- stats_test %>%
##   mutate(interessiert = stats_test$interest > 3) %>%
##   group_by(bestanden, interessiert) %>%
##   summarise(score_median_gruppe = median(score)) %>%
##   ggplot(aes( x = bestanden, y = score_median_gruppe,
##               color = interessiert,
##               shape = interessiert),
##          size = 6) +
##   geom_point(alpha = .7) +
##   labs(title = "Punkte (Mittelwerte)") +
##   theme_minimal() +
##   theme(text = element_text(size = 6),
##         title = element_text(size = 8),
##         legend.position = "none")
## 
## p8 <- p0 + aes(x = bestanden, y = score) +
##   geom_boxplot() +
##   labs(title = "stats_test")
## 
## p9 <- p0 +  aes(x = interest, y = score) +
##   stat_summary(fun.y = "mean", geom = "line") +
##   labs(title = "Linie")
## 
## grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, nrow = 3)


## ----profiles-income-summary-qplot2-exercise, eval = TRUE, echo = TRUE, out.width = "50%", fig.cap = "Zusammenfassungen zeigen mit Details", fig.asp=0.5----
profiles_income_summary %>%
  ggplot() +
  aes(x = sex, y = income_m) +
  geom_jitter(data = profiles,
             aes(y = income),
             color = "grey60",
             alpha = .1,
             width = .1) +
  geom_errorbar(aes(ymin = income_m - income_sd,
                    ymax = income_m + income_sd),
                width = 0,
                color = "grey40") +
  geom_line(aes(group = 1)) +
  geom_point(aes(color = sex,
                 shape = sex), size = 5,
             show.legend = FALSE) +
  labs(title = "Mittelwert des Gehalts nach Geschlecht",
       caption = "Fehlerbalken zeigen die SD",
       y = "Mittleres Einkommen",
       x = "Geschlecht") +
  theme(legend.position = "bottom") +
  scale_x_discrete(labels = c("f"= "female",
                              "m" = "male")) +
  coord_cartesian(ylim = c(0, 250000))


## 1. Diese Geome gehören zum Standard in ggplot2: bar, histogram, point, density, jitter, boxplot.

## 1. `qplot()` ist eine Funktion im Paket `ggplot2`.

## 1. Mit `aes` definiert man, wie "ästhetisch" das Diagramm sein soll (z.B. grauer Hintergrund vs. weißer Hintergrund, Farbe der Achsen etc.). Genauer gesagt, man definiert, welche Variable im Datensatz mit welchem "Ästhetikum", also welcher sichtbaren Eigenschaft des Diagramms korrespondiert.

## 1. Diese Geome gehören zum (Standard-)ggplot2: smooth, line, boxwhisker, mosaicplot.

## 1. Möchte man ein Diagramm erstellen, welches auf der X-Achse `total_bill`, auf der Y-Achse `tip` darstellt, als Geom Punkte verwendet und die Daten aus der Tabelle `tips` bezieht, so ist folgende Syntax korrekt: `qplot(x = total, bill, y = tip, geom = "point", data = tips)

## `1. `geom_jitter` zeigt "verwackelte" Punkte.

## 7. Mit `labs()` kann man Titel, Achsen und Legenden beschriften.

## 8. Fliesendiagramme eignen sich, um Kontingenztabellen zu visualisieren.

## 9. Mit dem Argument `alpha` kann man die Durchsichtigkeit von Geomen einstellen.

## 10. Möchte man die Grenzen der X- oder Y-Achse des Diagramms bestimmen, so führen `coord_cartesian()` und `xlim()` zu den gleichen Ergebnissen.

## 
