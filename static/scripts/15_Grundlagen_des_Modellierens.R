## ----echo = FALSE, out.width = "70%"------
knitr::include_graphics("images/modellieren/Modellieren.pdf")


## Lernziele:

## 
## - Erläutern können, was man unter einem Modell versteht

## - Die Ziele des Modellieren aufzählen und erläutern können

## - Die Vor- und Nachteile von einfachen vs. komplexen Modellen vergleichen können

## - Wissen, was man unter Bias-Varianz-Abwägung versteht

## - Um die Notwendigkeit von Trainings- und Test-Stichproben wissen

## - Wissen, was man unter Modellgüte versteht

## - Um die Schwierigkeiten der Prädiktorenauswahl wissen

## 

## ----libs-modellieren---------------------
library(tidyverse)
data(stats_test, package = "pradadata")


## ----libs-hidden-modellieren, echo = FALSE----
library(gridExtra)
library(caret)
library(viridis)
library(huxtable)


## ----libs-hidden, echo = FALSE------------
library(grid)
library(png)


## ----vwmodell, echo = FALSE, fig.cap = "Ein Modell eines VW-Käfers als Prototyp eines Modells", out.width = "40%"----
knitr::include_graphics("images/modellieren/vw_modell.JPG")


## ----modellieren-plot, echo = FALSE, fig.cap = "Modellieren"----
knitr::include_graphics("images/modellieren/Modell.pdf")


## Modellieren bedeutet hier, ein Verfahren zu erstellen, welches empirische Sachverhalte adäquat in numerische Sachverhalte umsetzt.

## 

## ----modellieren-formal, echo = FALSE, eval = FALSE, fig.cap = "Formaleres Modell des Modellierens"----
## knitr::include_graphics("images/modellieren/Modellieren_formal_crop.png")


## Viele statistische Modelle beantworten nicht, wie wahrscheinlich es ist, dass ein Modell gilt. Stattdessen beurteilen sie, wie gut die vorliegenden Daten zu einem vorab postulierten Modell passen. Den Grad der Übereinstimmung bezeichnet man als Modellgüte\index{Modellgüte, Model Fit} (model fit).

## 

## ----plot-stats-smooth, echo = FALSE------
stats_test <- read.csv("data/stats_test.csv")

stats_test %>%
ggplot(aes(y = score, x = self_eval)) +
geom_jitter() +
ggtitle("A") -> p1

p2 <- p1 + geom_smooth(method = "lm", se = FALSE) + ggtitle("B")


## ----plot-stats, echo = FALSE, out.width = "90%", fig.height = 1, fig.cap = "Ein Beispiel für Modellieren"----
# p3 <- grid::rasterGrob(readPNG("images/Modellieren_Bsp1.png"), interpolate=TRUE)
grid.arrange(p1, p2, nrow = 1)


## ----mod-beispiel, fig.cap = "Ein Beispiel für ein Pfadmodell", echo = FALSE----
imgs <- c("images/modellieren/Modellieren_Bsp1.png",
          "images/modellieren/Modellieren_Bsp2a-crop.png")


img <- imgs[1]

knitr::include_graphics("images/modellieren/Modellieren_Bsp1.pdf")


## Einfluss ist hier nicht (notwendigerweise) kausal gemeint, auch wenn es das Wort so vermuten lässt. Stattdessen ist nur ein statistischer Einfluss gemeint; letztlich nichts anderes als ein Zusammenhang. In diesem Sinne könnte man postulieren, dass die Größe des Autos, das man fährt, einen "Einfluss" auf das Vermögen des Fahrers habe. Empirisch ist es gut möglich, dass man Belege für dieses Modell findet. Jedoch wird dieser Einfluss nicht kausal sein.

## 

## ----fig-blackbox, echo = FALSE, fig.cap = "Modelle mit schwarzer Kiste; angeleitetes Lernen"----
knitr::include_graphics("images/modellieren/Modell_Blackbox-crop.pdf")


## Findet man, dass zwei Ereignisse (Variablen) zusammenhängen, so heißt das nicht, dass das eine Ereignis die Ursache des anderen sein muss. Man denke an Störche und Babies (wo es viele Störche gibt, gibt es viele Babies; so will es die Geschichte).

## 

## ----ungeleitetes-modellieren, echo = FALSE, fig.cap = "Die zwei Arten des ungeleiteten Modellierens"----

knitr::include_graphics("images/modellieren/ungeleitetes_Modellieren-crop.pdf")



## ----overfitting-prep-4-plots, echo = FALSE, include = FALSE, fig.height = 3, fig.width = 3----
x <- seq(from = 1, to = 10, by = .3)
y <- sin(x) + rnorm(n = length(x), mean = 0, sd = .3)

daten <- data_frame(x, y)

ggplot(daten) +
  aes(x = x, y = y) +
  coord_fixed(ratio = 5/1) +
  labs(y = "") +
  geom_point() +
  ggtitle("A") -> p1

ggplot(daten) +
  aes(x = x, y = y) +
  geom_point() +
  coord_fixed(ratio = 5/1) +
  labs(y = "") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  ggtitle("B")-> p2


ggplot(daten) +
  aes(x = x, y = y) +
  geom_point() +
  coord_fixed(ratio = 5/1) +
  labs(y = "") +
  geom_line(color = "blue") +
  ggtitle("C") -> p3

ggplot(daten) +
  aes(x = x, y = y) +
  geom_point() +
  coord_fixed(ratio = 5/1) +
  labs(y = "") +
  stat_function(n = 99, fun = sin, color = "darkgreen") +
  ggtitle("D") -> p4


## ----overfitting-4-plots, echo = FALSE, fig.cap = "Welches Modell (Teile B-D) passt am besten zu den Daten (Teil A) ?", out.width = "100%"----

grid.arrange(p1, p2, p3, p4, ncol = 4)



##  Beschreibt ein Modell (wie Modell C hier) eine Stichprobe sehr gut, heißt das noch *nicht*, dass es auch zukünftige (und vergleichbare) Stichproben gut beschreiben wird.  Die Güte (Vorhersagegenauigkeit) eines Modells sollte sich daher stets auf eine neue Stichprobe beziehen (Test-Stichprobe), die nicht in der Stichprobe beim Anpassen des Modells (Trainings-Stichprobe) enthalten war.


## Je komplexer ein Modell ist, desto besser kann es einen bekannten Datensatz (Trainings-Stichprobe) beschreiben. Allerdings ist das Modell, welches den Trainings-Datensatz am besten beschreibt, nicht zwangsläufig das Modell, welches neue, unbekannte Daten am besten beschreibt. Oft im Gegenteil!

## 

## ----overfitting-schema, echo = FALSE, fig.cap = "Mittlere Komplexität hat die beste Vorhersagegenauigkeit (am wenigsten Fehler) in der Test-Stichprobe", out.width = "50%"----
knitr::include_graphics("images/modellieren/overfitting.pdf")


## ----plot-bias-variance, echo = FALSE, fig.asp = 0.25, fig.cap = "Der Spagat zwischen Verzerrung und Varianz", out.width = "100%"----
poly_degree = 15
df <- data_frame(x = seq(from = 1, to = 10, by = .3),
                 y = sin(x) + rnorm(n = length(x), mean = 0, sd = .3))

df %>%
  mutate(binned = cut(.$x, breaks = c(-Inf, 5.5, +Inf))) %>%
  group_by(binned) %>%
  mutate(y_group_md = median(y)) -> df


p1 <- ggplot(df) +
  aes(x = x, y = y) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, poly_degree), se = FALSE)


p2 <-  ggplot(df) +
  aes(x = x) +
  geom_point(aes(y = y)) +
  geom_line(aes(y = y_group_md, group = binned), color = "firebrick")


grid.arrange(p1, p2, ncol = 2)


##  Die Güte des Modells sollte bei ausreichend großem Datensatz nur anhand eines -- bislang nicht verwendeten -- Test-Samples überprüft werden. Das Test-Sample darf bis zur Modellüberprüfung nicht analysiert werden.

## 

## -----------------------------------------
train <- slice(stats_test, 1:200)
test <- slice(stats_test, 201:306)


## -----------------------------------------
train <- stats_test %>%
  sample_frac(.8, replace = FALSE)  # Stichprobe von 80%, ohne Zurücklegen

test <- stats_test %>%
  anti_join(train)  
# Behalte nur Zeilen, die nicht in "train" vorkommen



## ----p-crossval, echo = FALSE, fig.cap = "Beispiel für eine k=4 Kreuzvalidierung", cache = FALSE----
# comb2pngs(c("images/modellieren/crossval.png",
#             "images/modellieren/crossval_repeated.png"))
knitr::include_graphics("images/modellieren/crossval-crop.pdf")




##  Die Kreuzvalidierung ist eine Methode zur Beurteilung der Modellgüte. Der Prozess der Aufteilung in Trainings- und Test-Sample wird dabei (mehrfach) vorgenommen. Die Modellgüte wird anschließend summarisch beurteilt.

## 

## - Kriterium der theoretischen Plausibilität: Ein statistisches Modell sollte theoretisch plausibel sein.

## - Kriterium der guten Vorhersage: Die Vorhersagen eines Modells sollen präzise sein.

## - Kriterium der Neuartigkeit: Die Vorhersagen sollen neues Wissen produzieren, sie sollen "überraschend" sein.

## - Kriterium der Situationsangemessenheit: Die Güte des Modells ist auf die konkrete Situation abzustellen.

## 

##  In numerischen Vorhersagemodellen beruht der Vorhersagefehler oft auf der Differenz zwischen tatsächlichen und vorhergesagten Werten.

## 

## ----resids-plot, echo = FALSE, results = "hold", fig.cap = "Geringer (links; A) vs. hoher (rechts, B) Vorhersagefehler"----

set.seed(42)  
N      <- 100
beta   <- 0.4
intercept <- 1


sim <- data_frame(
  x = rnorm(N),
  error1 = rnorm(N, mean = 0, sd = .5),
  error2 = rnorm(N, mean = 0, sd = 2),
  y1 = intercept + x*beta + error1,
  y2 = intercept + x*beta + error2,
  pred = 1 + x*beta
)



p1 <- ggplot(sim, aes(x, y1)) +
  geom_abline(intercept = intercept, slope = beta, colour = "red") +
  geom_point(colour = "#00998a") +
  geom_linerange(aes(ymin = y1, ymax = pred), colour = "grey40") +
  ylim(-6,+6) + labs(title = "A - wenig Vorhersagefehler")


p2 <- ggplot(sim, aes(x, y2)) +
  geom_abline(intercept = intercept, slope = beta, colour = "red") +
  geom_point(colour = "#00998a") +
  geom_linerange(aes(ymin = y2, ymax = pred), colour = "grey40") +
  ylim(-6,+6) + labs(title = "B - viel Vorhersagefehler")


grid.arrange(p1, p2, ncol = 2)


## ----fig-accuracy, echo = FALSE, fig.cap = "Sinnbild für die Trefferquote eines Klassifikationsmodells", out.width = "30%"----
knitr::include_graphics("images/modellieren/accuracy-crop.pdf")


## ----p-class-stat-bin-2d, echo = FALSE, fig.cap = "Ein einfaches Klassifikationsmodell", fig.asp = .7----

set.seed(42)
twoClassSim(n = 500, intercept = -5, linearVars = 10, noiseVars = 0,
  corrVars = 0, corrType = "AR1", corrValue = 0, mislabel = 0,
  ordinal = FALSE) -> sim_df

sim_df$Class_num <- ifelse(sim_df$Class ==
                           "Class1", 1, 0)


sim_df %>%
  filter(Linear01 > -2, Linear01 < 2) %>%
    filter(Linear02 > -2, Linear02 < 2) %>%
  ggplot() +
  aes(x = Linear01, y = Linear02, group = 1) +
  stat_summary_2d(aes(z = Class),
                  fun = function(z) names(which.max(table(z))),
                  bins = 3,
                  alpha = .5,
                  drop = TRUE) +
  geom_point(aes(color = Class, shape = Class), size = 3) +
annotate(geom = "point", x = 2, y = -1, shape = 8, size = 5) +
  labs(x = "x1", y = "x2",
       class = "Regen") +
  theme(legend.position = "none") +
  scale_color_manual(values = c("firebrick", "dodgerblue4")) +
  scale_fill_manual(values = c("grey50", "grey90"))




## ----curse2, echo = FALSE, fig.cap = "Die Anzahl der Zellen wächst exponentiell", out.width = "70%", cache = FALSE----
knitr::include_graphics("images/modellieren/curse2-crop.pdf")


## Eine große Anzahl an Prädiktoren (Dimensionen) verlangt im Allgemeinen nach einer großen Stichprobe. Genauer gesagt steigt die benötigte Stichprobengröße exponentiell mit der Anzahl der Dimensionen. Eine große Prädiktorenzahl kann -- muss aber nicht -- problematisch für eine Modellierung sein. Man sollte gut prüfen, ob die Anzahl der Dimensionen nicht verringert werden könnte (vgl. @kuhn2013applied).

## 

## ----plot-bias-variance2, echo = FALSE, fig.cap = "Bias-Varianz-Abwägung. Links (A): Wenig Bias, viel Varianz. Rechts (B): Viel Bias, wenig Varianz.", eval = FALSE----
## 
## poly_degree = 5
## df <- data_frame(x = seq(from = 1, to = 10, by = .3),
##                  y = cos(x) + rnorm(n = length(x), mean = 0, sd = .5))
## 
## df %>%
##   mutate(binned = cut(.$x, breaks = c(-Inf, 5.5, +Inf))) %>%
##   group_by(binned) %>%
##   mutate(y_group_md = mean(y)) -> df
## 
## 
## p1 <- ggplot(df) +
##   aes(x = x, y = y) +
##   geom_point() +
##   geom_smooth(method = "lm", formula = y ~ poly(x, poly_degree), se = FALSE) +
##   labs(title = "A")
## 
## 
## p2 <-  ggplot(df) +
##   aes(x = x) +
##   geom_point(aes(y = y)) +
##   geom_line(aes(y = y_group_md, group = binned), color = "firebrick")  +
##   labs(title = "B")
## 
## 
## grid.arrange(p1, p2, ncol = 2)


## 
## 1. Die Aussage "Pro Kilo Schoki steigt der Hüftumfang um einen Zentimeter" kann als Beispiel für ein deterministisches Modell herhalten.

## 1. Gruppiert man Kunden nach ähnlichen Kaufprofilen, so ist man insofern am "Reduzieren" der Datenmenge interessiert.

## 1. Es gilt: Je komplexer ein Modell, desto besser.

## 1. Mit "Bias" ist gemeint, dass ein Modellgraph "zittrig" oder "wackelig" ist -- sich also bei geringer Änderung der Stichprobendaten massiv in den Vorhersagen ändert.

## 1. In der Gleichung $Y=f(x)+\epsilon$ steht $\epsilon$ für den Teil der Kriteriums, der nicht durch das Modell erklärt wird.

## 1. Bei der k-fachen Kreuzvalidierung gilt: Jeder Fall kommt dabei genau einmal in das Test-Sample und k-1-mal in das Trainings-Sample.

## 7. Modelle mit hoher Komplexität haben meist den kleinsten Vorhersagefehler in der Trainings-Stichprobe.

## 8. Modelle mit hoher Komplexität haben meist den kleinsten Vorhersagefehler in der Test-Stichprobe.

## 9. Ein Modell sagt für die nächsten zehn Tage jeden Tag Regen vorher, und tatsächlich regnet es an sieben von zehn Tagen. Damit hat das Modell eine Gesamtgenauigkeit von 70%.

## 10. Ein Modell sagt für 90% der nächsten Tage Regen vorher; allerdings regnet es in dieser Gegend zu dieser Jahreszeit an 90% der Tage. Die Gesamtgenauigkeit des Modells ist hoch, aber trotzdem ist der Mehrwert des Modells gering.

## 
