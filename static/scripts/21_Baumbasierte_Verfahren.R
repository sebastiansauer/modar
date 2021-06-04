## Lernziele:

## 
## - Die wichtigsten Varianten baumbasierter Modelle kennen und unterscheiden können

## - Die grundlegende Funktionsweise (den Algorithmus) von Entscheidungsbäumen erklären können

## - Die Erweiterung von Entscheidungsbäumen zu Bagging und Random Forests kennen

## - Um die Stärken und Schwächen von baumbasierten Verfahren wissen

## 

## ----libs-baueme--------------------------
library(tidyverse)
library(rpart)  
library(partykit)  
library(caret)
library(gridExtra)
library(mosaic)  

data(Affairs, package = "AER")


## Erstellt man ein Modell, um eine Zielgröße zu erklären bzw. vorauszusagen, so sollte man prüfen, ob nicht "unerlaubte" Informationen als Prädiktoren verwendet werden. Möchten wir z.B. vorhersagen, ob jemand ein Hallodri ist, so darf die Anzahl der eingegangenen Affären nicht als Prädiktor verwendet werden. Das hört sich trivial an, aber es kann subtilere Fälle solcher Informationslecks geben (vgl. Kapitel \@ref(datenjudo), erster Absatz).

## 

## ----datenjudo-baum-----------------------
set.seed(42)  # Zufallszahlen festlegen
index_train <- sample(x = 1:601,
                      size = trunc(.8 * 601))

Affairs <- Affairs %>%
  mutate(is_halodrie = ifelse(affairs > 0, "ja", "nein"),
         is_halodrie = as.factor(is_halodrie))

train_df <- Affairs %>%
  filter(row_number() %in% index_train) %>%
  select(-affairs)

test_df <- Affairs %>%
  filter(!row_number() %in% index_train)

rm(index_train)  # Das Objekt wieder löschen, da nicht mehr benötigt


## ----p-baum1, fig.cap = "Ein einfacher Entscheidungsbaum, der die Stichprobe in zwei Gruppen aufteilt", fig.asp=0.5----
baum1 <- rpart(is_halodrie ~ rating+gender, data = train_df)
plot(as.party(baum1))


## ----eval = FALSE, echo = FALSE-----------
## knitr::include_graphics(paste0(path_to_images, "baum1.pdf"))


## -----------------------------------------
train_df %>%
  count(is_halodrie) %>%
  mutate(prop = n / sum(n))


## Eine homogenere Teilmenge, d.h. ein sehr hoher oder ein sehr geringer Hallodri-Anteil, lässt uns sichere (genauere) Vorhersagen treffen, erhöht unser Wissen. Es ist erstrebenswert, den Datensatz anhand der Prädiktoren so aufzuteilen, dass homogene Gruppen hinsichtlich des Kriteriums entstehen.

## Entscheidungsbäume wählen bei jeder Astgabelung den Prädiktor und den Trennwert, der die Homogenität insgesamt erhöht.

## 

## ----p-baum2, echo = FALSE, fig.cap = "Alternative Visualisierungen für Entscheidungsbäume", out.width = "100%", fig.asp = .6----
train_df %>%
  mutate(pred_halodrie = rating >= 2.5) %>%
  ggplot +
  aes(x = rating, y = is_halodrie) +
  geom_jitter(aes(color = pred_halodrie,
                 shape = pred_halodrie),
             height = 0.3) +
  theme(legend.position = "bottom") +
  guides(size = FALSE) +
  theme(legend.text = element_text(size = 5),
        legend.title = element_text(size = rel(.7))) +
  geom_vline(xintercept = 2.5, color = "firebrick",
             linetype = "dashed") -> p1

train_df %>%
  ggplot +
  aes(x = rating, y = age) +
  geom_count(aes(shape = is_halodrie,
                 color = is_halodrie),
             position = "jitter") +
  facet_wrap(~gender) +
  theme(legend.position = "bottom") +
  theme(legend.text = element_text(size = 5),
        legend.title = element_text(size = rel(.7))) +
  geom_vline(xintercept = 2.5) +  # node1
  geom_segment(x = 0, xend = 2.5,  # node2
               y = 44.5, yend = 44.5) +
  geom_segment(x = 0, xend = 2.5,  # node3
               y = 29.5, yend = 29.5) -> p2

grid.arrange(p1, p2, nrow = 1)


## ----p-baum3, out.width = "70%", fig.cap = "Ein komplexerer Entscheidungsbaum", fig.asp = 1----
baum2 <- rpart(is_halodrie ~ ., data = train_df)
plot(as.party(baum2))


## 
## - Prüfung: Ist die Ehezufriedenheit größer oder gleich 2.5? (Astgabel 1)

## - Wenn die Entscheidung "ja" lautet, ist die Hallodri-Quote (Konsequenz) ca. .20. (Blatt 9)

## - Ansonsten Prüfung: Ist das Alter größer oder gleich 44.5? (Astgabel 2)

## - Wenn ja, ist die Hallodri-Quote ca. .20. (Blatt 8)

## - Ansonsten Prüfung: Ist das Alter größer oder gleich 29.5? (Astgabel 3)

## - Wenn ja, ist die Hallodri-Quote ca. .75. (Blatt 4)

## - Ansonsten Prüfung: Ist es ein Mann? (Astgabel 5)

## - Wenn ja, ist die Hallodri-Quote ca. .75. (Blatt 6)

## - Ansonsten ist die Hallodri-Quote ca. .30. (Blatt 7)

## 

## -----------------------------------------
baum2


## -----------------------------------------
count(train_df, is_halodrie)  # Hallodris insgesamt
count(train_df, rating>=2.5)  # Prüfung in Knoten 1

train_df %>%   # Prüfung in Knoten 9
  filter(rating>=2.5) %>%
  count(is_halodrie)


## ----baum5, eval = FALSE------------------
## baum5 <- rpart(is_halodrie ~ ., data = train_df,
##                control = rpart.control(cp = .05))


## Für Entscheidungsbäume ist der Komplexitätsparameter eine Art Hebel, der die Arbeitsweise der Maschine beeinflusst. Man spricht von *Tuningparametern*\index{Tuningparameter}.  Es gibt keinen vorab bekannten "richtigen" Wert für einen Tuningparameter; der Anwender entscheidet über den Wert. Verschiedene Werte von Tuningparametern dürfen ausprobiert werden, solange dafür nur die Trainingsstichprobe, nicht die Test-Stichprobe, verwendet wird. Dann verwendet man den Wert des Tuningparameters, der im Trainings-Sample zu den besten Ergebnissen führte, im Test-Sample. Im Kapitel \@ref(caret) betrachten wir ein praktisches Beispiel zur Wahl von Tuningparametern.

## 
## 

## ----baum3--------------------------------
my_train_control <- trainControl(
  method = "cv",
  number = 10)

baum3 <- train(is_halodrie ~ .,
               data = train_df,
               method = "rpart",
               trControl = my_train_control)
baum3


## -----------------------------------------
baum3$results
baum3$finalModel


## ----baum3-resample-head------------------
baum3$resample %>%
  head()


## ----p-pred-baum3-p1-gather---------------
baum3$resample %>%
  gather(key = coefficient, value = value, - Resample) -> baum_folds


## ----p-baum-folds-------------------------
baum_folds %>%
  ggplot +
  aes(x = coefficient, y = value) +
  labs(caption = "Gütekennzahlen der zehn Folds des Baummodells") +
  geom_point(position = "jitter") -> baum_folds_p


## ----glm1-glm-folds-----------------------
my_train_control <- trainControl(
  method = "cv",
  number = 10)

glm_halodrie <- train(is_halodrie ~ .,
                  data = train_df,
                  method = "glm",
                  family = "binomial",
                  trControl = my_train_control)

glm_halodrie$resample %>%
  gather(key = coefficient, value = value, - Resample) %>%
  mutate(model = "glm") -> glm_folds



## ----baum-folds2--------------------------
baum_folds %>%
  mutate(model = "tree") %>%
  bind_rows(glm_folds) -> folds


## ----baum-folds-p2------------------------
folds %>%
  filter(coefficient == "Accuracy") %>%
  ggplot() +
  aes(x= model, y = value) +
  geom_point(position = "jitter",
             size = 3,
             aes(color = model,
                 shape = model)) +
  labs(x = "Accuracy") +
  stat_summary(fun.y = "mean",
               geom = "point",
               shape = 4,
               size = 4)  +
  labs(caption = "Kreuze geben den Mittelwert an") -> p_folds


## ----p-resample-performance, echo = FALSE, fig.cap = "Vergleich der Kreuzvalidierungsergebnisse für das Affären-Modell", out.width = "100%"----
grid.arrange(baum_folds_p, p_folds, nrow = 1)


## ----echo = FALSE, eval = FALSE-----------
## knitr::include_graphics(paste0(path_to_images, "baum4.pdf"))


## -----------------------------------------
folds %>%
  group_by(model, coefficient) %>%
  summarise(sd = sd(value))



## ----train-df-conf-matrix-----------------
train_df <- train_df %>%
  mutate(halodrie_predict = predict(baum3,
                                    type = "raw",
                                    newdata = train_df))

train_df %>%   # Konfusionsmatrix
  count(is_halodrie, halodrie_predict)  -> conf_matrix_baum3_train
conf_matrix_baum3_train


## ----richtigkeit-null, echo = FALSE-------
tally(~is_halodrie, data = test_df, format = "percent") -> richtigkeit_null


## ----richtigkeit-train-null, echo = FALSE----

richtigkeit_train <- (conf_matrix_baum3_train[[1,3]] + conf_matrix_baum3_train[[4,3]]) / nrow(train_df)
richtigkeit_train <- round(richtigkeit_train, 2)
richtigkeit_null <- round(tally(~is_halodrie, data = train_df, format = "proportion")[[2]], 2) %>% unname()


## ----max-print, echo = FALSE--------------
options(max.print = 100)


## ----cm-halodrie-test-df------------------
test_df <- test_df %>%
  mutate(halodrie_predict = predict(baum3,
                                    type = "raw",
                                    newdata = test_df))
confusionMatrix(
  data = test_df$halodrie_predict,
  reference = test_df$is_halodrie)


## ----baum3-halodrie-test-cm, echo = FALSE----
confusionMatrix(
  data = test_df$halodrie_predict,
  reference = test_df$is_halodrie) -> conf_matrix_baum3_test


richtigkeit_baum3_null <- round(conf_matrix_baum3_test$overall[["AccuracyNull"]], 2)
richtigkeit_baum3 <- round(conf_matrix_baum3_test$overall[["Accuracy"]], 2)
sensitivity_baum3 <- round(conf_matrix_baum3_test$byClass[["Sensitivity"]], 2)
specificity_baum3 <- round(conf_matrix_baum3_test$byClass[["Specificity"]], 2)


## ----baum3-nullmodell---------------------
# Sensitivität und Spezifität des Nullmodells:
test_df %>%
  count(is_halodrie, halodrie_predict) %>%
  group_by(is_halodrie) %>%
  mutate(prop = n / sum(n))

# Hallodri-Anteil:
mosaic::tally(~is_halodrie, data = test_df, format = "proportion")


## Die Gesamtgenauigkeit des Baummodells (`baum3`) entspricht in etwa der des Nullmodells, sowohl hinsichtlich Sensitivität als auch Spezifität. Unser Modell war offenbar wenig informiert. Man sieht, dass die Gesamtgenauigkeit von `baum3` nicht absolut, sondern im Vergleich zum Nullmodell beurteilt werden muss.

## 

## ----cm-glm-------------------------------
test_df <- test_df %>%
  mutate(halodrie_predict_glm = predict(glm_halodrie,
                                        type = "raw",
                                        newdata = test_df))
confusionMatrix(
  data = test_df$halodrie_predict_glm,
  reference = test_df$is_halodrie) -> conf_matrix_glm_test
conf_matrix_glm_test$overall[["Kappa"]]


## 1. Starte mit der gesamten Stichprobe als "Wurzelknoten".

## 2. Partioniere den aktuellen Knoten so, dass die resultierenden zwei Knoten insgesamt homogener sind als der aktuelle Knoten.

## 3. Weise den resultierenden zwei Knoten die entsprechende Teilstichprobe zu.

## 4. Wiederhole Schritte 2 und 3, bis ein Stop-Kriterium erreicht wird.

## 5. Nimm als Vorhersagewert die häufigste Klasse eines Endknotens.

## 

## ----p-purity, echo = FALSE, eval = FALSE, fig.cap = "Klassifikationsfehler und Gini-Koeffizient kommen manchmal zu unterschiedlichen Entscheidungen"----
## 
## knitr::include_graphics("images/baum/purity-crop.pdf")


## ----p-lm-trees, echo = FALSE, fig.cap = "Vergleich von linearen Modellen und Entscheidungsbäumen", out.width = "100%", fig.asp = .618----

library(tidyverse)

df <- data_frame(x = c(-3, -3, 3, 3),
                 y = c(-3, -2, 1, -3)
)

df2 <- data_frame(x = c(-3, -3, 3, 3),
                 y = c(-2, 3, 3, 1)
)



ggplot(df) +
  aes(x = x, y = y) +
  geom_polygon(fill = "grey80") +
  geom_polygon(data = df2, fill = "grey60") +
  geom_segment(x = -3, xend = 3,
               y = -2, yend = 1,
               color = "firebrick", size = 1) +
  scale_x_continuous(breaks = NULL) +
  labs(title = "A. Lineare Klassifizierung schneidet  gut ab") +
  theme(title = element_text(size = rel(0.6))) +
  scale_y_continuous(breaks = NULL) -> p1


ggplot(df) +
  aes(x = x, y = y) +
  geom_polygon(fill = "grey80") +
  geom_polygon(data = df2, fill = "grey60") +
  geom_segment(x = -3, xend = 3,
               y = -2, yend = -2,
               color = "firebrick", size = 1) +
  geom_segment(x = -3, xend = 3,
               y = +1, yend = +1,
               color = "firebrick", size = 1) +
    geom_segment(x = -1.5, xend = -1.5,
               y = -3, yend = -2,
               color = "firebrick", size = 1) +
    geom_segment(x = 1, xend = 1,
               y = -2, yend = 1,
               color = "firebrick", size = 1) +
    labs(title = "B. Rekursive Partionierung schneidet schlecht ab") +
    theme(title = element_text(size = rel(0.6))) +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
    geom_segment(x = -3, xend = 1,
               y = -.7, yend = -.7,
               color = "firebrick", size = 1) -> p2


df3 <- data_frame(x = c(-3, -3, 3, 3),
                  y = c(-3, 3, 3, -3)
)

df4 <- data_frame(x = c(-2, -2, 3, 3),
                  y = c(-3, 2, 2, -3)
            )

ggplot(df3) +
  aes(x = x, y = y) +
  geom_polygon(fill = "grey80") +
  geom_polygon(data = df4, fill = "grey60") +
  labs(title = "C. Lineare Klassifizierung schneidet schlecht ab") +
  theme(title = element_text(size = rel(0.6))) +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  geom_segment(x = -3, xend = 3,
               y = -3, yend = 2.5,
               color = "firebrick",
               size = 1) -> p3


ggplot(df3) +
  aes(x = x, y = y) +
  geom_polygon(fill = "grey80") +
  geom_polygon(data = df4, fill = "grey60") +
  geom_segment(x = -2, xend = -2,
               y = -3, yend = 3,
               size = 1,
               color = "firebrick") +
  labs(title = "D. Rekursive Partionierung schneidet gut ab") +
  theme(title = element_text(size = rel(0.6))) +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  geom_segment(x = -2, xend = 3,
               size = 1,
               y = 2, yend = 2,
               color = "firebrick") -> p4

library(gridExtra)
grid.arrange(p1, p2, p3, p4,
             ncol = 2)  



## ----echo = FALSE, eval = FALSE-----------
## comp_tree_lm.pdf


## ----p-simu-tree, echo = FALSE, fig.cap = "Klassifikationsgüte in Abhängigkeit von der Anzahl der Knoten im Baum", out.width = "100%"----

# Baum mit CV:
my_train_control <- trainControl(
  method = "cv",
  number = 10
)

Grid <- expand.grid(.maxdepth=seq(1,10,1))

baum4 <- train(is_halodrie ~ .,
               data = train_df,
               method = "rpart2",
               trControl = my_train_control,
               tuneGrid = Grid)

# Klassifikationsgüte für Trainings- und Testsample
# in Abhängigkeit der Anzahl der Knoten
rpart_loop <- vector(mode = "list",
                     length = 10)


for (i in 1:10) {
  tree <- rpart(is_halodrie ~ .,
                       data = train_df,
                       control = rpart.control(maxdepth = i))

  rpart_predict_test <- predict(tree,
                                newdata = test_df,
                                type = "class")
  rpart_predict_train <- predict(tree,
                                 newdata = train_df,
                                 type = "class")

  rpart_loop[[i]]$rpart_acc_test <- confusionMatrix(data = rpart_predict_test,
                                    reference =
                                      test_df$is_halodrie)$overall["Accuracy"]
  rpart_loop[[i]]$rpart_acc_train <- confusionMatrix(data = rpart_predict_train,
                                     reference =
                                      train_df$is_halodrie)$overall["Accuracy"]
}

rpart_loop %>%
  purrr::reduce(bind_rows) -> tree_acc_df


tree_acc_df %>%
  mutate(rpart_acc_cv = baum4$results$Accuracy,
         maxdepth_tree = row_number()) -> tree_acc_df

tree_acc_df %>%
  gather(key = measure,
         value = accuracy,
         -maxdepth_tree) %>%
  mutate(measure = factor(measure,
                          labels = c("Genauigkeit kreuzvalidiert",
                                    "Genauigkeit Test-Sample",
                                    "Genauigkeit Trainings-Sample"))) %>%
  ggplot +
  aes(x = maxdepth_tree,
      y = accuracy,
      color = measure,
      shape = measure) +
  geom_line(aes(group = measure)) +
  geom_point(size = 2) +
  theme(legend.position = "right") +
  scale_y_continuous(limits = c(.7, .85)) +
  scale_x_continuous(breaks = 1:10) +
  labs(x = "Anzahl der Knoten",
       y = "Richtigkeit",
       color = "",
       shape = "")


## ----bagging-no-eval, eval = FALSE--------
## bag1 <- train(is_halodrie ~ .,
##               data = train_df,
##               method = "treebag",
##               trControl = my_train_control)


## ----rf-intuit, echo = FALSE, fig.cap = "Intuitive Darstellung des Random-Forest-Modells", out.width="50%"----

knitr::include_graphics("images/baum/rf-crop.pdf")



## ----rf1-no-eval, eval = FALSE------------
## rf1 <- train(is_halodrie ~ .,
##              data = train_df,
##              method = "rf",
##              trControl = my_train_control)
## rf1


## ----rf1-readRDS, echo = FALSE------------
rf1_baeume <- readRDS(file = "data/rf1-baeume.RDS")
rf1_baeume


## -----------------------------------------
my_tuning_grid <- expand.grid(mtry = 1:10)


## ----rf2-no-eval, eval = FALSE------------
## rf2 <- train(is_halodrie ~ .,
##              data = train_df,
##              method = "rf",
##              trControl = my_train_control,
##              ntree = 1000,
##              importance = TRUE,
##              tuneGrid = my_tuning_grid)


## ----rf2-readRDS, echo = FALSE------------
rf2 <- readRDS(file = "data/rf2-baeume.RDS")


## -----------------------------------------
rf2$results %>%
  top_n(n = 1, wt = Accuracy)


## ----rf2-results, echo = FALSE------------
rf2_acc <- rf2$results$Accuracy[[1]] %>% round(3)
rf2_kappa <- rf2$results$Kappa[[1]] %>% round(3)


## ----rf2-predict--------------------------
rf2_predict <- predict(rf2, newdata = test_df, type = "raw")
postResample(pred = rf2_predict, obs = test_df$is_halodrie)


## -----------------------------------------
varImp(rf2)


## ----eval = FALSE-------------------------
## train_df$is_halodrie_chr <- as.character(train_df$is_halodrie)
## baum2b <- rpart(is_halodrie_chr ~ rating + age, data = train_df)
## plot(as.party(baum2b))
## str(baum2b)
## identical(baum2b$frame, baum2a$frame)

