## Lernziele:

## 
## - Wissen, welche Aufgaben mit dem R-Paket `caret` erledigt werden können

## - Die grundlegende Syntax von `caret` kennen

## - Prädiktive Modelle mit `caret` berechnen können

## - Resampling-Methoden mit `caret` anwenden können

## - Einen Datensatz für eine prädiktive Modellierung vorbereiten können

## 

## ----libs-caret---------------------------
library(tidyverse)
library(caret)
library(modelr)
library(broom)
library(doMC)
  
data(GermanCredit, package = "caret")
data(stats_test, package = "pradadata")


## Einige Funktionen, auch bei `caret,` verkraften (noch) keine Tibbles. Merkwürdige Fehlermeldungen können die Ursache haben, dass sich eine Funktion an einem Tibble verschluckt. Im Zweifel sollte man einen Tibble mit `as.data.frame()` in einen klassischen Dataframe umwandeln.

## 

## -----------------------------------------
str(GermanCredit$Class)
levels(GermanCredit$Class)


## -----------------------------------------
anyNA(GermanCredit)


## ----train-test---------------------------
n_train <- round(.8 * nrow(GermanCredit), digits = 0)
train_index <- sample(1:nrow(GermanCredit), size = n_train)
train <- GermanCredit[train_index, ]
test <- GermanCredit[-train_index, ]


## ----train-test-sample--------------------
Trainings_Faelle <- createDataPartition(GermanCredit$Class, p = .8)
Trainings_Faelle_vec <- unlist(Trainings_Faelle)

GermanCredit %>%
  filter(row_number() %in% train_index) -> train

GermanCredit %>%
  filter(!(row_number() %in% train_index)) -> test


## -----------------------------------------
mosaic::tally(~Class, data = train, format = "proportion")
mosaic::tally(~Class, data = test, format = "proportion")


## ----nearzerovar-train--------------------
train %>%
  nearZeroVar(saveMetrics = TRUE) %>%
  rownames_to_column() %>%
  filter(nzv == TRUE) %>%
  pull(rowname) -> train_nzv

length(train_nzv)


## ----nearzerovar-remove-------------------
train %>%
  select(-one_of(train_nzv)) -> train

attr(train, "nzv checked") <- TRUE


## ----findhighcorr, cache = TRUE-----------
train %>%
  select_if(is.numeric)  -> train_num

findCorrelation(cor(train_num, use = "complete.obs"),
                names = TRUE)


## ----register-cores-----------------------
registerDoMC(cores = 2)


## ----my-crossval--------------------------
my_crossval <- trainControl(method = "repeatedcv",
                           number = 10,  # k
                           repeats = 5,  # r
                           allowParallel = TRUE)


## ----eval = FALSE-------------------------
## train(ziel ~ .,
##       data = daten,
##       method = "methode",
##       trControl = my_crossval)


## Vorsicht: Hat eine Variable zehn Stufen, so erstellt `dummyVars()` zehn Variablen; das kann die Dimensionalität des Datensatzes über die Maßen aufblähen. Unter Umständen ist man besser beraten, auf einige dieser Variablen zu verzichten. Eine Möglichkeit ist, nur einige vom Vorwissen (Theorie) favorisierte Stufen herauszupicken und nur diese Kontraste zu betrachten.

## 

## Es ist davon abzuraten, stumpf alle Variablen des Datensatzes als Prädiktoren in ein lineares Modell aufzunehmen. Besser ist es, eine Vorauswahl zu treffen; entweder anhand theoretischer Überlegungen oder aufgrund von statistisch basierten Auswahlstrategien. Eine Möglichkeit dazu ist es, sich z.B. über Random Forests (s. Abschnitt \@ref(caret-rf)) die Variablenwichtigkeit ausgeben zu lassen und nur diese "wichtigen" Variablen als Prädiktoren in das lineare Modell aufzunehmen. Näheres zur Auswahl von Prädiktoren findet sich bei  @introstatlearning oder bei @kuhn2013applied.

## 

## ----glm-fit1-----------------------------
set.seed(42)  # Zufallszahlen fixieren
glm_fit1 <- train(Class ~ Amount + Age + Duration,  # Modellgleichung
                  data = train,  # Daten
                  method = "glm",  # Modell
                  family = "binomial",  # Modelldetails
                  trControl = my_crossval)  # Kreuzvalidierung
glm_fit1


## Entspricht die Genauigkeitskennzahl `Accuracy` in etwa dem Anteil des gesuchten Ereignisses oder seines Gegenereignisses, so ist Vorsicht geboten: Häufig wird entweder die Sensitivität oder die Spezifität der Vorhersage schlecht ausfallen.

## 

## ----test-sample-proc, eval = FALSE-------
## d <- data_frame(id = 1:10)
## n_train <- round(.8 * nrow(d), digits = 0)
## train_index <- sample(1:nrow(d), size = n_train)
## length(train_index)
## d_train <- d[train_index, ]
## d_test <- d[-train_index, ]
## any(d_train$id %in% d_test$id)


## ----rf-grid------------------------------
rf_grid <- expand.grid(.mtry = c(5, 6, 7, 8),
                       .splitrule = "gini",
                       .min.node.size = c(10, 20))


## ----rf-fit1-no-eval, results = "hide", eval = FALSE----
## set.seed(123)
## rf_fit1 <- train(Class ~ .,
##                  data = train,
##                  method = "ranger",  # Random Forest
##                  trControl = trainControl(method = "repeatedcv",
##                                           number = 10,  # k
##                                           repeats = 5,  # r
##                                           allowParallel = TRUE,
##                                           verboseIter = T,
##                                           classProbs = T),
##                  tuneGrid = rf_grid,
##                  num.trees = 500,
##                  importance =  "permutation",  # Variablenwichtigkeit
##                  verbose = TRUE)  # ausführlicher Output
## 
## rf_fit1


## ----load-rf-fit1, echo = FALSE-----------
options(print.max = 100)
#saveRDS(rf_fit1, "data/rf_fit1.rds")
rf_fit1 <- readRDS("data/rf_fit1.rds")
rf_fit1


## ----rf1-pred-----------------------------
rf_pred1 <- predict(rf_fit1, newdata = test)
postResample(pred =  rf_pred1 , obs = test$Class)


## ----conf-matrix-rf-pref1, echo = FALSE----
cm_rf1 <- confusionMatrix(data = rf_pred1, reference = test$Class)
cm_rf1_sens <- cm_rf1$byClass[["Sensitivity"]] %>%  round(2)
cm_rf1_spec <- cm_rf1$byClass[["Specificity"]] %>%  round(2)


## ----svm-fit1-no-eval, eval = FALSE-------
## set.seed(1056)
## svm_fit1 <- train(Class ~ ., data = train,
##                  method = "svmRadial",  # Radial kernel
##                  trControl = my_crossval,
##                  preProcess = c("center", "scale"),
##                  tuneLength = 10,
##                  verboseIter = TRUE)
## #svm_fit1


## ----svm-fit1, echo = FALSE---------------
#saveRDS(svm_fit1, file = "data/svm_fit1.rds")
svm_fit1 <- readRDS("data/svm_fit1.rds")

svm_fit1_kappa_min <- svm_fit1$results$Kappa %>% min %>% round(2)
svm_fit1_kappa_max <- svm_fit1$results$Kappa %>% max %>% round(2)


## ----svm-grid-----------------------------
svm_grid <- expand.grid(C = c(3.5, 3.75, 4.00, 4.25),
                        sigma = c(0.01, 0.015, .02, .05, .1))


## ----svm-fit2-no-eval, eval = FALSE-------
## set.seed(123)
## svm_fit2 <- train(Class ~ ., data = train,
##                  method = "svmRadial",  # Radial kernel
##                  trControl = my_crossval,
##                  preProcess = c("center", "scale"),
##                  tuneGrid = svm_grid)
## #svm_fit2


## ----svm-fit2-load, echo = FALSE----------
#saveRDS(svm_fit2, file = "data/svm_fit2.rds")
svm_fit2 <- readRDS("data/svm_fit2.rds")


## ----svm_fit2-bestTune--------------------
svm_fit2$bestTune


## Achtung! Das Ausprobieren von verschiedenen Werten der Modellparameter ist nur im Trainings-Sample erlaubt. Die Modellgüte muss anhand eines Datensatzes geprüft werden, der nicht zum Ausprobieren verwendet wurde (das Test-Sample). Andernfalls ist die Gefahr, Zufallsrauschen zu interpretieren, hoch.

## 

## -----------------------------------------
glmnet_grid = expand.grid(alpha = seq(0, 1, length = 10),
                         lambda = 10^seq(-2, 2, length = 10))


## ----glmnet-fit1-no-eval, eval = FALSE----
## set.seed(123)
## glmnet_fit1 <- train(Class ~ .,
##                  method = "glmnet",
##                  data = train,
##                  trControl = my_crossval,
##                  tuneGrid = glmnet_grid)
## #glmnet_fit1


## ----glmnet-fit1-load, echo = FALSE-------
glmnet_fit1 <- readRDS("data/glmnet_fit1.RDS")


## ----glm-fit2-----------------------------
glm_fit2 <- glm(Class ~ Amount + Age + Duration,
                data = train,
                family = "binomial")
broom::tidy(glm_fit2)
broom::glance(glm_fit2)

train$pred_glm_fit2 <- predict(glm_fit2, type = "response")

threshold <- 0.5
confusionMatrix(factor(train$pred_glm_fit2 > threshold),
                factor(train$Class == "Good"),
                positive = "FALSE")


## ----all-pred-to-test---------------------
test %>%
  mutate(glm1_pred = predict(glm_fit1, newdata = test),
         rf_pred = predict(rf_fit1, newdata = test),
         svm_pred1 = predict(svm_fit1, newdata = test),
         svm_pred2 = predict(svm_fit2, newdata = test),
         glmnet_pred = predict(glmnet_fit1, newdata = test)) -> test

test %>%
  select(contains("_pred")) %>%
  tail()


## ----cm-GermanCredit----------------------
cm_GermanCredit <- confusionMatrix(data = test$rf_pred, reference = test$Class)
cm_GermanCredit


## ----test-accuracies----------------------
test_accuracies <- 
  test %>%
  select(contains("_pred")) %>%
  map_dfc(~confusionMatrix(data = .,
                          reference = test$Class)$overall[c("Accuracy",
                                                            "Kappa")]) %>% 
  mutate(coefficient = c("Accuracy", "Kappa")) 
   
test_accuracies


## ----all-perf-germancredit, fig.cap = "Performanzwerte der getesteten Modelle (Test-Sample)"----
test_accuracies %>%  # in langes Format umwandeln
  gather(key = "Modell", value = "Wert", -coefficient) %>%  
  ggplot() +
  aes(x = reorder(Modell, Wert),
      y = Wert,
      color = coefficient,
      shape = coefficient
      ) +
  geom_point(size = 3)


## ----tuning-rf, fig.cap = "Der Einfluss des Tuningparameters beim Random-Forest-Modell auf die Modellgüte in der Kreuzvalidierung", fig.asp=0.7----
ggplot(rf_fit1)


## ----resampling-profile-glm, fig.cap = "Das Profil der Modellgüte des GLM-Modells in den Stichproben der Kreuzvalidierung"----
densityplot(glm_fit1)


## ----resamples-between, fig.cap = "Modellgüte zwischen den Modellen in der Kreuzvalidierung"----
model_results <- resamples(list(glm = glm_fit1,
                                rf = rf_fit1,
                                svm1 = svm_fit1,
                                svm2 = svm_fit2,
                                glmnet = glmnet_fit1))

bwplot(model_results)


## ----ex-acc1, eval = FALSE----------------
## set.seed(1234567)
## data_small <- sample_n(drop_na(GermanCredit),
##                        size = 100)
## 
## Trainings_Faelle <- createDataPartition(data_small$Class,
##                                         p = .79)
## Trainings_Faelle <- unlist(Trainings_Faelle)
## 
## train_small <- data_small %>% dplyr::filter(row_number() %in% Trainings_Faelle)
## test_small <- data_small %>% dplyr::filter(!(row_number() %in% Trainings_Faelle))
## # glm ohne Kreuzvalidierung
## glm1_small <- glm(Class ~ Amount + Age + Duration,
##                   data = train_small,
##                   family = "binomial")
## glm1_small_pred_prob <- predict(glm1_small,
##                                 newdata = test_small,
##                                 type = "response")
## glm1_small_pred_class <- factor("Bad", levels = c("Bad", "Good"))
## glm1_small_pred_class[glm1_small_pred_prob > .5] <- "Good"
## 
## 
## confusionMatrix(glm1_small_pred_class, reference = test_small$Class)$overall["Accuracy"]
## 
## # glm mit Kreuzvalidierung
## glm_fit1_small <- train(Class ~ Amount + Age + Duration,
##                         data = train_small,
##                         method = "glm",
##                         family = "binomial",
##                         trControl = my_crossval)
## glm_fit1_small_pred_class <- predict(glm_fit1_small, newdata = test_small)
## confusionMatrix(glm_fit1_small_pred_class, test_small$Class)$overall["Accuracy"]


## ----compare-accuracies1, eval = TRUE-----
compare_accuracies <- function(sample_size = 100){

  set.seed(1234567)  # Zufallszahlen fixieren
  data_small <- sample_n(drop_na(GermanCredit),
                         size = sample_size)
  Trainings_Faelle <- createDataPartition(data_small$Class,
                                          p = .79)
  Trainings_Faelle <- unlist(Trainings_Faelle)

  train_small <- data_small %>% dplyr::filter(row_number() %in%
                                                Trainings_Faelle)
  test_small <- data_small %>% dplyr::filter(!(row_number() %in%
                                                 Trainings_Faelle))
  # glm ohne Kreuzvalidierung
  glm1_small <- glm(Class ~ Amount + Age + Duration,
                    data = train_small,
                    family = "binomial")
  glm1_small_pred_prob <- predict(glm1_small,
                                  newdata = test_small,
                                  type = "response")
  glm1_small_pred_class <- factor("Bad", levels = c("Bad", "Good"))
  glm1_small_pred_class[glm1_small_pred_prob > .5] <- "Good"

  # glm mit Kreuzvalidierung
  glm_fit1_small <- train(Class ~ Amount + Age + Duration,
                          data = train_small,
                          method = "glm",
                          family = "binomial",
                          trControl = my_crossval)
  glm_fit1_small_pred_class <- predict(glm_fit1_small,
                                       newdata = test_small)

  # Accuracies:
  cm_no_resampling <- confusionMatrix(glm_fit1_small_pred_class,
                                            test_small$Class)
  Accuracy_no_resampling <- cm_no_resampling$overall["Accuracy"] %>%
    unname()
  cm_with_resampling <- confusionMatrix(glm1_small_pred_class,
                                              reference = test_small$Class)
  Accuracy_with_resampling <- cm_with_resampling$overall["Accuracy"] %>%
    unname()

  results <- list(cm_no_resampling,
                  Accuracy_no_resampling,
                  cm_with_resampling,
                  Accuracy_with_resampling)

  return(results)
}


## ----compare-accuracies2------------------
run_50 <- compare_accuracies(50)
run_100 <- compare_accuracies(100)
run_500 <- compare_accuracies(500)
run_1000 <- compare_accuracies(1000)


## ----p-compare-accuracies, fig.cap = "Vergleich der Vorhersagegüte eines Modells mit Kreuzvalidierung und eines ohne"----
simu_results <- data.frame(
  accuracies = c(run_50[[2]], run_50[[4]],
                 run_100[[2]], run_100[[4]],
                 run_500[[2]], run_500[[4]],
                 run_1000[[2]], run_1000[[4]]),
  resampling = rep(c("no", "yes"), times = 4),
  sample_size = c(50, 50, 100, 100, 500, 500, 1000, 1000))

simu_results %>%
  ggplot(aes(x = sample_size,
             y = accuracies,
             color = resampling,
             shape = resampling)) +
  geom_point(size = 4) +
  geom_line(aes(group = resampling), alpha = .7) +
  scale_x_continuous(breaks = c(50, 100, 500, 1000))


## -----------------------------------------
filterVarImp(x = select(test, -Class),
             y = test$Class) %>%
  head()


## ----fisher-test-or-----------------------
tab1 <- table(test$EmploymentDuration.Unemployed,
              test$Class)
fi_test1 <- fisher.test(tab1)


## ----str-fitest1--------------------------
str(fi_test1)


## ----binary-predictors--------------------
binary_predictors <- test %>%
  select(8:62, -Class) %>%
  names()

head(binary_predictors)


## ----test-nzv-----------------------------
test %>%
  nearZeroVar(saveMetrics = TRUE) %>%
  rownames_to_column() %>%
  filter(nzv == TRUE) %>%
  pull(rowname) -> test_nzv


## ----fisher-test-all----------------------
test %>%
  select(one_of(binary_predictors)) %>%
  select(-one_of(test_nzv)) %>%
  map(~table(., test$Class)) %>%
  map(~fisher.test(.)) %>%
  map_df("estimate") %>%
  gather(key = "predictor", value = "OR") %>%
  arrange(-OR)


## ----eval = FALSE-------------------------
## is_binary <- function(col){
##   n_unique <- length(unique(col))
##   result <- ifelse(n_unique == 2, TRUE, FALSE)
##   return(result)
##   }


## -----------------------------------------
stats_test %>%
  select_if(is.numeric) %>%
  drop_na() %>%
  filterVarImp(x = .,
               y = .$score,
               nonpara = TRUE)


## ----varimp-rf-fit, echo = TRUE-----------
varImp(rf_fit1)$importance %>%
  rownames_to_column %>%
  arrange(-Overall) %>%
  head


## -----------------------------------------
varImp(glm_fit1)


## ----caret-a2, eval = FALSE---------------
## # Laden und Betrachten:
## data(Boston, package = "MASS")
## glimpse(Boston)
## 
## # Nur numeric/ integer betrachten:
## Boston %>%
##   select_if(is.numeric)
## 
## # nur *nicht* numerische Variablen betrachten:
## Boston %>%
##   select_if(Negate(is.numeric))
## 
## # Deskriptive Statistiken für alle Variablen:
## Boston %>%
##   skimr::skim()
## 
## # Anzahl fehlender Werte:
## sum(is.na(Boston))


## ----eval = FALSE-------------------------
## Boston_long <- tidyr::gather(Boston)
## ggplot(Boston_long, aes(value)) +
##   geom_density() + facet_wrap(~key, scales = 'free')


## ----eval = FALSE-------------------------
## Boston %>%
##   map2(., names(.),
##        ~{ggplot(data_frame(.x), aes(.x)) +
##            geom_histogram() +
##            labs(x = .y,
##                 title = .y)
##          })


## 1. Nimm den Datensatz `Boston` UND DANN

## 2. Ordne jede Spalte und den Namen (`names()`) jeder Spalte folgendem Befehl zu:

## 3. dl `ggplot()`; die ggplot-Gruppe soll als Ganze geplottet werden, daher die geschweifte Klammer `{`.

## Mit `.x` werden die Spalten angesprochen und mit `.y` die Namen der Spalten.

## 

## ----my-crossval-a2-3, eval = FALSE-------
## k <- 10
## r <- 5
## my_crossval <- trainControl(method = "repeatedcv",
##                            number = k,
##                            repeats = r)


## ----train-test-boston, eval = FALSE------
## n_train <- round(.8 * nrow(Boston), digits = 0)
## train_index <- sample(1:nrow(Boston), size = n_train)
## train <- Boston[train_index, ]
## test <- Boston[-train_index, ]


## ----rf-fit-boston, eval = FALSE----------
## Boston_rf_fit <- train(medv ~ ., data = train,
##                  method = "rf",  # Random Forest
##                  trControl = my_crossval,
##                  tuneLength = 5,
##                  importance = TRUE)
## Boston_rf_fit


## ----rf-boston-performance, eval = FALSE----
## test$Boston_rf_pred <- predict(Boston_rf_fit, newdata = test)
## postResample(pred = test$Boston_rf_pred, obs = test$medv)


## ----eval = FALSE-------------------------
## varImp(Boston_rf_fit)

