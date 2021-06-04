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


library(ggplot2)


# change ggplot2 color scheme:
theme_set(theme_minimal())

options(ggplot2.continuous.colour="viridis")
options(ggplot2.continuous.fill = "viridis")

scale_colour_discrete <- scale_colour_viridis_d
scale_fill_discrete <- scale_fill_viridis_d



library(methods)  # sometimes not loaded, although built-in, appears like a bug


## ----step-Einlesen, echo = FALSE----------
knitr::include_graphics("images/tidy/Einlesen.png")


## Lernziele:

## 
## - Die wesentlichen Datenstrukturen von R kennen

## - Verstehen, warum bzw. inwiefern Vektoren im Zentrum der Datenstrukturen stehen

## - Einige Unterschiede zwischen den Datenstrukturen kennen

## - Datenstrukturen erzeugen können

## 

## ----libs-data-objects--------------------
library(tidyverse)


## ----libs-hidden-datenstrukturen, echo = FALSE----
library(knitr)
library(kableExtra)


## ----objektenarten-tab, echo = FALSE------
d <- tribble(
  ~Dimension,          ~Homogen,         ~Heterogen,
  "1",          "reiner Vektor, Faktor",             "Liste",
  "2",          "Matrix",                            "Dataframe (Tibble",
  "beliebig",   "Array",                             ""   
    )

knitr::kable(d, format = "latex", caption = "Zentrale Objektarten in R") %>%
  row_spec(0, bold = TRUE)


## ----zentrale-objektarten, echo = FALSE, fig.cap = "Zentrale Objektarten in R"----

knitr::include_graphics("images/Rahmen/zentrale_Objektarten-crop.pdf")



## -----------------------------------------
ein_vektor <- c(1, 2, 3)
ein_kurzer_vektor <- c(1)



## ----datenstrukturen, echo = FALSE, fig.cap = "Datenstrukturen in R; der Vektor steht im Mittelpunkt"----
knitr::include_graphics("images/Rahmen/Datenstrukturen-crop.pdf")


## -----------------------------------------
num_vector <- c(1, 2.71, 3.14)
int_vector <- c(1L, 2L, 3L)  # Das L sorgt für ganze Zahlen (nicht reell)
chr_vector <- c("Hallo ", "R")
lgl_vector <- c(TRUE, FALSE, T, F)  # TRUE/T sind gleich, F/FALSE auch



## -----------------------------------------
ein_vektor <- c(a = 1, b = 2, c = 3)
str(ein_vektor)


## -----------------------------------------
attributes(ein_vektor)
attr(ein_vektor, "Autor") <- "student"
attr(ein_vektor, "Datum") <- 2017
attr(ein_vektor, "Datum")


## -----------------------------------------
sex <- factor(c("mann", "frau", "frau", "frau"))
str(sex)


## -----------------------------------------
fieber <- factor(c(41, 40, 40.5, 41))
fieber
fieber_nichtnum <- as.numeric(fieber)
fieber_nichtnum


## -----------------------------------------
fieber_chr <- as.character(fieber)
fieber_num <- as.numeric(fieber_chr)
fieber_num
fieber_num <- as.numeric(as.character(fieber))  # kompakter und identisch


## -----------------------------------------
tage <- factor(c("Mittwoch", "Montag", "Mittwoch", "Dienstag", "Dienstag"))
levels(tage)


## -----------------------------------------
levels(tage)


## -----------------------------------------
tage <- factor(tage, levels = c("Montag", "Dienstag", "Mittwoch"))
levels(tage)


## -----------------------------------------
fct_inorder(tage)


## -----------------------------------------
attributes(tage)


## -----------------------------------------
eine_liste <- list(1, c(1, 2), c(TRUE, FALSE), c("Hallo, R ", "Liste"))
str(eine_liste)


## -----------------------------------------
eine_matrix <- 1:6
dim(eine_matrix) <- c(3, 2)  # erst Zeilenzahl, dann Spaltenzahl
eine_matrix
class(eine_matrix)  # Objekttyp?


## -----------------------------------------
ein_df <- data.frame(essen = c("Suppe", "Suppe", "Pizza"),
                     geschmack = c(2, 2, 5))
str(ein_df)


## ----einfacher-df, echo = FALSE-----------
knitr::kable(ein_df, caption = "Ein einfacher Dataframe", format = "latex") %>%
  row_spec(0, bold = TRUE)


## -----------------------------------------
ein_df2 <- data_frame(essen = c("Suppe", "Suppe", "Pizza"),
                      geschmack = c(2, 2, 5))
str(ein_df2)


## ----eval = FALSE-------------------------
## ein_df <- as_tibble(ein_df)
## ein_tbl <- as_tibble(x)
## ein_tbl2 <- as_tibble(eine_liste)


## -----------------------------------------
ein_vektor


## -----------------------------------------
x <- c(2.2, 3.3, 4.4)


## -----------------------------------------
x[1]


## -----------------------------------------
x[c(1,2)]


## -----------------------------------------
x[c(1, 1, 2, 3, 1)]


## -----------------------------------------
x[-1]
x[-c(1,2)]


## -----------------------------------------
x[c(TRUE, FALSE, TRUE)] # Ich will das 1. und 3. Objekt sehen


## -----------------------------------------
x > 3


## -----------------------------------------
x[x > 3]


## -----------------------------------------
pruefung_x <- x > 3
x[pruefung_x]


## -----------------------------------------
Noten <- c("Anna" = 1.3, "Berta" = 2.7, "Carla" = 4.3)
Noten["Anna"]  # nur ein Element indizieren
Noten[c("Anna", "Anna", "Carla")]  # oder mehrere


## -----------------------------------------
eine_matrix[1,1]
eine_matrix[c(1,2), 1]


## -----------------------------------------
eine_matrix[ ,1]
eine_matrix[1, ]


## -----------------------------------------
eine_matrix[5]


## -----------------------------------------
eine_liste[1]  # erstes Element
eine_liste[c(2,3)]  # zweites und drittes Element


## -----------------------------------------
x <- eine_liste[[2]]
str(x)


## -----------------------------------------
eine_liste <- list(L1 = 1,
                   L2 = c(l2_1 = 1, l2_2 = 2),
                   L3 = c(l3_1 = TRUE, l3_2 = FALSE),
                   L4 = c(l4_1 = "Hallo ", l4_2 = "Liste"))
eine_liste$L2


## -----------------------------------------
x <- eine_liste[["L2"]]
str(x)


## -----------------------------------------
x[1]


## -----------------------------------------
eine_liste[["l2"]][1]


## -----------------------------------------
eine_liste[[c(2, 1)]]


## -----------------------------------------
# gibt Dataframe zurück:
ein_df[1]
ein_df["essen"]

# gibt Vektor zurück:
ein_df[[1]]
ein_df[["essen"]]

ein_df$essen
ein_df$essen[1]


## -----------------------------------------
ein_df[1, 1]  # zuerst die  Zeile, dann die Spalte
ein_df[2, c(1,2)]
ein_df[1, ]


## Tippt man in RStudio einen oder mehrere Buchstaben eines Objekts ein, das in der Arbeitsumgebung existiert, so kann man mit der Tabulator-Taste Vorschläge für passende Objekte bekommen. Das ist praktisch; man spart Tipparbeit.

