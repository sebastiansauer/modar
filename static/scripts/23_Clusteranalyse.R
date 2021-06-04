## Lernziele:

## 
## - Das Ziel einer Clusteranalyse erläutern können

## - Das Konzept der euklidischen Abstände verstehen

## - Eine k-Means-Clusteranalyse berechnen und interpretieren können

## 

## -----------------------------------------
library(tidyverse)
library(cluster)
library(broom)

data(extra, package = "pradadata")


## ----cluster-intuition, echo = FALSE------


set.seed(2014)
centers <- data.frame(cluster=factor(1:3), size=c(100, 150, 50), x1=c(5, 0, -3), x2=c(-1, 1, -2))
points <- centers %>% group_by(cluster) %>%
    do(data.frame(x1=rnorm(.$size[1], .$x1[1]),
                  x2=rnorm(.$size[1], .$x2[1])))

p1 <- ggplot(points, aes(x1, x2)) + geom_point() +
  xlab("Lernzeit") + ylab("Klausurpunkte")

p2 <- ggplot(points, aes(x1, x2, color=cluster, shape = cluster)) + geom_point() +
  xlab("Lernzeit") + ylab("Klausurpunkte")


## ----cluster1, echo = FALSE, out.width = "100%", fig.cap = "Ein Streudiagramm -- ohne und mit gekennzeichneten Clustern"----
gridExtra::grid.arrange(p1, p2, nrow = 1)


## ----cluster3, echo = FALSE, fig.cap = "Unterschiedliche Anzahlen von Clustern im Vergleich", out.width = "100%", fig.asp = .5----


points.matrix <- cbind(x1 = points$x1, x2 = points$x2)
kclust <- kmeans(points.matrix, 3)
kclusts <- data.frame(k=1:9) %>% group_by(k) %>% do(kclust=kmeans(points.matrix, .$k))

clusters <- kclusts %>% group_by(k) %>% do(tidy(.$kclust[[1]]))
assignments <- kclusts %>% group_by(k) %>% do(augment(.$kclust[[1]], points.matrix))
clusterings <- kclusts %>% group_by(k) %>% do(glance(.$kclust[[1]]))

p3 <- ggplot(assignments, aes(x1, x2)) + geom_point(aes(color=.cluster)) + facet_wrap(~ k)


p4 <- p3 + geom_point(data=clusters, size=10, shape="x")
p4


## ----cluster4, echo = FALSE, fig.cap = "Die Summe der Innerhalb-Varianz in Abhängigkeit von der Anzahl von Clustern. Ein Screeplot."----
ggplot(clusterings, aes(k, tot.withinss)) + geom_line() + geom_vline(xintercept = 3, linetype = "dashed", color = "grey30") +
  xlab("Anzahl der Cluster") +
  ylab("Summe Varianz within") +
  scale_x_continuous(breaks = 1:9)



## ----distanz, echo = FALSE, fig.cap = "Pythagoras in der Ebene (links) und in 3D (rechts)", out.width = "45%"----

imgs <- c("images/cluster/distanz-crop.pdf",
          "images/cluster/pythagoras2-crop.pdf")

knitr::include_graphics(imgs)


## ----pythagoras, echo = FALSE, fig.cap = "Pythagoras in Reihe geschaltet"----

knitr::include_graphics("images/cluster/pythagoras-crop.pdf")


## Um den Abstand zweier Objekte mit $k$ Merkmalen zu bestimmen, kann der euklidische Abstand berechnet werden mit. Bei $k=3$ Merkmalen lautet die Formel dann $e^2 = a^2 + b^2 + d^2$. Bei mehr als drei Merkmalen erweitert sich die Formel entsprechend.

## 

## ----profs-films--------------------------
profs <- data_frame(
  film1 = c(9, 1, 8),
  film2 = c(8, 2, 7),
  film3 = c(1, 8, 3),
  film4 = c(2, 3, 2),
  film5 = c(7, 2, 6))


## ----dist-profs---------------------------
dist(profs)


## ----echo = FALSE-------------------------
options(max.print = 100)


## ----dist-segment-------------------------
extra %>%
  dplyr::select(n_facebook_friends, n_hangover, extra_single_item) %>%
  head() %>%
  dist()


## ----daisy--------------------------------
extra %>%
  dplyr::select(n_facebook_friends, sex, extra_single_item) %>%
  drop_na() %>%
  mutate(sex = factor(sex)) %>%
  mutate_if(is.numeric, .funs = funs(scale)) %>%
  head() %>%
  cluster::daisy(.)


## ----kmeans1------------------------------
set.seed(42)

extra %>%
  mutate(Frau = sex == "Frau") %>%
  dplyr::select(n_facebook_friends, Frau, extra_single_item) %>%
  drop_na() %>%
  scale -> extra_cluster

kmeans_extra_4 <- kmeans(extra_cluster, centers = 4, nstart = 10)


## -----------------------------------------
kmeans_extra_4$centers


## Je größer die Varianz innerhalb der Cluster, umso schlechter ist die Clusterlösung.

## 

## ----cluster-bsp, echo = FALSE, fig.cap = "Schematische Darstellung zweier einfacher Clusterlösungen; links: geringe Varianz innerhalb der Cluster; rechts: hohe Varianz innerhalb der Cluster", out.width="50%"----

knitr::include_graphics("images/cluster/cluster_bsp-crop.pdf")



## -----------------------------------------
kmeans_extra_2 <- kmeans(extra_cluster, centers = 2, nstart = 10)
kmeans_extra_3 <- kmeans(extra_cluster, centers = 3, nstart = 10)
kmeans_extra_5 <- kmeans(extra_cluster, centers = 5, nstart = 10)
kmeans_extra_6 <- kmeans(extra_cluster, centers = 6, nstart = 10)



## -----------------------------------------
streuung_innerhalb <- c(kmeans_extra_2$tot.withinss,
                        kmeans_extra_3$tot.withinss,
                        kmeans_extra_4$tot.withinss,
                        kmeans_extra_5$tot.withinss,
                        kmeans_extra_6$tot.withinss)

streuung_df <- data_frame(
  streuung_innerhalb,
  anzahl_cluster = 2:6
)


## ----clust-streuung-df, fig.cap = "Abnahme der Innerhalb-Streuung als Funktion der Clusterzahl"----
ggplot(streuung_df) +
  aes(x = anzahl_cluster,
      y = streuung_innerhalb) +
  geom_col() +
  geom_line()


## 
## 1. Die Clusteranalyse wird gemeinhin dazu verwendet, Objekte nach Ähnlichkeit zu Gruppen zusammenzufassen.

## 2. Die Varianz innerhalb eines Clusters kann als Maß für die Anzahl der zu extrahierenden Cluster herangezogen werden.

## 3. Unter euklidischer Distanz versteht man jedes Maß, welches den Abstand zwischen Punkten in der Ebene misst.

## 4. Bei der k-Means-Clusteranalyse darf man die Anzahl der zu extrahierenden Cluster nicht vorab festlegen.

## 5. Cluster einer k-Means-Clusteranalyse werden so bestimmt, dass die Cluster möglichst homogen sind, d.h. möglichst wenig Streuung aufweisen (also möglichst nah am Cluster-Zentrum liegen).

## 6. Der Satz des Pythagoras ist auch im 3D-Raum anwendbar.

## 7. Der Satz des Pythagoras ist auch im $n$D-Raum anwendbar.

## 8. Mit der Funktion `dist()` kann man sich die euklidische Distanz zwischen Objekten mit mehreren Attributen ausgeben lassen.

## 9. Bei einer Clusteranalyse müssen alle Attribute metrisch sein.

## 10. Ein Ellbogen-Diagramm ist eine Methode, um die Anzahl der "richtigen" Cluster in einer Clusteranalyse zu bestimmen.

## 

## ----kmeans2, results = "hide", eval = FALSE----
## set.seed(42)
## 
## extra %>%
##   select(i01:i10) %>%
##   drop_na() -> extra_items
## 
## kmeans(extra_items, centers = 1) -> kmeans_items_1
## kmeans(extra_items, centers = 2) -> kmeans_items_2
## kmeans(extra_items, centers = 3) -> kmeans_items_3
## kmeans(extra_items, centers = 4) -> kmeans_items_4
## 
## streuung_items_innerhalb <- c(c1 = kmeans_items_1$tot.withinss,
##                               c2 = kmeans_items_2$tot.withinss,
##                               c3 = kmeans_items_3$tot.withinss,
##                               c4 = kmeans_items_4$tot.withinss)
## 
## streuung_items_innerhalb %>%
##   as_tibble %>%
##   add_column(n_centers = 1:4) %>%
##   ggplot(aes(x = n_centers, y = value)) +
##   geom_line(color = "grey20") +
##   geom_point(size = 3)


## ----kmeans3, results = "hide", eval = FALSE----
## centers <- 1:10
## 
## centers %>%
##   map(~kmeans(extra_items, centers = .)) -> kmeans_extra_list
## 
## kmeans_extra_list %>%
##   map("tot.withinss") %>%
##   map_dfr(~data_frame(tot_wihin_ss = .)) %>%
##   add_column(n_centers = centers) %>%
##   ggplot(aes(x = n_centers, y = tot_wihin_ss, group = 1)) +
##   geom_line(color = "grey40") +
##   geom_point(size = 3) +
##   scale_x_continuous(breaks = centers)

