## Lernziele:

## 
## - Farbschemata wählen

## - Nicht-Datenteile eines ggplot-Diagramms ändern

## - Interaktive Diagramme erstellen

## 

## ----libs-visualisieren-------------------
library(tidyverse)  
library(gridExtra)
library(wesanderson)  
library(RColorBrewer)
library(gridExtra)  
library(ggthemes)  
library(viridis)

data(flights, package = "nycflights13")
data(movies, package = "ggplot2movies")


## ----p-brewer-qual, out.width = "100%", fig.asp = .62, out.width="50%", fig.cap = "Farbpaletten für nominale Variablen", echo = FALSE----
display.brewer.all(type="qual")


## ----p-brewer-seq, out.width = "100%", fig.asp = 1, out.width="50%", fig.cap = "Farbpaletten für unipolare numerische Variablen", echo = FALSE----
display.brewer.all(type="seq")


## ----p-brewer-diverging, out.width = "100%", fig.asp = .62, out.width="50%", fig.cap = "Farbpaletten für bipolare numerische Variablen", echo = FALSE----
display.brewer.all(type="div")


## ----flights-brewerpal, out.width = "100%", fig.cap = "Brewer-Paletten"----
data(flights)

p0 <- flights %>%
  filter(dest %in% c("BOS", "ATL", "LAX")) %>%
  ggplot() +
  aes(x = dest, y = arr_delay, color = dest) +
  geom_boxplot()

p_brewer1 <- p0 + scale_color_brewer(palette = "Set1")

p_brewer2 <- p0 + scale_color_brewer(palette = "Set2")

grid.arrange(p_brewer1, p_brewer2, nrow = 1)



## ----transform-movies---------------------
movies %>%
  gather(key = genre, value = is_true, -c(title:mpaa)) %>%
  filter(is_true == 1) %>%
  mutate(multiple_genre = duplicated(title)) %>%
  mutate(genre = ifelse(multiple_genre, "multiple", genre)) -> movies2


## ----wes-plot, out.width = "100%", fig.cap = "Die Farbpaletten von Wes Anderson"----
movies2 %>%
  filter(genre %in% c("Action", "Drama", "multiple")) %>%
  sample_n(5000) %>%
  ggplot() +
  aes(x = budget, y = rating, color = genre) +
  geom_point(alpha = .5) +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_text(size = rel(.5))) -> p_blank

p_wes1 <- p_blank + scale_color_manual(values = wes_palette("Darjeeling1")) +
  labs(title = "Palette: Darjeeling1")

p_wes2 <- p_blank + scale_color_manual(values = wes_palette("GrandBudapest1")) +
  labs(title = "Palette: GrandBudapest1")


meine_farben <- c("red", "blue", "#009981")

p_own <- p_blank + scale_color_manual(values = meine_farben) +
  labs(title = "Palette: c('red', 'blue', '#009981')")

grid.arrange(p_wes1, p_wes2, p_own, ncol = 3)


## ----movies-boxplot, eval = FALSE---------
## movies2 %>%
##   filter(genre %in% c("Action", "Drama", "multiple")) %>%
##   ggplot(aes(x = genre, y = budget)) +
##   geom_boxplot(fill = c("red", "blue", "green")) -> movies_boxplot
## 


## ----viridis-test, fig.cap = "Die Viridis-Farbpalette", out.width= "100%", out.width="100%"----
p_viridis1 <- movies2 %>%
  filter(genre %in% c("Action", "Drama", "multiple")) %>%
  ggplot(aes(x = genre, y = budget, color = genre)) +
  geom_boxplot() +
  scale_color_viridis(discrete = TRUE, guide = FALSE)

p_viridis2 <- movies2 %>%
  filter(genre %in% c("Action", "Drama", "multiple")) %>%
  ggplot() +
  aes(x = genre, fill = genre) +
  geom_bar() +
  scale_fill_viridis(discrete = TRUE, guide = FALSE) +
  theme(legend.position = "right")


p_viridis3 <- ggplot(data.frame(x = rnorm(10000), y = rnorm(10000)), 
                     aes(x = x, y = y)) +
  geom_hex() + coord_fixed() +
  scale_fill_viridis() +
  theme(legend.position = "bottom")


grid.arrange(p_viridis1,  p_viridis3, nrow = 1)



## ----ggplot-themes, out.width = "100%", fig.cap = "Themen von ggplot2", fig.asp = .75----
p_flights <- flights %>%
  filter(dest %in% c("BOS", "ATL", "LAX")) %>%
  ggplot() +
  aes(x = dest, y = air_time) +
  geom_boxplot() +
  theme(legend.position = "none")

p_flights1 <- p_flights +
  theme_classic() +
  ggtitle("theme_classic")

p_flights2 <- p_flights +
  theme_bw()  +
  ggtitle("theme_bw")

p_flights3 <- p_flights +
  theme_minimal() +
  ggtitle("theme_minimal")

p_flights4 <- p_flights +
  theme_void() +
 ggtitle("theme_void")

grid.arrange(p_flights1, p_flights2,
             p_flights3, p_flights4, ncol = 2)


## ----cowplot, fig.cap = "GGplot mit dem Thema 'cowplot'"----
library(cowplot)

flights %>%
  filter(dest %in% c("BOS", "ATL", "LAX")) %>%
  ggplot() +
  aes(x = dest, y = air_time, color = dest) +
  geom_boxplot()


## ----unload-cowplot, echo = FALSE---------
detach("package:cowplot", unload=TRUE)


## ----bw-plots, out.width= "100%", fig.cap = "Diagramme in Schwarz-Weiß bzw. Graustufen"----
flights %>%
  sample_n(100) %>%
  ggplot() +
  aes(x = origin, y = arr_delay, shape = origin, fill = origin) +
  theme_bw() -> p_bw_base

p_bw1 <- p_bw_base + geom_point() + theme(legend.position = "none")
p_bw2 <- p_bw_base + geom_boxplot() + scale_fill_grey() +
  theme(legend.position = "none") + ylab("")
p_bw3 <- p_bw_base + geom_line(aes(x = dep_delay, linetype = origin)) + ylab("")

grid.arrange(p_bw1, p_bw2, p_bw3, nrow = 1)


## ----plotly-demo-no-eval, eval = FALSE----
## p_stats <- ggplot(data = stats_test) +
##            aes(x = interest, fill = bestanden) +
##            geom_density(alpha = .3)
## 
## p_stats_plotly <- ggplotly(p_stats)
## p_stats_plotly


## ----plotly-demo, fig.cap = "Eine statische Variante eines dynamischen Diagramms mit plotly", echo = FALSE, out.width = "50%"----
#file.exists("images/visualisieren/plotly_demo.png")
knitr::include_graphics("images/visualisieren/plotly_demo.png")


## ----3dplotly-no-eval, eval = FALSE-------
## plot_ly(mpg, x = ~cty, y = ~hwy, z = ~cyl) %>%
##   add_markers(color = ~cyl)


## ----threedplotly, fig.cap = "Ein 3D-Punktediagramm mit Plotly", echo = FALSE, out.width = "50%"----
knitr::include_graphics("images/visualisieren/plotly_3d_demo.png")


## ----3d-network-no-eval, eval = FALSE-----
## data(MisLinks, MisNodes)
## forceNetwork(Links = MisLinks, Nodes = MisNodes, Source = "source",
##              Target = "target", Value = "value", NodeID = "name",
##              Group = "group", opacity = 0.2)


## ----3d-network, echo = FALSE, fig.cap = "Ein interaktives Netzwerk-Diagramm"----
if (file.exists("images/visualisieren/network3d.png")) knitr::include_graphics("images/visualisieren/network3d.png")


## 1. Das Farbschema Viridis ist für Rot-Grün-Blindheit ungeeignet.

## 2. Um eine nominale Variable in Farben abzubilden, werden in den Brewer-Paletten andere Farben verwendet als zur Abbildung von quantitativen Farben.

## 3. Um sich eine eigene Farbpalette zu definieren, ist in ggplot der Befehl `scale_color_manual(values = meine_farben)` gedacht.

## 4. Viridis ist für Schwarz-Weiß-Druck ungeeignet.

## 6. Bei ggplot ist das Thema mit grauem Hintergrund `theme_gray` der Standard.

## 7. Ggplot erlaubt es nicht, den grauen Hintergrund abzustellen bzw. eine andere Art des Hintergrunds auszuwählen.

## 8. Mit *interaktiv*  bei Diagrammen ist gemeint, dass der Nutzer das Diagramm verändern kann.

## 8. Über `ggplotly()` kann man interaktive Varianten von ggplot erstellen.

## 9. Über das Paket `htmlwidgets` kann man HTML-Applikationen in R einlesen.

## 10. Das Paket `cowplot` bietet eine Vielzahl von interaktiven Diagrammen für R.

## 
