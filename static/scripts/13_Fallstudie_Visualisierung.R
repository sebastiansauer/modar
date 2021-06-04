## Lernziele:

## 
## - Diagramme für nominale Variablen erstellen können

## - Balkendiagramme mit Prozentpunkten auf der Y-Achse erstellen können

## - Balkendiagramme drehen können

## - Text-Labels an Balkendiagramme anfügen können

## - Farbschemata von Balkendiagrammen ändern können

## 

## ----libs-vis-casestudy-------------------
library(likert)
library(viridis)  
library(scales)
library(tidyverse)  

data(extra, package = "pradadata")


## ----libs-hideen-vis-fallstudie, echo = FALSE----
#library(magrittr)


## ----extra-items--------------------------
extra %>%
  select(i01:i10) %>%
  mutate_all(factor) %>%
  as.data.frame() -> extra_items


## ----extra-items-likert-------------------
extra_items %>%
  likert() -> extra_items_likert


## ----p-likert1, fig.cap = "Umfrageergebnisse visualisieren mit `likert`"----
plot(extra_items_likert)


## ----extra-items-recode-factor------------
extra_items %>%
  mutate_all(recode_factor,
             "1" = "stimme nicht zu",
             "2" = "stimme eher nicht zu",
             "3" = "stimme eher zu",
             "4" = "stimme voll und ganz zu") %>%
  as.data.frame -> extra_items_r


## ----extra-items2-------------------------
extra_items <- dplyr::select(extra, 3:12)


## ----extra-items-long---------------------
extra_items %>%
  gather(key = items, value = Antwort) %>%
  mutate(items = factor(items),
         Antwort = factor(Antwort)) -> extra_items_long


## ----vis1-p1, fig.cap = "Relative Häufigkeiten dargestellt anhand von Balkendiagrammen"----
p1 <- ggplot(data = extra_items_long) +
  aes(x = items)  +
  geom_bar(aes(fill = Antwort), position = "fill")  +
  theme(legend.position = "bottom",
        text = element_text(size = 4))  +
  scale_fill_viridis(discrete = TRUE)


## Was macht dieser `ggplot`-Befehl? Schauen wir es uns im Einzelnen an:

## 
## `ggplot(data = ...)`: Wir sagen: "Ich möchte gern die Funktion ggplot nutzen, um den Datensatz ... zu plotten."

## `aes(...)`: Hier definieren wir die "aesthetics" des Diagramms, d.h. alles "Sichtbare". Wir ordnen in diesem Fall der X-Achse die Variable `items` zu. Per Standardeinstellung geht `ggplot` davon aus, dass Sie die Häufigkeiten der X-Werte auf der Y-Achse haben wollen, wenn Sie nichts über die Y-Achse sagen. Jetzt haben wir ein Koordinatensystem definiert (das noch leer ist).

## `geom_bar()`: "Hey, R oder ggplot, jetzt male mal einen barplot in den ansonsten noch leeren plot."

## `aes(fill = Antwort)`: Genauer gesagt nutzen wir `aes`, um einem sichtbaren Aspekte des Diagramms (wie die X-Achse) eine Variable des Datensatzes zuzuordnen. Jetzt sagen wir, dass die Füllung (im Balkendiagramm) durch die Werte von `Antwort` definiert sein sollen (also "1", "2" etc.).

## `position = "fill"` sagt, dass die Gesamt-Höhe des Balkens aufgeteilt werden soll mit den "Teil-Höhen" der Gruppen (Antwort-Kategorien 1 bis 4); wir hätten die Teil-Höhen auch nebeneinanderstellen können.

## `theme()` definiert die Nicht-Datenteile des Diagramms wie Textgröße oder Position der Legende.

## `scale_fill_viridis()` wendet die Viridis-Farbpalette zur Füllung der Geome an (nicht für deren Linien); dazu muss das Paket `viridis` verfügbar sein. Bei diskreten Variablen, wie im vorliegenden Fall, muss der Parameter `discrete = TRUE` ergänzt werden.

## 

## ----p-nona, eval = FALSE-----------------
## extra_items_long <- drop_na(extra_items_long)
## 
## ggplot(data = extra_items_long) +
##   aes(x = items)  +
##   geom_bar(aes(fill = Antwort), position = "fill") +
##   scale_fill_viridis(discrete = TRUE)


## ----visp1-flip, fig.cap = "Rotiertes Balkendiagramm", eval = TRUE----
p1_flip <- p1 + coord_flip()  


## ----vis1, echo = FALSE, fig.cap = "Balkendiagramm, unrotiert und rotiert", out.width = "100%"----
gridExtra::grid.arrange(p1, p1_flip, nrow = 1)


## ----rev-levels-items---------------------
extra_items_long %>%
  mutate(Antwort = factor(Antwort,
                          levels = rev(levels(Antwort)))) ->
  extra_items_long_rev


## ----eval = FALSE-------------------------
## levels(extra_items_long_rev$Antwort)
## levels(extra_items_long_rev$Antwort) %>% rev


## ----eval = FALSE-------------------------
## extra_items_long_rev %>%
##   ggplot(aes(x = items)) +
##   geom_bar(aes(fill = Antwort), position = "fill") +
##   coord_flip()


## -----------------------------------------
item_labels <- c("Ich bin das erste Item",
                 "Das zweite Item",
                 "Item 3 sdjfkladsjk",
                 "Ein Couch-Potato UMKODIERT",
"i5 asf", "i6 sdf", "adfjks", "sfjlkd", "sdfkjl", "sdfjkl") %>% factor()


## ----vis7-left-plot-----------------------
ggplot(extra_items_long_rev, aes(x = items)) +
  geom_bar(aes(fill = Antwort), position = "fill") +
  coord_flip() +
  scale_fill_viridis(discrete = TRUE) +
  theme(legend.position = "bottom") +
  labs(x = "", y = "") +
  scale_x_discrete(labels = rev(levels(extra_items_long_rev$items))) +
  guides(fill = guide_legend(reverse = TRUE)) -> p_itemnummern


## ----item-labels2-------------------------
item_labels2 <- fct_inorder(c("Ich bin das erste Item",
                              "Das zweite Item",
                              "Item 03 ",
                              "Beispiel für\nZeilenumbruch",
                              "Item 05", "Item 06", "Item 07",
                              "Item 08", "Item 09", "Item 10"))


## ----meine-palette, results = "hide"------
meine_palette <- c("red", "green", "blue", "yellow")

p2 <- p_itemnummern + scale_fill_manual(values = meine_palette)


## ----change-antwort-levels-no-eval, eval = TRUE----
antwort_labels_rev <- fct_inorder(c("stimme zu",
                                     "stimme eher zu",
                                     "stimme eher nicht zu",
                                     "stimme nicht zu"))

extra_items_long_rev$Antwort2 <- extra_items_long_rev$Antwort
levels(extra_items_long_rev$Antwort2) <- antwort_labels_rev


## ----vis7, fig.cap = "Itemnummern von oben nach unten aufsteigend (links)\n; aussagekräftige Item-Labels", out.width = "100%", fig.asp= 0.7----
p_itemlabels <-
  extra_items_long_rev %>%
  drop_na() %>%
  ggplot(aes(x = items)) +
  geom_bar(aes(fill = Antwort2), position = "fill") +
  coord_flip() +
  scale_fill_grey(name = "",
                  guide = guide_legend(reverse = TRUE,
                                       nrow = 2,
                                       keywidth = 0.5,
                                       keyheight = 0.5)) +
  scale_x_discrete(labels = rev(levels(item_labels2))) +
  labs(title = "Die Ergebnisse \nder Kundenbefragung",
       caption = paste0("N = ",nrow(extra))) +
  theme(axis.text = element_text(size = rel(0.7)),
        plot.title = element_text(size = rel(0.7)),
        legend.position = "bottom",
        legend.text = element_text(size = rel(.5)))


gridExtra::grid.arrange(p_itemnummern, p_itemlabels, nrow = 1)


## ----p2-plus-labs, eval = FALSE-----------
## p2 + labs(title = "Häufigkeiten der Antworten",
##           subtitle = paste0("N = ",nrow(extra)),
##           caption = "Die Daten wurden 2016 erhoben")


## ----eval = FALSE-------------------------
## p2 + labs(title = "Häufigkeiten der Antworten",
##           subtitle = "N = viel",
##           caption = "Die Daten wurden 2016 erhoben") +
##   theme_classic() +
##   theme(axis.ticks = element_blank(),
##         axis.text = element_text(size = 6))


## ----extra-items-count--------------------
extra_items_long_rev %>%
  filter(items == "i01") %>%
  count(Antwort) %>%
  mutate(n_prop = n / sum(n)) -> extra_items_count


## ----p5-----------------------------------
legend_position <- c(.8, .7)

extra_items_count %>%
  ggplot() +
  aes(x = Antwort, y = n) +
  geom_col(aes(fill = Antwort)) +
  geom_text(aes(label = n), vjust = 1.5, size = 2)+
  theme(legend.position = legend_position,
        text = element_text(size = 6)) -> p5


## ----p6-----------------------------------
extra_items_count %>%
  ggplot() +
  aes(x = Antwort, y = n_prop) +
  geom_col(aes(fill = Antwort)) +
  geom_text(aes(label = percent(round(n_prop, 2))), vjust = 1.5, size = 2) +
  theme(legend.position = legend_position,
        text = element_text(size = 6)) +
  scale_y_continuous(labels = percent) -> p6


## ----show-levels--------------------------
levels(extra_items_count$Antwort)


## ----p7-----------------------------------
library(tidyverse)
extra_items_count %>%
  ggplot() +
  aes(x = reorder(Antwort, -n_prop), y = n_prop) +
  geom_col(aes(fill = Antwort)) +
  geom_text(aes(label = percent(round(n_prop, 2))), vjust = 1.5, size = 2) +
  theme(legend.position = legend_position,
       text = element_text(size = 6)) +
  scale_y_continuous(labels = percent) -> p7


## ----p-balken-schrift-sortiert, echo = FALSE, fig.cap = "Balkendiagramme mit Zahlen und sortiert", out.width = "100%"----
gridExtra::grid.arrange(p5, p6, p7, nrow = 1)

