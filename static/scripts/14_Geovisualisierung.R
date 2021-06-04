## Lernziele:

## 
## - Grundlegende Aspekte der Visualisierung von Kartenmaterial kennen

## - Kartenmaterial mit anderen Daten assoziieren

## - Erste Erfahrungen mit Listenspalten sammeln

## 

## ----geo-libs-----------------------------
library(tidyverse)
library(viridis)
library(stringr)
library(gridExtra)
library(sf)
library(rworldmap)
library(leaflet)
library(ggmap)
library(googleVis)
library(datasets)
library(stringr)
library(ISOcodes)

data(socec, package = "pradadata")
data(cult_values, package = "pradadata")
data(wahlkreise_shp, package = "pradadata")
data(wellbeing, package = "pradadata")
data(elec_results, package = "pradadata")
data(countries, package = "pradadata")


## ----download-de-L------------------------
url <- "https://osf.io/nhy4e/?action=download"
dest_file <- "data/de_L.RData"
if (!file.exists(dest_file)) download.file(url, dest_file)
load(dest_file)
load("data/de_L.RData")


## ----plot-de-total, eval = FALSE----------
## ggplot(data = de_L) +
##   geom_sf(size = .1, color = "lightgrey")


## ----p-de2, fig.cap = "Verwaltungsgrenzen von Bund (links) und Ländern (rechts)", out.width = "100%"----
de_L %>%
  filter(AGZ %in% c(1)) %>%
  ggplot +
  geom_sf() -> p_de1

de_L %>%
  filter(AGZ %in% c(1,2)) %>%
  ggplot +
  geom_sf() -> p_de2

gridExtra::grid.arrange(p_de1, p_de2, nrow = 1)


## ----load-data-wahlkreise-----------------
data("wahlkreise_shp")
glimpse(wahlkreise_shp)


## ----alq-socec-short, eval = TRUE---------
socec_short <- socec %>%  # Spalten wählen und umbenennen
  select(WKR_NAME = V03,
         WKR_NR = V02,
         wegzug = V11, # Wegzugssalde
         migration = V19,  # Migrationsanteil
         auslaender = V08,  # Auslaenderanteil
         alq = V47,  # Arbeitslosenquote
         migration = V19)  # Anteil mit Migrationshintergrund

socec_short %>%  # Plotten (Arbeitslosigkeit)
  left_join(wahlkreise_shp) %>%
  ggplot +
   aes(fill = alq) +
   theme_void()
   geom_sf() -> p1

socec_short %>%  # Joinen und Plotten (Migration)
  left_join(wahlkreise_shp) %>%
  ggplot +
   aes(fill = migration) +
   scale_fill_viridis() +
   theme_void() +
   geom_sf() -> p2



## ----p-alq-no-eval, include=FALSE, eval = FALSE----
## grid.arrange(p1, p2, nrow = 1)


## ----p-alq, echo = FALSE, out.width = "100%", fig.cap = "Arbeitslosigkeit nach Wahlkreis"----
knitr::include_graphics("images/visualisieren/p1p2.png")


## ----eval = FALSE-------------------------
## elec_results %>%
##   select(parent_district_nr, district_name, district_nr) %>%
##   filter(parent_district_nr == 99)


## ----plot-p3------------------------------
elec_results %>%
  filter(parent_district_nr != 99) -> elec_results

elec_results %>%
  select(WKR_NR = district_nr, AfD_3, votes_3) %>%
  mutate(afd_prop = AfD_3 / votes_3) %>%
  left_join(wahlkreise_shp) %>%
  ggplot() +
  aes(fill = afd_prop) +
  geom_sf() +
  scale_fill_viridis() +
  theme_void() +
  labs(caption = "Anteil gültiger Zweitstimmen für die AfD",
       fill = "") -> p3


## ----afd-df1------------------------------
elec_results %>%
  select(WKR_NR = district_nr, AfD_3, votes_3) %>%
  mutate(afd_prop = AfD_3 / votes_3) %>%
  inner_join(wahlkreise_shp) %>%
  inner_join(socec_short) %>%
  mutate(afd_vorhersagefehler1 = lm(afd_prop ~ alq, data = .)$residuals) -> afd_df


## Nimm den Datensatz `elec_results` UND DANN

## wähle die Spalten Distriktnummer, AfD- und Gesamtstimmen aus UND DANN

## vereinige mit den Shape-Daten UND DANN

## vereinige mit den Strukturdaten UND DANN

## erzeuge eine Spalte mit dem Vorhersagefehler.

## 

## ----p-afd-lm1-no-eval, eval = FALSE------
## afd_df %>%
##   ggplot +
##   aes(fill = afd_vorhersagefehler1) +
##   geom_sf() +
##   labs(caption = "Abweichung vom geschätzten AfD-Anteil",
##        fill = "") +
##   scale_fill_viridis() +
##   theme_void() -> p4
## 
## grid.arrange(p3, p4, nrow = 1)


## ----p-afd-lm1, out.width = "100%", fig.cap = "AfD-Wahlergebnisse nach Wahlkreisen", echo = FALSE----
knitr::include_graphics("images/visualisieren/afd_stimmen_btw17.pdf")


## ----afd-lm2------------------------------
afd_df %>%
  mutate(afd_vorhersagefehler2 =
           lm(afd_prop ~ alq + wegzug + migration + auslaender,
              data = .)$residuals) -> afd_df


## ----p-afd-lm2-no-eval, eval = FALSE------
## afd_df %>%
##   ggplot +
##   aes(fill = afd_vorhersagefehler2) +
##   geom_sf() +
##   labs(fill = "") +
##   theme_void()  -> p5
## 
## grid.arrange(p5 + scale_fill_viridis(),
##              p5 + scale_fill_distiller(palette = "Spectral"),
##              nrow = 1)


## ----p-afd-lm2, fig.cap = "Ein komplexeres Erklärungsmodell zum AfD-Erfolg;\nmehrere Strukturparameter wurden zur Vorhersage des AfD-Wahlerfolgs herangezogen", out.width = "100%", echo = FALSE----
knitr::include_graphics("images/visualisieren/afd_multmodell.pdf")


## ----df-will-ich--------------------------
will_ich <- data_frame(
  hin = c("TTO", "COK", "MNG", "CAN"),
  ja_wirklich = rep("will ich hin", times = 4)  # rep wie repeat
)


## ----p-mapcountrydata-no-eval, eval = FALSE----
## will_ich_hin_map <- joinCountryData2Map(will_ich,
##                                         joinCode = "ISO3",
##                                         nameJoinColumn = "hin",
##                                         verbose = TRUE)
## mapCountryData(will_ich_hin_map, nameColumnToPlot="ja_wirklich",
##                catMethod = "categorical",
##                missingCountryCol = gray(.8),
##                addLegend = FALSE,
##                mapTitle = "")


## ----p-mapcountrydata, fig.cap = "Einfaches Choropleth-Diagramm", out.width = "70%", echo = FALSE, eval = TRUE----
knitr::include_graphics("images/geo/worldmap1-crop.pdf")


## ----will-ich-geom-sf---------------------
world_sf <- st_as_sf(countriesLow)  

p_world_sf1 <- ggplot() +
  geom_sf(data = world_sf) +
  aes(fill = REGION) +  # Füllfarbe entsprechend der Region
  theme(legend.position = "none")


## ----will-ich-geom-sf2-no-eal, eval = FALSE----
## p_world_sf2 <- world_sf %>%
##   filter(REGION == "Europe") %>%
##   ggplot() +
##   aes(fill = REGION) +
##   geom_sf()  +
##   theme(legend.position = "none")


## ----eval = FALSE, echo = FALSE-----------
## ggsave(p_world_sf1, file = "images/geo/p_world_sf1.png",
##        width = 6,
##        height = 6)
## 
## ggsave(p_world_sf2, file = "images/geo/p_world_sf2.png",
##        width = 6,
##        height = 6)


## ----eu-sf, fig.cap = "Geo-Visualisierung mit sf", echo = FALSE, out.width = "100%", eval = TRUE, cache = FALSE----

knitr::include_graphics("images/geo/world_sf2-crop.png")



## ----eu-names-----------------------------
eu <- c("BE", "BG", "CZ", "DK", "DE", "EE", "IE", "GR", "ES", "FR",
        "HR", "IT", "CY", "LV", "LT", "LU", "HU", "MT", "NL", "AT",
        "PL", "PT", "RO", "SI", "SK", "FI", "SE", "GB")


## ----load-eu-names------------------------
countries %>%
  filter(cca2 %in% eu) %>% nrow


## ----eu-countries-------------------------
countries %>%
  mutate(is_EU = ifelse(cca2 %in% eu, TRUE, FALSE)) -> countries


## ----eval = FALSE-------------------------
## world_sf %>%
##   filter(ISO_A2 %in% eu) %>%
##   ggplot() +
##   aes() +
##   geom_sf(fill = "steelblue", alpha = .5) +
##   coord_sf(xlim = c(-20, 40), ylim = c(30, 70))


## ----echo = FALSE, eval = FALSE-----------
## ggsave(file = "images/geo/eu.png")


## ----p-world-sf-no-eval, eval = FALSE-----
## world_sf %>%
##   filter(ISO_A2 %in% eu) %>%
##   ggplot() +
##   aes() +
##   geom_sf(aes(fill = POP_EST)) +
##   scale_fill_viridis() +
##   coord_sf(xlim = c(-20, 40), ylim = c(30, 70))


## ----echo = FALSE, eval = FALSE-----------
## ggsave(file = "images/geo/eu_pop.png")


## ----p-world-sf, out.width = "100%", fig.cap = "Visualisierung der Bevölkerungszahlen der EU", echo = FALSE, fig.asp= 0.7----
# imgs <- c("images/geo/eu.png",
#           "images/geo/eu_pop.png")
# prada::comb2pngs(imgs)

knitr::include_graphics("images/geo/p-world-sf.png")


## ----perc-rank-demo-----------------------
x <- c(1, 2, 3)
percent_rank(x)


## ----eval = FALSE-------------------------
## S <- (X - min(X)) / (max(X) - min(X))


## ----eval = FALSE-------------------------
## x <- c(1, 2, 3)
## scale(x) %>% as.numeric()


## ----eval = FALSE-------------------------
## names(wellbeing)
## names(cult_values)


## ----mutate-cult-wellbeing----------------
cult_values %>%
  mutate(intel_auton_rank = percent_rank(intel_auton)) -> cult_values

wellbeing %>%
  mutate(Life_satisfaction_rank = percent_rank(Life_satisfaction),
         Country = tolower(Country)) -> wellbeing


## ----recode-missspelled-countries---------
cult_values %>%
  mutate(country_long = recode(country_long,
   "unitedstates" = "united states", # erst alter Wert, dann neuer Wert
   "unitedkingdom" = "united kingdom")) -> cult_values

c("united states", "united kingdom") %in% cult_values$country_long


## ----join-cultvalues-wellbeing------------

wellbeing %>%
  filter(region_type == "country_whole") %>%
  inner_join(cult_values,
             by = c("Country" = "country_long")) -> df_joined


## ----p-rank-comp--------------------------
df_joined %>%
  select(Country, Life_satisfaction_rank, intel_auton_rank) %>%
  gather(key = indicator, value = perc_rank, -Country) %>%
  ggplot +
  aes(x = indicator, y = perc_rank) +
  geom_point(size = 5, alpha = .5) +
  scale_x_discrete(labels = c("Autonomie", "Zufried")) +
  geom_line(aes(group = Country)) -> p_rank_comp


## ----df-joined-cor------------------------
df_joined %>%
  select(Life_satisfaction_rank, intel_auton_rank) %>%
  cor(method = "spearman")


## ----df-joined-cor-hidden, echo = FALSE----
df_joined %>%
  select(Life_satisfaction_rank, intel_auton_rank) %>%
  cor(method = "spearman") %>%
  `[[`(1,2) -> cor_lifsat_intaut


## ----df-joined-small----------------------
df_joined %>%
  mutate(konkordanz = abs(Life_satisfaction_rank - intel_auton_rank)) %>%
  select(Country, konkordanz, 
         Life_satisfaction_rank, intel_auton_rank) -> df_joined_small



## ----plot-countries-----------------------
data(countriesLow)
world_sf <- st_as_sf(countriesLow)


## ----join-worlfd-df-joined----------------
world_sf %>%
  mutate(NAME = tolower(NAME)) %>%
  inner_join(df_joined_small,
             by = c("NAME" = "Country")) -> world_sf_joined


## ----p-condordance-no-eval, eval = FALSE----
## world_sf_joined %>%
##   ggplot +
##   aes(fill = konkordanz) +
##   geom_sf() +
##   theme(legend.position = "bottom") +
##   scale_fill_viridis() -> p_concordance
## 
## grid.arrange(p_rank_comp, p_concordance,
##              layout_matrix = rbind(c(1, 2, 2), c(1,2, 2)))


## ----p-condordance, fig.cap = "Konkordanz von intellektueller Autonomie und Lebenszufriedenheit", out.width = "100%", echo = FALSE----
knitr::include_graphics("images/visualisieren/p_concordance.pdf")


## ----leaflet1-no-eval, eval = FALSE-------
## geo1 <- geocode("Nuremberg")
## 
## m <- leaflet() %>%
##   addTiles() %>%
##   addMarkers(lng = geo1$lon[1], lat = geo1$lat[1],
##              popup = "Wo dieses Buch geschrieben wurde")


## ----leaflet1, echo = FALSE, fig.cap = "Eine interaktive Karte mit `leaflet`"----
knitr::include_graphics("images/visualisieren/leaflet1.png")


## ----GeoStates-no-eval, eval = FALSE, results = "asis"----
## states <- data.frame(state.name, state.x77)
## GeoStates <- gvisGeoChart(data = states,
##                           locationvar = "state.name",
##                           colorvar = "Illiteracy",
##                           options=list(region="US",
##                                        displayMode="regions",
##                                        resolution="provinces",
##                                        width=600, height=400))
## plot(GeoStates)


## ----GeoStates, echo = FALSE, fig.cap = "Ein Choropleth-Diagramm mit googleVis"----
knitr::include_graphics("images/visualisieren/gvis1.png")


## ----de-gvis-iso, eval = TRUE-------------
ISO_de <- ISO_3166_2 %>%
  filter(str_detect(Code, "DE-")) %>%
  arrange()


## ----de-gvis, echo = FALSE, fig.cap = "Eine interaktive Tabelle mit googleVis"----
data("ISO_3166_2")
knitr::include_graphics("images/visualisieren/gvis2.png")


## ----de-google-socec-no-eval, eval = FALSE----
## socec %>%
##   full_join(ISO_de, by = c("V01" = "Name")) %>%
##   filter(V01 != "Deutschland") -> socec_ISO
## 
##  geoChartDE <- list(region="DE",
##                    resolution="provinces",
##                    legend="{numberFormat:'#,###.00'}")
## plot(
##   gvisGeoChart(socec_ISO, locationvar = "Code",
##                colorvar = "V51",
##                options=geoChartDE)
## )


## ----de-google-socec, echo = FALSE, fig.cap = "Arbeitslosigkeit mit googleVis"----
knitr::include_graphics("images/visualisieren/gvis3.png")


## ----a3-----------------------------------
data("socec", package = "pradadata")
library(corrr)

afd_df %>%
  select(afd_prop, alq, migration) %>%
  correlate() %>%
  shave()

lm_a3_1 <- lm(afd_prop ~ alq, data = afd_df)
lm_a3_2 <- lm(afd_prop ~ migration, data = afd_df)


## -----------------------------------------
library(broom)
lm_a4_1 <- lm(afd_prop ~ alq + migration, data = afd_df)
tidy(lm_a3_1)
tidy(lm_a3_2)
tidy(lm_a4_1)


## ----a5-vis, eval = FALSE-----------------
## afd_df$lm_a4 <- lm(afd_prop ~ alq + migration,
##                    data = afd_df)$residuals
## 
## afd_df %>%
##   ggplot +
##   aes(fill = lm_a4) +
##   geom_sf() +
##   labs(title = "Abweichung vom geschätzten AfD-Anteil",
##        fill = "",
##        caption = "Prädiktoren: Ausländer- und Migrationsquote") +
##   scale_fill_viridis() +
##   theme_void()


## -----------------------------------------
elec_results %>%
  select(WKR_NR = district_nr, AfD_3, votes_3) %>%
  mutate(afd_prop = AfD_3 / votes_3) %>%
  inner_join(wahlkreise_shp) %>%
  inner_join(socec_short) -> afd_data

afd_lm1 <- lm(afd_prop ~ alq, data = afd_data)
broom::tidy(afd_lm1)
# broom::glance(afd_lm1)
afd_rsme_lm1 <- mean(afd_lm1$residuals^2) %>% sqrt()

afd_lm2 <- lm(afd_prop ~ alq + wegzug + migration + auslaender,
               data = afd_data)
broom::tidy(afd_lm1)
# broom::glance(afd_lm2)
afd_rsme_lm2 <- mean(afd_lm2$residuals^2) %>% sqrt()



## ----world-sf3, eval = FALSE--------------
## world_sf3 <- world_sf %>%
##   filter(REGION == "Asia") %>%
##   ggplot() +
##   aes(fill = GEO3) +
##   geom_sf() +
##   theme(legend.position = "none")
## world_sf3 + scale_fill_brewer(palette = 2)

