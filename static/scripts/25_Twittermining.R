## Lernziele:

## 
## - Grundlagen des Twitter-Minings lernen

## - Grundlegende Konzepte des Text-Minings und speziell des Sentiment-Minings anwenden

## - Die Polarität der Emotion in einem Textcorpus anwenden

## - Die Bedeutung und die Konsequenzen für die Organisationsdiagnose einschätzen

## 

## ----libs-twitter-------------------------
library(pradadata)  
library(tidyverse)
library(tidytext)
library(twitteR)
library(tidytext)


## ----twitterauth, echo = FALSE, fig.cap = "Authentifizierung bei Twitters Entwicklungsumgebung"----
knitr::include_graphics("images/Twitter/twitter_auth_cen.jpg")


## ----eval = FALSE-------------------------
## appname <- "meine_app"
## requestURL <- "https://api.twitter.com/oauth/request_token"
## accessURL <- "http://api.twitter.com/oauth/access_token"
## authURL <- "http://api.twitter.com/oauth/authorize"
## consumerKey <- "Abfolge_von_Buchstaben_und_Zahlen"
## consumerSecret <- "lange_Abfolge_von_Buchstaben_und_Zahlen"
## accessToken = "Abfolge_von_Buchstaben_und_Zahlen"
## accessSecret = "Abfolge_von_Buchstaben_und_Zahlen"


## ----read-creds, eval = FALSE-------------
## source("~/Documents/Div/credentials/twitter_oauth.R")
## 
## setup_twitter_oauth(consumer_key = consumerKey,
##                     consumer_secret = consumerSecret,
##                     access_token = NULL,
##                     access_secret = NULL)


## ----search-twitter-pop, eval = FALSE-----
## tweets <- searchTwitter("populismus", lang = "de", n = 100)


## ----tslisttoDF-no-eval, eval = FALSE-----
## tweets %>%
##   twListToDF() -> tweets_df


## ----read-tweets-RDS, echo = FALSE--------
tweets_df <- readRDS(file = "data/tweets_df.RDS")


## ----tweets-s-no-eval, eval = FALSE-------
## tweets_s <- userTimeline("sauer_sebastian",
##                          n = 100)
## tweets_s %>%
##   twListToDF() -> tweets_s_df


## ----tweets-s, echo = FALSE---------------
tweets_s_df <- readRDS(file = "data/tweets_s_df.RDS")


## ----tweets-encoding----------------------
Encoding(tweets_df$text) <- "UTF8"
Encoding(tweets_s_df$text) <- "UTF8"


## -----------------------------------------
tweets_s_df %>%
  dplyr::select(text) %>%
  unnest_tokens(output = Wort, input = text) %>%
  filter(str_detect(Wort, "[a-z]")) %>%
  count(Wort, sort = TRUE) %>%
  head()


## -----------------------------------------
tweets_s_df_cleared <- tweets_s_df %>%
  mutate(text = str_replace_all(string = text,
                                pattern = "[:blank:]*https://[:graph:]+",""))


## ----eval = FALSE-------------------------
## tweets_s_df$text %>% head()
## tweets_s_df_cleared$text %>% head()


## -----------------------------------------
data(polits_twitter, package = "pradadata")
head(polits_twitter)


## ----eval = FALSE-------------------------
## start_time <- Sys.time()
## # tue etwas Wichtiges...
## end_time <- Sys.time()
## end_time - start_time


## ----read-strange3-no-eval, eval = FALSE----
## usernames <- c("realDonaldTrump", "sauer_sebastian", "TweetOfGod")
## start_time <- Sys.time()
## usernames %>%
##   map(userTimeline, n = 50) -> tweets_strange3
## end_time <- Sys.time()
## end_time - start_time


## ----read-strange3, echo = FALSE----------
usernames <- c("realDonaldTrump", "sauer_sebastian", "TweetOfGod")

tweets_strange3 <- readRDS(file = "data/tweets_strange3.RDS")


## Schaue auf die Uhr.

## Nimm den Vektor `usernames` UND DANN

## ordne jedem Element dieses Vektors (d.h. jedem Nutzerkonto) die Funktion

## `userTimeline()` zu, welche die Tweets ausliest UND DANN

## speichere den resultierenden Dataframe als `tweets_strange3`.

## Schau zum Schluss wieder auf die Uhr und sag,

## wie viel Zeit verstrichen ist.

## 

## ----strange3-to-df-----------------------
tweets_strange3 %>%
  map_dfr(twListToDF) -> tweets_strange3_df


## ----read-twitter2, eval = FALSE----------
## tweets <- data.frame()
## 
## for (i in seq_along(usernames)){
##   temp_df <- twListToDF(userTimeline(usernames[i], n = 5))
##   tweets <- rbind(tweets_df, temp_df)
## }


## -----------------------------------------
seq_along(usernames)


## ----init-db, eval = FALSE----------------
## tweets_db <- tempfile()
## register_sqlite_backend(tweets_db)


## ----store-tweets-to-db, results = "hide", eval = FALSE----
## tweets %>%
##   map(store_tweets_db)


## ----read-tweets-from-db, eval = FALSE----
## tweets_aus_db <- load_tweets_db(as.data.frame = TRUE)
## glimpse(tweets_aus_db)


## 1. Twitter gibt *nicht* preis, wer der Autor eines Tweets ist (aus Datenschutzgründen).

## 1. Man darf Tweets unter bestimmten Bedingungen speichern und weitergeben, vorausgesetzt, es handelt sich um "dehydrierte" Tweets.

## 3. Aus Datenschutzgründen macht Twitter nie die Geo-Daten (Längen- und Breitengrade) eines Tweets öffentlich.

## 4. Löscht ein Nutzer einen Tweet, so ist dieser aus Datenschutzgründen nicht mehr über die API abrufbar.

## 5. Über die Twitter-API können nicht mehr als 1000 Tweets pro Tag kostenlos heruntergeladen werden.

## 6. Man kann Twitter über die API sowohl nach Themen, Trends als auch nach Nutzernamen durchsuchen.

## 7. Diese Regex "\\d\\d\\d_\\w+.Rmd" wird zu diesem String "112_NSE.Rmd" passen. Hilfestellung: `str_detect(string, pattern)`.

## 8. Diese Regex "\\d\\d\\d_\\w+.Rmd" wird zu diesem String "12_NSE.Rmd" passen.

## 9. Diese Regex "\\d\\d\\d_\\w+.Rmd" wird zu diesem String "_112.Rmd" passen.

## 10. Diese Regex "^\\d\\d\\d_\\w+.Rmd" wird zu diesem String "_112_NSE.Rmd" passen.

## 
