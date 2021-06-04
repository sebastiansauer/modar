## Lernziele:

## 
## - Sie kennen zentrale Ziele und Begriffe des Textminings

## - Sie wissen, was ein Tidytext-Dataframe ist

## - Sie können Worthäufigkeiten auszählen

## - Sie können Worthäufigkeiten anhand einer Wordcloud visualisieren

## 

## ----libs-textmining----------------------
library(tidyverse)
library(stringr)  
library(tidytext)  
library(lsa)  
library(SnowballC)  
library(wordcloud)  
library(skimr)
library(pdftools)  

data(afd, package = "pradadata")
data(stopwords_de, package = "lsa")
data(sentiws, package = "pradadata")


## ----libs-textmining-hidden, echo = FALSE----
library(knitr)


## -----------------------------------------
text <- c("Wir haben die Frauen zu Bett gebracht,",
          "als die Männer in Frankreich standen.",
          "Wir hatten uns das viel schöner gedacht.",
          "Wir waren nur Konfirmanden.")


## -----------------------------------------
text_df <- data_frame(Zeile = 1:4,
                      text = text)


## ----brecht, echo = FALSE-----------------
knitr::kable(text_df, caption = "Gedicht von Erich Kästner",
booktabs = TRUE)


## ----eval = FALSE-------------------------
## text <- read_lines("Brecht.txt")


## ----tidytextdf, echo = FALSE, fig.cap = "Illustration eines Tidytext-Dataframes (rechts)"----
knitr::include_graphics("images/textmining/tidytext-crop.pdf")


## -----------------------------------------
text_df %>%
  unnest_tokens(output = wort, input = text) -> tidytext_df
tidytext_df %>% head()


## Nimm den Datensatz "text_df" UND DANN

## dehne die einzelnen Elemente der Spalte "text", so dass jedes Element seine eigene Spalte bekommt.

## Ach ja: Diese "gedehnte" Spalte soll "wort" heißen (weil nur einzelne Wörter drinnen stehen) UND DANN

## speichere den resultierenden Dataframe ab als `tidytext_df`.

## 

## In einem *Tidytext-Dataframe* steht in jeder Zeile ein Wort (Token) und die Häufigkeit dieses Worts im Dokument.

## 

## -----------------------------------------
tidytext_df %>%
  filter(str_detect(wort, "[a-z]")) -> tidytext_df_lowercase


## Nimm den Datensatz "tidytext_df" UND DANN

## filtere die Spalte "wort", so dass nur noch Kleinbuchstaben übrig bleiben (keine Ziffern etc.). FERTIG.

## 

## -----------------------------------------
string <- paste0("Correlation of unemployment and #AfD votes",
                 "at #btw17: ***r = 0.18***",
                 "https://t.co/YHyqTguVWx")


## -----------------------------------------
str_detect(string, "[:digit:]")


## -----------------------------------------
str_locate(string, "[:digit:]")
str_extract(string, "[:digit:]")


## -----------------------------------------
str_extract_all(string, "[:digit:]")


## -----------------------------------------
str_extract_all(string, "[:digit:]{2}")


## -----------------------------------------
str_extract_all(string, "#[:alnum:]+")


## -----------------------------------------
str_extract_all(string, "https?://[:graph:]+")


## -----------------------------------------
str_count(string, boundary("word"))


## -----------------------------------------
str_extract_all(string, "[:alpha:]+")


## -----------------------------------------
str_replace_all(string, "[^[:alpha:]+]", "")


## ----eval = FALSE-------------------------
## afd_pfad <- "data/afd_programm.pdf"
## content <- pdf_text(afd_pfad)
## afd <- data_frame(Seite = 1:96,
##                   content)


## -----------------------------------------
afd %>%
  unnest_tokens(output = token, input = content) %>%
  dplyr::filter(str_detect(token, "[a-z]")) -> afd_long
head(afd_long)


## -----------------------------------------
afd_long %>%
  na.omit() %>%  # fehlende Werte löschen
  count(token, sort = TRUE)


## -----------------------------------------
stopwords_de <- data_frame(word = stopwords_de)

afd_long %>%
  anti_join(stopwords_de, by = c("token" = "word") ) -> afd_no_stop


## -----------------------------------------
afd_no_stop %>%
  count(token, sort = TRUE) -> afd_count


## -----------------------------------------
afd_no_stop %>%
  mutate(token_stem = wordStem(.$token, language = "de")) %>%
  count(token_stem, sort = TRUE) -> afd_count_stemmed


## ----afd-count-no-stop, echo = FALSE------

afd_count %>%
  top_n(10) -> afd_count_top10

afd_count_stemmed %>%
  top_n(10) %>%
  bind_cols(afd_count_top10) %>%
  knitr::kable(caption = "Die häufigsten Wörter -- mit Stemming (links) und ohne Stemming (rechts)",
  booktabs = TRUE)


## ----show-wordcloud, fig.cap = "Eine Wordwolke zum AfD-Parteiprogramm"----
wordcloud(words = afd_count_stemmed$token_stem,
          freq = afd_count_stemmed$n,
          max.words = 100,
          scale = c(2,.5),
          colors=brewer.pal(6, "Dark2"))


## Warnung: Eine Wordcloud hat keinen inhaltlichen Mehrwert; im besten Fall ist sie schick. Nutzen Sie sie nicht, um wichtige Informationen zu vermitteln, sondern höchstens, um optisch zu schmeicheln.

## 

## -----------------------------------------
afd_count_stemmed %>%
  top_n(30) %>%
  ggplot() +
  aes(x = reorder(token_stem, n), y = n) +
  geom_col() +
  labs(title = "mit Trunkierung") +
  coord_flip() -> p1

afd_count %>%
  top_n(30) %>%
  ggplot() +
  aes(x = reorder(token, n), y = n) +
  geom_col() +
  labs(title = "ohne Trunkierung") +
  coord_flip() -> p2


## ----p-word-freq, echo = FALSE, fig.cap = "Worthäufigkeiten im AfD-Parteiprogramm", out.width = "100%", fig.asp=0.7----
gridExtra::grid.arrange(p1, p2, ncol = 2)


## 
## 1. Schau dir jeden Token aus dem Text an.

## 2. Prüfe, ob sich das Wort im Lexikon der Sentiments wiederfindet.

## 3. Wenn ja, dann addiere den Sentimentwert dieses Tokens zum bestehenden Sentimentwert.

## 4. Wenn nein, dann gehe weiter zum nächsten Wort.

## 5. Liefere zum Schluss die Summenwerte pro Sentiment zurück.

## 

## ----sentiws-head, echo = FALSE-----------
knitr::kable(head(sentiws), caption = "Auszug aus SentiwS",
booktabs = TRUE)


## -----------------------------------------
afd_long %>%
  inner_join(sentiws, by = c("token" = "word")) %>%
  select(-inflections) -> afd_senti  # die Spalte brauchen wir nicht

afd_senti %>%
  group_by(neg_pos) %>%
  summarise(polarity_sum = sum(value),
            polarity_count = n()) %>%
  mutate(polarity_prop = (polarity_count / sum(polarity_count)) %>%
           round(2)) -> afd_senti_tab


## ----afd-senti-tab, echo = FALSE----------
knitr::kable(afd_senti_tab, caption = "Zusammenfassung von SentiWS",
             booktabs = TRUE)


## -----------------------------------------
afd_senti %>%
  distinct(token, .keep_all = TRUE) %>%
  mutate(value_abs = abs(value)) %>%
  top_n(20, value_abs) %>%
  pull(token)


## ----skim-no-eval, eval = FALSE-----------
## sentiws %>%
##   select(value, neg_pos) %>%
##   skim()


## ----skimr-no-histogram, echo = FALSE-----
skim_with(numeric = list(hist = NULL))


## ----skim-neg-pos-------------------------
sentiws %>%
  select(value, neg_pos) %>%
  group_by(neg_pos) %>%
  skim_to_wide()


## -----------------------------------------
afd_senti %>%
  summarise(senti_sum = mean(value) %>% round(2))


## 1. Unter einem Token versteht man die größte Analyseeinheit in einem Text.

## 1. In einem Tidytext-Dataframe steht jedes Wort in einer (eigenen) Zeile.

## 1. Eine hinreichende Bedingung für einen Tidytext-Dataframe ist es, dass in jeder Zeile ein Wort steht (beziehen Sie sich auf den Tidytext-Dataframe wie in diesem Kapitel erörtert).

## 1. Gibt es 'Stop-Wörter' in einem Dataframe, dessen Text analysiert wird, so kommt es -- per definitionem -- zu einem Stop.

## 1. Mit dem Befehl `unnest_tokens` kann man einen Tidytext-Dataframe erstellen.

## 1. Balkendiagramme sind sinnvolle und auch häufige Diagrammtypen, um die häufigsten Wörter (oder auch Tokens) in einem Corpus darzustellen.

## 1. In einem 'Tidytext-Dataframe' steht in jeder Zeile ein Wort (token), *aber nicht* die Häufigkeit des Worts im Dokument.

## 1. Unter *Stemming* versteht man (bei der Textanalyse), die Etymologie eines Worts (Herkunft) zu erkunden.

## 9. Der Datensatz `tidytext_df` ist tidy.

## 10. Die folgende Syntax ist geeignet, um die Anzahl der Wörter im Dataframe `afd` auszulesen, stimmt's? `afd %>% unnest_tokens(output = token, input = content) %>% dplyr::filter(str_detect(token, "[a-z]")) %>% count()`.

## 
