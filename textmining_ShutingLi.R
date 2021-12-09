## ----setup, include=FALSE-------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(knitr)
library(gutenbergr)
library(tidyverse)
library(tnum)
library(dplyr)
library(tidytext)
library(stringr)
library(ggplot2)
library(tidyr)
library(scales)
library(wordcloud)
library(reshape2)
library(gridExtra)


## -------------------------------------------------------------------------------------------
Metamorphosis <- gutenberg_download(gutenberg_id = 5200)
# write.table(Metamorphosis, "Metamorphosis.txt") # add <> for every chapter in text file
Metamorphosis <- read.table("Metamorphosis.txt", header = T)


## ----warning=FALSE,message=FALSE------------------------------------------------------------
Metamorphosis <- Metamorphosis %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, 
                                     regex("^chapter [\\divxlc]",
                                           ignore_case = TRUE)))) %>%
  ungroup()

tidy_Metamorphosis <- Metamorphosis %>%
  unnest_tokens(word, text)
#########################################
data("stop_words")
tidy_Metamorphosis <- tidy_Metamorphosis %>% anti_join(stop_words)
#########################################
commonwords <- tidy_Metamorphosis %>% count(word, sort = TRUE)

tidy_Metamorphosis %>%
  count(word, sort = TRUE) %>%
  filter(n > 16) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)


## ----message=FALSE--------------------------------------------------------------------------
get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")


## ----message=FALSE--------------------------------------------------------------------------
### analysis based on "bing" ###
Metamorphosis_sentiment_bing <- tidy_Metamorphosis %>%
  inner_join(get_sentiments("bing")) %>%
  count(index = linenumber %/% 8, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative)

ggplot(Metamorphosis_sentiment_bing, aes(index, sentiment, fill=factor(sentiment))) +
  geom_col(show.legend = FALSE)


## ----message=FALSE--------------------------------------------------------------------------
### analysis based on "afinn" ###
Metamorphosis_sentiment_afinn <- tidy_Metamorphosis %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(index = linenumber %/% 8) %>% 
  summarise(sentiment = sum(value)) %>% 
  mutate(method = "AFINN")

ggplot(Metamorphosis_sentiment_afinn, aes(index, sentiment,fill=factor(sentiment))) +
  geom_col(show.legend = FALSE)


## ----message=FALSE--------------------------------------------------------------------------
### compare three sentiment dictionaries ###
Metamorphosis_sentiment_bing_and_nrc <- bind_rows(
  tidy_Metamorphosis %>% 
    inner_join(get_sentiments("bing")) %>%
    mutate(method = "Bing"),
 tidy_Metamorphosis %>% 
    inner_join(get_sentiments("nrc") %>% 
                 filter(sentiment %in% c("positive", 
                                         "negative"))
    ) %>%
    mutate(method = "NRC")) %>%
  count(method, index = linenumber %/% 8, sentiment) %>%
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = 0) %>% 
  mutate(sentiment = positive - negative)

bind_rows(Metamorphosis_sentiment_afinn, 
          Metamorphosis_sentiment_bing_and_nrc) %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")


## ----message=FALSE--------------------------------------------------------------------------
tidy_Metamorphosis_less <- select(tidy_Metamorphosis,4)

bing_word_counts <- tidy_Metamorphosis_less %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

afinn_word_counts <- tidy_Metamorphosis_less %>%
  inner_join(get_sentiments("afinn")) %>%
  mutate(sentiment=ifelse(value>=0,"positive","negative")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

nrc_word_counts <- tidy_Metamorphosis_less %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive",
                                         "negative"))
    ) %>%
 count(word, sentiment, sort = TRUE) %>%
  ungroup()

# word_counts_Metamorphosis <- bind_rows(
#   (tidy_Metamorphosis_less) %>%
#   inner_join(get_sentiments("bing")) %>%
#   count(word, sentiment, sort = TRUE) %>%
#   ungroup() %>% mutate(method="Bing"),
#   tidy_Metamorphosis_less %>%
#   inner_join(get_sentiments("afinn")) %>%
#   mutate(sentiment=ifelse(value>=0,"positive","negative")) %>%
#   count(word, sentiment, sort = TRUE) %>%
#   ungroup() %>% mutate(method = "AFINN"),
#   tidy_Metamorphosis_less %>%
#   inner_join(get_sentiments("nrc") %>% filter(sentiment %in% c("positive","negative"))) %>%
#   count(word, sentiment, sort = TRUE) %>% ungroup() %>% mutate(method="NRC"))


bing <- bing_word_counts %>%
  group_by(sentiment) %>%
  slice_max(n, n = 5) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Bing",
       y = NULL)
afinn <- afinn_word_counts %>%
  group_by(sentiment) %>%
  slice_max(n, n = 5) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "AFINN",
       y = NULL)
nrc <- nrc_word_counts %>%
  group_by(sentiment) %>%
  slice_max(n, n = 5) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "NRC",
       y = NULL)

grid.arrange(bing,afinn,nrc, ncol=1)


## ----message=FALSE,warning=FALSE------------------------------------------------------------
tidy_Metamorphosis %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

tidy_Metamorphosis %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)


## ----message=FALSE--------------------------------------------------------------------------
tidy_Metamorphosis %>%
  count(word) %>%
  inner_join(get_sentiments("loughran"), by = "word") %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  facet_wrap(~ sentiment, scales = "free") +
  labs(x = "Frequency of this word in Metamorphosis", y = NULL)

