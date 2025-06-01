library(dplyr)
library(rvest)
library(stringr)
library(tidytext)
library(stopwords)
library(wordcloud2)
library(ggplot2)
library(stm)
library(quanteda)
library(Sentida)
library(tidyr)
library(readtext)
library(stringi)

# data retrieval
HP1 <- readtext("data/Harry Potter 1 - Sorcerer's Stone.txt")
HP2 <- readtext("data/Harry Potter 2 - Chamber of Secrets.txt")
HP3 <- readtext("data/Harry Potter 3 - The Prisoner Of Azkaban.txt")
HP4 <- readtext("data/Harry Potter 4 - The Goblet Of Fire.txt")
HP5 <- readtext("data/Harry Potter 5 - Order of the Phoenix.txt")
HP6 <- readtext("data/Harry Potter 6 - The Half Blood Prince.txt")
HP7 <- readtext("data/Harry Potter 7 - Deathly Hollows.txt")

HP <- list(HP1,HP2,HP3,HP4,HP5,HP6,HP7)

HP_df <- data.frame(title = NULL, text = NULL)
for (i in 1:length(HP)) {
  tmp_title <- HP[[i]]$doc_id
  tmp_title <- str_replace(tmp_title, ".txt","") # 1. Clean title
  tmp_text <- HP[[i]]$text
  tmp_df <- data.frame(title = tmp_title, text = tmp_text)
  HP_df <- rbind(HP_df, tmp_df)
}

HP_df <- HP_df %>%
  mutate(text = iconv(text, from = "", to = "UTF-8", sub = "byte"))


HP_df <- HP_df %>%
  mutate(text = stri_enc_toutf8(text))

HP_token_raw <- HP_df %>% unnest_tokens(output = sentences, input = text, token = "sentences", to_lower = F)

HP_s <- HP_token_raw %>% 
  group_by(title) %>% 
  mutate(linenumber = row_number(),
         chapter = cumsum(
           str_detect(sentences,
                      regex("chapter \\w+", ignore_case = T))
         )) %>% 
  ungroup()

HP_w <- HP_s %>% unnest_tokens(output = word, input = "sentences", token = "words", to_lower = F)
sw <- c("stop_words")

HP_w_SW <- HP_w %>% 
  filter(!word %in% stopwords("en") & nchar(word) > 2)

HP_w_SW_C <- HP_w_SW %>% 
  count(word, sort = T)

wordcloud2(data = HP_w_SW_C, size = 0.7)



HP_dfm <- HP_w_SW %>% 
  count(title, word) %>% 
  cast_dfm(title, word, n)

topics <- stm(HP_dfm, K = 6, init.type = "Spectral")
summary(topics)

tidy_beta <- tidy(topics)

tidy_beta %>% 
  group_by(topic) %>% 
  slice_max(beta, n = 10, with_ties = F) %>% 
  mutate(term = reorder_within(term, beta, topic)) %>% 
  ggplot(aes(term, beta, fill = topic)) +
  geom_col(show.legend = F) +
  facet_wrap(~topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()

tidy_tokens <- HP_w_SW %>% 
  count(title, word, sort = T) %>% 
  bind_tf_idf(word, title, n) %>% 
  group_by(title) %>% 
  ungroup(title) %>% 
  mutate(word = reorder(word, tf_idf))
  
tidy_tokens %>% 
  group_by(title) %>% 
  slice_max(tf_idf, n = 10) %>% 
  mutate(word = reorder_within(word, tf_idf, title)) %>% 
  ggplot(aes(word, tf_idf, fill = title)) +
  geom_col(show.legend = F) +
  facet_wrap(~title, scales = "free") +
  coord_flip() +
  scale_x_reordered()




# Sentiment ----
HP_s$sentiment <- HP_s %>% 
  rowwise() %>% 
  mutate(sentiment = round(sentida(sentences, output = "mean"), 1))


# 2. Remove table of contens
# 3. Also devide into chapters
# Maybe others?

