library(dplyr)
library(rvest)
library(stringr)
library(tidytext)
library(topicmodels)
library(stopwords)
library(wordcloud2)
library(ggplot2)
library(stm)
library(quanteda)
library(quanteda.textplots)
library(quanteda.textstats)
library(Sentida)
library(readtext)
library(igraph)
library(ggraph)


alle_taler <- readRDS("R/examprep/previous/Q98/alletaler.rds")
sp <- readRDS("R/examprep/previous/Q98/allSp(1).rds") 
sp <- sp %>% 
  select(taler, year)

alle_taler <- alle_taler %>% 
  rowwise() %>% 
  mutate(year = str_extract(title, regex("[0-9]{4}")),
         taler = str_extract(url, "tale/[^\"\\s]+"),
         folketing = str_detect(title, "aabningstale-[0-9]{4}")) %>% 
  arrange(year) %>% 
  distinct()

table(alle_taler$year)


alle_folk <- alle_taler %>% 
  filter(folketing == T) %>% 
  ungroup() %>% 
  mutate(index = row_number())

alle_folk <- alle_folk %>% 
  rowwise() %>% 
  mutate(score = sentida(content, output = "mean"))
hist(alle_folk$score, breaks = 12)

table(alle_folk$year) 
alle_folk <- alle_folk %>% 
  filter(!title == "aabningstale-1948-2" & !title == "aabningstale-1947-3")

alle_folk_sub <- alle_folk %>% filter(year %in% c(1986, 1996, 2009, 2011))

#### Tokennize ####
library(reticulate)
Sys.setenv(RETICULATE_PYTHON = "/opt/anaconda3/envs/spacy/bin/python")
library(spacyr)
spacy_initialize(
  model            = "da_core_news_md",
  refresh_settings = TRUE,
  verbose          = TRUE
)

texts <- alle_folk_sub$content
names(texts) <- alle_folk_sub$year 

spacy_df <- spacy_parse(texts)
spacy_df <- spacy_df %>% 
  filter(lemma == str_extract(lemma, regex("[a-zæøåA-ZÆØÅ]+$")))

# make doc_id more redable, in this example grouped by priest
spacy_df <- spacy_df %>% 
  rename(word = lemma)
spacy_df$year <- spacy_df$doc_id

raw_tokens_spacy <- spacy_df

raw_tokens <- raw_tokens_spacy

# sentiment sentida
sentiment_df <- raw_tokens %>%
  group_by(doc_id) %>%
  summarise(text = paste(word, collapse = " ")) %>%
  rowwise() %>%
  mutate(score = sentida(text, output = "mean")) %>%
  ungroup()

# join into the parsed
tokens <- raw_tokens %>% 
  left_join(sentiment_df %>% select(doc_id, score), by = "doc_id")

count <- tokens %>% 
  group_by(doc_id) %>% 
  count(word)

TFIDF <- count %>% bind_tf_idf(word, doc_id, n)

# sentiment via csv
sentiment_file2 <- read_csv("R/examprep/previous/Q1-master/dsl.csv")
colnames(sentiment_file2) = c("word", "x2", "pos","x4","score","x6")

TFIDF_sentiment <- inner_join(TFIDF, sentiment_file2, by = "word")

total_sentiment_score <- TFIDF_sentiment %>% 
  group_by(doc_id) %>% 
  mutate(sls_score = n*score) %>% 
  summarise(total_score = sum(sls_score))

dkstop <- c(stopwords("da"),"kan", "så","få")

TFIDF_clean <- TFIDF %>% 
  filter(str_detect(word, "\\D") &
         !word %in% dkstop)

dtm <- TFIDF_clean %>% 
  cast_dtm(doc_id, word, n)
dtm
allWords <- dtm$dimnames[[2]] # just a word list

lda <- LDA(dtm, k = 4, control = list(seed = 1980))

###### beta ######
# beta = which word relates to which topic
topic <- tidy(lda, matrix = "beta")
topic
# one-topic-per-word probability
top_terms <- topic %>% 
  group_by(topic) %>% 
  slice_max(beta, n = 5) %>% 
  ungroup() %>% 
  arrange(topic, -beta)
top_terms

top_terms %>% 
  mutate(term = reorder_within(term, beta, topic)) %>% 
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = F) +
  facet_wrap(~topic, scales = "free") +
  scale_y_reordered()

###### gamma ######
# gamma = which topics relate to which document
gamma <- tidy(lda, matrix = "gamma")
gamma
# 2006 gemma = 1.00 for topic 1, meaning it is purely filled by that topic
gamma %>% 
  mutate(speech = document, gamma * topic) %>% 
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~speech) +
  labs(x = "topic", y = expression(gamma))


gamma %>%
  ggplot(aes(x = factor(topic), y = document, fill = gamma)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "red") +
  labs(x = "Topic", y = "Tale (år)", fill = "Gamma") +
  theme_minimal()


##### plot with top terms too #####
# get topic labels per topic
gamma_terms <- left_join(gamma, top_terms, by = "topic", relationship = "many-to-many")

ggplot(gamma_terms, aes(x = document, y = factor(topic), fill = gamma)) +
  geom_tile(color = "white") +
  geom_text(aes(label = term),
            position = position_jitter(width = 0.5, height = 0.5),
            size = 3.2, alpha = 0.85) +
  scale_fill_gradient2(low = "white", high = "red", midpoint = 0.25) +
  labs(x = "Tale (document)", y = "Topic", fill = "Gamma") +
  theme_minimal()



#### Top 20 tf wordcloud ####
top_beta <- topic %>% 
  group_by(topic) %>% 
  top_n(20, beta) %>% 
  ungroup()

wc_beta <- top_beta %>% 
  filter(topic == 4) %>% 
  select(term, beta)
wordcloud2(wc_beta, size = 0.5)


#### unique terms in each topic ####
TFIDF_clean %>%
  group_by(doc_id) %>%
  slice_max(tf_idf, n = 10, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(word = reorder_within(word, tf_idf, doc_id)) %>%
  ggplot(aes(word, tf_idf, fill = doc_id)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~doc_id, scales = "free") +
  coord_flip() +
  scale_x_reordered()
