files_df <- readtext("R/examprep/previous/Q4-master/*") # automatically reads all files
files_df <- files_df %>% 
  mutate(year = str_extract(text, regex("[0-9]{4}"))) %>% # get year
  filter(year >= 2012 & year <= 2020)


# now to remove the title, pattern = we can see its the year just before text starts
files_df <- files_df %>%
  mutate(text_raw = text,
         text = str_squish(text_raw),  # remove extra whitespace and line breaks
         text = str_extract(text, "(?<=\\b[0-9]{4}\\b\\s).*")) %>% 
  arrange(desc(as.numeric(year)))

all_speeches <- files_df

#### Tokennize ####
library(reticulate)
Sys.setenv(RETICULATE_PYTHON = "/opt/anaconda3/envs/spacy/bin/python")
library(spacyr)
spacy_initialize(
  model            = "da_core_news_md",
  refresh_settings = TRUE,
  verbose          = TRUE
)

texts <- all_speeches$text
names(texts) <- all_speeches$year

spacy_df <- spacy_parse(texts)
spacy_df <- spacy_df %>% 
  filter(lemma == str_extract(lemma, regex("[a-zæøåA-ZÆØÅ]+$"))) %>% 
  mutate(word = lemma)

raw_tokens_spacy <- spacy_df
raw_tokens <- raw_tokens_spacy
 
count <- raw_tokens %>% 
  group_by(doc_id) %>% 
  count(word)

TFIDF <- count %>% bind_tf_idf(word, doc_id, n)
TFIDF

dkstop <- c(stopwords("da"),"kan", "så","få","al","vores")

TFIDF_clean <- TFIDF %>% 
  filter(str_detect(word, "\\D") &
           !word %in% dkstop)

dtm <- TFIDF_clean %>% 
  cast_dtm(doc_id, word, n)
dtm
dtm

allWords <- dtm$dimnames[[2]] # just a word list

lda <- LDA(dtm, k = 2, control = list(seed = 1980))

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
  mutate(document, gamma * topic) %>% 
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~document) +
  labs(x = "topic", y = expression(gamma))

