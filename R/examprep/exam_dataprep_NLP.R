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

#### Data Retrieval ####
files_df <- readtext("R/examprep/previous/Q4-master/*") # * means every file


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

spacy_df <- spacy_parse(all_speeches$text)
spacy_df <- spacy_df %>% 
  filter(lemma == str_extract(lemma, regex("[a-zæøåA-ZÆØÅ]+$")))

# make doc_id more redable, in this example grouped by priest
doc_ids <- unique(spacy_df$doc_id)
better_ids <- unique(all_speeches$doc_id)
spacy_df <- spacy_df %>% 
  mutate(doc_id = better_ids[match(doc_id, doc_ids)]) %>% 
  rename(word = lemma)
spacy_df$year <- spacy_df$doc_id

raw_tokens_spacy <- spacy_df

raw_tokens <- raw_tokens_spacy





#remove stop words
dkstop <- c(stopwords("da"),"kan","så","må","ved","al")
tokens <- raw_tokens %>% 
  filter(!word %in% dkstop)



##### count #####
# total count per word
total_count <- tokens %>% 
  count(word, name = "global_total", sort = T)


# count grouped by priest
tokens_count <- tokens %>% 
  group_by(doc_id) %>% 
  count(word, sort = T) %>% 
  mutate(total = sum(n),
         percent = round(n / total * 100,1)) %>% 
  ungroup() %>% 
  left_join(total_count, by = "word") %>% 
  mutate(global_percent = round(global_total / sum(global_total) * 100,4))

###### wordcloud ######
wc_data <- total_count %>% 
  rename(n = global_total) %>% 
  select(word, n)
wordcloud2(data = head(wc_data,100), size = 0.5)
