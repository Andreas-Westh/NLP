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

spacy_df <- spacy_parse(all_speeches$text)
spacy_df <- spacy_df %>% 
  filter(lemma == str_extract(lemma, regex("[a-zæøåA-ZÆØÅ]+$")))

# make doc_id more redable, in this example grouped by priest
doc_ids <- unique(spacy_df$doc_id)
better_ids <- unique(all_speeches$year)
spacy_df <- spacy_df %>% 
  mutate(doc_id = better_ids[match(doc_id, doc_ids)]) %>% 
  rename(word = lemma)
spacy_df$year <- spacy_df$doc_id

raw_tokens_spacy <- spacy_df
raw_tokens <- raw_tokens_spacy




zipf_df <- raw_tokens %>%
  count(word, sort = TRUE) %>%
  mutate(rank = row_number(),
         percent = n / sum(n) * 100)
ggplot(zipf_df, aes(x = rank, y = percent)) +
  geom_point(alpha = 0.6) +
  scale_x_log10() +
  scale_y_log10() +
  labs(title = "Zipf: ordandel vs. rang",
       x = "Rank (log10)", y = "Andel (%) (log10)")


freq_by_rank <- raw_tokens %>% 
  group_by(doc_id) %>% 
  count(word, sort = T) %>% 
  mutate(rank = row_number(),
         total = sum(n),
         term_frequency = n/total,
         procent = round(n/total * 100,2)) %>% 
  ungroup()

freq_by_rank %>% 
  ggplot(aes(rank, term_frequency, color = doc_id)) + 
  geom_line(linewidth = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()

rank_subset <- freq_by_rank %>% 
  filter(rank < 110,
         rank > 5)

lm <- lm(log10(term_frequency) ~ log10(rank), data = rank_subset)
lm
# our slope is close to -1 (log10(rank)), which is like zips law


freq_by_rank %>% 
  ggplot(aes(rank, term_frequency, color = doc_id)) + 
  geom_abline(intercept = lm$coefficients[1], slope = lm$coefficients[2], 
              color = "gray50", linetype = 2) +
  geom_line(linewidth = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()

# the text follow the classic example of Zipf's law
