#### Data retrieval ####
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




# freq by term and zipf's law
# zipf's law is that a terms rank is inversely proportional to its freq (lowest rank = highest used)
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
  geom_line(linewidth = 1.1, alpha = 0.8, show.legend = T) + 
  scale_x_log10() +
  scale_y_log10()

rank_subset <- freq_by_rank %>% 
  filter(rank < 400,
         rank > 10)
# the higher the cutoff, the more we sway from Zipfs law

lm <- lm(log10(term_frequency) ~ log10(rank), data = rank_subset)
lm

# our slope is close to -1 (log10(rank)), which is like zips law


freq_by_rank %>% 
  ggplot(aes(rank, term_frequency, color = doc_id)) + 
  geom_abline(intercept = lm$coefficients[1], slope = lm$coefficients[2], 
              color = "gray50", linetype = 2) +
  geom_line(linewidth = 1.1, alpha = 0.8, show.legend = T) + 
  scale_x_log10() +
  scale_y_log10()
# the text follow the classic example of Zipf's law
