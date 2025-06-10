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

#remove stop words
dkstop <- c(stopwords("da"),"kan","så","må","ved","al","vores","gøre","ny")
tokens <- raw_tokens %>% 
  filter(!word %in% dkstop)

##### count #####
# total count per word (global)
total_count <- tokens %>% 
  count(word, name = "global_n", sort = T) %>% 
  mutate(global_total = sum(global_n))


# count grouped per document
tokens_count <- tokens %>% 
  group_by(doc_id) %>% 
  count(word, sort = T) %>% 
  mutate(total = sum(n),
         percent = round(n / total * 100,1)) %>% 
  ungroup() %>% 
  left_join(total_count, by = "word") %>% 
  mutate(global_percent = round(global_n / global_total * 100,4))

##### top 5 most used words #####
top_5 <- tokens_count %>% 
  select(word, global_n, global_percent) %>% 
  distinct() %>% 
  arrange(desc(global_n)) %>% 
  slice_max(global_n, n = 5)

top_5 %>% 
  ggplot(aes(x=reorder(word, global_percent), global_percent)) +
  geom_bar(stat = "identity") +
  labs(x = "word", y = "procent", title = "top 5 mest brugte") +
  coord_flip()



###### wordcloud ######
wc_data <- total_count %>% 
  rename(n = global_n) %>% 
  select(word, n)
wordcloud2(data = head(wc_data,100), size = 0.5)
