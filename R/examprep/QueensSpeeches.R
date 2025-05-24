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


#### data retrieval####
# get urls
URL_site <- "https://www.kongehuset.dk/monarkiet-i-danmark/nytaarstaler/#laes-de-seneste-nytaarstaler"
url <- read_html(URL_site)
links <- url %>% html_nodes(".accordion__container__item__content .field-item a") %>% 
  html_attr("href") %>% as.data.frame()
links <- links[-1,]




# get speeches
base_url <- "https://www.kongehuset.dk"

all_speeches <- data_frame(url=NULL, title = NULL, text=NULL)

for (link in links) {
  Sys.sleep(runif(1, min = 0.2, max = 0.6))
  print(link)
  loop_url <- paste0(base_url,link) # match base url with scraped speech url
  tmp_year <- str_extract(loop_url,"[0-9]{4}") # reGex out the year
  tmp_content <- read_html(loop_url) %>% html_element(".rich-text") %>% html_text() # get the speech
  tmp_df <- data_frame(url = loop_url, year = tmp_year, text = tmp_content)  
  all_speeches<- rbind(all_speeches,tmp_df)
}


#### NLP ####
# Tokenize
raw_tokens <- all_speeches %>% 
  unnest_tokens(word, text)



# remove stopwords
dkstop <- c(stopwords(language = "da"),"kan","så","må","ved","al")
tokens <- raw_tokens %>% filter(!word %in% dkstop)



##### wordcloud #####
wc_data <- tokens %>% count(word, sort = T)
wordcloud2(data = wc_data, size = 0.5)


##### Spacy #####
Sys.setenv(RETICULATE_PYTHON = "/opt/anaconda3/envs/spacy/bin/python")
library(spacyr)
spacy_initialize(
  model            = "da_core_news_md",
  refresh_settings = TRUE,
  verbose          = TRUE
)

speeches_spacy <- spacy_parse(all_speeches$text)
# make doc_id better
doc_ids <- unique(speeches_spacy$doc_id)
better_ids <- unique(all_speeches$year)
speeches_spacy <- speeches_spacy %>% 
  mutate(doc_id = better_ids[match(doc_id, doc_ids)])

# quick cleaning
# remove non words
speeches_spacy <- speeches_spacy %>% 
  mutate(lemma = str_extract(lemma,"^[A-Za-zÆØÅæøå]+$")) %>% drop_na(lemma)

# remove stop words
speeches_spacy <- speeches_spacy %>% filter(!lemma %in% dkstop)

###### Sentiment per speech ######
speeches_sentiment <- speeches_spacy %>% 
  group_by(doc_id) %>% 
  summarise(speech = paste(lemma, collapse = " ")) %>% 
  rowwise() %>% 
  mutate(score_mean = round(sentida(speech, output = "mean"), 1),
         score_total = round(sentida(speech, output = "total"), 1))

speeches_spacy_count <- speeches_spacy %>% count(doc_id, lemma, sort = T)

speeches_spacy_count %>% 
  group_by(doc_id) %>% 
  top_n(10) %>%  
  ggplot(aes(lemma, n, fill = doc_id)) +
  geom_col(show.legend = F) +
  facet_wrap(~doc_id, scales = "free") +
  coord_flip()

###### part of speech tags ######
# find proper nouns
speeches_PROPN <- speeches_spacy %>% filter(pos == "PROPN") %>% count(doc_id, lemma, sort = T)

#### Good plot template with facet_wrap ####
speeches_PROPN %>% 
  group_by(doc_id) %>% 
  slice_max(n, n = 3, with_ties = F) %>% # to fix top_n() with facet_wrap 
  mutate(lemma = reorder_within(lemma, n, doc_id)) %>% # needed for the reordering of sort
  ggplot(aes(lemma, n, fill = doc_id)) +
  geom_col(show.legend = F) +
  facet_wrap(~doc_id, scales = "free") + 
  coord_flip() +
  scale_x_reordered() # reorders in actual plot


#### Sentiment per sentince ####
raw_sentences <- all_speeches %>% 
  unnest_tokens(output = sentence, input = text, token = "sentences") %>% 
  rowwise() %>% 
  mutate(score = round(sentida(sentence, output = "mean"),1)) %>% 
  ungroup() %>% 
  arrange(desc(score))

# most positive sentence
raw_sentences %>% 
  filter(year == "2023") %>% 
  top_n(3) %>%
  ggplot(aes(sentence, score, fill = year)) + 
  geom_col(show.legend = F) + 
  facet_wrap(~year, scales = "free") + 
  coord_flip()
# least
raw_sentences %>% 
  filter(year == "2023") %>% 
  top_n(-3) %>%
  ggplot(aes(sentence, score, fill = year)) + 
  geom_col(show.legend = F) + 
  facet_wrap(~year, scales = "free") + 
  coord_flip()


# for all (doesnt really word)
raw_sentences %>% 
  group_by(year) %>% 
  top_n(3) %>% 
  ggplot(aes(sentence, score, fill = year)) +
  geom_col(show.legend = F) +
  facet_wrap(~year, scales = "free") + 
  coord_flip()


#### Bigrams ####
bigrams_raw <- all_speeches %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

bigrams_sep <- bigrams_raw %>% 
  separate(bigram,c("word1","word2"), sep = " ") %>% 
  filter(!word1 %in% dkstop,
         !word2 %in% dkstop) %>% 
  count(word1, word2, sort = T)

# split between next year and previous
country_filter <- c("grønland","danmark")
bigrams_country <- bigrams_sep %>% 
  filter(word1 %in% country_filter | word2 %in% country_filter) %>% 
  mutate(country = case_when(
    word1 == "grøndland" | word2 == "grøndland" ~ "grønland",
    TRUE ~ "danmark"
  )) %>%   group_by(country)

  # bad example



#### TO DO ####



# topics


