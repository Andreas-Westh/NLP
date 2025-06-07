library(dplyr)
library(rvest)
library(stringr)
library(tidytext)
library(stopwords)
library(wordcloud2)
library(ggplot2)
library(stm)
library(quanteda)
library(quanteda.textplots)
library(quanteda.textstats)
library(Sentida)

# data retrieval ----
priests_raw <- read_html("R/examprep/priests.html")
all_text <- priests_raw %>% html_text()
urls <- str_extract_all(all_text, "/praedikener/tale/[^\"\\s]+")%>% unlist()
url_df <- data.frame(url = urls)
url_df <- url_df %>% mutate(full_url = paste0("https://www.dansketaler.dk",url))


all_texts <- data_frame(url=NULL,priest = NULL,text=NULL)
for (url in url_df$full_url) {
  print(url)
  mtest=read_html(url)
  tagfortale=".speech-article-content"
  tale=mtest %>% html_node(tagfortale) %>% html_text()
  priest <- mtest %>% html_node(".speech-speaker") %>% html_text(trim = T)
  tale_df <- data_frame(url = url, priest = priest, text = tale)  
  all_texts <- rbind(all_texts,tale_df)
}

all_texts$priest <- str_extract(all_texts$priest, "^[^ ]+( [^ ]+)*(?=  )")

# data prep ----
# tokennize
    # version with chapter´syntax and such
#sample_raw <- all_texts %>%
#  group_by(priest) %>%                       # group by each novel
#  mutate(linenumber = row_number(),       # add line number per book
#         chapter = cumsum(                # increment chapter count
#           str_detect(text,               # if line matches "chapter x"
#                      regex("^chapter [0-9]", 
#                            ignore_case = TRUE)))) %>%
#  ungroup() 

tokens_raw <- all_texts %>% 
  unnest_tokens(output = "word", 
                input = text, 
                token = "words",
                to_lower = T) # True so better works with stopwords


# stopwords
dkstop <- c(stopwords("da"),"kan","så","saa","paa", "oc","ved", "naar",
            "vaar","fordi")
tokens_SW <- tokens_raw %>% 
  filter(!word %in% dkstop)

#### Tidy ####
# count 
# total count per word
total_count <- tokens_SW %>% 
  count(word, name = "global_total", sort = T)
  

# count grouped by priest
tokens_count <- tokens_SW %>% 
  group_by(priest) %>% 
  count(word, sort = T) %>% 
  mutate(total = sum(n),
         percent = round(n / total * 100,1)) %>% 
  ungroup() %>% 
  left_join(total_count, by = "word") %>% 
  mutate(global_percent = round(global_total / sum(global_total) * 100,4))



# dette kan også gøres via spaCy, så vi har Lemma af ordet  
library(reticulate)
Sys.setenv(RETICULATE_PYTHON = "/opt/anaconda3/envs/spacy/bin/python")
library(spacyr)
spacy_initialize(
  model            = "da_core_news_md",
  refresh_settings = TRUE,
  verbose          = TRUE
)

spacy_df <- spacy_parse(all_texts$text)

# make doc_id more redable, in this example grouped by priest
doc_ids <- unique(spacy_df$doc_id)
better_ids <- unique(all_texts$priest)
spacy_df <- spacy_df %>% 
  mutate(doc_id = better_ids[match(doc_id, doc_ids)])

spacy_df_SW <- spacy_df %>% 
  filter(!lemma %in% dkstop)

spacy_df_SW$lemma <- str_extract(spacy_df_SW$lemma, regex("[a-zæøåA-ZÆØÅ]+"))

spacy_total_count <- spacy_df_SW %>% 
  filter(!is.na(lemma)) %>% 
  count(lemma, name = "total_count", sort = T)

spacy_count <- spacy_df_SW %>% 
  group_by(doc_id) %>% 
  filter(!is.na(lemma) & !is.na(doc_id)) %>% 
  count(lemma, sort = T) %>% 
  mutate(priest_percent = round(n / sum(n) * 100,2)) %>% 
  ungroup() %>% 
  left_join(spacy_total_count, by = "lemma") %>% 
  mutate(global_percent = round(total_count / sum(total_count) * 100,4))

# wordcloud
wc_data <- total_count %>% 
  rename(n = global_total) %>% 
  select(word, n)
wordcloud2(data = head(wc_data,100), size = 0.5)
# normal ggplot

#### Analyzing word and document frequency: tf-idf ####
spacy_count

ggplot(spacy_count, aes(n/sum(n))) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009)

# evt find a way to filter plot into like top 5 priests or smth
# maybe a total per priest, save in vector and use %in%

##### Zipf law? #####

#redo code under, but with raw word

#Zipf’s law states that the frequency that a word appears is inversely proportional to its rank. 
# this rename is just so it works seamlessly, code orignally made with spacy

spacy_count_full <- spacy_df %>% 
  group_by(doc_id) %>% 
  filter(!is.na(lemma) & !is.na(doc_id)) %>% 
  filter(lemma %in% str_extract(lemma, regex("[a-zæøåA-ZÆØÅ]+"))) %>% 
  count(lemma, sort = T) %>% 
  ungroup() %>% 
  mutate(total = sum(n))



word_totals <- spacy_count_full %>%
  filter(!is.na(lemma) & !is.na(doc_id)) %>%
  group_by(doc_id) %>%
  summarise(total_words = n(), .groups = "drop")

top_5 <- word_totals %>%
  slice_max(order_by = total_words, n = 5, with_ties = FALSE)
top_5_priests <- top_5$doc_id

freq_by_rank <- spacy_count_full %>% 
  filter(doc_id %in% top_5_priests) %>% 
  group_by(doc_id) %>% 
  mutate(rank = row_number(),
         term_frequency = n/total) %>% 
  ungroup()
freq_by_rank



freq_by_rank %>% 
  ggplot(aes(rank, term_frequency, color = doc_id)) + 
  geom_line(linewidth = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()


rank_subset <- freq_by_rank %>% 
  filter(rank < 500,
         rank > 10)

zip <- lm(log10(term_frequency) ~ log10(rank), data = rank_subset)

freq_by_rank %>% 
  ggplot(aes(rank, term_frequency, color = doc_id)) + 
  geom_abline(intercept = zip$coefficients[1], slope = zip$coefficients[2], 
              color = "gray50", linetype = 2) +
  geom_line(linewidth = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()

#Only a small number of words occur frequently
#the steep drop at the start of the Zipf plot shows that just a few words dominate the text.
#showing strong tendencies of natural human language

tf_idf <- spacy_count_full %>% 
  bind_tf_idf(lemma, doc_id, n)
# we can see that words like 'i' appear in every book (tf & tf_idf = 0)

# see words with high tf_idf
tf_idf %>% 
  arrange(desc(tf_idf))
# mainly odd words, should probs be added to SW

library(forcats)

tf_idf %>%
  filter(doc_id %in% top_5_priests) %>% 
  group_by(doc_id) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(lemma, tf_idf), fill = doc_id)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~doc_id, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)


# topic modelling ----










# sentiment ----
# grouped in text
sentiment_df <- all_texts %>% 
  mutate(speech_number = row_number()) %>% 
  rowwise() %>% 
  mutate(score = sentida(text, output = "mean"))


tokens_sentence <- all_texts %>% 
  unnest_tokens(output = sentence, input = text, token = "sentences") %>% 
  group_by(url) %>% 
  mutate(linenumber = row_number()) %>% 
  ungroup()

tokens_sentence <- tokens_sentence %>% 
  rowwise() %>% 
  mutate(score = sentida(sentence, output = "mean"))

most_negative <- tokens_sentence %>% 
  group_by(priest) %>% 
  select(priest, score, url) %>% 
  summarise(mean = mean(score),
            total_speeches = n_distinct(url))









# bigrams ----
bigrams_raw <- all_texts %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>% 
  filter(!is.na(bigram))
bigrams_raw

library(tidyverse)
bigrams_sep <- bigrams_raw %>% 
  separate(bigram, c("word1","word2"), sep = " ")

bigram_stop <- dkstop[!dkstop == "han" & !dkstop == "hun"] # relevent later

bigrams_filtered <- bigrams_sep %>% 
  filter(!word1 %in% bigram_stop &
         !word2 %in% bigram_stop) %>% 
  count(word1, word2, sort = T)
bigrams_filtered

genderfilter <- c("ham","hun")
bigrams_gender <- bigrams_sep %>% 
  filter(word1 %in% genderfilter) %>% 
  mutate(gender = ifelse(word1 == "ham","M","F")) %>% group_by(gender) %>% count(word2)

bigrams_gender %>% 
  filter(!word2 %in% dkstop) %>%     
  group_by(gender) %>%                        
  top_n(5, n) %>%
  ungroup() %>%
  ggplot(aes(x = n, y = reorder(word2, n), fill = gender)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ gender, scales = "free_y") +  
  labs(
    x = "Antal",
    y = "Ord",
    title = "Top 5 word2 efter køn"
  ) +
  theme_minimal()
  # example with split by gender

# relationsgrafer
# korrelede ord


# quanteda
corpus <- corpus(all_texts, text_field = "text")
corpus

corpus_clean <- corpus |>
  tokens(remove_punct = T, remove_numbers = T, remove_symbols = T) |>   ## tokenize, removing unnecessary noise
  tokens_tolower() |>                                                   ## normalize
  tokens_remove(dkstop) |>                                     ## remove stopwords (English)
  tokens_wordstem()                                                      ## stemming
dtm <- dfm(corpus_clean)
dtm
dtm <- dfm_trim(dtm, min_termfreq = 10)
dtm

textplot_wordcloud(dtm, max_words = 50)                          ## top 50 (most frequent) words
textplot_wordcloud(dtm, max_words = 50, color = c('blue','red')) ## change colors
textstat_frequency(dtm, n = 10)                                  ## view the frequencies 


keyness <- textstat_keyness(dtm) # finr something to reference to

kwic <- kwic(tokens(corpus), "synd*", window = 7) # window is max words tothe left and right
head(kwic,10)


# spaCy?
