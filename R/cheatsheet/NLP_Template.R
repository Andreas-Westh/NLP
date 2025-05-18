# ---- 0. Setup and Data Loading ----
library(tidyverse)
library(tidytext)
library(readtext)
library(stopwords)
library(wordcloud2)
library(topicmodels)
library(spacyr)
library(textstem)
library(janeaustenr)
library(gutenbergr)


#test data
#gutenbergr
lovecraft_works <- gutenberg_works(author == "Lovecraft, H. P. (Howard Phillips)")

ids <- lovecraft_works$gutenberg_id
raw_lovecraft <- gutenberg_download(ids, 
                                    meta_fields = "title",
                                    mirror = "http://mirror.csclub.uwaterloo.ca/gutenberg/")

sample_love <- raw_lovecraft %>% 
  group_by(title) %>% 
  mutate(
    linenumber = row_number()
  ) %>% 
  ungroup()

raw_text <- sample_love
raw_text <- sample_love %>% filter(!title == "Writings in the United Amateur, 1915-1922")


# austen books
sample_raw <- austen_books() %>%
  group_by(book) %>%                       # group by each novel
  mutate(linenumber = row_number(),       # add line number per book
         chapter = cumsum(                # increment chapter count
           str_detect(text,               # if line matches "chapter x"
                      regex("^chapter [\\divxlc]", 
                            ignore_case = TRUE)))) %>%
  ungroup() 

raw_text <- sample_raw































# For single text file
raw_text <- readtext("data/YOUR_DATA_HERE.docx")


# For multiple




# ---- Choose language: 'en' for English, 'da' for Danish ----
lang <- "en" # CHANGE THIS TO "da" IF DANISH

# ---- 1. Pre-processing ----
# Tokenization and Stopword removal
raw_tokens <- raw_text %>% 
  unnest_tokens(word, text)

word_counts <- raw_tokens %>% 
  count(word, sort = TRUE)

word_counts %>% 
  top_n(10, n) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(title = "Top 10 Words", y = NULL)

# Tokenize, clean, remove stopwords, and lemmatize
tokens <- raw_text %>%
  unnest_tokens(word, text) %>%                              # split into words
  filter(str_detect(word, "^[A-Za-zæøåÆØÅ]+$")) %>%           # keep letters only
  filter(!word %in% stopwords(lang)) %>%                     # remove stopwords
  mutate(original = word,                                     # keep raw form
         word     = lemmatize_words(word))                   # lemmatize

# Quick check
tokens %>% 
  count(word, sort = TRUE) %>% 
  slice_head(n = 10) %>%                                      # top 10 after cleaning
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(n, word)) +
  geom_col() +
  labs(title = "cleaned top 10", y = NULL)

word_counts <- df_tokens %>% 
  count(word, sort = TRUE)


# by unique title
top10_by_title <- tokens %>%
  count(title, word, sort = TRUE) %>%        # count occurrences per title
  group_by(title) %>%                        
  slice_max(n, n = 10) %>%                   # top 10 per title
  ungroup() %>%
  # reorder word factor *within each title* so each facet sorts nicely:
  mutate(word = tidytext::reorder_within(word, n, title))

ggplot(top10_by_title, aes(x = n, y = word, fill = title)) +
  geom_col(show.legend = FALSE) +
  scale_y_reordered() +                      # undo the within-title reordering
  facet_wrap(~ title, scales = "free_y") +   # one panel per book
  labs(
    title = "Top 10 Words by Title",
    x     = "Count",
    y     = NULL
  ) +
  theme_minimal()



# ---- 3. Wordcloud ----
wc_data <- word_counts %>% rename(freq = n)
wordcloud2(data = wc_data, size = 0.5)







# ---- 4. Sentiment Analysis ----
##### A) AFINN -----

# For English
afinn_en <- get_sentiments("afinn")

# For Danish
afinn_da <- read.delim("sentiment_lexicons/AFINN-da-32.txt", 
                       header = FALSE, 
                       col.names = c("word", "value"))

# Use one of the above (afinn_en or afinn_da)
sentiment_data_afinn <- df_tokens %>%
  inner_join(afinn_en, by = "word") %>% # <- Change here afinn_da or afinn_en
  mutate(sentiment_label = case_when(
    value < 0 ~ "negative",
    value > 0 ~ "positive",
    TRUE ~ "neutral"
  ))

# Quick Sentiment Count
sentiment_data_afinn %>%
  count(sentiment_label)

# Sentiment barplot
sentiment_data_afinn %>%
  count(sentiment_label) %>%
  ggplot(aes(sentiment_label, n, fill = sentiment_label)) +
  geom_col(show.legend = FALSE) +
  labs(title = "AFINN Sentiment Distribution")



##### B) Bing Sentiment (English Only) -----

bing_sentiments <- get_sentiments("bing")

sentiment_data_bing <- tokens %>%
  inner_join(bing_sentiments, by = "word")

# Sentiment count
sentiment_data_bing %>%
  count(sentiment)

# Bing sentiment plot
sentiment_data_bing %>%
  count(sentiment) %>%
  ggplot(aes(sentiment, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  labs(title = "Bing Sentiment Distribution")


###### C) NRC Sentiment (English Only) ----
nrc_sentiments <- get_sentiments("nrc")

sentiment_data_nrc <- tokens %>%
  inner_join(nrc_sentiments, by = "word")

# NRC sentiment summary (top 10)
sentiment_data_nrc %>%
  count(sentiment, sort = TRUE) %>%
  top_n(10, n) %>%
  ggplot(aes(reorder(sentiment, n), n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(title = "Top NRC Sentiments", x = NULL, y = "Count")

# NRC sentiment categories within each title
sentiment_counts <- tokens %>%
  inner_join(get_sentiments("nrc"), by = "word") %>%
  count(title, sentiment) %>%       # how many words of each sentiment in each book
  ungroup()

ggplot(sentiment_counts,
       aes(x = n, y = sentiment, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ title, scales = "free_x") +
  labs(
    title = "NRC Sentiment Distribution by Title",
    x     = "Count of words",
    y     = NULL
  ) +
  scale_fill_brewer(palette = "Dark2") +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.y = element_text(size = 8)
  )


# Top words within each



# ---- !!!!5. Part of speech tags----

spacy_initialize(model = "en_core_web_sm")
# To Do






# ---- 6. Topic Modeling ----
# Document-Term Matrix preparation
dtm <- tokens %>%
  mutate(document = "doc1") %>%  # single document scenario, adjust if multiple docs
  count(document, word) %>%
  cast_dtm(document, word, n)

# LDA with 2 topics (adjustable)
lda_model <- LDA(dtm, k = 2, control = list(seed = 1234))
topics <- tidy(lda_model, matrix = "beta")

# Top terms per topic
top_terms <- topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 5) %>% 
  ungroup() %>%
  mutate(term = reorder_within(term, beta, topic))

# Plot top terms per topic
top_terms %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  scale_y_reordered() +
  labs(title = "Top Terms per Topic")




# ---- Bigrams ----
bigrams_raw <- raw_text %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>% 
  filter(!is.na(bigram))
bigrams_raw

# without stopwords
bigrams_seperated <- bigrams_raw %>% 
  separate(bigram, c("word1","word2"), sep = " ")
bigrams_filtered <- bigrams_seperated %>% 
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word) %>% 
  count(word1, word2, sort = TRUE)
bigrams

# trigrams
raw_text %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  filter(!is.na(trigram)) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>%
  count(word1, word2, word3, sort = TRUE)


# analysing them
bigrams_filtered %>% 
  filter(word2 == "street") %>% 
  count(title, word1, sort = T)
