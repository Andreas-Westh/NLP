library(dplyr)
library(tidytext)
library(janeaustenr)
library(igraph) # for he network visual
library(ggraph)

# bigrams network
# gender analysis jane austen, filter he/she, what verbs are mutual, which are only he/she
sample_raw <- austen_books() %>%
  group_by(book) %>%                       # group by each novel
  mutate(linenumber = row_number(),       # add line number per book
         chapter = cumsum(                # increment chapter count
           str_detect(text,               # if line matches "chapter x"
                      regex("^chapter [0-9]", 
                            ignore_case = TRUE)))) %>%
  ungroup() 

raw_text <- sample_raw

library(reticulate)
library(spacyr)
Sys.setenv(RETICULATE_PYTHON = "/opt/anaconda3/envs/spacy/bin/python")
spacy_initialize(model = "en_core_web_sm", refresh_settings = TRUE)


parsed_text <- cbind(
  raw_text %>% select(book, chapter, linenumber),
  spacy_parse(raw_text$text)
)

# ---- Bigrams ----
bigrams_raw <- raw_text %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>% 
  filter(!is.na(bigram))
bigrams_raw

# without stopwords
bigrams_seperated <- bigrams_raw %>% 
  separate(bigram, c("word1","word2"), sep = " ")
bigrams_filtered <- bigrams_seperated %>% 
  filter(!word1 %in% stopwords("en"),
         !word2 %in% stopwords("en")) %>% 
  count(word1, word2, sort = TRUE)
bigrams_filtered

# example: find the difference within gendered pronouns
genderfilter <- c("he","she")
bigrams_gender <- bigrams_seperated %>% 
  filter(word1 %in% genderfilter) %>% 
  mutate(gender = ifelse(word1 == "he","M","F")) %>% group_by(gender) %>% count(word2)

bigrams_gender %>% 
  filter(!word2 %in% stop_words$word) %>%     
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


bgraph <- graph_from_data_frame(bigrams_filtered)
bgraph

ggraph(bgraph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

