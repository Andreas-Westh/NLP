library(tidyverse)
library(tidytext)
library(janeaustenr)
library(igraph) # for he network visual
library(ggraph)
library(stopwords)

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
 
parsed_text <- spacy_parse(raw_text$text, doc_id = raw_text$book, to_lower = T)

raw_text <- parsed_text %>%
  filter(str_detect(lemma, "^[A-Za-zæøåÆØÅ]+$")) #%>%           # keep letters only
 # filter(!lemma %in% stopwords("en"))



# he / she er i stopord, find fix




bigrams_df <- raw_text %>%
  group_by(doc_id) %>%
  mutate(next_lemma = lead(lemma),
         bigram = paste(lemma, next_lemma, sep = " ")) %>%
  filter(!is.na(next_lemma)) %>%
  ungroup()


bigrams_seperated <- bigrams_df %>% 
  separate(bigram, c("word1","word2"), sep = " ")
bigrams_filtered <- bigrams_seperated %>% 
  count(word1, word2, sort = TRUE)
bigrams_filtered

# example: find the difference within gendered pronouns
genderfilter <- c("he","she")
bigrams_gender <- bigrams_seperated %>% 
  filter(word1 %in% genderfilter) %>% 
  mutate(gender = ifelse(word1 == "he","M","F")) %>% group_by(gender) %>% count(word2)

bigrams_gender %>% 
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


### FIX to_lower, det SKAL være T
