all_reviews <- readRDS("R/Opgaver/Opg3/elgiganten.rds")
all_parsed <- readRDS("R/Opgaver/Opg3/elgiganten_parsed.rds")

AFINN <- read.csv("R/Opgaver/Opg3/2_headword_headword_polarity.csv")
colnames(AFINN) = c("word1","x2","pos","x4","AFINN","x6")
AFINN_adj <- AFINN %>% 
  filter(pos == "adj.") %>% 
  select(word1, pos, AFINN)

# count
all_NOUNS <- all_parsed %>% 
  filter(pos == "NOUN")

NOUNS_top_20 <- all_NOUNS %>% 
  ungroup() %>% 
  count(lemma, sort = T) %>% 
  head(20)

wcdata <- NOUNS_top_20 %>% 
  rename(word = lemma)
wordcloud2(data = wcdata, size = 0.5)




bigrams_spacy <- all_parsed %>%
  group_by(doc_id) %>%
  mutate(next_lemma = lead(lemma)) %>%
  filter(!is.na(next_lemma)) %>%
  mutate(bigram = paste(lemma, next_lemma, sep = " ")) %>%
  select(doc_id, sentence_id, token_id, bigram)
bigrams_sep <- bigrams_spacy %>% separate(bigram,c("word1","word2"), sep = " ")

bigrams_sub <- bigrams_sep %>% 
  filter(word1 %in% AFINN_adj$word &
         word2 %in% NOUNS_top_20$lemma)

bigrams_count <- bigrams_sub %>% 
  ungroup() %>% 
  count(word1, word2, sort = T)

bigrams_sentiment <- left_join(bigrams_count, AFINN_adj, by = "word1")

bigrams_sentiment <- bigrams_sentiment %>% 
  rowwise() %>% 
  mutate(score = n * AFINN) %>% 
  arrange(desc(score))



# plot
bigrams_unite <- bigrams_sentiment %>% 
  unite(col = bigram, c("word1","word2"), sep = " ") 

bigrams_top = head(bigrams_unite,10)
bigrams_bottom = tail(bigrams_unite,10)
bigrams_TB = rbind(bigrams_top,bigrams_bottom)

# plot top and bottom
ggplot(bigrams_TB, aes(x=reorder(bigram, score),y=score,fill=score>0))+
  geom_bar(stat = "identity")+
  coord_flip() +
  labs(title = "Top and Bottom sentiment words",x="Word",y="Total Score")

top_20 <- head(bigrams_sentiment,20)

bigram_graph <- top_20 %>%
  filter(n > 2) %>% 
  ungroup() %>% # if the graph gives numbers, remember to ungroup!!
  select(word1, word2, n) %>% 
  graph_from_data_frame()
bigram_graph

set.seed(1980)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()



