elgiganted <- readRDS("R/examprep/previous/Q3/elgiganten.rds")
parsed <- readRDS("R/examprep/previous/Q3/elgiganten_parsed.rds")

df_afinn <- read.csv("R/examprep/previous/Q3/2_headword_headword_polarity.csv")
df_adj <- df_afinn %>% 
  filter(sb. == "adj.")

count <- parsed %>% 
  count(lemma, pos, sort = T)

count_noun <- count %>% 
  filter(pos == "NOUN")

noun_20 <- count_noun %>% 
  head(20)
wcdata <- noun_20 %>% 
  rename(word = lemma) %>% 
  select(word, n)
wordcloud2(data = wcdata, size = 0.5)



bigrams_spacy <- parsed %>%
  group_by(doc_id) %>%
  mutate(next_lemma = lead(lemma)) %>%
  filter(!is.na(next_lemma)) %>%
  mutate(bigram = paste(lemma, next_lemma, sep = " ")) %>%
  select(doc_id, sentence_id, token_id, bigram)
bigrams_sep <- bigrams_spacy %>% separate(bigram,c("word1","word2"), sep = " ")


bigrams_subset <- bigrams_sep %>% 
  filter(word1 %in% df_adj$begejstringsrus &
         word2 %in% noun_20$lemma)

# count
bigrams_count <- bigrams_subset %>% 
  ungroup() %>% 
  count(word1, word2, sort = T) %>% 
  mutate(total = sum(n),
         procent = round(n/total,2))

df_adj_lj <- df_adj %>% 
  select(begejstringsrus, sb., X5) %>% 
  rename(word1 = begejstringsrus,
         pos = sb.,
         AFINN = X5)

bigrams_count <- left_join(bigrams_count, df_adj_lj, by = "word1")
bigrams_count <- bigrams_count %>% 
  mutate(score = n * AFINN)

bigrams_count <- bigrams_count %>% 
  unite(col = bigram, c("word1","word2"), sep = " ") 

bigrams_count <- bigrams_count %>% 
  arrange(desc(score))

# plot
bigrams_top = head(bigrams_count,10)
bigrams_bottom = tail(bigrams_count,10)
bigrams_TB = rbind(bigrams_top,bigrams_bottom)

# plot top and bottom
ggplot(bigrams_TB, aes(x=reorder(bigram, score),y=score,fill=score>0))+
  geom_bar(stat = "identity")+
  coord_flip() +
  labs(title = "Top and Bottom sentiment words",x="Word",y="Total Score")



# bigrams network
bigrams_count_sep <- bigrams_subset %>% 
  count(word1, word2, sort = T)
bigrams_count_sep

bigram_graph <- bigrams_count_sep %>%
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
