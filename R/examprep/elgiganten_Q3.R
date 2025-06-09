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
