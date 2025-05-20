library(dplyr)
library(stringr)
library(tidytext)
library(tidyverse)
library(Sentida)
library(quanteda)
library(ggplot2)

#### data retrieval ####
raw_elgiganten <- readRDS("R/mini projects/Reviews/elgiganten.rds")
raw_power <- readRDS("R/mini projects/Reviews/powerreviews.rds")



#### data prep ####
elgiganten_reviews <- raw_elgiganten %>% filter(nchar(content) > 35) %>% # only reviews with 35 or more chars
  filter(!str_detect(content,"Dato for oplevelse")) %>% 
  mutate(text = str_replace_all(content, "[^a-zæøåÆØÅ\\s\\.,-]",""),
         length = nchar(text),
         rating = as.integer(rating / 10)
         ) 
elgiganten_reviews <- elgiganten_reviews %>% rowwise() %>% mutate(score = sentida(text, output = "mean"))
hist(elgiganten_reviews$rating)
hist(elgiganten_reviews$score)


power_reviews <- raw_power %>% filter(nchar(content) > 35) %>% # only reviews with 35 or more chars
  filter(!str_detect(content,"Dato for oplevelse")) %>% 
  mutate(text = str_replace_all(content, "[^a-zæøåÆØÅ\\s\\.,-]",""),
         length = nchar(text),
         rating = as.integer(rating / 10)
  ) 
power_reviews <- power_reviews %>% rowwise() %>% mutate(score = sentida(text, output = "mean"))
hist(power_reviews$rating)
hist(power_reviews$score)

###### lix-score which is the readability, higher = more advanced #####
mylix <- function(text){
  words = unlist(str_split(text, " ")) # split text after a space
  lixscore = mean(nchar(words))
  return(lixscore)
}

elgiganten_reviews <- elgiganten_reviews %>% mutate(lix = mylix(text))
hist(elgiganten_reviews$lix)
power_reviews <- power_reviews %>% mutate(lix = mylix(text))
hist(power_reviews$lix)

##### making the sentiment score more like trustpilots labeling #####
labels <- c("5","4","3","2","1")
breaks <- c(5,3,1,-1,-3,-5) 
elgiganten_reviews <- elgiganten_reviews %>% rowwise() %>% 
  mutate(catscore = cut(score,breaks = breaks, labels = labels))
ggplot(elgiganten_reviews, aes(x=catscore))+geom_bar()
power_reviews <- power_reviews %>% rowwise() %>% 
  mutate(catscore = cut(score, breaks = breaks, labels = labels))
ggplot(power_reviews, aes(x=catscore))+geom_bar()


#### spacy ####
Sys.setenv(RETICULATE_PYTHON = "/opt/anaconda3/envs/spacy/bin/python")
library(spacyr)
spacy_initialize(
  model            = "da_core_news_md",
  refresh_settings = TRUE,
  verbose          = TRUE
)

elgiganten_spacy <- spacy_parse(elgiganten_reviews$text)
power_spacy <- spacy_parse(power_reviews$text)

##### NOUNS #####
elgiganten_NOUNS <- elgiganten_spacy %>% filter(pos == "NOUN") %>% select(doc_id, lemma)
elgiganten_NOUNS_Count <- elgiganten_NOUNS %>% count(lemma)
power_NOUNS <- power_spacy %>% filter(pos == "NOUN") %>% select(doc_id, lemma)
power_NOUNS_count <- power_NOUNS %>% count(lemma)

##### inner join by nouns #####
el_pw <- inner_join(elgiganten_NOUNS_Count, power_NOUNS_count, by = 'lemma')
colnames(el_pw)=c("lemma","ElgigantenCount","PowerCount")
el_pw <- el_pw %>% mutate(total = ElgigantenCount+PowerCount,
                          Ratio_Elgiganten = round(ElgigantenCount / PowerCount,1),
                          Elgiganten_Majority = ifelse(Ratio_Elgiganten > 1,1,0),
                          Ratio_Power = round(PowerCount / ElgigantenCount,1),
                          Power_Majority = ifelse(Ratio_Power > 1,1,0))

el_pw %>% filter(total > 4000) %>% filter(!str_detect(lemma,"lgigant")) %>% 
  ggplot(aes(x=reorder(lemma,Ratio_Elgiganten),y=Ratio_Elgiganten, fill = as.factor(Elgiganten_Majority)))+
  geom_bar(stat="identity")+
  coord_flip()+
  labs(title="Elgiganten have packages named more than 2x of Power",y="Ratio between Elgiganten and Power", x="Word")

#### Bigrams ####
Elgiganten_Bigrams <- elgiganten_reviews %>% 
  unnest_tokens(bigram, text, token = "ngrams", n=2)

Power_Bigrams <- power_reviews %>% 
  unnest_tokens(bigram, text, token = "ngrams", n=2) 

##### Split Bigrams #####
El_Bi_Sep <- Elgiganten_Bigrams %>% separate(bigram,c("word1","word2"),sep=" ") # seperate at a space
Pw_Bi_Sep <- Power_Bigrams %>% separate(bigram,c("word1","word2"),sep=" ")


##### Sentiment-library from DSL #####
dk_Sentiment <- read_csv("R/mini projects/Reviews/2_headword_headword_polarity(in).csv")
colnames(dk_Sentiment)=c("word1","v2","pos","v3","score","family")
dk_Sentiment_adj <- dk_Sentiment %>% filter(pos == "adj.") %>% select(word1,score)

# Elgiganten
El_Bi_Sep_u <- distinct(El_Bi_Sep)
El_Bi_Sep_Filter <- El_Bi_Sep_u %>% filter(word1 %in% dk_Sentiment_adj$word1,
                                         word2 %in% el_pw$lemma)

Elgrams_C <- El_Bi_Sep_Filter %>% count(word1,word2,sort = T)
Elgrams_C <- inner_join(Elgrams_C,dk_Sentiment_adj, by = "word1")
Elgrams_C <- Elgrams_C %>% mutate(total_score = n*score) %>% arrange(total_score)
# find top and bottom
El_Bottom = head(Elgrams_C,10)
El_Top = tail(Elgrams_C,10)
El_TopBottom = rbind(El_Bottom,El_Top)
# Bind bigrams together again
El_TopBottom <- El_TopBottom %>% unite(col="bigram",c("word1","word2"))

ggplot(El_TopBottom, aes(x=reorder(bigram,total_score),y=total_score,fill=total_score>0))+
  geom_bar(stat = "identity")+
  coord_flip() +
  labs(title = "Top and Bottom bigrams for Elgiganten",x="Bigram",y="Total Score")


# Power
Pw_Bi_Sep_u <- distinct(Pw_Bi_Sep)
Pw_Bi_Sep_Filter <- Pw_Bi_Sep_u %>% filter(word1 %in% dk_Sentiment_adj$word1,
                                           word2 %in% el_pw$lemma)

Pwgrams_C <- Pw_Bi_Sep_Filter %>% count(word1,word2,sort=T)
Pwgrams_C <- inner_join(Pwgrams_C,dk_Sentiment_adj, by = "word1")
Pwgrams_C <- Pwgrams_C %>% mutate(total_score = n*score) %>% arrange(total_score)

Pw_Bottom = head(Pwgrams_C,10)
Pw_Top = tail(Pwgrams_C,10)
Pw_TopBottom = rbind(Pw_Bottom, Pw_Top)
Pw_TopBottom <- Pw_TopBottom %>% unite(col = "bigram",c("word1","word2"))
ggplot(Pw_TopBottom, aes(x=reorder(bigram,total_score),y=total_score,fill=total_score>0))+
  geom_bar(stat = "identity")+
  coord_flip() +
  labs(title = "Top and Bottom bigrams for Power",x="Bigram",y="Total Score")
