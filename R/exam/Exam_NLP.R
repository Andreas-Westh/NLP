library(dplyr)
library(rvest)
library(stringr)
library(tidytext)
library(topicmodels)
library(stopwords)
library(wordcloud2)
library(ggplot2)
library(stm)
library(quanteda)
library(quanteda.textplots)
library(quanteda.textstats)
library(Sentida)
library(readtext)
library(igraph)
library(ggraph)

# hent reviews og indlæs drenge navne
all_reviews <- readRDS("R/exam/elgiganten.rds")
male_names <- read.csv("R/exam/drengenavne.csv", header = F)
colnames(male_names) = "name"
male_names$gender = "Male"


# subset af 300 obs
all_reviews <- all_reviews %>% 
  rowwise() %>% 
  mutate(length = nchar(content)) %>% 
  arrange(desc(length))

#top_300 <- head(all_reviews, 300)
top_300 <- all_reviews %>%
  ungroup() %>%  
  filter(length > 200, length < 3000) %>%
  sample_n(300)


# devide into gender based on the male_names
all_reviews_gender <- top_300 %>% 
  mutate(first_name = str_extract(name, "^[A-ZÆØÅa-zæøå]+")) %>% 
  mutate(gender = ifelse(first_name %in% male_names$name,"M","U"))

# Lix
lix <- function(content){
  words = unlist(str_split(content, " "))
  lix_score = mean(nchar(words))
  return(lix_score)
}  

all_reviews_gender <- all_reviews_gender %>%
  rowwise() %>% 
  mutate(lix = lix(content))


gender_sentiment <- all_reviews_gender %>% 
  rowwise() %>% 
  mutate(score = sentida(content, output = "mean"))



# sammenhæng
plot_data <- gender_sentiment %>%
  group_by(gender) %>%
  summarise(mean_lix = mean(lix, na.rm = TRUE),
            mean_sentiment = mean(score, na.rm = TRUE))

ggplot(plot_data) +
  geom_bar(aes(x = "Lix", y = mean_lix, fill = gender), 
           stat = "identity", position = "dodge") +
  geom_bar(aes(x = "Sentiment", y = mean_sentiment, fill = gender), 
           stat = "identity", position = "dodge") +
  labs(x = NULL, y = "Mean Value", fill = "Gender") +
  theme_minimal()



glm <- glm(as.factor(gender) ~ score + lix, data = gender_sentiment, family = "binomial")
summary(glm)


lm_score <- lm(score~as.factor(gender), data = gender_sentiment)
summary(lm_score)

lm_lix <- lm(lix~as.factor(gender), data = gender_sentiment)
summary(lm_lix)

lm_test <- lm(lix~score, data = gender_sentiment)
summary(lm_test)


plot(gender_sentiment$rating, gender_sentiment$score)



# med dsl
#### Tokennize ####
library(reticulate)
Sys.setenv(RETICULATE_PYTHON = "/opt/anaconda3/envs/spacy/bin/python")
library(spacyr)
spacy_initialize(
  model            = "da_core_news_md",
  refresh_settings = TRUE,
  verbose          = TRUE
)

texts <- top_300$content
names(texts) <- top_300$name

spacy_df <- spacy_parse(texts)
spacy_df <- spacy_df %>% 
  filter(lemma == str_extract(lemma, regex("[a-zæøåA-ZÆØÅ]+$"))) %>% 
  mutate(word = lemma)

raw_tokens_spacy <- spacy_df
raw_tokens <- raw_tokens_spacy

