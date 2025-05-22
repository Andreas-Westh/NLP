library(dplyr)
library(rvest)
library(stringr)
library(tidytext)
library(stopwords)
library(wordcloud2)
library(ggplot2)
library(stm)
library(quanteda)

priests_raw <- read_html("R/mini projects/priests.html")
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


raw_tokens <- all_texts %>% 
  unnest_tokens(word, text)

dkstop=c(stopwords(language = "da"),"kan","så","på","paa","når","saa","naar","ved")
tokens = raw_tokens %>% filter(!word %in% dkstop)
tokens_count <- tokens %>% count(url,word,sort=T)

clean_count <- tokens %>% count(word, sort=T)
wordcloud2(data = clean_count, size = 0.4)

tidy_tokens <- tokens %>% count(url,word,sort=T) %>% 
  bind_tf_idf(word, url, n) %>% 
  group_by(url) %>% 
  top_n(10) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, tf_idf))

tidy_tokens_dfm <- tokens %>% 
  count(url, word) %>% 
  cast_dfm(url, word, n)

topics <- stm(tidy_tokens_dfm, K = 3, init.type = "Spectral")
summary(topics)

tidy_beta <- tidy(topics)

tidy_beta %>% 
  group_by(topic) %>% 
  top_n(10) %>% 
  ungroup() %>% 
  mutate(term = reorder(term, beta)) %>% 
  ggplot(aes(term, beta, fill = topic)) +
  geom_col(show.legend = F) +
  facet_wrap(~topic, scales = "free") +
  coord_flip()

tidy_gamma <- tidy(topics, matrix = "gamma",
                   document_names = rownames(tidy_tokens_dfm))

ggplot(tidy_gamma, aes(gamma, fill = as.factor(topic))) +
  geom_histogram(show.legend = F) +
  facet_wrap(~topic, ncol = 3)
  




