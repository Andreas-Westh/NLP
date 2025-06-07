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

#### Data Retrieval ####
# get urls
URL_site <- "https://www.kongehuset.dk/monarkiet-i-danmark/nytaarstaler/#laes-de-seneste-nytaarstaler"
url <- read_html(URL_site)
links <- url %>% html_nodes(".accordion__container__item__content .field-item a") %>% 
  html_attr("href") %>% as.data.frame()
links <- links[-1,]




# get speeches
base_url <- "https://www.kongehuset.dk"

all_speeches <- data_frame(url=NULL, title = NULL, text=NULL)

for (link in links) {
  Sys.sleep(runif(1, min = 0.2, max = 0.6))
  print(link)
  loop_url <- paste0(base_url,link) # match base url with scraped speech url
  tmp_year <- str_extract(loop_url,"[0-9]{4}") # reGex out the year
  tmp_content <- read_html(loop_url) %>% html_element(".rich-text") %>% html_text() # get the speech
  tmp_df <- data_frame(url = loop_url, year = tmp_year, text = tmp_content)  
  all_speeches<- rbind(all_speeches,tmp_df)
}


#### Tidy ####
raw_tokens <- all_speeches %>%
  unnest_tokens(sentence, text, token = "sentences") %>%
  group_by(year) %>%
  mutate(sentence_number = row_number()) %>% # this first part was done to get sentence number for each word, for timeline plot or smth
  ungroup() %>%
  unnest_tokens(word, sentence, token = "words") %>%
  select(url, year, sentence_number, word)

#remove stop words
dkstop <- c(stopwords("da"),"kan","så","må","ved")
tokens <- raw_tokens %>% 
  filter(!word %in% dkstop)

##### count #####
# total count per word
total_count <- tokens %>% 
  count(word, name = "global_total", sort = T)


# count grouped by priest
tokens_count <- tokens %>% 
  group_by(year) %>% 
  count(word, sort = T) %>% 
  mutate(total = sum(n),
         percent = round(n / total * 100,1)) %>% 
  ungroup() %>% 
  left_join(total_count, by = "word") %>% 
  mutate(global_percent = round(global_total / sum(global_total) * 100,4))

##### wordcloud #####
wc_data <- total_count %>% 
  rename(n = global_total) %>% 
  select(word, n)
wordcloud2(data = head(wc_data,100), size = 0.5)


##### Sentiment #####
sentiments_df <- all_speeches %>% 
  rowwise() %>% 
  mutate(score = sentida(text, output = "mean"))

# most negative speech
negativ_speech <- tokens %>% 
  filter(year == "2005") %>% 
  count(word, sort = T) %>% 
  rowwise() %>% 
  mutate(sentiment = sentida(word, output = "total")) %>% 
  arrange(desc(sentiment))

negativ_speech_top = head(negativ_speech,10)
negativ_speech_bottom = tail(negativ_speech,10)
negativ_speech_TB = rbind(negativ_speech_top,negativ_speech_bottom)

# plot top and bottom
ggplot(negativ_speech_TB, aes(x=reorder(word, sentiment),y=sentiment,fill=sentiment>0))+
  geom_bar(stat = "identity")+
  coord_flip() +
  labs(title = "Top and Bottom sentiment words",x="Word",y="Total Score")


###### Sentiment timeline #####
mean_score = mean(sentiments_df$score)
lm <- lm(score ~ as.numeric(year),sentiments_df)
sentiments_df %>% 
  mutate(year = as.numeric(year)) %>% 
  ggplot(aes(x=year,y=score)) +
  geom_line() +
  geom_abline(intercept = lm$coefficients[1], slope = lm$coefficients[2], 
              color = "gray50", linetype = 2) +
  geom_point(aes(color = year)) +
  geom_hline(yintercept = mean_score, 
             linetype = "dashed",
             color = "darkgray") +
  labs(title = "the sentiment has become more positive over time")



##### Lix score #####




##### Topics #####


##### Bigrams #####

###### specifik bigrams (like genderered) ######

##### Zips law #####





#### SpaCy ####

##### Parts of speech #####




#### Misc ####
##### Regex #####

##### Functions #####


