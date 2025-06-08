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

#### Data Retrieval ####
##### Scraping #####
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

##### .txt files #####
# loop i gennem filer, gem i liste 
# lav et lignende loop som ovenstående 
files_df <- readtext("R/examprep/previous/Q4-master/*") # automatically reads all files
files_df <- files_df %>% 
  mutate(year = str_extract(text, regex("[0-9]{4}"))) # get year

# now to remove the title, pattern = we can see its the year just before text starts
files_df <- files_df %>%
  mutate(text_raw = text,
         text = str_squish(text_raw),  # remove extra whitespace and line breaks
         text = str_extract(text, "(?<=\\b[0-9]{4}\\b\\s).*")) 
# \\?<= is proceeded by 
# \\b boundery boxes for the pattern
# \\s any white space (just to not get it with us)
# .* everything after





#### Tokennize ####
library(reticulate)
Sys.setenv(RETICULATE_PYTHON = "/opt/anaconda3/envs/spacy/bin/python")
library(spacyr)
spacy_initialize(
  model            = "da_core_news_md",
  refresh_settings = TRUE,
  verbose          = TRUE
)

spacy_df <- spacy_parse(all_speeches$text)
spacy_df <- spacy_df %>% 
  filter(lemma == str_extract(lemma, regex("[a-zæøåA-ZÆØÅ]+$")))

# make doc_id more redable, in this example grouped by priest
doc_ids <- unique(spacy_df$doc_id)
better_ids <- unique(all_speeches$year)
spacy_df <- spacy_df %>% 
  mutate(doc_id = better_ids[match(doc_id, doc_ids)]) %>% 
  rename(word = lemma)
spacy_df$year <- spacy_df$doc_id

raw_tokens_spacy <- spacy_df


raw_tokens_tidy <- all_speeches %>%
  unnest_tokens(sentence, text, token = "sentences") %>%
  group_by(year) %>%
  mutate(sentence_number = row_number()) %>% # this first part was done to get sentence number for each word, for timeline plot or smth
  ungroup() %>%
  unnest_tokens(word, sentence, token = "words") %>%
  select(url, year, sentence_number, word)


###########################
#### PICK SPACY OR TIDY ###
###########################

raw_tokens <- raw_tokens_tidy
raw_tokens <- raw_tokens_spacy










#remove stop words
dkstop <- c(stopwords("da"),"kan","så","må","ved","al")
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

###### wordcloud ######
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

###### Sentiment distribution ######
sentiments_df %>% 
  ggplot(aes(x = score)) +
  geom_histogram(binwidth = 0.05, fill = "steelblue", color = "white") +
  geom_vline(xintercept = mean(sentiments_df$score), 
             linetype = "dashed", color = "darkgray") +
  labs(title = "Sentiment distribution",
       x = "Mean sentiment score",
       y = "Number of speeches") +
  theme_minimal()

##### Word and document frequency #####

###### Zips law ######

##### Lix score #####
mylix <- function(text){
  words = unlist(str_split(text, " ")) # split text after a space
  lixscore = round(mean(nchar(words)),2)
  return(lixscore)
}

all_speeches <- all_speeches %>% 
  rowwise() %>% 
  mutate(lix_score = mylix(text))



##### Topics #####
topic_sw <- c("kan", "så", "må", "ved", "al",
              "danmark", "år", "godt", "danske", "vore", "vores", "nye", 
              "aften", "nytår", "hele", "samfund", "sammen", "ønsker", 
              "tak", "helt", "tid", "går", "både", "andre", "verden")
topic_tokens <- tokens %>% filter(!word %in% topic_sw & 
                                          word == str_extract(word, regex("^[a-zæøåA-ZÆØÅ]+$")))

tidy_tokens_clean <- topic_tokens %>% count(year, word, sort = T) %>% 
  bind_tf_idf(word, year, n) %>% 
  group_by(year) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, tf_idf))
#tf (term freq) = how often a word is said in a document (%)
#idf (inverse doc freq) = how rare a word is across all docs
#tf_idf(tf * idf) = high if a word is important in one document, less in others

###### unique words within each speech ######
tidy_tokens_clean %>%
  group_by(year) %>%
  slice_max(tf_idf, n = 5, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(word = reorder_within(word, tf_idf, year)) %>%
  ggplot(aes(word, tf_idf, fill = year)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~year, scales = "free") +
  coord_flip() +
  scale_x_reordered()
# this one is a little different depending on if its ran with spacy or tidy in raw_tokens
# 2023 does not show palestine with spacy


# make into data term matrix
speeches_dtm <- tidy_tokens_clean %>% 
  cast_dtm(year, word, n)
speeches_dtm

# use LDA to make the topics
speeches_lda <- LDA(speeches_dtm, k = 4, control = list(seed = 1980))
speeches_lda

###### beta ######
# beta = which word relates to which topic
speeches_topic <- tidy(speeches_lda, matrix = "beta")
speeches_topic
# one-topic-per-word probability

top_terms <- speeches_topic %>% 
  group_by(topic) %>% 
  slice_max(beta, n = 5) %>% 
  ungroup() %>% 
  arrange(topic, -beta)
top_terms

top_terms %>% 
  mutate(term = reorder_within(term, beta, topic)) %>% 
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = F) +
  facet_wrap(~topic, scales = "free") +
  scale_y_reordered()

###### gamma ######
# gamma = which topics relate to which document
speeches_gamma <- tidy(speeches_lda, matrix = "gamma")
speeches_gamma
# 2006 gemma = 1.00 for topic 1, meaning it is purely filled by that topic
speeches_gamma %>% 
  mutate(speech = document, gamma * topic) %>% 
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~speech) +
  labs(x = "topic", y = expression(gamma))


# this finds the dominant topic within each speech
speeches_classifications <- speeches_gamma %>%
  group_by(document) %>%
  slice_max(gamma) %>% # this finds the highest gamma topic
  ungroup()

speeches_classifications


speech_topics <- speeches_classifications %>%
  count(document, topic) %>%
  group_by(document) %>%
  slice_max(n, n = 1) %>% 
  ungroup() %>%
  transmute(consensus = document, topic)

speeches_classifications %>%
  inner_join(speech_topics, by = "topic") %>%
  filter(document != consensus)
# 2004 is the main example for topic 2, tho 2001 is also fully topic 2
# therefore they are both very much alike
# seems like the clustering did quite well

assignments <- augment(speeches_lda, data = speeches_dtm)
assignments
# here 'år' is assigned mutiple topics, why is that?
# The same word can belong to different topics across documents because 
# LDA assigns topics per word occurrence, not per word globally.


# innerjoin with topics, to get the consensus 
assignments <- assignments %>%
  inner_join(speech_topics, by = c(".topic" = "topic"))
assignments

# plot into a CM
library(scales)
assignments %>%
  count(document, consensus, wt = count) %>%
  mutate(across(c(document, consensus), ~str_wrap(., 20))) %>%
  group_by(document) %>%
  mutate(percent = n / sum(n)) %>%
  ggplot(aes(consensus, document, fill = percent)) +
  geom_tile() +
  scale_fill_gradient2(high = "darkred", label = percent_format()) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid = element_blank()) +
  labs(x = "Speeches words were assigned to",
       y = "Speeches words came from",
       fill = "% of assignments")
# here it seems quite messy, but that is due to the topic overlap, general language, and all being new years speeches


##### Bigrams #####
# chapter 4

###### specifik bigrams (like genderered) ######


## bigrams with spacy ##





#### SpaCy specific ####

##### Parts of speech #####




#### Misc ####
##### Regex #####

##### Functions #####


