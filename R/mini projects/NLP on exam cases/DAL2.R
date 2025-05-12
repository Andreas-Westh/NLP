library(tidyverse)
library(tidytext)
library(readtext)
library(stopwords)
library(reshape2)
library(wordcloud2)


#### DAL2 Examcase ####
DAL2 <- readtext("mini projects/NLP on exam cases/Eksamenscase_DAL_F2025.docx")

DAL2_Token <- DAL2 %>% unnest_tokens(word, text)
DAL2_Token

tidy_DAL2_wSW <- DAL2_Token%>% 
  count(word, sort = T)
tidy_DAL2_wSW

#stopwords("da", source="stopwords-iso)
danish_stopwords <- stopwords::stopwords("da")
tidy_DAL2 <- DAL2_Token %>%
  filter(!word %in% danish_stopwords & !word %in% c("if","jeres","kan","h","ved","3")) %>% 
  count(word, sort = T)
tidy_DAL2

DAL2_top10 <- tidy_DAL2[1:10,]
DAL2_top10 %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)

##### Wordcloud #####
DAL2_wc <- tidy_DAL2 %>%
  rename(freq = n) 

wordcloud2(data = DAL2_wc,
           backgroundColor = "white",
           size = 1)




#### Previous Exam report from MAK ####
MAK_raw <- readtext("mini projects/NLP on exam cases/Fuld Opgave.docx")
MAK_Token <- MAK_raw %>% unnest_tokens(word, text)
danish_stopwords <- stopwords::stopwords("da")
tidy_MAK <- MAK_Token %>%
  filter(!word %in% danish_stopwords& !word %in% c("kan","ved","hvilket")) %>% 
  count(word, sort = T)
tidy_MAK
MAK_top10 <- tidy_MAK[1:10,]
MAK_top10 %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)

##### sentiment #####
afinn_da <- read.delim("mini projects/NLP on exam cases/AFINN-da-32.txt", header = FALSE, col.names = c("word", "score"))
tidy_dansk <- tidy_MAK %>%
  inner_join(afinn_da, by = "word") %>%
  mutate(sentiment_label = case_when(
    score < 0 ~ "negativ",
    score > 0 ~ "positiv",
    score == 0 ~ "neutral"
  ))
tidy_dansk %>%
  count(sentiment_label) %>% print()
# summeret 
tidy_dansk %>%
  group_by(sentiment_label) %>%
  summarise(total_score = sum(score), .groups = "drop") %>% print()

###### wordcloud #####
tidy_dansk %>%
  count(word, sentiment_label, sort = TRUE) %>%
  acast(word ~ sentiment_label, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)

###### wordcloud2 #####
#######  without sentiment #######
wc_data <- tidy_MAK %>%
  rename(freq = n)   # wordcloud2 kræver kolonnenavnene: word og freq

wordcloud2(data = wc_data,
           size = 1,              # Skaleringsfaktor
           color = "random-light",  # Eller fx "random-dark"
           backgroundColor = "white")



#######  with sentiment #######
wc_data <- tidy_dansk %>%
  group_by(word) %>%
  summarise(freq = sum(n),
            sentiment = first(sentiment_label), .groups = "drop") %>%
  mutate(color = case_when(
    sentiment == "positiv" ~ "forestgreen",
    sentiment == "negativ" ~ "firebrick",
    sentiment == "neutral" ~ "gray50"
  ))

# Plot
wordcloud2(data = wc_data[, c("word", "freq")],
           color = wc_data$color,
           backgroundColor = "white",
           size = 0.8)
