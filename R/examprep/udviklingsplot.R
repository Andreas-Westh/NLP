library(tidyverse)
library(janeaustenr)
library(dplyr)
library(stringr)
library(tidytext)

books_raw <- austen_books() %>%
  group_by(book) %>%
  mutate(
    length = nchar(text),
    linenumber = row_number(),
    chapter = cumsum(str_detect(text, 
                                regex("^chapter [\\divxlc]", 
                                      ignore_case = TRUE)))) %>%
  ungroup() 
hist(books_raw$length)

books_clean <- books_raw %>% 
  filter(book == "Sense & Sensibility") %>% 
  filter(length>60)
hist(books_clean$length)

# to tidy
tokens_raw <- books_clean %>% unnest_tokens(word,text)

#### AFINN ####
afinn=get_sentiments("afinn")
sentiment_df <- inner_join(tokens_raw,afinn, by = "word")
sentimentScore_df <- sentiment_df %>% 
  group_by(chapter, linenumber) %>%
  summarise(total_score = sum(value), .groups = "drop") # to not have repeating line numbers, fucks up timeline

table(sentimentScore_df$linenumber)

# timeline plot
sentimentScore_df %>% 
  ggplot(aes(x=linenumber, y=total_score)) +
  geom_line(alpha=0.2) +
  geom_smooth(method = "loess", span = 0.01)

