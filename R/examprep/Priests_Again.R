library(dplyr)
library(rvest)
library(stringr)
library(tidytext)
library(stopwords)
library(wordcloud2)
library(ggplot2)
library(stm)
library(quanteda)

# data retrieval ----
priests_raw <- read_html("R/examprep/priests.html")
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

all_texts$priest <- str_extract(all_texts$priest, "[^ ]+( [^ ]+)*(?=  )")

# data prep ----
# tokennize
    # version with chapter´syntax and such
#sample_raw <- all_texts %>%
#  group_by(priest) %>%                       # group by each novel
#  mutate(linenumber = row_number(),       # add line number per book
#         chapter = cumsum(                # increment chapter count
#           str_detect(text,               # if line matches "chapter x"
#                      regex("^chapter [0-9]", 
#                            ignore_case = TRUE)))) %>%
#  ungroup() 

tokens_raw <- all_texts %>% 
  unnest_tokens(output = "word", 
                input = text, 
                token = "words",
                to_lower = T) # True so better works with stopwords


# stopwords
dkstop <- c(stopwords("da"),"kan","så","saa","paa", "oc")
tokens_SW <- tokens_raw %>% 
  filter(!word %in% dkstop)

#### Tidy ####
# count
tokens_count <- tokens_SW %>% 
  group_by(priest) %>% 
  count(word, sort = T) %>% 
  mutate(total = sum(n),
         percent = round(n / total * 100,1)) %>% 
  ungroup() %>% 
  mutate(global_percent = round(n / sum(n) * 100,2))


# wordcloud
# normal ggplot

# Zipf law?



# topic modelling
# document term matrix
# if_idf



# bigrams
  # example with split by gender
# relationsgrafer
# korrelede ord





# spaCy?
