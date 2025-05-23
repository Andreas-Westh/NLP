library(dplyr)
library(rvest)
library(stringr)
library(tidytext)
library(stopwords)
library(wordcloud2)
library(ggplot2)
library(stm)
library(quanteda)


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

