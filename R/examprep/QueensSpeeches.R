library(dplyr)
library(rvest)
library(stringr)
library(tidytext)
library(stopwords)
library(wordcloud2)
library(ggplot2)
library(stm)
library(quanteda)


# get speeches
base_url <- "https://www.kongehuset.dk/monarkiet-i-danmark/nytaarstaler/nytaarstale-"
base_url_upper <- "https://www.kongehuset.dk/nyheder/hendes-majestaet-dronningens-nytaarstale-"
years <- 2001:2023



title <- read_html(loop_url) %>% html_element(".headline") %>% html_text()
content <- read_html(loop_url) %>% html_element(".pad") %>% html_text()

# scrape loop

all_speeches <- data_frame(url=NULL, year = NULL, title = NULL, text=NULL)

for (year in years) {
  if (year > 2010) {
    base_url = "https://www.kongehuset.dk/nyheder/hendes-majestaet-dronningens-nytaarstale-"
  } else {
    base_url = "https://www.kongehuset.dk/monarkiet-i-danmark/nytaarstaler/nytaarstale-"
  }
  Sys.sleep(runif(1, min = 0.2, max = 0.6))
  print(paste0(year, loop_url))
  loop_url <- paste0(base_url,year)
  tmp_title <- read_html(loop_url) %>% html_element(".headline") %>% html_text()
  tmp_content <- read_html(loop_url) %>% html_element(".pad") %>% html_text()
  tmp_df <- data_frame(url = loop_url, year = year, title = tmp_title, text = tmp_content)  
  all_speeches<- rbind(all_speeches,tmp_df)
}
