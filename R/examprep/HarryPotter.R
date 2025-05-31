library(dplyr)
library(rvest)
library(stringr)
library(tidytext)
library(stopwords)
library(wordcloud2)
library(ggplot2)
library(stm)
library(quanteda)
library(Sentida)
library(tidyr)
library(readtext)

# data retrieval
HP1 <- readtext("data/Harry Potter 1 - Sorcerer's Stone.txt")
HP2 <- readtext("data/Harry Potter 2 - Chamber of Secrets.txt")
HP3 <- readtext("data/Harry Potter 3 - The Prisoner Of Azkaban.txt")
HP4 <- readtext("data/Harry Potter 4 - The Goblet Of Fire.txt")
HP5 <- readtext("data/Harry Potter 5 - Order of the Phoenix.txt")
HP6 <- readtext("data/Harry Potter 6 - The Half Blood Prince.txt")
HP7 <- readtext("data/Harry Potter 7 - Deathly Hollows.txt")

HP <- list(HP1,HP2,HP3,HP4,HP5,HP6,HP7)

HP_df <- data.frame(title = NULL, text = NULL)
for (i in 1:length(HP)) {
  tmp_title <- HP[[i]]$doc_id
  tmp_text <- HP[[i]]$text
  tmp_df <- data.frame(title = tmp_title, text = tmp_text)
  HP_df <- rbind(HP_df, tmp_df)
}

# RegEx
# 1. Clean title
# 2. Remove table of contens
# 3. Also devide into chapters
# Maybe others?

