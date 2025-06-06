




string <- "Fra 2020 til 2022 boede Kurt Verner i .nr 34"
årstal_list <- str_extract_all(string, regex("[0-9]{4}+"))
årstal <- årstal_list[[1]]


#q2
# read the html file, and retrieve 1 line and regex it to a working link
raw_lines <- readLines("data/Q/Q2/edclinks.html", encoding = "UTF-8")
library(rvest)
html <- read_html(paste(raw_lines, collapse = "\n"))
links <- html %>% 
  html_nodes("meta[itemprop='url']") %>%
  html_attr("content") %>% 
  as.data.frame()

base_url <- "https://www.edc.dk"
paste0(base_url, links[1,1])

# make a loop and save in a list
full_links <- list()
for (link in links[[1]]) {
  tmp_link <- paste0(base_url,link)
  full_links <- append(full_links, tmp_link)
}


