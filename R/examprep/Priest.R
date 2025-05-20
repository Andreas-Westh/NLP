library(wordcloud2)
library(rvest)
library(tidytext)
library(Sentida)
library(stopwords)
library(tidyverse)


# FORMÅL
# Hvilken præst er
# 1) mindst konservativ
# 2) mest positiv
# 3) bruger flest kristne termer

# loop
url="https://www.dansketaler.dk/praedikener/tale/kathrine-lilleoers-praediken-juledag-2018"
url="https://www.dansketaler.dk/praedikener/tale/kirsten-jorgensens-praediken-juledag"
url="https://www.dansketaler.dk/praedikener/tale/nikolaj-hartung-kjaerbys-praediken-juledag"
url="https://www.dansketaler.dk/praedikener/tale/alex-vestergaard-nielsens-praediken-2-juledag"
url="https://www.dansketaler.dk/praedikener/tale/henrik-hojlunds-praediken-juledag"
url="https://www.dansketaler.dk/praedikener/tale/peter-sanders-praediken-2-juledag"
url="https://www.dansketaler.dk/praedikener/tale/jesper-stanges-praediken-2-juledag"
url="https://www.dansketaler.dk/praedikener/tale/lisbet-kjaer-mullers-praediken-2-juledag"

# MAKE REGEX to get names

urls=c(url_alex_vestergaard_nielsens_praediken,url_henrik_hojlunds,url_jesper_stanges_praediken,
       url_kathrine_lilleoers_praediken_juledag, url_kirsten_jorgensens_praediken_juledag, url_lisbet_kjaer_mullers_praediken,
       url_nikolaj_hartung_kjaerbys_praediken_juledag, url_peter_sanders_praediken)

all_texts <- data_frame(url=NULL,text=NULL)
for (url in urls) {
  print(url)
  mtest=read_html(url)
  tagfortale=".speech-article-content"
  tale=mtest %>% html_node(tagfortale) %>% html_text()
  tale_df <- data_frame(url=url,text=tale)
  all_texts <- rbind(all_texts,tale_df)
}

# question from teach
# make an empty list with each url
tom <- list()
for (i in 1:length(urls)){
  tom[i] = urls[i]
  print(urls[i])
}
class(tom) # check its class

# question from teach
# make a df with top 10 used words from each priest in as few lines

all_texts$præst <- str_extract(all_texts$url, "(?<=tale/).*")
all_texts$præst <- str_extract(all_texts$præst, "^.*(?=praediken)")
all_texts$præst <- gsub("-", " ", all_texts$præst)
all_texts$præst <- trimws(all_texts$præst)
tale_tk_w=unnest_tokens(tbl=all_texts,output = "text",input = text, token = "words",to_lower = T)
tale_tk_w_sub= tale_tk_w %>% filter(!text %in% dkstop)

top <- tale_tk_w_sub %>% 
  group_by(præst) %>% count(text) %>% 
  top_n(500)
df <- split(top, top$præst)

common_all <- reduce(df, inner_join, by = "text")


# FOR HVER TALE....
# hent talen vha rvest
mtest=read_html(url)
tagfortale=".speech-article-content"
tale=mtest %>% html_node(tagfortale) %>% html_text() 

tale <- as.data.frame(tale)

# tokenize på ord og sætninger
tale_tk_w=unnest_tokens(tbl=tale,output = "text",input = tale, token = "words",to_lower = T)

tale_tk_s=unnest_tokens(tbl=tale,output = "text",input = tale, token = "sentences")
  
  
  
  
  # count og wordcloud
tale_w_c <- tale_tk_w %>% count(text)
wordcloud2(data = tale_w_c, size = 0.5)




  # fjerne danske stopord
  dkstop=stopwords(language = "da")
tale_tk_w_sub= tale_tk_w %>% filter(!text %in% dkstop)
  # lav en optælling af ord
  tale_tk_w_sub_ct= tale_tk_w_sub %>% count(text, sort = T)
  tale_tk_w_sub_ct <- tale_tk_w_sub_ct %>% rename(word=text,freq = n) 
  
  # Forbered data til wordcloud2
  #colnames(tale_tk_w_sub_ct)=
  
  # Lav wordcloud med en given frekvens som nedre grænse  
  wordcloud2(data = tale_tk_w_sub_ct, size = 0.4)


# Fjer mindst ét ord fra stopordene som du gerne vil ha' med i wordclouden
  dkstop <- dkstop[!dkstop == "han"]
# Tilføj stopord ud fra wordclouden
  dkstop <- append(dkstop,"så")

  
  # Find alle ord der starter med stort
  test <- tale_tk_w %>% count(text, sort = T) %>% 
  filter(str_detect(text, "^[A-ZÆØÅ]"))
  
  better_words <- tale_tk_w %>% filter(!text %in% dkstop) %>% 
    count(text, sort = T) %>% rename(word=text,freq = n) 
  wordcloud2(data = better_words, size = 0.4)
  
  
  
  
# pulje sætninger sammen så det giver mening at køre en Sentida-score på tekstn
  tale_tk_s$sentiment= tale_tk_s %>% rowwise() %>% 
    mutate(score = round(sentida(text, output = "mean"),1)) 
  
  # tæl hvor mange kristne begreber der indgår
  kristne_begraber = c("gud","guds","kristus","synd","synde","engel")
  kristine_df <- tale_tk_w %>% filter(text %in% kristne_begraber) 
  kristine_df$priest = "lisbet-kjaer-mullers"

  
  mean_sent <- taler_tk_s %>% group_by(præst) %>% mutate(mean = mean(score))
  mean_grouped <- mean_sent %>% select(præst,mean) %>% group_by(præst) %>% distinct()
  
  
  
  
  #total_real_priests <- data_frame(text=NA,priest=NA)
  total_real_priests <- rbind(total_real_priests,kristine_df)
  priest_count <- total_real_priests %>% group_by(priest) %>% 
    count(text) %>% mutate(total = n) %>% ungroup()
  
  gud <- priest_count %>% filter(text == "gud" | text == "guds")
  
  priest_count <- total_real_priests %>% count(priest, text) %>% 
    group_by(priest) %>% mutate(total = sum(n)) %>% 
    ungroup()
  
  # SAML resultaterne i én dataframe så præsterne kan sammenlignes
  priest_total <- priest_count %>% select(priest, total) %>% distinct(priest, .keep_all = T)
  
  
  