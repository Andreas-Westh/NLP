#https://github.com/ccs-amsterdam/r-course-material/blob/master/tutorials/R_text_3_quanteda.md
library(tidyverse)
library(quanteda)
library(quanteda.textplots)
library(quanteda.textstats)
# step 1 import text and make corpud ----
url <- 'https://bit.ly/2QoqUQS'
US_speeches <- read_csv(url)
head(US_speeches)


# create corpus - data structure made for NLP
corpus <- corpus(US_speeches, text_field = "text")
corpus

# step 2 creating the dtm (dfm) ----
# create a document term matrix (in this case document-feature)
SW <- c(stopwords("en"),"will")
corpus_clean <- corpus |>
  tokens(remove_punct = T, remove_numbers = T, remove_symbols = T) |>   ## tokenize, removing unnecessary noise
  tokens_tolower() |>                                                   ## normalize
  tokens_remove(SW) |>                                     ## remove stopwords (English)
  tokens_wordstem()                                                      ## stemming
dtm <- dfm(corpus_clean)
dtm # features = unqiue words, so with 16k a high sparse makes sense

dtm <- dfm_trim(dtm, min_termfreq = 10)
dtm # dowjn to 5000 words, thats been used more than 10



# step 3 analysis ----
textplot_wordcloud(dtm, max_words = 50)                          ## top 50 (most frequent) words
textplot_wordcloud(dtm, max_words = 50, color = c('blue','red')) ## change colors
textstat_frequency(dtm, n = 10)                                  ## view the frequencies 


is_obama <- docvars(dtm)$President == 'Barack Obama' # T of F
obama_dtm <- dtm[is_obama,] # selects all the rows where T
textplot_wordcloud(obama_dtm, max_words = 50)

##### Compare corpora #####
    # relative freq, what words are unique to obamma 
keyness <- textstat_keyness(dtm, is_obama) # 1. is the full text, 2. is the subset to compare to
head(keyness, 20)
    # n_target is in the subset, n_reference is all others combined
tail(keyness, 20)
    # a negative chi2 is an under representation

textplot_keyness(keyness)

##### keyword in context #####
# remember to use corpus, since in the dtm we have dropped the 'context'
kwic <- kwic(tokens(corpus), "freedom", window = 7) # window is max words tothe left and right
head(kwic,10)

##### Make a new corpus/dtm out of a keyword and context #####
terror_kwic <- kwic(tokens(corpus), "terror*") # * is a wild, so it can be like terrorism too and so on
terror_corpus <- corpus(terror_kwic)
terror_dtm <- terror_corpus |>
  tokens(remove_punct = T, remove_numbers = T, remove_symbols = T) |>
  tokens_tolower() |>
  tokens_remove(SW) |>
  tokens_wordstem() |>
  dfm()

textplot_wordcloud(terror_dtm, max_words = 50) 


##### Dictionary search #####
# simply a list of words to look for
dict <- dictionary(list(terrorism = 'terror*',
                        economy = c('econom*', 'tax*', 'job*'),
                        military = c('army','navy','military','airforce','soldier'),
                        freedom = c('freedom','liberty')))
dict_dtm <- dfm_lookup(dtm, dict, exclusive=TRUE) # takes original dtm, subsets with dictionary
dict_dtm 

# shows which of the defined terms is most used
textplot_wordcloud(dict_dtm)


###### best practice for verifying a valid dictionary ######
kwic(tokens(corpus), dict$terrorism, window = 5)
# here we can varify, that terror is a valid search term for terrorism within these speeches at least
