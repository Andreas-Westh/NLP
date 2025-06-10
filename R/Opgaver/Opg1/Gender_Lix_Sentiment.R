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
library(igraph)
library(ggraph)
library(tidyr)

# get reviews and male names
all_reviews <- readRDS("R/Opgaver/Opg1/elgiganten.rds")
male_names <- read.csv("R/Opgaver/Opg1/drengenavne.csv", header = F)
colnames(male_names) = "name"
male_names$gender = "Male"

# subset if it takes too long idk


# make length variable
all_reviews_gender <- all_reviews %>% 
  mutate(first_name = str_extract(name, "^[A-ZÆØÅa-zæøå]+")) %>% 
  rowwise() %>% 
  mutate(gender = ifelse(first_name %in% male_names$name,"M","U"))
  
# sentiment
gender_sentiment <- all_reviews_gender %>% 
  rowwise() %>% 
  mutate(length = nchar(content),
         score = sentida(content, output = "mean")) %>% 
  arrange(desc(length)) 

# lix score
lix <- function(content){
  words = unlist(str_split(content, " "))
  lix_score = mean(nchar(words))
  return(lix_score)
}  

gender_sentiment <- gender_sentiment %>%
  rowwise() %>% 
  mutate(lix = lix(content))

  

# plot
plot_data <- gender_sentiment %>%
  group_by(gender) %>%
  summarise(mean_lix = mean(lix, na.rm = TRUE),
            mean_sentiment = mean(score, na.rm = TRUE))

ggplot(plot_data) +
  geom_bar(aes(x = "Lix", y = mean_lix, fill = gender), 
           stat = "identity", position = "dodge") +
  geom_bar(aes(x = "Sentiment", y = mean_sentiment, fill = gender), 
           stat = "identity", position = "dodge") +
  labs(x = NULL, y = "Mean Value", fill = "Gender") +
  theme_minimal()



glm <- glm(as.factor(gender) ~ score + lix, data = gender_sentiment, family = "binomial")
summary(glm)
#Coefficients:
#           Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  0.21636    0.06356   3.404 0.000664 ***
#score        0.16702    0.01433  11.653  < 2e-16 ***
#lix         -0.07468    0.01319  -5.661  1.5e-08 ***

lm_score <- lm(score~as.factor(gender), data = gender_sentiment)
summary(lm_score)
#Coefficients:
#                  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)        0.416585   0.005032   82.79   <2e-16 ***
#as.factor(gender)U 0.077702   0.007243   10.73   <2e-16 ***

lm_lix <- lm(lix~as.factor(gender), data = gender_sentiment)
summary(lm_lix)
#Coefficients:
#                   Estimate Std. Error t value Pr(>|t|)    
#(Intercept)         4.862377   0.005491 885.490  < 2e-16 ***
#as.factor(gender)U -0.024994   0.007905  -3.162  0.00157 ** 

