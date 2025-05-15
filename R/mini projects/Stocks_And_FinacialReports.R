library(quantmod)
library(readtext)
library(tidytext)
library(tidyverse)
library(SnowballC)
library(textstem)
library(lubridate)

#### Data retieval
# Set date range (optional)
start_date <- as.Date("2008-01-01")
end_date <- Sys.Date()

# template for making into df
# PNDORA.CO <- data.frame(Date = index(PNDORA.CO), coredata(PNDORA.CO))

# Get stock data
getSymbols("NVO", src = "yahoo", from = start_date, to = end_date)         # Novo Nordisk (listed on NYSE)
getSymbols("PNDORA.CO", src = "yahoo", from = start_date, to = end_date)   # Pandora (listed on Copenhagen Stock Exchange)

# Plot closing prices
chartSeries(NVO, theme = chartTheme("black"), name = "Novo Nordisk")
chartSeries(PNDORA.CO, theme = chartTheme("black"), name = "Pandora")


# Convert xts to data.frame with date column
nvo_df <- data.frame(Date = index(NVO), coredata(NVO))

# Extract year, quarter
nvo_df <- nvo_df %>%
  mutate(
    year = year(Date),
    quarter = quarter(Date),
    closing_price = NVO.Adjusted  # or NVO.Close if preferred
  )

# Get summary per year
nvo_summary <- nvo_df %>%
  group_by(year) %>%
  summarise(
    avg_price = mean(closing_price, na.rm = TRUE),
    sd_price = sd(closing_price, na.rm = TRUE),
    avg_q1 = mean(closing_price[quarter == 1], na.rm = TRUE),
    avg_q4 = mean(closing_price[quarter == 4], na.rm = TRUE),
    first_day = closing_price[which.min(Date)],
    last_day = closing_price[which.max(Date)]
  ) %>%
  arrange(year) %>%
  mutate(
    pct_change = (avg_price - lag(avg_price)) / lag(avg_price) * 100
  )



#### Annual reports
# Get AFINN
afinn <- get_sentiments("afinn")

# Create empty result df
sentiment_summary <- data.frame(
  year = integer(),
  avg_sentiment = numeric(),
  total_sentiment = numeric(),
  total_words = integer(),
  stringsAsFactors = FALSE
)

# Loop over years
for (year in 18:24) {
  path <- paste0("R/mini projects/Annual_reports/novo_letter_20", year, ".docx")
  df <- readtext(path)
  
  novo_df <- df %>%
    unnest_tokens(word, text) %>%
    filter(str_detect(word, "^[a-zA-Z]+$")) %>%
    anti_join(stop_words) %>%
    mutate(original = word,
           word = lemmatize_words(word)) %>%
    inner_join(afinn, by = "word")
  
  summary_row <- novo_df %>%
    summarise(
      year = 2000 + year,
      avg_sentiment = mean(value, na.rm = TRUE),
      total_sentiment = sum(value, na.rm = TRUE),
      total_words = n()
    )
  
  sentiment_summary <- bind_rows(sentiment_summary, summary_row)
}

# View results
sentiment_summary



# regression
combined_df <- inner_join(sentiment_summary, nvo_summary, by = "year")
model <- lm(last_day ~ avg_sentiment, data = combined_df)
summary(model)


max_price <- max(combined_df$avg_price, na.rm = TRUE)
max_sent <- max(abs(combined_df$avg_sentiment), na.rm = TRUE)
ggplot(combined_df, aes(x = year)) +
  geom_line(aes(y = avg_price), color = "blue", size = 1.2) +
  geom_line(aes(y = avg_sentiment * (max_price / max_sent)), color = "red", size = 1.2) +
  scale_y_continuous(
    name = "Avg Stock Price (Blue)",
    sec.axis = sec_axis(~ . / (max_price / max_sent), name = "Avg Sentiment (Red)")
  ) +
  labs(title = "Novo Nordisk: Sentiment vs. Stock Price",
       x = "Year") +
  theme_minimal()





# Wordcloud
library(wordcloud2)


# --- 2022 ---

# Read and process
novo_22 <- readtext("R/mini projects/Annual_reports/novo_letter_2022.docx")

novo_22_df <- novo_22 %>%
  unnest_tokens(word, text) %>%
  filter(str_detect(word, "^[a-zA-Z]+$")) %>%
  anti_join(stop_words) %>%
  mutate(word = lemmatize_words(word)) %>%
  inner_join(afinn, by = "word") %>%
  mutate(sentiment_label = case_when(
    value < 0 ~ "negative",
    value > 0 ~ "positive",
    TRUE ~ "neutral"
  )) %>%
  count(word, sentiment_label, sort = TRUE)

novo_22_raw <- novo_22 %>%
  unnest_tokens(word, text) %>%
  filter(str_detect(word, "^[a-zA-Z]+$")) %>%
  anti_join(stop_words) %>% 
  count(word, sort=T) %>% 
  mutate(total = sum(n),
         year = "2022",
         freq = n / total)

# Assign colors
novo_22_df$color <- case_when(
  novo_22_df$sentiment_label == "positive" ~ "green",
  novo_22_df$sentiment_label == "negative" ~ "red",
  TRUE ~ "grey"
)

# Plot
wordcloud2(data = novo_22_df[, c("word", "n", "color")],
           color = novo_22_df$color,
           backgroundColor = "black",
           size = 0.7)

# --- 2018 ---

# Read and process
novo_18 <- readtext("R/mini projects/Annual_reports/novo_letter_2018.docx")

novo_18_raw <- novo_18 %>%
  unnest_tokens(word, text) %>%
  filter(str_detect(word, "^[a-zA-Z]+$")) %>%
  anti_join(stop_words) %>% 
  count(word,sort = T) %>%
  mutate(total = sum(n),
         year = "2018",
         freq = n / total)


novo_18_df <- novo_18 %>%
  unnest_tokens(word, text) %>%
  filter(str_detect(word, "^[a-zA-Z]+$")) %>%
  anti_join(stop_words) %>%
  mutate(word = lemmatize_words(word)) %>%
  inner_join(afinn, by = "word") %>%
  mutate(sentiment_label = case_when(
    value < 0 ~ "negative",
    value > 0 ~ "positive",
    TRUE ~ "neutral"
  )) %>%
  count(word, sentiment_label, sort = TRUE)

# Assign colors
novo_18_df$color <- case_when(
  novo_18_df$sentiment_label == "positive" ~ "green",
  novo_18_df$sentiment_label == "negative" ~ "red",
  TRUE ~ "grey"
)

# Plot
wordcloud2(data = novo_18_df[, c("word", "n", "color")],
           color = novo_18_df$color,
           backgroundColor = "black",
           size = 0.7)




word_freqs <- bind_rows(novo_22_raw, novo_18_raw)

novo_22_raw %>%
  count(word, sort = TRUE) %>%
  slice_max(n, n = 10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = n, y = word)) +
  geom_col(fill = "steelblue") +
  labs(title = "Top 10 Words – 2022", x = "Count", y = NULL) +
  theme_minimal()


# --- Plot for 2018 ---
novo_18_raw %>%
  count(word, sort = TRUE) %>%
  slice_max(n, n = 10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col(fill = "darkred") +
  labs(title = "Top 10 Words – 2018", y = NULL, x = "Frequency") +
  theme_minimal()

