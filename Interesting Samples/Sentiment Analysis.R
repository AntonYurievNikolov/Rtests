library(tidytext)
library(dplyr)
library(tidyr)
library(ggplot2)

#tokenization

tidy_shakespeare <- shakespeare %>%
  group_by(title) %>%
  mutate(linenumber = row_number() ) %>%
  # Transform the non-tidy text data to tidy text data
  unnest_tokens(word, text) %>%
  ungroup()

shakespeare_sentiment <- tidy_shakespeare %>%
  inner_join(get_sentiments("bing")) 

shakespeare_sentiment %>%
  count(title, sentiment)

#through a text
tidy_shakespeare %>%
  inner_join(get_sentiments("bing")) %>%
  count(title, type, index = linenumber %/% 70, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>%
  ggplot(aes(x = index,y=sentiment, fill = type)) +
  geom_col() +
  facet_wrap(~title, scales = "free_x")

#stop words - anti join
#sentiment over time
# Load the lubridate package
library(lubridate)

sentiment_by_time <- tidy_tv %>%
  # Define a new column using floor_date()
  mutate(date = floor_date(show_date, unit = "6 months")) %>%
  # Group by date
  group_by(date) %>%
  mutate(total_words = n()) %>%
  ungroup() %>%
  # Implement sentiment analysis using the NRC lexicon
  inner_join(get_sentiments("nrc")) 

sentiment_by_time %>%
  # Filter for positive and negative words
  filter(sentiment %in% c("negative","positive") ) %>%
  # Count by date, sentiment, and total_words
  count(date,sentiment,total_words) %>%
  ungroup() %>%
  mutate(percent = n / total_words) %>%
  # Set up the plot with aes()
  ggplot(aes(x = date, y = percent, col = sentiment) ) +
  geom_line(size = 1.5) +
  geom_smooth(method = "lm", se = FALSE, lty = 2) +
  expand_limits(y = 0)


#