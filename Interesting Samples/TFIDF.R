install.packages("janeaustenr")
install.packages("tidytext")
library(janeaustenr)
library(tidytext)
library(tidyverse)

book_words <- austen_books() %>%
  unnest_tokens(word, text) %>%
  count(book, word, sort = TRUE) %>%
  ungroup()

total_words <- book_words %>% 
  group_by(book) %>% 
  summarise(total = sum(n))

book_words <- left_join(book_words, total_words)

book_words


library(ggplot2)

ggplot(book_words, aes(n/total, fill = book)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~book, ncol = 2, scales = "free_y")