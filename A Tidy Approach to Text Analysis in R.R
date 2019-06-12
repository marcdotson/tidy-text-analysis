# Tokenizing and Visualization --------------------------------------------
# Text, Characters, and Strings
library(tidyverse)
library(tidytext)

text <- c(
  "So long and thanks for all the fish,",
  "So sad that it should come to this,",
  "We tried to warn you all but oh dear!"
)

text
str(text)

text_df <- tibble(
  line = 1:3,
  text = text
)

text_df

# Tokenize
text_df %>% 
  unnest_tokens(word, text)

# Down the Rabbit Hole
library(gutenbergr)

tidy_carroll <- gutenberg_download(11) %>% 
  unnest_tokens(word, text)

tidy_carroll %>% 
  count(word) %>% 
  arrange(desc(n))

# Remove Stop Words
stop_words

tidy_carroll <- tidy_carroll %>%
  anti_join(stop_words)

tidy_carroll %>% 
  count(word) %>% 
  arrange(desc(n))

# Visualize Word Frequencies
tidy_carroll %>% 
  count(word) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(x = word, y = n)) +
  geom_col()

tidy_carroll %>% 
  count(word) %>% 
  mutate(word = reorder(word, n)) %>% 
  filter(n > 30) %>% 
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  coord_flip()

# Word Clouds
library(wordcloud)

tidy_carroll %>% 
  count(word) %>% 
  with(wordcloud(word, n, min.freq = 10))

# Exercise
tidy_carroll2 <- gutenberg_download(12) %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words)

tidy_carroll2 %>% 
  count(word) %>% 
  mutate(word = reorder(word, n)) %>% 
  filter(n > 30) %>% 
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  coord_flip()

tidy_carroll2 %>% 
  count(word) %>% 
  with(wordcloud(word, n, min.freq = 10))

# Sentiment Analysis ------------------------------------------------------
# Web Scraping
library(rvest)

text <- read_html(
  "https://en.wikipedia.org/wiki/Provo,_Utah"
  ) %>% 
  html_nodes("#content") %>% 
  html_text() %>% 
  str_split("\\\n\\\n\\\n") %>% 
  unlist()

# Tokenize, Tidy, and Visualize
tidy_text <- tibble(text) %>% 
  mutate(section = row_number()) %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words)

tidy_text %>% 
  count(word) %>% 
  mutate(word = reorder(word, n)) %>% 
  filter(n > 20) %>% 
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  coord_flip()

# Sentiment Dictionaries
get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")

get_sentiments("nrc") %>% 
  count(sentiment)

# Sentiment Analysis
sentiment_nrc <- tidy_text %>% 
  inner_join(get_sentiments("nrc"))

sentiment_nrc %>% 
  count(sentiment) %>% 
  arrange(desc(n))

sentiment_nrc %>%
  filter(sentiment == "joy") %>% 
  count(word) %>% 
  arrange(desc(n))

# Changing Sentiment
tidy_carroll <- gutenberg_download(11) %>% 
  mutate(line = row_number()) %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words)

tidy_carroll %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(index = line %/% 30, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative) %>% 
  ggplot(aes(x = index, y = sentiment)) +
  geom_col()

# Exercise
tidy_carroll2 <- gutenberg_download(12) %>% 
  mutate(line = row_number()) %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words)

tidy_carroll2 %>% 
  inner_join(get_sentiments("nrc")) %>% 
  count(sentiment) %>% 
  arrange(desc(n))

tidy_carroll2 %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(index = line %/% 30, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative) %>% 
  ggplot(aes(x = index, y = sentiment)) +
  geom_col()

# Topic Modeling ----------------------------------------------------------
# Word Frequencies
tidy_carroll <- gutenberg_download(c(11, 12)) %>% 
  unnest_tokens(word, text) %>%
  mutate(
    book = factor(
      gutenberg_id, 
      labels = c(
        "Alice's Adventures in Wonderland", 
        "Through the Looking-Glass"
      )
    )
  ) %>% 
  count(book, word) %>% 
  arrange(desc(n))

tidy_carroll

# Term Frequency-Inverse Document Frequency
tidy_carroll %>% 
  bind_tf_idf(word, book, n)

tidy_carroll <- tidy_carroll %>% 
  bind_tf_idf(word, book, n) %>% 
  arrange(desc(tf_idf))

tidy_carroll

# Visualize tf-idf by Document
tidy_carroll %>%
  group_by(book) %>% 
  top_n(10, tf_idf) %>% 
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(word, tf_idf, fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ book, scales = "free") +
  coord_flip()

# Create a Document Term Matrix
library(topicmodels)

roomba_650 <- read_csv("Roomba 650 Amazon Reviews.csv") %>% 
  mutate(review = row_number()) %>% 
  unnest_tokens(word, Review) %>% 
  anti_join(stop_words) %>% 
  select(review, word)

dtm_text <- roomba_650 %>% 
  count(review, word) %>% 
  cast_dtm(review, word, n)

# Run a Topic Model
lda_out <- dtm_text %>% 
  LDA(
    k = 2, 
    method = "Gibbs",
    control = list(seed = 42)
  )

# Topic Word Probabilities
lda_topics <- lda_out %>% 
  tidy(matrix = "beta")

lda_topics

# Visualize, Name, and Choose K
lda_topics %>%
  group_by(topic) %>% 
  top_n(15, beta) %>% 
  ungroup() %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

# Exercise
lda_out <- vector("list", length = 6)
for (i in seq_along(lda_out)) {
  # Run the topic model and save the output.
  lda_out[[i]] <- dtm_text %>% 
    LDA(
      k = i + 1, 
      method = "Gibbs",
      control = list(seed = 42)
    )
  # Visualize.
  lda_out[[i]] %>% 
    tidy(matrix = "beta") %>% 
    group_by(topic) %>% 
    top_n(15, beta) %>% 
    ungroup() %>%
    mutate(term = reorder(term, beta)) %>%
    ggplot(aes(term, beta, fill = as.factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    coord_flip()
}

