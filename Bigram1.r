#http://uc-r.github.io/creating-text-features

# package required
library(dplyr)
library(tidyverse)
library(tidytext)
library(data.table)

# import data and do some initial cleaning
df <- data.table::fread("D:\\Users\\Lakshmi Narasimhan\\Data\\womens-ecommerce-clothing-reviews\\Womens Clothing E-Commerce Reviews.csv", data.table = FALSE) %>%
  rename(ID = V1) %>%
  select(-Title) %>%
  mutate(Age = as.integer(Age))

glimpse(df)


df %>%
  select(`Review Text`) %>%
  unnest_tokens(word, `Review Text`) %>%
  count(word, sort = TRUE)

df %>%
  unnest_tokens(word, `Review Text`) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)

df %>%
  unnest_tokens(word, `Review Text`) %>%
  anti_join(stop_words) %>%
  count(word) %>%
  ggplot(aes(n)) +
  geom_histogram() +
  scale_x_log10()

df %>%
  unnest_tokens(word, `Review Text`) %>%
  anti_join(stop_words) %>%
  count(word) %>%
  arrange(n)

df %>%
  unnest_tokens(word, `Review Text`) %>%
  anti_join(stop_words) %>%
  filter(
    !str_detect(word, pattern = "[[:digit:]]"), # removes any words with numeric digits
    !str_detect(word, pattern = "[[:punct:]]"), # removes any remaining punctuations
    !str_detect(word, pattern = "(.)\\1{2,}"),  # removes any words with 3 or more repeated letters
    !str_detect(word, pattern = "\\b(.)\\b")    # removes any remaining single letter words
  ) %>%
  count(word) %>%
  arrange(n)

df %>%
  unnest_tokens(word, `Review Text`) %>%
  anti_join(stop_words) %>%
  filter(
    !str_detect(word, pattern = "[[:digit:]]"), # removes any words with numeric digits
    !str_detect(word, pattern = "[[:punct:]]"), # removes any remaining punctuations
    !str_detect(word, pattern = "(.)\\1{2,}"),  # removes any words with 3 or more repeated letters
    !str_detect(word, pattern = "\\b(.)\\b")    # removes any remaining single letter words
  ) %>%
  count(word) %>%
  filter(n >= 10) %>% # filter for words used 10 or more times
  arrange(n)

df %>%
  unnest_tokens(word, `Review Text`) %>%
  anti_join(stop_words) %>%
  filter(
    !str_detect(word, pattern = "[[:digit:]]"), # removes any words with numeric digits
    !str_detect(word, pattern = "[[:punct:]]"), # removes any remaining punctuations
    !str_detect(word, pattern = "(.)\\1{2,}"),  # removes any words with 3 or more repeated letters
    !str_detect(word, pattern = "\\b(.)\\b")    # removes any remaining single letter words
  ) %>%
  count(word) %>%
  mutate(word = if_else(n < 10, "infrequent", word)) %>% # categorize infrequent words
  group_by(word) %>%
  summarize(n = sum(n)) %>%
  arrange(desc(n))

library(corpus)

df %>%
  unnest_tokens(word, `Review Text`) %>%
  anti_join(stop_words) %>%
  filter(
    !str_detect(word, pattern = "[[:digit:]]"), 
    !str_detect(word, pattern = "[[:punct:]]"),
    !str_detect(word, pattern = "(.)\\1{2,}"),  
    !str_detect(word, pattern = "\\b(.)\\b")    
  ) %>%
  mutate(word = corpus::text_tokens(word, stemmer = "en") %>% unlist()) %>% # add stemming process
  count(word) %>% 
  group_by(word) %>%
  summarize(n = sum(n)) %>%
  arrange(desc(n))

# create a vector of all words to keep
word_list <- df %>%
  unnest_tokens(word, `Review Text`) %>%
  anti_join(stop_words) %>%
  filter(
    !str_detect(word, pattern = "[[:digit:]]"), # removes any words with numeric digits
    !str_detect(word, pattern = "[[:punct:]]"), # removes any remaining punctuations
    !str_detect(word, pattern = "(.)\\1{2,}"),  # removes any words with 3 or more repeated letters
    !str_detect(word, pattern = "\\b(.)\\b")    # removes any remaining single letter words
  ) %>%
  count(word) %>%
  filter(n >= 10) %>% # filter for words used 10 or more times
  pull(word)

# create new features
bow_features <- df %>%
  unnest_tokens(word, `Review Text`) %>%
  anti_join(stop_words) %>%
  filter(word %in% word_list) %>%     # filter for only words in the wordlist
  count(ID, word) %>%                 # count word useage by customer ID
  spread(word, n) %>%                 # convert to wide format
  map_df(replace_na, 0)               # replace NAs with 0

bow_features


# join original data and new feature set together
df_bow <- df %>%
  inner_join(bow_features, by = "ID") %>%   # join data sets
  select(-`Review Text`)                    # remove original review text

# dimension of our new data set
dim(df_bow)
## [1] 22640  2839

as_tibble(df_bow)

# create bigrams
df %>%
  unnest_tokens(bigram, `Review Text`, token = "ngrams", n = 2) %>%
  head()

# create a vector of all bi-grams to keep 
ngram_list <- df %>%
  unnest_tokens(bigram, `Review Text`, token = "ngrams", n = 2) %>%  
  separate(bigram, c("word1", "word2"), sep = " ") %>%               
  filter(
    !word1 %in% stop_words$word,                 # remove stopwords from both words in bi-gram
    !word2 %in% stop_words$word,
    !str_detect(word1, pattern = "[[:digit:]]"), # removes any words with numeric digits
    !str_detect(word2, pattern = "[[:digit:]]"),
    !str_detect(word1, pattern = "[[:punct:]]"), # removes any remaining punctuations
    !str_detect(word2, pattern = "[[:punct:]]"),
    !str_detect(word1, pattern = "(.)\\1{2,}"),  # removes any words with 3 or more repeated letters
    !str_detect(word2, pattern = "(.)\\1{2,}"),
    !str_detect(word1, pattern = "\\b(.)\\b"),   # removes any remaining single letter words
    !str_detect(word1, pattern = "\\b(.)\\b")
  ) %>%
  unite("bigram", c(word1, word2), sep = " ") %>%
  count(bigram) %>%
  filter(n >= 10) %>% # filter for bi-grams used 10 or more times
  pull(bigram)

# sneak peek at our bi-gram list
head(ngram_list)

# create new bi-gram features
ngram_features <- df %>%
  unnest_tokens(bigram, `Review Text`, token = "ngrams", n = 2) %>%
  filter(bigram %in% ngram_list) %>%    # filter for only bi-grams in the ngram_list
  count(ID, bigram) %>%                 # count bi-gram useage by customer ID
  spread(bigram, n) %>%                 # convert to wide format
  map_df(replace_na, 0)                 # replace NAs with 0

ngram_features

# create bi-grams and clean them up.
bigrams <- df %>%
  select(`Review Text`) %>%
  unnest_tokens(bigram, `Review Text`, token = "ngrams", n = 2) %>%
  filter(bigram %in% ngram_list) %>%
  separate(bigram, c("word1", "word2"), sep = " ")  

# sneak peak
head(bigrams)

# compute counts for word 1 & 2 independently
count_w1 <- bigrams %>%
  count(word1)

count_w2 <- bigrams %>%
  count(word2)

# compute counts for bi-grams
count_w12 <- bigrams %>%
  count(word1, word2)

# get the original number of all bi-grams
N <- nrow(bigrams)

# join this information and compute log-likelihood
LL_test <- count_w12 %>%
  left_join(count_w1, by = "word1") %>%
  left_join(count_w2, by = "word2") %>%
  rename(c_w1 = n.y, c_w2 = n, c_w12 = n.x) %>%
  mutate(
    p = c_w2 / N,
    p1 = c_w12 / c_w1,
    p2 = (c_w2 - c_w12) / (N - c_w1),
    LL = log((pbinom(c_w12, c_w1, p) * pbinom(c_w2 - c_w12, N - c_w1, p)) / (pbinom(c_w12, c_w1, p1) * pbinom(c_w2 - c_w12, N - c_w1, p)))
  )
head(LL_test)

# 
unique_bigrams <- LL_test %>%
  mutate(
    Chi_value = -2 * LL,
    pvalue = pchisq(LL, df = 1)
  ) %>%
  filter(pvalue < 0.05) %>%
  select(word1, word2) %>%
  unite(bigram, word1, word2, sep = " ")

head(unique_bigrams)
