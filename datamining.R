rm(list=ls())
library(dplyr)
library(tidytext)
library(janeaustenr)
library(stringr)
library(gutenbergr)
library(tidyr)
library(ggplot2)
library(scales)
library(ggraph)
library(igraph)
data("stop_words")

# text <- c("Because I could not stop for Death -",
#           "He kindly stopped for me -",
#           "The Carriage held but just Ourselves -",
#           "and Immortality")
# 
# text
# text_df <- as.data.frame(text)
# text_df <- data_frame(line = 5:6, text)
# 
# text_df %>%
#   unnest_tokens(word,text)

original_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>%
  ungroup()

tidy_books <- original_books %>%
  unnest_tokens(word, text)

tidy_books <- tidy_books %>%
  anti_join(stop_words)

tidy_books %>% count(word, sort = TRUE)

tidy_books %>%
  count(word, sort = TRUE) %>%
  filter(n > 600) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab('NULL') +
  coord_flip()

hgwells <- gutenberg_download(c(35,36,5230,159))

tidy_hgwells <- hgwells %>%
  unnest_tokens(word,text) %>%
  anti_join(stop_words)

tidy_hgwells %>% count(word, sort = TRUE) %>%
    mutate(word = reorder(word, n)) %>%
    filter(n > 200) %>%
    ggplot(aes(word, n)) +
    geom_col() +
    xlab('NULL') +
    coord_flip()

tidy_hgwells %>% count(word, sort = T)

bronte <- gutenberg_download(c(1260,768,969,9182, 767))

tidy_bronte <- bronte %>% unnest_tokens(word,text) %>% anti_join(stop_words)

tidy_bronte %>% count(word,sort=T)

frequency <- bind_rows(mutate(tidy_bronte, author = "Bronte Sisters"),
                       mutate(tidy_hgwells, author = "H. G. Wells"), 
                       mutate(tidy_books, author = "Jane Austen")) %>% 
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  spread(author, proportion) %>% 
  gather(author, proportion, `Bronte Sisters`:`H. G. Wells`)


ggplot(frequency, aes(x = proportion, y = `Jane Austen`, color = abs(`Jane Austen` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  facet_wrap(~author, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "Jane Austen", x = NULL)

abdulbahabooks <- gutenberg_download(c(19237, 19238, 19239, 19250, 19279, 19284, 19285, 19287, 19289, 19292, 19296, 19299, 19300, 19312))
tidy_abdulbahabooks <- abdulbahabooks %>% unnest_tokens(word, text) %>% anti_join(stop_words)
tidy_abdulbahabooks %>% count(word, sort = TRUE) %>%
  mutate(word = reorder(word, n)) %>%
  filter(n > 500) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab('NULL') +
  coord_flip()

abdulbaha_bigrams <- abdulbahabooks %>% unnest_tokens(bigram, text, token = 'ngrams', n=2) #%>% anti_join(stop_words)
abdulbaha_bigrams %>% count(bigram, sort = TRUE)
bigrams_separated <- abdulbaha_bigrams %>% separate(bigram, c('word1','word2'), sep=' ')
bigrams_filtered <- bigrams_separated %>% 
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) 
bigram_united <- bigrams_filtered %>% unite(bigram, word1, word2, sep=' ')
bigram_united %>% count(bigram, sort = TRUE)%>%
  mutate(word = reorder(bigram, n)) %>%
  filter(n > 50) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab('NULL') +
  coord_flip()

bigram_graph <- bigrams_filtered %>% count(word1, word2, sort = TRUE)%>%
  #mutate(word = reorder(bigram, n)) %>%
  filter(n > 50) %>%
  graph_from_data_frame()

set.seed(2017)
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)
