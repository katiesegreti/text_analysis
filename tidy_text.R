###TIDY TEXT MINING ##########
### Chapter 1 ################
##############################
library(dplyr)
library(tidyr)
library(broom)
library(ggplot2)
library(tidytext)

# 1.2 The unnest_tokens function
# Emily Dickinson wrote some lovely text in her time.

text <- c("Because I could not stop for Death -",
          "He kindly stopped for me -",
          "The Carriage held but just Ourselves -",
          "and Immortality")

text
# This is a typical character vector that we might want to analyze. 
# In order to turn it into a tidy text dataset, we first need 
# to put it into a data frame.
text_df <- tibble(line = 1:4, text = text)
# A tibble is a modern class of data frame within R, available in 
# the dplyr and tibble packages, that has a convenient 
# print method, will not convert strings to factors, and does not use row names.
text_df

# We need to convert this so that it has one-token-per-document-per-row.
# To do this, we use tidytext’s unnest_tokens()
text_df %>%
  unnest_tokens(word, text)

# 1.3 Tidying the works of Jane Austen
library(janeaustenr)
library(stringr)

original_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>%
  ungroup()

original_books

# restructure in one-token-per-row format
tidy_books <- original_books %>%
  unnest_tokens(word, text)

tidy_books

# We can remove stop words (kept in the tidytext dataset stop_words) with an anti_join().
data(stop_words)

tidy_books <- tidy_books %>%
  anti_join(stop_words)

# We can also use dplyr’s count() to find the most common words in all the books as a whole.
tidy_books %>%
  count(word, sort = TRUE) 

tidy_books %>%
  count(word, sort = TRUE) %>%
  filter(n > 600) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill = "turquoise") +
  xlab(NULL) +
  coord_flip()

# The gutenbergr package provides access to the public domain works from the Project 
# Gutenberg collection. The package includes tools both for downloading books (stripping 
# out the unhelpful header/footer information), 
# and a complete dataset of Project Gutenberg metadata that can be used to find works of interest.

library(gutenbergr)

hgwells <- gutenberg_download(c(35, 36, 5230, 159))

tidy_hgwells <- hgwells %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_hgwells %>%
  count(word, sort = TRUE)

# Bronte sisters
bronte <- gutenberg_download(c(1260, 768, 969, 9182, 767))
tidy_bronte <- bronte %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_bronte %>%
  count(word, sort = TRUE)

# Now, let’s calculate the frequency for each word for the works of Jane Austen, 
# the Brontë sisters, and H.G. Wells by binding the data frames together. We can 
# use spread and gather from tidyr to reshape our dataframe so that it is just 
# what we need for plotting and comparing the three sets of novels.
frequency <- bind_rows(mutate(tidy_bronte, author = "Brontë Sisters"),
                       mutate(tidy_hgwells, author = "H.G. Wells"), 
                       mutate(tidy_books, author = "Jane Austen")) %>% 
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  spread(author, proportion) %>% 
  gather(author, proportion, `Brontë Sisters`:`H.G. Wells`)

how <- bind_rows(mutate(tidy_bronte, author = "Brontë Sisters"),
                       mutate(tidy_hgwells, author = "H.G. Wells"), 
                       mutate(tidy_books, author = "Jane Austen"))  %>% 
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n / sum(n)) %>% 
  #select(-n) %>% 
  spread(author, proportion) 

library(scales)

# expect a warning about rows with missing values being removed
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
  
# Let’s quantify how similar and different these sets of word frequencies are using 
# a correlation test. How correlated are the word frequencies between Austen and the 
# Brontë sisters, and between Austen and Wells?
cor.test(data = frequency[frequency$author == "Brontë Sisters",],
         ~ proportion + `Jane Austen`)

cor.test(data = frequency[frequency$author == "H.G. Wells",], 
         ~ proportion + `Jane Austen`)

## SUMMARY ##
# In this chapter, we explored what we mean by tidy data when it comes to text, and 
# how tidy data principles can be applied to natural language processing. When text 
# is organized in a format with one token per row, tasks like removing stop words 
# or calculating word frequencies are natural applications of familiar operations 
# within the tidy tool ecosystem. The one-token-per-row framework can be extended 
# from single words to n-grams and other meaningful units of text, as well as to many 
# other analysis priorities that we will consider in this book.

gm <- gutenberg_metadata 
sotu <- gm %>%
  filter(str_detect(title, "State of the Union"))

# Obama sotu
obama <- gutenberg_download(50950)

tidy_obama <- obama %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_obama %>%
  count(word, sort = TRUE)

# Obama sotu
obama <- gutenberg_download(50950)

tidy_obama <- obama %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_obama %>%
  count(word, sort = TRUE)

gwb <- gutenberg_download(5049)

tidy_gwb <- gwb %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_gwb %>%
  count(word, sort = TRUE)

clinton <- gutenberg_download(5048)

tidy_clinton <- clinton %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_clinton %>%
  count(word, sort = TRUE)

ghwb <- gutenberg_download(5047)

tidy_ghwb <- ghwb %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_ghwb %>%
  count(word, sort = TRUE)

reagan <- gutenberg_download(5046)

tidy_reagan <- reagan %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_reagan %>%
  count(word, sort = TRUE)

carter <- gutenberg_download(5045)

tidy_carter <- carter %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_carter %>%
  count(word, sort = TRUE)

ford <- gutenberg_download(5044)

tidy_ford <- ford %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_ford %>%
  count(word, sort = TRUE)
