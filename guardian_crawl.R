library(guardianapi)
library(tidyverse)

#key = 2aace634-35f7-4064-b0a3-536c2cba6b21

gu_api_key(check_env = FALSE)


# 1400 articles
monbiot_articles <-
  gu_items(
    query = "profile/georgemonbiot",
    show_fields = "all",
    show_tags = "all",
    tag = NULL,
    from_date = "1994-01-01",
    to_date = "2022-03-31",
    use_date = "published",
    verbose = TRUE,
    tidy = TRUE,
    tidy_style = "snake_case"
  )

# philippa = 100 articles

philippa_articles <-
  gu_items(
    query = "profile/philippa-perry",
    show_fields = "all",
    show_tags = "all",
    tag = NULL,
    from_date = "2010-01-01",
    to_date = "2022-03-31",
    use_date = "published",
    verbose = TRUE,
    tidy = TRUE,
    tidy_style = "snake_case"
  )

# should be around 813 articles

mariella_articles <-
  gu_content(
    query = NULL,
    show_fields = "all",
    show_tags = "all",
    tag = "lifeandstyle/dear-mariella-relationships",
    from_date = "2000-01-01",
    to_date = "2022-03-31",
    #use_date = "published",
    verbose = TRUE,
    tidy = TRUE,
    tidy_style = "snake_case"
  )


mariella_articles <-
  gu_items(
    query = "profile/mariellafrostrup",
    show_fields = "all",
    show_tags = "all",
    #tag = "lifeandstyle/series/dearmariella",
    from_date = "2000-01-01",
    to_date = "2022-03-31",
    #use_date = "published",
    verbose = TRUE,
    tidy = TRUE,
    tidy_style = "snake_case"
  )


# annalisa, ask annalisa = 675 articles

annalisa_articles2 <- gu_content(
  query = NULL,
  show_fields = "all",
  show_tags = "series",
  tag = "lifeandstyle/series/ask-annalisa-barbieri",
  from_date = "2000-01-01",
  to_date = "2022-03-31",
  use_date = "published",
  verbose = TRUE,
  tidy = TRUE,
  tidy_style = "snake_case"
)

# pamela, sexual healing = 727 articles

pamela <- gu_content(
  query = NULL,
  show_fields = "all",
  show_tags = "series",
  tag = "lifeandstyle/series/sexualhealing",
  from_date = "2000-01-01",
  to_date = "2022-03-31",
  use_date = "published",
  verbose = TRUE,
  tidy = TRUE,
  tidy_style = "snake_case"
)


################################

install.packages("tidytext")
install.packages("textdata")
install.packages("topicmodels")
library(tidytext)
library(textdata)
library(topicmodels)

sentiments_afinn <- get_sentiments("afinn")
sentiments_bing <- get_sentiments("bing")
senrtiments_nrc <- get_sentiments("nrc")

pamela_text <- pamela %>% 
  select(web_publication_date, body_text, web_title) %>% 
  rowid_to_column() %>% 
  tidytext::unnest_tokens(word, body_text)

pamela_text$word <- gsub('[[:digit:]]+', '', pamela_text$word)
pamela_text$word <- gsub('[[:punct:]]+', '', pamela_text$word)

pamela_text <- pamela_text %>% filter(!(nchar(word) == 1))%>% 
  anti_join(stop_words)

tokens <- pamela_text %>% filter(!(word==""))
tokens <- tokens %>% mutate(ind = row_number())
words_to_remove <- as_tibble(c("pamela", "dont", "stephenson", "connolly", "im", "description", "theguardiancom"))
tokens <- tokens %>% anti_join(words_to_remove, by = c("word" = "value"))
tokens <- tokens %>% 
  count(rowid, word, sort = TRUE) %>% 
  tidytext::cast_dtm(rowid, word, n)

pamela_lda <- LDA(tokens, k = 10, control = list(seed = 1234))
pamela_topics <- tidy(pamela_lda, matrix = "beta")

pamela_top_terms <- pamela_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

pamela_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()  

  # tokens <- tokens %>% group_by(rowid) %>% mutate(ind = row_number()) %>%
#   tidyr::spread(key = ind, value = word)
# tokens [is.na(tokens)] <- ""
# tokens <- tidyr::unite(tokens, text,-c(rowid, web_title, web_publication_date),sep =" " )
# tokens$text <- trimws(tokens$text)
# install.packages("quanteda")
# #create DTM
# dtm <- CreateDtm(tokens$text, 
#                  doc_names = tokens$ID, 
#                  ngram_window = c(1, 2))
# #explore the basic frequency
# tf <- TermDocFreq(dtm = dtm)
# original_tf <- tf %>% select(term, term_freq,doc_freq)
# rownames(original_tf) <- 1:nrow(original_tf)
# # Eliminate words appearing less than 2 times or in more than half of the
# # documents
# vocabulary <- tf$term[ tf$term_freq > 1 & tf$doc_freq < nrow(dtm) / 2 ]
# dtm = dtm




data("AssociatedPress")
# set a seed so that the output of the model is predictable
ap_lda <- LDA(AssociatedPress, k = 2, control = list(seed = 1234))

ap_topics <- tidy(ap_lda, matrix = "beta")

ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()
