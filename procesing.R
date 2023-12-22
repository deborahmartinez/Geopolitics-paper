library(tidyverse)
library(dplyr)
library(readr)
library(tidytext)
library(lubridate)
library(ggchicklet)
library(scales)
data(stop_words)
get_sentiments("afinn")

# Reading the data --------------------------------------------------------------
trump <- read_csv("~/Desktop/Geopolitics paper/Data/trump_tweets.csv") %>% 
  select(username = ID, text = `Tweet Text`, date = Time)
trump <- trump %>%  mutate(date = floor_date( ymd_hms(date), unit = "day"))

wild <- read_csv("~/Desktop/Geopolitics paper/Data/wilders_tweets.csv")
wild <- wild %>%  mutate(username = "@geertwilderspvv", date= dmy(date)) 

dutch_election <- seq(min(wild$date), max(as.Date(wild$date)), by = 'days') %>% 
  as_tibble() %>% arrange(desc(value)) %>%  mutate(election_days= row_number()) %>% 
  rename(date = value)

us_election <- seq(min(as.Date(trump$date)), max(as.Date(trump$date)), by = 'days') %>% 
  as_tibble() %>% arrange(desc(value)) %>%  mutate(election_days= row_number()) %>% 
  rename(date = value)

wild <- wild %>%  left_join(dutch_election)

trump <- trump %>%  left_join(us_election)


bd <- bind_rows(trump, wild)

bd <- bd %>%  filter(election_days< 51)
stop_words2 <- read_csv("~/Desktop/Geopolitics paper/Data/stopwords2.csv")
# text analisys -----------------------------------------------------------
words <- bd %>%  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% 
  anti_join(stop_words2)

words %>% group_by(username) %>%  count(word, sort = T) %>% top_n(10) %>% 
  ggplot(aes(x =  fct_reorder(word, n), y = n))+ 
  geom_chicklet(width = .5)+
  facet_wrap(~username, scales = "free")+
  coord_flip()+
  theme_minimal()


words %>% group_by(username) %>%  count(word, sort = T) %>% 
  mutate(pct = n/sum(n)) %>%  top_n(10) %>% 
  ggplot(aes(x =  fct_reorder(word, n), y = n))+ 
  geom_chicklet(width = .5)+
  facet_wrap(~username, scales = "free")+
  coord_flip()+
  theme_minimal()

frequency <- words %>% 
  count(username, word, sort = TRUE) %>% 
  left_join(words %>% 
              count(username)) %>%
  group_by(username) %>% 
  mutate(freq = n/sum(n))

frequency <- frequency %>% 
  select(username, word, freq) %>% 
  pivot_wider(names_from = username, values_from = freq) %>%
  arrange(`@realDonaldTrump` ,`@geertwilderspvv`) %>%  na.omit()

ggplot(frequency, aes(`@realDonaldTrump` ,`@geertwilderspvv`)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red")



# key words per day -------------------------------------------------------

words %>%  group_by(username, election_days)  %>%
  count(word) %>% 
  summarize(total = sum(n)) %>% 
  left_join(words %>%  group_by(username, election_days)  %>% count(word, sort= T)) %>% 
  inner_join(wild %>%  select(election_days))%>%  distinct()%>%
  bind_tf_idf(word, election_days, n) %>% 
  arrange(desc(tf_idf)) %>%
  group_by(election_days) %>%
  slice_max(tf_idf, n = 1) %>%
  ggplot(aes(x= election_days, y= tf_idf, group = username, color = username) )+
  geom_line() +
  facet_wrap(~username, nrow = 2)+
  geom_label( 
    aes(label=word), position = "jitter",
  )


# sentiment analysis ------------------------------------------------------

get_sentiments("bing")
get_sentiments("nrc")
sent_word <- get_sentiments("nrc")

words %>% filter(election_days<20) %>%  group_by(username, election_days)  %>%
  count(word) %>% 
  summarize(total = sum(n)) %>% 
  left_join(words %>%  group_by(username, election_days)  %>% count(word, sort= T)) %>% 
  inner_join(wild %>%  select(election_days))%>%  distinct()%>%
  bind_tf_idf(word, election_days, n) %>% 
  arrange(desc(tf_idf)) %>%
  group_by(election_days) %>%
  slice_max(tf_idf, n = 10) %>%  left_join(sent_word) %>% 
  ungroup() %>%  na.omit() %>%  
  count(username, sentiment, sort = T)
