# scrape Amazon
rm(list=ls())
ls()

#install.packages("pacman")
pacman::p_load(XML, dplyr, tidyr, stringr, rvest, audio, xml2, purrr, tidytext, ggplot2)
update.packages()


sapiens_code = "1846558239"
deus_ex_code = "1910701874"

function_product <- function(prod_code){
  url <- paste0("https://www.amazon.co.uk/dp/",prod_code)
  doc <- xml2::read_html(url)
  prod <- html_nodes(doc,"#productTitle") %>% html_text() %>%
    gsub("\n","",.) %>%
    gsub("^\\s+|\\s+$", "", .) #Remove all white space
  prod
}

sapiens <- function_product(sapiens_code)
deus_ex <- function_product(deus_ex_code)

sapiens
deus_ex



#Source funtion to Parse Amazon html pages for data
source("https://raw.githubusercontent.com/rjsaito/Just-R-Things/master/Text%20Mining/amazonscraper.R")

pages <- 13

function_page <- function(page_num, prod_code){
  url2 <- paste0("http://www.amazon.co.uk/product-reviews/",prod_code,"/?pageNumber=", page_num)
  doc2 <- read_html(url2)
  
  reviews <- amazon_scraper(doc2, reviewer = F, delay = 2)
  reviews
}

sapiens_reviews <- map2(1:pages, sapiens_code, function_page) %>% bind_rows()
str(sapiens_reviews)

deusex_reviews <- map2(1:pages, deus_ex_code, function_page) %>% bind_rows()
str(deusex_reviews)

sapiens_reviews$comments <- gsub("\\.", "\\. ", sapiens_reviews$comments)
deusex_reviews$comments <- gsub("\\.", "\\. ", deusex_reviews$comments)

### sentiment analysis ####

tokens_function <- function(df){
  df_words <- df %>% 
  select(comments, format, stars, helpful) %>% 
  unnest_tokens(word, comments)
  
  data("stop_words")
  
  df_words <- df_words %>%
    anti_join(stop_words)
  
  df_words
}

sapiens_words <- tokens_function(sapiens_reviews)
deusex_words <- tokens_function(deusex_reviews)

head(sapiens_words)
tail(sapiens_words)

head(deusex_words)
tail(deusex_words)

get_sentiments("bing") %>% head
get_sentiments("nrc") %>% head
get_sentiments("afinn") %>% head

sapiens_sent <- sapiens_words %>% 
  left_join(get_sentiments("bing"), by = "word") %>% 
  left_join(get_sentiments("afinn"), by = "word") %>% 
  mutate(book = "Sapiens")

deusex_sent <- deusex_words %>% 
  left_join(get_sentiments("bing"), by = "word") %>% 
  left_join(get_sentiments("afinn"), by = "word") %>% 
  mutate(book = "Deus Ex")

all_sent = bind_rows(sapiens_sent, deusex_sent)


str(all_sent)
summary(all_sent)

### # how many words for analysis ###
# sapiens
sum(is.na(sapiens_sent$sentiment))/nrow(sapiens_sent)
nrow(sapiens_sent)-sum(is.na(sapiens_sent$sentiment))


# deus_ex 
sum(is.na(deusex_sent$sentiment))/nrow(deusex_sent)
nrow(deusex_sent)-sum(is.na(deusex_sent$sentiment))

str(deusex_sent)
str(sapiens_sent)



### number of stars per review 

round(table(sapiens_sent$stars)/sum(table(sapiens_sent$stars)), 2)
round(table(deusex_sent$stars)/sum(table(deusex_sent$stars)), 2)

all_sent %>%
  group_by(book, stars) %>%
  summarize(n_stars = n()) %>%
  group_by(book) %>% 
  mutate(n_reviews = sum(n_stars),
         percent = paste0(round(n_stars*100/n_reviews, 0), "%")) %>% 
  select(-c(n_stars, n_reviews)) %>% 
  spread(stars, percent)

### average sentiment score 

all_sent %>% 
  group_by(book) %>% 
  summarise(mean = mean(score, na.rm = TRUE), median = median(score, na.rm  = TRUE)) 

all_sent %>% 
  ggplot(aes(x= book, y = score, color = book)) +
  geom_boxplot(outlier.shape=NA)  #avoid plotting outliers twice
  
  

### sentiment score spread 


all_sent %>% 
  ggplot(aes(x= book, y = score, color = book)) +
  geom_boxplot(outlier.shape=NA) + #avoid plotting outliers twice
  geom_jitter(position=position_jitter(width=.1, height=0))

### average sentiment score per star 

all_sent %>% 
  ggplot(aes(as.factor(stars), score)) +
  geom_boxplot(aes(fill = book)) #+
  #facet_wrap( ~ stars)#, scales="free")

### ratio of positive / negative words per review

all_sent %>%
  filter(!is.na(sentiment)) %>%
  group_by(book, sentiment) %>% 
  summarise(n = n() ) %>%
  group_by(book) %>%
  mutate(sum = sum(n),
         percent = paste0(round(n*100/sum, 0), "%")) %>%
  select(-c(n, sum)) %>%
  spread(sentiment, percent)
  
### ratio of positive / negative words per star per review

all_sent %>% 
  filter(!is.na(sentiment)) %>%
  group_by(book, stars, sentiment) %>%
  summarise(n = n()) %>%
  group_by(book, stars) %>%
  mutate(sum = sum(n), 
         percent = paste0(round(n*100/sum, 0), "%"),
         percent2 = round(n/sum, 3)) %>% 
  select(-c(n, sum, percent)) %>%
  spread(sentiment, percent2) %>%
  ggplot(aes(x = stars, y = positive, fill = book)) +
    geom_bar(stat = "identity", position = "identity", alpha = 0.6)
  