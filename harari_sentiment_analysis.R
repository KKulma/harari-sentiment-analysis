# scrape Amazon
rm(list=ls())
ls()

#install.packages("pacman")
pacman::p_load(XML, dplyr, stringr, rvest, audio, xml2, purrr, tidytext)
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
  left_join(get_sentiments("afinn"), by = "word")

deusex_sent <- deusex_words %>% 
  left_join(get_sentiments("bing"), by = "word") %>% 
  left_join(get_sentiments("afinn"), by = "word")

head(sapiens_sent)
tail(sapiens_sent)

head(deusex_sent)
tail(deusex_sent)
summary(deusex_sent)  
str(deusex_sent)  
  