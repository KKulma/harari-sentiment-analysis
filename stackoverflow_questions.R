install.packages("pacman")
pacman::p_load(XML, dplyr, tidyr, stringr, rvest, audio, xml2, purrr, tidytext, ggplot2)

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

deusex_reviews <- map2(1:pages, deus_ex_code, function_page) %>% bind_rows()

sapiens_reviews$comments <- gsub("\\.", "\\. ", sapiens_reviews$comments)
deusex_reviews$comments <- gsub("\\.", "\\. ", deusex_reviews$comments)


sentence_function <- function(df){
  df_sentence <- df %>% 
    select(comments, format, stars, helpful) %>% 
    unnest_tokens(sentence, comments, token = "sentences")
  
  df_sentence
}

sapiens_sentence <- sentence_function(sapiens_reviews)
deusex_sentence <- sentence_function(deusex_reviews)

deusex_sentence <- deusex_sentence %>% 
  mutate(sentence_score <- unname(calculate_score(sentence)))

unname(calculate_score(sapiens_sentence[1, 4]))
