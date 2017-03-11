# scrape Amazon
rm(list=ls())
ls()

install.packages("pacman")
pacman::p_load(XML, dplyr, stringr, rvest, audio, xml2)
update.packages()

#Remove all white space
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

sapiens_code = "1846558239"
deus_ex_code = "1910701874"

function_product <- function(prod_code){
  trim <- function (x) gsub("^\\s+|\\s+$", "", x)
  
  url <- paste0("https://www.amazon.co.uk/dp/",prod_code)
  doc <- xml2::read_html(url)
  prod <- html_nodes(doc,"#productTitle") %>% html_text() %>%
    gsub("\n","",.) %>% trim()
  prod
}


sapiens <- function_product(sapiens_code)
deus_ex <- function_product(deus_ex_code)

sapiens
deus_ex



#Source funtion to Parse Amazon html pages for data
source("https://raw.githubusercontent.com/rjsaito/Just-R-Things/master/Text%20Mining/amazonscraper.R")




### sugestion

library("purrr")
library("dplyr")
pages <- 50

function_page <- function(page){
  url2 <- paste0("http://www.amazon.co.uk/pro...",prod_code,"/?pageNumber=", page_num)
  doc2 <- read_html(url2)
  
  reviews <- amazon_scraper(doc2, reviewer = F, delay = 2)
  reviews
}

reviews_all <- map(1:pages, function_page) %>% bind_rows()