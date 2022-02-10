library(RCurl)
library(XML)
library(stringr)
library(RSelenium)
library(tidyverse)
library(rlist)

remDr <- rsDriver(verbose = T,
                  remoteServerAddr = "localhost",
                  port = 4447L,
                  browser=c("firefox"))
rm <- remDr$client


#navigate link searching all existing articles featuring the keywork "donna" from 1984 to 2022
rm$navigate("https://ricerca.repubblica.it/ricerca/repubblica?query=donna&fromdate=1984-01-01&todate=2022-02-10&sortby=score&author=&mode=any") 



#list storing articles
# Author on la Repubblica Archive xpath : //em[@class = 'author']
time_list <- c()
headlines_list <- c()
authors_list <- c()


for(index in seq(1,3)){
  
  article_list_tmp <- paste0("https://ricerca.repubblica.it/ricerca/repubblica?query=donna&fromdate=1984-01-01&todate=2022-02-10&sortby=score&author=&mode=any&page=", index)
  rm$navigate(article_list_tmp) 
  page <- unlist(rm$getPageSource())
  tpage <- htmlParse(page)
  time <- xpathSApply(tpage, "//time", xmlValue)
  headlines <- xpathSApply(tpage, "//h1/a", xmlValue)
  author <- xpathSApply(tpage, "//em[@class = 'author']", xmlValue)
  time_list <- append(time_list, time)
  headlines_list <- append(headlines_list, headlines)
  authors_list <- append(authors_list, author)

  
}


df <- bind_cols(time_list, headlines_list)

#close selenium
rm$close()
rm(remDr)
rm(rm)
gc()




############################# ALL ARTICLES #####################################

all_articles <- "https://ricerca.repubblica.it/ricerca/repubblica?page=1"

remDr <- rsDriver(verbose = T,
                  remoteServerAddr = "localhost",
                  port = 4446L,
                  browser=c("firefox"))
rm <- remDr$client


#navigate link searching all existing articles featuring the keywork "donna" from 1984 to 2022
rm$navigate(all_articles) 



#list storing articles
# Author on la Repubblica Archive xpath : //em[@class = 'author']
time_list <- c()
headlines_list <- c()
authors_list <- c()


for(index in seq(1,10)){
  
  article_list_tmp <- paste0("https://ricerca.repubblica.it/ricerca/repubblica?page=", index)
  rm$navigate(article_list_tmp) 
  page <- unlist(rm$getPageSource())
  tpage <- htmlParse(page)
  time <- xpathSApply(tpage, "//time", xmlValue)
  headlines <- xpathSApply(tpage, "//h1/a", xmlValue)
  author <- xpathSApply(tpage, "//em[@class = 'author']", xmlValue)
  time_list <- append(time_list, time)
  headlines_list <- append(headlines_list, headlines)
  authors_list <- append(authors_list, author)
  
  
}


df <- bind_cols(time_list, headlines_list)

#close selenium
rm$close()
rm(remDr)
rm(rm)
gc()