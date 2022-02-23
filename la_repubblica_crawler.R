library(RCurl)
library(XML)
library(stringr)
library(RSelenium)
library(tidyverse)
library(rlist)

remDr <- rsDriver(browser = 'firefox', port = 4458L)
rm <- remDr$client


# navigate link
rm$navigate("https://ricerca.repubblica.it/repubblica/archivio/repubblica/2022/02/01")

# list storing articles
article_list <- c()
time_list <- c()
headlines_list <- c()
authors_list <- c()
sezione_list <- c()

# start for loop
for (index in seq(1, 3)) {
  article_list_tmp <-
    paste0(
      "https://ricerca.repubblica.it/repubblica/archivio/repubblica/2022/02/01?page=",
      index
    )
  rm$navigate(article_list_tmp)
  page <- unlist(rm$getPageSource())
  tpage <- htmlParse(page)
  article <- xpathSApply(tpage, "//article", xmlValue)
  time <- xpathSApply(tpage, "//time", xmlValue)
  headlines <- xpathSApply(tpage, "//h1/a", xmlValue)
  author <- xpathSApply(tpage, "//em[@class = 'author']", xmlValue)
  sezione <-
    xpathSApply(tpage, "//span[@class = 'section']", xmlValue)
  article_list <- append(article_list, article)
  time_list <- append(time_list, time)
  headlines_list <- append(headlines_list, headlines)
  authors_list <- append(authors_list, author)
  sezione_list <- append(sezione_list, sezione)
}

# putting them in a df
df <- bind_cols(article_list, time_list, headlines_list)

#close selenium
rm$close()
rm(remDr)
rm(rm)
gc()

############################# ALL ARTICLES #####################################


rm$findElement(using = 'xpath', ("//*[text() = 'Successiva']"))$clickElement()
