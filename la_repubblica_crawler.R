library(RCurl)
library(XML)
library(stringr)
library(RSelenium)
library(tidyverse)
library(rlist)


start_date <- as.Date("2021/01/01")
dates <- as.character(gsub("-", "/", seq(start_date, by = "day", length.out = 2)))


remDr <- rsDriver(browser = 'firefox', port = 4460L)
rm <- remDr$client

# navigate link
#rm$navigate("https://ricerca.repubblica.it/repubblica/archivio/repubblica/2022/02/01")

# list storing articles
article_list <- c()
time_list <- c()
headlines_list <- c()
authors_list <- c()
sezione_list <- c()

# start for loop
for (date in dates) {
  page <- 1
  condition <- TRUE

  #print(date)

  while (TRUE) {
    article_list_tmp <-
      paste0(
        "https://ricerca.repubblica.it/repubblica/archivio/repubblica/",
        date,
        "?page=",
        page
      )
    rm$navigate(article_list_tmp)
    page <- unlist(rm$getPageSource())
    tpage <- htmlParse(page)
    article <- xpathSApply(tpage, "//article", xmlValue)
    # print("article")
    # print(length(article))
    if (length(article) > 0) {
    time <- xpathSApply(tpage, "//time", xmlValue)
    headlines <- xpathSApply(tpage, "//h1/a", xmlValue)
    author <-
      xpathSApply(tpage, "//em[@class = 'author']", xmlValue)
    sezione <-
      xpathSApply(tpage, "//span[@class = 'section']", xmlValue)
    article_list <- append(article_list, article)
    time_list <- append(time_list, time)
    headlines_list <- append(headlines_list, headlines)
    authors_list <- append(authors_list, author)
    sezione_list <- append(sezione_list, sezione)
    #i = i + 1
    # print(article_list_tmp)
    # 
    # 
    # print("pagina attuale")
    # print(page)
    page <- page + 1
    # print("pagina +1",page)
    # print(page)
    
    }
    else {
      condition <- FALSE
    }
  }
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
