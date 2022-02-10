
library(tidyverse)
library(rvest)
library(RSelenium)
library(XML)
library(stringr)

# generate series of dates
date <- str_replace_all(as.character(seq(as.Date("2022-01-25"), as.Date("2022-01-31"), "days")), "-", "")

# newspaper selection
newspaper <- c("corriere-della-sera", "il-fatto-quotidiano")


# generate unique urls
links <- c()

for (x in newspaper) {
   for (y in date) {
    el <- paste('https://www.pressreader.com/italy/', x, '/', y, '/textview', sep="")
    links <- append(links, el)
   }
}

# was trying a function but does not work well

# #Create query URL link
# 
# build_url <- function(date, newspaper){
#   
#   url <- paste0('https://www.pressreader.com/italy/', newspaper, '/', date, '/page/1/textview')
# 
#     return(url)
# }
# 
# url <- build_url(date, newspaper)



################################################################################
            # Scraping step by step - to be input in a function
################################################################################


# access the server
rD <- rsDriver(browser = 'firefox', port = 4658L)

# pick client
rm <- rD$client

# OPEN BROWSER
rm$navigate(links[1])
Sys.sleep(10)


# SWITCH FRAME and ACCEPT COOKIES
rm$switchToFrame("snackbar snackbar-light")
rm$findElement(using = "xpath", '//button[@class="btn btn-outlined btn-action"]'
               )$clickElement()
Sys.sleep(6)


#rm$switchToFrame("root-container-panel")
rm$findElement(using = "xpath", "//li[@id='thumbsToolbarBottom_0' and @item-id='0']"
               )$clickElement()
Sys.sleep(5)

# THEN ON FIRST PAGE
rm$findElement(using = "xpath", "//li[@id='thumbsToolbarBottom_1' and @item-id='1']"
               )$clickElement()
Sys.sleep(6) 

# CLICK ON BACK BUTTON
rm$findElement(using = "xpath", "//div[@class='toolbar-slider-left']"
               )$clickElement()
Sys.sleep(8) 


# THEN ON HOME AGAIN
rm$findElement(using = "xpath", "//li[@id='thumbsToolbarBottom_0' and @item-id='0']"
               )$clickElement()
Sys.sleep(3)  


page <- htmlParse(rm$getPageSource()[[1]])
query <- '//section[@section-id="0"]//li[@class="art-author"]'

# WITH HTTR
frontpage_authors <- xpathSApply(page, query, xmlValue)






# # SCRAPING FUNCTION
# 
# scrape_homepage_authors <- function(url){
# 
# 
#   # access the server
#   rD <- rsDriver(browser = 'firefox', port = 4658L)
#   
#   # pick client
#   rm <- rD$client
#   
#   # OPEN BROWSER
#   rm$navigate(url)
#   Sys.sleep(10)
#   
#   
#   # SWITCH FRAME and ACCEPT COOKIES
#   rm$switchToFrame("snackbar snackbar-light")
#   rm$findElement(using = "xpath", '//button[@class="btn btn-outlined btn-action"]'
#   )$clickElement()
#   Sys.sleep(6)
#   
#   
#   #rm$switchToFrame("root-container-panel")
#   rm$findElement(using = "xpath", "//li[@id='thumbsToolbarBottom_0' and @item-id='0']"
#   )$clickElement()
#   Sys.sleep(5)
#   
#   # THEN ON FIRST PAGE
#   rm$findElement(using = "xpath", "//li[@id='thumbsToolbarBottom_1' and @item-id='1']"
#   )$clickElement()
#   Sys.sleep(6) 
#   
#   # CLICK ON BACK BUTTON
#   rm$findElement(using = "xpath", "//div[@class='toolbar-slider-left']"
#   )$clickElement()
#   Sys.sleep(8) 
#   
#   
#   # THEN ON HOME AGAIN
#   rm$findElement(using = "xpath", "//li[@id='thumbsToolbarBottom_0' and @item-id='0']"
#   )$clickElement()
#   Sys.sleep(3)  
#   
#   
#   page <- htmlParse(rm$getPageSource()[[1]])
#   query <- '//section[@section-id="0"]//li[@class="art-author"]'
#   
#   # WITH HTTR
#   frontpage_authors <- xpathSApply(page, query, xmlValue)
# 
# 
# 
#   #closing remove driver
#   rm$close()
#   rm(remDr)
#   rm(rm)
#   gc()
#   return(frontpage_authors)
# }
# 
# 
# test <- lapply(links, scrape_homepage_authors)

















