# PACKAGES
library(RCurl)
library(XML)
library(RSelenium)
library(tidyverse)


#### STEP 1_1: NAVIGATE USING A HEADLESS BROWSER TO GET TO DESIRED PAGE ###################################################

# storing my url
link <- 'https://www.pressreader.com/italy/corriere-della-sera/20220207/page/1/textview'
link2 <- 'https://www.pressreader.com/italy/corriere-della-sera/20220201/page/1/textview'



# access the server
rD <- rsDriver(browser = 'firefox', port = 4655L)

# pick client
rm <- rD$client

# OPEN BROWSER
rm$navigate(link) 
Sys.sleep(5)


# SWITCH FRAME and ACCEPT COOKIES
rm$switchToFrame("snackbar snackbar-light")
rm$findElement(using = "xpath", '//button[@class="btn btn-outlined btn-action"]')$clickElement()

#rm$switchToFrame("root-container-panel")
rm$findElement(using = "xpath", "//li[@id='thumbsToolbarBottom_0' and @item-id='0']")$clickElement()
Sys.sleep(3)


#### EXTRACT AUTHORS ###########################################################



# USING HTTR INSTEAD OF RVEST TO PARSE PAGE
library(httr)
page2 <- htmlParse(rm$getPageSource()[[1]])
#doc <- htmlTreeParse(rm$getPageSource()[[1]],useInternal=TRUE) (no, cattura solo gli elementi visibili on screen)

query <- '//section[@section-id="0"]//li[@class="art-author"]'

# WITH HTTR
#frontpage_authors <- xpathSApply(page2, '//li[@class="art-author"]', xmlValue)
frontpage_authors1 <- xpathSApply(page2, query, xmlValue)




page <- htmlParse(rm$getPageSource()[[1]])

frontpage_authors2 <- xpathSApply(page, query, xmlValue)



#### PREDICT GENDER ############################################################



library(gender)

frontpage_authors_adj <- as.data.frame(frontpage_authors2) %>% 
  mutate(frontpage_authors2 = str_replace(frontpage_authors2, "di ", ""),
         frontpage_authors2 = str_replace(frontpage_authors2, "Di ", "")) %>% 
  separate(frontpage_authors2, c("a", "b", "c"), sep = c(", | e ")) %>% 
  pivot_longer(cols = c(a, b, c), names_to = "journalist") %>% 
  na.omit() %>% 
  mutate(value = gsub("\u00AD", "", value)) %>% 
  mutate(first_name = word(value, 1)) 

gender <- gender(
  frontpage_authors_adj$first_name,
  method = "genderize"
)


# x <- "https://raw.githubusercontent.com/mrblasco/genderNamesITA/master/gender_firstnames_ITA.csv"
# z <- "https://github.com/mrblasco/genderNamesITA/blob/master/gender_firstnames_ITA.csv"


library(readxl)
italian_names <- read_excel("C:/Users/simon/Desktop/SHELDON/italian_names.xlsx")

# get % of men and women with that name
italian_names <- italian_names %>% 
  mutate(perc_men = round(male / tot, 2),
         perc_female = round(female / tot, 2)) %>% 
  mutate(name = str_to_title(nome))


gender_merged <- gender %>% 
  left_join(italian_names, by = "name")



Encoding(a[[1]]) <- "UTF-8"
Encoding(italian_names[[7]]) <- "UTF-8"
