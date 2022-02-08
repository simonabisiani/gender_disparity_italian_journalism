
library(tidyverse)


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
