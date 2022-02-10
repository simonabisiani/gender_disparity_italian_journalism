library(guardianapi)

gu_api_key(check_env = FALSE)

all_articles <- gu_content(query = NULL, show_fields = "all", show_tags = "all", 
                                                                    tag = NULL, from_date = "2022-01-01", to_date = "2022-01-31", 
                                                                    use_date = "newspaper-edition", verbose = TRUE, tidy = TRUE, 
                                                                    tidy_style = "snake_case")



all_articles_section_author <- all_articles %>% 
  select(section_name, id, byline)
