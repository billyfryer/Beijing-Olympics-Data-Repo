# Summer Olympics Sports
library(jsonlite)
library(magrittr)
get_summer_api_sport <- function() {
  url <- "https://www.nbcolympics.com/api/sport"
  
  temp <- jsonlite::fromJSON(url) %>%
    as.data.frame() %>% 
    jsonlite::flatten()
  return(temp)
}

get_summer_api_sport() %>% View()
