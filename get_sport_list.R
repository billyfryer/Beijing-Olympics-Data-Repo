# Get sport list
library(jsonlite)
library(magrittr)
get_sport_list <- function() {
  url <- "https://api-gracenote.nbcolympics.com/svc/games_v2.svc/json/GetSportList?competitionSetId=2&season=20212022&languageCode=2"
  
  temp <- jsonlite::fromJSON(url)  %>%  
    as.data.frame() %>% 
    jsonlite::flatten()
  return(temp)
}

