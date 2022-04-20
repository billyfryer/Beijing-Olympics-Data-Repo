# Get Schedule Sport
library(magrittr)
library(jsonlite)
get_sport_schedule <- function(sportId){
  url <- paste0("https://api-gracenote.nbcolympics.com/svc/games_v2.svc/json/GetScheduleSport?competitionSetId=2&season=20212022&sportId=", sportId, "&languageCode=2")

  jackpot <- jsonlite::fromJSON(url)  %>%  
    as.data.frame()
  
  return(jackpot)
}
