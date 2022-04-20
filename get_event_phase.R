# Get Event Phase List
library(jsonlite)
library(magrittr)
get_event_phase <- function(sportId){
  url <- paste0("https://api-gracenote.nbcolympics.com/svc/games_v2.svc/json/GetEventPhaseList_Season?competitionSetId=2&season=20212022&sportId=", sportId, "&languageCode=2")
  
  jackpot <- jsonlite::fromJSON(url)  %>%  
    as.data.frame()
  
  return(jackpot)
}

