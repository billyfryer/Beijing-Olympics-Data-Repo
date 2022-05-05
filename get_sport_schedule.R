# Get Schedule Sport
library(magrittr)
library(jsonlite)
get_sport_schedule <- function(sportID){
  url <- paste0("https://api-gracenote.nbcolympics.com/svc/games_v2.svc/json/GetScheduleSport?competitionSetId=2&season=20212022&sportId=", sportID, "&languageCode=2")

  jackpot <- jsonlite::fromJSON(url)  %>%  
    as.data.frame() %>% 
    unnest(cols = "DateList.EventPhaseMatchList") %>% 
    jsonlite::flatten() %>% 
    as.data.frame()
  
  if(sportID %in% c(113, 212)) {
    jackpot$Match.PeriodList <- unlist(jackpot$Match.PeriodList)
  }

  return(jackpot)
}
