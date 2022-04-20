# Get Schedule Matrix
library(jsonlite)
library(magrittr)
library(tidyr)
get_schedule_matrix <- function() {
  url <- "https://api-gracenote.nbcolympics.com/svc/games_v2.svc/json/GetScheduleMatrix?competitionSetId=2&season=20212022&languageCode=2"
  
  jackpot <- jsonlite::fromJSON(url)  %>%  
    as.data.frame() %>% 
    tidyr::unnest(DateList)
  return(jackpot)
}
