# Get Date list
library(jsonlite)
library(magrittr)
library(stringr)
get_date_list <- function() {
  url <- "https://api-gracenote.nbcolympics.com/svc/games_v2.svc/json/GetDateList?competitionSetId=2&season=20212022&languageCode=2"
  
  temp <- jsonlite::fromJSON(url) %>% 
    .$Dates %>% 
    as.data.frame()
  
  names(temp) <- str_split(names(temp), 
                           pattern = "_", 
                           n = 2, 
                           simplify = TRUE)[,2]


  return(temp)
}