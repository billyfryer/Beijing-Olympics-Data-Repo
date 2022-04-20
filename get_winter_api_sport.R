library(magrittr)
library(jsonlite)
url <- "https://www.nbcolympics.com/api/sport_front?sort=title&filter%5Bstatus%5D=1&include=sport&filter%5Bgames-league%5D%5Bcondition%5D%5Bpath%5D=sport.games_league&filter%5Bgames-league%5D%5Bcondition%5D%5Boperator%5D=IN&filter%5Bgames-league%5D%5Bcondition%5D%5Bvalue%5D%5B0%5D=winter&filter%5Bgames-league%5D%5Bcondition%5D%5Bvalue%5D%5B1%5D=all"

temp <- jsonlite::fromJSON(url) %>% 
  as.data.frame() %>% 
  jsonlite::flatten()

#' temp$included.attributes.gracenote_id is the sportid
#' temp$included.attributes.name is the name of the sport