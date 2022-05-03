library(magrittr)
library(jsonlite)
library(dplyr)
url <- "https://www.nbcolympics.com/api/sport_front?sort=title&filter%5Bstatus%5D=1&include=sport&filter%5Bgames-league%5D%5Bcondition%5D%5Bpath%5D=sport.games_league&filter%5Bgames-league%5D%5Bcondition%5D%5Boperator%5D=IN&filter%5Bgames-league%5D%5Bcondition%5D%5Bvalue%5D%5B0%5D=winter&filter%5Bgames-league%5D%5Bcondition%5D%5Bvalue%5D%5B1%5D=all"

temp <- jsonlite::fromJSON(url) %>% 
  as.data.frame() %>% 
  jsonlite::flatten()

# Columns that immediately seem useless
temp2 <- temp[,-c(1,2, 3, 5, 10, 11, 12, 17, 18, 26, 27,
                  33, 34, 40)]
temp3 <- temp2 %>% 
  select(-c(href, included.relationships.parent.links.related.href,
            included.relationships.parent.links.self.href,
            included.links.self.href, data.relationships.sport.links.related.href))

#' temp$included.attributes.gracenote_id is the sportid
#' temp$included.attributes.name is the name of the sport