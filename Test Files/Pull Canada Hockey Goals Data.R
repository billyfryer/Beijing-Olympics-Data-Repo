library(tidyverse)
source("NBC Scraper/get_sport_list.R")
source("NBC Scraper/get_sport_schedule.R")
source("NBC Scraper/get_sport_data.R")


sport_list <- get_sport_list()
# Find out from sport_list that Hockey ID is 113
hockey_schedule <- get_sport_schedule(113) %>% 
  .$DateList.EventPhaseMatchList %>% 
  bind_rows()

# Filter to Only the Women's Tournament
temp <- hockey_schedule %>% 
  filter(GenderEvent$c_Name == "Women's Tournament")

# Get Match Info
temp2 <- temp %>% 
  select(Match) %>% 
  unpack(cols = "Match") 

# Pull All Game IDs and Competitor Info
temp3 <- temp2 %>% 
  select(n_ID, Competitor1, Competitor2) %>% 
  unpack(cols = c("Competitor1", "Competitor2"),
         names_repair = "unique")

# Select and Rename Columns
temp4 <- temp3 %>% 
  select(n_ID, c_Name...4, n_Result...22, c_Name...33, n_Result...51) %>% 
  rename("MatchID" = "n_ID",
         "Team1Name" = "c_Name...4",
         "Team1Score" = "n_Result...22" ,
         "Team2Name" = "c_Name...33",
         "Team2Score" = "n_Result...51")

write.csv(temp4, 
          file = "Data Sets/W Hockey Scores.csv")

