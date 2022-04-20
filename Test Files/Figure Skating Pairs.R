source("NBC Scraper/get_sport_list.R")
source("NBC Scraper/get_event_phase.R")
source("NBC Scraper/get_sport_data.R")
library(tidyverse)
library(ggplot2)

figureskatingID <- get_sport_list() %>% 
  filter(c_Sport == "Figure Skating") %>% 
  pull(n_SportID)

event_phase <- get_event_phase(figureskatingID) %>% 
  filter(c_Event == "Pairs") %>% 
  pull(n_EventPhaseID)

final <- get_sport_data(figureskatingID, phaseId = event_phase) %>%
  .$Standings %>% 
  .$ParticipantList %>% 
  do.call(rbind, .) %>% 
  as.data.frame() %>% 
  unnest_wider(NOC) %>% 
  select(c_ParticipantShort, n_StartOrder, c_Name, c_Result) %>% 
  data.frame()

final <- lapply(final, unlist) %>% data.frame()

# write.csv(file = "Data Sets/Figure Skating Pairs.csv",
#           x = final,
#           row.names = FALSE)
