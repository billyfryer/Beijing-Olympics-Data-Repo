# Monobob Stats
source("NBC Scraper/get_sport_list.R")
source("NBC Scraper/get_event_phase.R")
source("NBC Scraper/get_sport_data.R")
library(tidyverse)
library(ggplot2)

# Get Biathlon Sport ID
BobsledID <- get_sport_list() %>% 
  filter(c_Sport == "Bobsled") %>% 
  pull(n_SportID)

# Get Specific PhaseID
event_phase <- get_event_phase(BobsledID) %>% 
  filter(c_GenderShort == "W") %>% 
  filter(c_Event == "Monobob") %>% 
  pull(n_EventPhaseID)

# Get Sport Data
monobob_data_full <- get_sport_data(sportId = BobsledID, phaseId = event_phase)

participant_list <- monobob_data_full %>% 
  .$Standings %>% 
  .$ParticipantList

low_level_data <- participant_list %>% 
  do.call(rbind, .) %>% 
  data.frame()

df_of_lists <- low_level_data %>% 
  .$ResultPhaseList %>% 
  do.call(rbind, .) %>% 
  data.frame()
  
wide_data <- df_of_lists %>% 
  unpack(cols = c(X1)) %>% 
  unnest_wider(col = c(X1, X2, X3, X4),
               names_repair = "unique")

# names(wide_data)[1:17]
#' The column names repeat every 17 so I'm planning to pivot_longer 17 times
#' There has to be a better way but idk it rn and oh well it would work.
#' Not doing it now because I don't want to.
#' "n_PhaseID...1"               "n_Rank...2"                 
#' "n_RankSort...3"              "c_Rank...4"                 
#' "c_Result...5"                "c_ResultAbs...6"            
#' "c_ODF_QualificationMark...7" "c_ODF_IRM...8"              
#' "n_TimeAbs...9"               "n_TimeRel...10"             
#' "n_PointsAbs...11"            "c_ResultInfo_1...12"        
#' "c_ResultInfo_2...13"         "n_BDF_StartTime...14"       
#' "c_BDF_StartTime...15"        "n_BDF_VideoSessionID...16"  
#' "c_ClientStreamURL...17" 

#n_TimeAbs = 60,000 * min + 1,000 * sec + 10 * milli

monobob_data_small <- low_level_data %>% 
  select(c_ParticipantFirstName, c_ParticipantLastName, n_StartOrder, n_TimeAbs) %>% 
  unnest(cols = c(c_ParticipantFirstName, c_ParticipantLastName, n_StartOrder, 
                  n_TimeAbs)) %>% 
  data.frame()

# write.csv(monobob_data_small,
#           file = "Data Sets/Small Monobob Data.csv",
#           row.names = FALSE)
