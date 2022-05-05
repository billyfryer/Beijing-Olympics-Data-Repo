# Scraping 3 Sport Id and Event Id

# Libraries and Sourced Functions
library(tidyverse)
source("get_sport_schedule.R")
source("get_event_phase.R")
source("get_sport_data.R")
# These are the rest of the functions that I need to pull from

all_schedules <- paste0("Data/Sport Schedules/",
                        list.files("Data/Sport Schedules"))

lookup_df <- data.frame(SportID = c(),
                        SportTypeID = c(),
                        MatchID = c(),
                        PhaseID = c())

for (schedule in all_schedules) {
  
  print(paste("Testing:", schedule))
  raw_csv <- read_csv(schedule)
  
  # If it's Hockey or Curling
  if(str_detect(schedule, pattern = "113") | str_detect(schedule, pattern = "212")) {
    temp <- raw_csv %>% 
      select(SportID = Sport.n_ID,
             SportTypeID = Sport.n_TypeID,
             MatchID = Match.n_ID) %>% 
      mutate(PhaseID = NA) %>% 
      select(SportID, SportTypeID, MatchID, PhaseID)
  } else {
    # Other Sports besides Hockey and Curling
    temp <- raw_csv %>% 
      select(SportID = Sport.n_ID,
             SportTypeID = Sport.n_TypeID,
             PhaseID = n_MatchID) %>% 
      mutate(MatchID = NA) %>% 
      select(SportID, SportTypeID, MatchID, PhaseID)
    }
  
  lookup_df <- bind_rows(lookup_df, temp)
}

##############################################
### Grab Data For All Sports
##############################################
for (i in 1:nrow(lookup_df)) {
  
  # Pull Individual Values Out of Each Row
  SportID <- lookup_df[i,]$SportID
  SportTypeID <- lookup_df[i,]$SportTypeID
  MatchID <- lookup_df[i,]$MatchID
  PhaseID <- lookup_df[i,]$PhaseID
  

  if(SportTypeID == 1) {
    print(paste("SportID:", SportID, "MatchID:", MatchID, i, "/", nrow(lookup_df)))
    data <- get_sport_data(sportId = SportID, matchId = MatchID)
    output_path <- paste0("Data/", SportID, " JSONs/", MatchID, ".json")
  } else {
    print(paste("SportID:", SportID, "PhaseID:", PhaseID, i, "/", nrow(lookup_df)))
    data <- get_sport_data(sportId = SportID, phaseId = PhaseID)
    output_path <- paste0("Data/", SportID, " JSONs/", PhaseID, ".json")
    
  }
  
  # Save as JSON
  export_json <- toJSON(data)
  write(export_json, file = output_path)
  
}
