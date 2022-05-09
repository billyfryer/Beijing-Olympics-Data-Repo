# Scraping 3 Sport Id and Event Id

# Libraries and Sourced Functions
library(tidyverse)
source("get_sport_schedule.R")
source("get_event_phase.R")
source("get_sport_data.R")
# These are the rest of the functions that I need to pull from

all_schedules <- paste0("Data/Sport-Schedules/",
                        list.files("Data/Sport-Schedules"))

complete_schedule <- data.frame(SportID = c(),
                        SportTypeID = c(),
                        MatchID = c(),
                        PhaseID = c())

for (i in 1:length(all_schedules)) {
  
  print(paste(i, "/", length(all_schedules)))
  schedule <- all_schedules[i]
  print(paste("Getting:", schedule))
  raw_csv <- read_csv(schedule)
  
  # If it's Hockey or Curling
  if(unique(raw_csv$Sport.n_TypeID) == 1) {
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
  
  complete_schedule <- bind_rows(complete_schedule, temp)
}

##############################################
### Grab Data For All Sports
##############################################
for (i in 1:nrow(complete_schedule)) {
  
  # Pull Individual Values Out of Each Row
  SportID <- complete_schedule[i,]$SportID
  SportTypeID <- complete_schedule[i,]$SportTypeID
  MatchID <- complete_schedule[i,]$MatchID
  PhaseID <- complete_schedule[i,]$PhaseID
  
  # Get Data and Output
  print(paste("SportID:", SportID,
              "MatchID:", MatchID,
              "PhaseID:", PhaseID,
              i, "/", nrow(complete_schedule)))
  data <- get_sport_data(sportId = SportID, matchId = MatchID, phaseId = PhaseID)
  # Output path depends on Phase or Match ID, whichever is not NA
  if(is.na(MatchID)) {
  output_path <- paste0("Data/", SportID, "-JSONs/", PhaseID, ".json")
  } else {
    output_path <- paste0("Data/", SportID, "-JSONs/", MatchID, ".json")
    
  }
  
  # Save as JSON
  export_json <- toJSON(data)
  write(export_json, file = output_path)
}
