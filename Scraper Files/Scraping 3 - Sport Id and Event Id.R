# Scraping 3 Sport Id and Event Id

# Libraries and Sourced Functions
library(tidyverse)
source("get_sport_schedule.R")
source("get_event_phase.R")
source("get_sport_data.R")
# These are the rest of the functions that I need to pull from

sport_list <- read_csv("Data/Sport List.csv")


sportids <- sport_list %>% pull(n_SportID)

lookup_df <- data.frame(sportID = c(),
                        phaseID = c(),
                        matchID = c())
for (i in 1:length(sportids)) {
  print(paste("Sport ID ", i, "/", length(sportids)))
  id <- sportids[i]
  
  sport <- sport_list$c_Sport[i]

  event_phase <- get_event_phase(id) 
  
  phase_id <- event_phase %>% 
    pull(n_EventPhaseID)
  
  SportTypeID <- event_phase %>% 
    pull(n_SportTypeID) %>% 
    unique()
  
  if(unique(SportTypeID) == 3) {
    expanded <- expand.grid(sportID = id, 
                            phaseID = phase_id, 
                            matchID = NA)
  } else {
    expanded <- expand.grid(sportID = id, 
                            phaseID = NA, 
                            matchID = phase_id)
  }
  lookup_df <- bind_rows(lookup_df, expanded)
}

##############################################
### phase ID Sports
##############################################
phase_sports <- lookup_df %>% 
  filter(is.na(matchID))

phase_sports <- left_join(phase_sports, sport_list,
                          by = c("sportID" = "n_SportID"))

# Works for Match IDs sports
for (sport_id in unique(phase_sports$sportID)){
  
  sport <- phase_sports %>% 
    filter(sportID == sport_id) %>% 
    pull(c_Sport) %>% 
    unique()
  
  # New Schedule
  phase_id_key <- get_event_phase(sport_id) %>% 
    select(sport_id = n_SportID,
           match_id = n_EventPhaseID,
           event_name = c_GenderEvent)
  
  # Internal For Loop for individual games
  for (i in 1:nrow(phase_id_key)) {
    sport_id <- as.numeric(phase_id_key[i, 1])
    phase_id <- as.numeric(phase_id_key[i,2])
    event_name <- as.character(phase_id_key[i,3])
    print(paste("Event",event_name, ":", i, "/", nrow(phase_id_key)))
    sport_json <- get_sport_data(sportId = sport_id, phaseId = phase_id)
    export_json <- toJSON(sport_json)
    output_path <- paste0("Data/", sport_id, " JSONs/",
                          phase_id, ".json")    
    write(export_json, file = output_path)
  }
}

##############################################
### match ID Sports
##############################################
match_sports <- lookup_df %>% 
  filter(!is.na(matchID))

match_sports <- left_join(match_sports, sport_list, by = c("sportID" = "n_SportID"))

for (sport_id in unique(match_sports$sportID)){
  
  sport <- match_sports %>% 
    filter(sportID == sport_id) %>% 
    pull(c_Sport) %>% 
    unique()
  
  # New Schedule
  schedule <- get_sport_schedule(sport_id) %>% 
    .$DateList.EventPhaseMatchList %>% 
    bind_rows() %>%
    select(Match, Sport, GenderEvent) %>% 
    unpack(cols = c("Match", "Sport", "GenderEvent"),
          names_repair = "unique") %>% 
    select("Sport_ID" = n_ID...11, 
           "Event_Name" = c_Name...16, 
           "Match_ID" = n_ID...1)
  
  # Internal For Loop for individual games
  for (i in 1:nrow(schedule)) {
    print(paste("Match ", i, "/", nrow(schedule)))
    sport_id = schedule[i, 1]
    event_name = schedule[i,2]
    match_id = schedule[i,3]
    sport_json <- get_sport_data(sportId = sport_id, matchId = match_id)
    export_json <- toJSON(sport_json)
    output_path <- paste0("Data/", sport_id, " JSONs/",
                          match_id, ".json")
    write(export_json, file = output_path)
  }
}