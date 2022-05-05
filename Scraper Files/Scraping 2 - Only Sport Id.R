library(tidyverse)
library(qdapRegex)
# Only things we need is SportIds
sport_list <- read_csv("Data/Sport List.csv")

sportids <- sport_list %>% pull(SportID)

### Create Folders Based on SportID
for (i in 1:length(sportids)) {
  id <- sportids[i]
  
  path <- paste0("Data/", id)
  #dir.create(path)
}

### Get Event Phases
source("get_event_phase.R")
for (i in 1:length(sportids)) {
  id <- sportids[i]
  print(paste("Sport ID ", i, "/", length(sportids)))
  event_phase <- get_event_phase(id)
  output_path <- paste0("Data/Event Matches/", id, " Event Matches.csv")
  # Change "Phase" to match
  names(event_phase) <- str_replace_all(names(event_phase), 
                                  pattern = "Phase",
                                  replacement = "Match")
  
  # Clean Names a bit by getting rid of underscore prefix
  names(event_phase) <- str_split(names(event_phase), 
                                  pattern = "_", 
                                  n = 2, 
                                  simplify = TRUE)[,2]
  # Final Name Cleaning
  event_phase <- event_phase %>% janitor::clean_names(case = "big_camel")
  # write.csv(event_phase, output_path,
  #           row.names = FALSE)
}

### Get Sport Schedule
source("get_sport_schedule.R")
for (id in sportids) {
  print(paste("Sport ID:", id))
  sport_schedule <- get_sport_schedule(id)
  
  # Change "Phase" to match
  names(sport_schedule) <- str_replace_all(names(sport_schedule), 
                                        pattern = "Phase",
                                        replacement = "Match")
  
  # Clean Names a bit by getting rid of most of the underscore prefixes
  names(sport_schedule) <- qdapRegex::rm_between(names(sport_schedule), 
                                  left = ".",
                                  right = "_")
  # Final Name Cleaning
  sport_schedule <- sport_schedule %>% 
    janitor::clean_names(case = "big_camel")

  output_path <- paste0("Data/Sport Schedules/", id, " Sport Schedule.csv")
  write.csv(sport_schedule, output_path,
            row.names = FALSE)
}
