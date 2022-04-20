library(tidyverse)
# Only things we need is SportIds
sport_list <- read_csv("Output Folder/Sport List.csv")

sportids <- sport_list %>% pull(n_SportID)

### Create Folders Based on SportID
for (i in 1:length(sportids)) {
  id <- sportids[i]
  
  path <- paste0("Output Folder/", id)
  #dir.create(path)
}

### Get Event Phases
source("get_event_phase.R")
for (i in 1:length(sportids)) {
  id <- sportids[i]
  print(paste("Sport ID ", i, "/", length(sportids)))
  event_phase <- get_event_phase(id)
  output_path <- paste0("Output Folder/", id, "/Event Phase.csv")
  # write.csv(event_phase, output_path,
  #           row.names = FALSE)
}

### Get Sport Schedule
source("get_sport_schedule.R")
for (i in 1:length(sportids)) {
  id <- sportids[i]
  print(paste("Sport ID ", i, "/", length(sportids)))
  sport_schedule <- get_sport_schedule(id)
  export_json <- toJSON(sport_schedule)
  output_path <- paste0("Output Folder/", id, "/Sport Schedule.json")
  # write(export_json, file = output_path)
}
