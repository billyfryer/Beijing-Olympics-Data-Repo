library(tidyverse)
# Only things we need is SportIds
sport_list <- read_csv("Data/Sport_List.csv")

sportids <- sport_list %>% pull(n_SportID)

### Get Event Phases
source("get_event_phase.R")
for (i in 1:length(sportids)) {
  id <- sportids[i]
  print(paste("Sport ID ", i, "/", length(sportids)))
  event_phase <- get_event_phase(id)
  output_path <- paste0("Data/Event-Matches/", id, "_Event_Matches.csv")
  # Change "Phase" to match
  names(event_phase) <- str_replace_all(names(event_phase), 
                                  pattern = "Phase",
                                  replacement = "Match")
  write.csv(event_phase, output_path,
            row.names = FALSE)
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

  output_path <- paste0("Data/Sport-Schedules/", id, "_Sport_Schedule.csv")
  write.csv(sport_schedule, output_path,
            row.names = FALSE)
}
