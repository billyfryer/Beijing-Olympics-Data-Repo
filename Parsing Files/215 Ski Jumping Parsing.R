# Ski Jumping Parsing

# Libraries
library(tidyverse)
library(jsonlite)
library(lubridate)

# Read in All File Names
# Code Stolen From:
# https://www.geeksforgeeks.org/read-all-files-in-directory-using-r/#:~:text=To%20list%20all%20files%20in,files%20in%20the%20specified%20directories.
all_files <- list.files(path = "Data/215 Ski Jumping JSONs")

for (json_file_name in all_files){
  
  # Sanity Check
  print(json_file_name)
  
  # Read in the json file
  # Don't know how this works, but it does.
  # Stolen From Stack Overflow:
  # https://stackoverflow.com/questions/38074926/unable-to-parse-locally-stored-json-file-with-special-character-like-backslash
  file_path <- paste0("Data/215 SKi Jumping JSONs/", json_file_name)
  raw_json <- fromJSON(gsub("\\\\","",readLines(file_path)))
  
  # Date of Match and Gender
  Date <- raw_json$Result$PhaseList$DateTimes$Start$c_Local
  # Get MatchID
  MatchID <- str_remove(json_file_name, pattern = ".json")
  # Event
  Event <- raw_json$PhaseInfo$Event$c_Name

  # Get Basic Results
  Results <- raw_json$Result$PhaseList$ParticipantList
  
  # Do call if necessary, otherwise do as.data.frame
  if (length(Results) == 1) {
    # Easy Way
    Results <- Results %>% as.data.frame()
  } else {
    # Harder Way
    Results <- lapply(Results, unlist)
    Results <- lapply(Results, FUN = function(x){ data.frame(t(x),
                                                             stringsAsFactors = F) })
    Results <- do.call("bind_rows", Results)
  }  
  # Always Unlist Phase Result List b/c nothing there
  Results$PhaseResultList <- unlist(Results$PhaseResultList)
  

  # Two Different Types: Team and Individual
  if(str_detect(Event, pattern = "Individual")) {
    # Individual Events Don't have a Team Member List
    Results$TeamMemberList <- unlist(Results$TeamMemberList)
    Full_Results <- Results
  } else {
    # Make a Key Variable For joining on Team members
    Results <- Results %>% mutate(key = 1:nrow(Results))
    
    # Results looks ok, just have to figure out the team members now 
    TeamMembers <- Results$TeamMemberList
    TeamMembers <- lapply(TeamMembers, unlist)
    TeamMembers <- lapply(TeamMembers, FUN = function(x){ data.frame(t(x), stringsAsFactors = F) })
    TeamMembers <- do.call("bind_rows", TeamMembers)
    # Mutate On key to join with Results
    TeamMembers <- TeamMembers %>% mutate(key = 1:nrow(TeamMembers))
    
    # Now I need to Join back the Team Members to the Results
    Full_Results <- full_join(Results, TeamMembers, by = "key") %>% 
      select(-key, -TeamMemberList)
  }
  
  # Write to CSV
  # Wrapped in a unique because we only need 1 filename
  # The team event stuff was being a little silly
  output_file_name <- unique(paste0("Data/215 Ski Jumping CSVs/", 
                                    MatchID, ".csv"))
  
  write.csv(x = Full_Results, 
            file = output_file_name,
            row.names = FALSE)
  
  # Print that it worked
  print(paste(Event, "was a Success", "/n"))

}