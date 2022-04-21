# Bobsled Parsing
library(tidyverse)
library(jsonlite)
library(lubridate)

# Read in All File Names
# Code Stolen From:
# https://www.geeksforgeeks.org/read-all-files-in-directory-using-r/#:~:text=To%20list%20all%20files%20in,files%20in%20the%20specified%20directories.
all_files <- list.files(path = "Output Folder/213 Bobsled JSONs",
                        # To make sure I grab only the relevant files 
                        pattern = "Match ID")


# Comment Out For Loop For now
for (json_file_name in all_files){
  # Sanity Check
  print(json_file_name)
  
  # Read in the json file
  # Don't know how this works, but it does.
  # Stolen From Stack Overflow:
  # https://stackoverflow.com/questions/38074926/unable-to-parse-locally-stored-json-file-with-special-character-like-backslash
  file_path <- paste0("Output Folder/213 Bobsled JSONs/", json_file_name)
  raw_json <- fromJSON(gsub("\\\\","",readLines(file_path)))
  
  # Date of Match and Gender
  Date <- raw_json$Result$PhaseList$DateTimes$Start$c_Local
  # Gender is in the json file name
  Event <- str_split(json_file_name, 
                     pattern = "Match ID", 
                     n = 2)[[1]][1] %>% 
    # Trim Whitespace
    str_trim()
  
  # Get Basic Results
  Results <- raw_json$Result$PhaseList$ParticipantList %>% 
    as.data.frame()
  
  # Phase List needs to be unlisted
  Results$PhaseResultList <- unlist(Results$PhaseResultList)
  
  # Monobob doesn't have a team member list since there is only 1 person
  # Hence the If-Else
  if(str_detect(Event, pattern = "Monobob", negate = TRUE)){
    # Make a Key Variable For joining on Team members
    Results <- Results %>%  mutate(key = 1:nrow(Results))
    
    # Results looks ok, just have to figure out the team members now 
    TeamMembers <- Results$TeamMemberList
    TeamMembers <- lapply(TeamMembers, unlist)
    TeamMembers <- lapply(TeamMembers, FUN = function(x){ data.frame(t(x), stringsAsFactors = F) })
    TeamMembers <- do.call("bind_rows", TeamMembers)
    # Mutate On key to join with Results
    TeamMembers <- TeamMembers %>%  mutate(key = 1:nrow(TeamMembers))
    
    # Now I need to Join back the Team Members to the Results
    Full_Results <- full_join(Results, TeamMembers, by = "key") %>% 
      select(-key, -TeamMemberList)
  } else {
    Results$TeamMemberList <- unlist(Results$TeamMemberList)
    Full_Results <- Results
  }
  
  # Write to CSV
  # Wrapped in a unique because we only need 1 filename
  # The team event stuff was being a little silly
  output_file_name <- unique(paste0("Output Folder/213 Bobsled CSVs/", 
                                    # Only the actual date, not the time of game
                                    Event, "-", substr(Date, 1,10), ".csv"))
  
  write.csv(x = Full_Results, 
            file = output_file_name,
            row.names = FALSE)
  print(paste(Event, "was a Success", "/n"))
}
