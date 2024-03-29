# Cross-Country Skiing Parsing

# Libraries
library(tidyverse)
library(jsonlite)
library(lubridate)

# Read in All File Names
# Code Stolen From:
# https://www.geeksforgeeks.org/read-all-files-in-directory-using-r/#:~:text=To%20list%20all%20files%20in,files%20in%20the%20specified%20directories.
all_files <- list.files(path = "Data/220-JSONs")
url_starter <- "https://raw.githubusercontent.com/b4billy/Beijing-Olympics-Data-Repo/main/Data/220-JSONs/"
# Repeat for every file in Speed Skating
for (i in 1:length(all_files)) {
  
  json_file_name <- all_files[i]
  
  # Sanity Check
  print(json_file_name)
  
  final_url <- paste0(url_starter, json_file_name)
  
  # Not exactly sure, but Saiem does this.
  res <- httr::RETRY("GET", final_url)
  
  # To get the accents
  resp <- res %>%
    httr::content(as = "text", encoding = "UTF-8")
  
  raw_json <- jsonlite::fromJSON(resp)
  # Date of Match and Gender
  Date <- raw_json$Result$PhaseList$DateTimes$Start$c_Local
  # Get MatchID
  MatchID <- raw_json$PhaseInfo$n_PhaseID
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
  
  
  # Two Different Types: Team/Relay and Everything else
  if(str_detect(Event, pattern = "Team") |
     str_detect(Event, pattern = "Relay")) {
    
    # Results looks ok, just have to figure out the team members now 
    if(! is.null(Results$TeamMemberList)) {
      Results <- Results %>% mutate(key = 1:nrow(Results))
      TeamMembers <- Results$TeamMemberList
      TeamMembers <- lapply(TeamMembers, unlist)
      TeamMembers <- lapply(TeamMembers, FUN = function(x){ data.frame(t(x), stringsAsFactors = F) })
      TeamMembers <- do.call("bind_rows", TeamMembers)
      # Mutate On key to join with Results
      TeamMembers <- TeamMembers %>% mutate(key = 1:nrow(TeamMembers))
      
      # Now I need to Join back the Team Members to the Results
      Full_Results <- full_join(Results, TeamMembers, by = "key") %>% 
        select(-key, -TeamMemberList)
    } else {
      Full_Results <- Results
    }
  } else if(str_detect(json_file_name, pattern = "Relay")){
    
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
  } else {
    # Individual Events Don't have a Team Member List
    Results$TeamMemberList <- unlist(Results$TeamMemberList)
    Full_Results <- Results
  }
  
  # Write to CSV
  # Wrapped in a unique because we only need 1 filename
  # The team event stuff was being a little silly
  output_file_name <- unique(paste0("Data/220-CSVs/", 
                                    MatchID, ".csv"))
  
  write.csv(x = Full_Results, 
            file = output_file_name,
            row.names = FALSE)
  
  # Print that it worked
  print(paste(Event, "was a Success", "/n"))
}