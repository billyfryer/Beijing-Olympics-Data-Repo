# Figure Skating Parsing

# Libraries
library(tidyverse)
library(jsonlite)
library(lubridate)

# Read in All File Names
# Code Stolen From:
# https://www.geeksforgeeks.org/read-all-files-in-directory-using-r/#:~:text=To%20list%20all%20files%20in,files%20in%20the%20specified%20directories.
all_files <- list.files(path = "Data/217-JSONs")
url_starter <- "https://raw.githubusercontent.com/b4billy/Beijing-Olympics-Data-Repo/main/Data/217-JSONs/"
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
  Event <- paste(raw_json$PhaseInfo$Event$c_Name, raw_json$PhaseInfo$c_Phase)
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
  # Two Different Types: Multiple People in once routine vs only one
  # This was stupidly hard to figure out
  if(!is_empty(Results$TeamMemberList[[1]])) {
   # Team Member List
   # Make a dataframe
   TeamMemberList <- Results$TeamMemberList %>% 
     do.call("bind_rows", .) %>%
     # Make Wide
     mutate(key = rep(1:(nrow(.)/2), each = 2)) %>% 
    group_by(key) %>% 
     nest() %>% 
     ungroup() %>% 
     unnest_wider(col = -key) %>% 
     unnest_wider(col = -key, 
                  names_sep = ".")
   
   Results <- Results %>% 
     select(-TeamMemberList) %>% 
     mutate(key = 1:nrow(.))
   
   Full_Results <- left_join(Results, TeamMemberList, by = "key")
  } else {
    Results$TeamMemberList <- unlist(Results$TeamMemberList)
    Full_Results <- Results
   }
  
  # Write to CSV
  # Wrapped in a unique because we only need 1 filename
  # The team event stuff was being a little silly
  output_file_name <- unique(paste0("Data/217-CSVs/", 
                                    MatchID, ".csv"))
  
  write.csv(x = Full_Results, 
            file = output_file_name,
            row.names = FALSE)
  
  # Print that it worked
  print(paste(Event, "was a Success", "/n"))
}