# Figure Skating Parsing

# Libraries
library(tidyverse)
library(jsonlite)
library(lubridate)

# Read in All File Names
# Code Stolen From:
# https://www.geeksforgeeks.org/read-all-files-in-directory-using-r/#:~:text=To%20list%20all%20files%20in,files%20in%20the%20specified%20directories.
all_files <- list.files(path = "Data/307-JSONs")
url_starter <- "https://raw.githubusercontent.com/b4billy/Beijing-Olympics-Data-Repo/main/Data/307-JSONs/"
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
  
  # Phase List and Team Member List needs to be unlisted
  Results$PhaseResultList <- unlist(Results$PhaseResultList)
  Results$TeamMemberList <- unlist(Results$TeamMemberList)
  Full_Results <- Results
  
  # Write to CSV
  # Wrapped in a unique because we only need 1 filename
  # The team event stuff was being a little silly
  output_file_name <- unique(paste0("Data/307-CSVs/", 
                                    MatchID, ".csv"))
  
  write.csv(x = Results, 
            file = output_file_name,
            row.names = FALSE)
  
  # Print that it worked
  print(paste(Event, "was a Success", "/n"))
}
