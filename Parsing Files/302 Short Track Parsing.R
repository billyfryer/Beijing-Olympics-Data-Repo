# Short Track Parsing

# Libraries
library(tidyverse)
library(jsonlite)
library(lubridate)

key <- data.frame(Event = c(),
                  MatchID = c())

# Read in All File Names
# Code Stolen From:
# https://www.geeksforgeeks.org/read-all-files-in-directory-using-r/#:~:text=To%20list%20all%20files%20in,files%20in%20the%20specified%20directories.
all_files <- list.files(path = "Data/302 Short Track JSONs")

for (json_file_name in all_files){
  
  #json_file_name <- all_files[5]
  # Sanity Check
  print(json_file_name)
  
  # Read in the json file
  # Don't know how this works, but it does.
  # Stolen From Stack Overflow:
  # https://stackoverflow.com/questions/38074926/unable-to-parse-locally-stored-json-file-with-special-character-like-backslash
  file_path <- paste0("Data/302 Short Track JSONs/", json_file_name)
  raw_json <- fromJSON(gsub("\\\\","",readLines(file_path)))
  
  # Date of Match and Gender
  Date <- raw_json$Result$PhaseList$DateTimes$Start$c_Local
  # Get MatchID
  MatchID <- str_remove(json_file_name, pattern = ".json")
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
  
  # Always Unlist Phase Result List b/c nothing there
  Results$PhaseResultList <- unlist(Results$PhaseResultList)
  
  Results$TeamMemberList <- unlist(Results$TeamMemberList)

  # Write to CSV
  # Wrapped in a unique because we only need 1 filename
  # The team event stuff was being a little silly
  output_file_name <- unique(paste0("Data/302 Short Track CSVs/", 
                                    MatchID, ".csv"))
  
  write.csv(x = Results, 
            file = output_file_name,
            row.names = FALSE)
  
  # Print that it worked
  print(paste(Event, "was a Success", "/n"))
}