# Luge Parsing

# Bobsled Parsing
library(tidyverse)
library(jsonlite)
library(lubridate)

# Read in All File Names
# Code Stolen From:
# https://www.geeksforgeeks.org/read-all-files-in-directory-using-r/#:~:text=To%20list%20all%20files%20in,files%20in%20the%20specified%20directories.
all_files <- list.files(path = "Output Folder/307 Skeleton JSONs",
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
  file_path <- paste0("Output Folder/307 Skeleton JSONs/", json_file_name)
  raw_json <- fromJSON(gsub("\\\\","",readLines(file_path)))
  
  # Date of Match and Gender
  Date <- raw_json$Result$PhaseList$DateTimes$Start$c_Local
  # Gender is in the json file name
  Event <- str_split(json_file_name, 
                     pattern = "Match ID", 
                     n = 2)[[1]][1] %>% 
    # Trim Whitespace
    str_trim()
  
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
  output_file_name <- unique(paste0("Output Folder/307 Skeleton CSVs/", 
                                    # Only the actual date, not the time of game
                                    Event, "-", substr(Date, 1,10), ".csv"))
  
  write.csv(x = Full_Results, 
            file = output_file_name,
            row.names = FALSE)
  print(paste(Event, "was a Success", "/n"))
}
