# Speed Skating Parsing

# Libraries
library(tidyverse)
library(jsonlite)
library(lubridate)

# Read in All File Names
# Code Stolen From:
# https://www.geeksforgeeks.org/read-all-files-in-directory-using-r/#:~:text=To%20list%20all%20files%20in,files%20in%20the%20specified%20directories.
all_files <- list.files(path = "Output Folder/103 JSONs",
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
  file_path <- paste0("Output Folder/103 JSONs/", json_file_name)
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
  Results <- raw_json$Result$PhaseList$ParticipantList
  
  if(str_detect(Event, pattern = "Team")) {
    # In Team Events, this gets a little messy, but we'll try and do it
    # This first part is exactly the same as Alyssa's Hockey Parsing
    Results <- lapply(Results, unlist)
    Results <- lapply(Results, FUN = function(x){ data.frame(t(x),
                                                             stringsAsFactors = F) })
    Results <- do.call("bind_rows", Results)
    # Make it so there is only 1 team per row rather than 2
    Results <- Results %>% 
      summarize(ParticipantType = c(n_ParticipantType1, n_ParticipantType2),
                ParticipantID = c(n_ParticipantID1, n_ParticipantID2),
                Participant = c(c_Participant1, c_Participant2),
                ParticipantShort = c(c_ParticipantShort1, c_ParticipantShort2),
                ParticipantLastName = c(c_ParticipantLastName1, c_ParticipantLastName2),
                StartOrder = c(c_ParticipantLastName1, c_ParticipantLastName2),
                NOCID = c(NOC.n_ID1, NOC.n_ID2),
                NOCGeoID = c(NOC.n_GeoID1, NOC.n_GeoID2),
                NOCName = c(NOC.c_Name1, NOC.c_Name2),
                NOCShort = c(NOC.c_Short1, NOC.c_Short2),
                n_Rank = c(n_Rank1, n_Rank2),
                n_RankSort = c(n_RankSort1, n_RankSort2),
                c_Rank = c(c_Rank1, c_Rank2),
                c_Result = c(c_Result1, c_Result2),
                c_ResultAbs = c(c_ResultAbs1, c_ResultAbs2),
                n_TimeAbs = c(n_TimeAbs1, n_TimeAbs2),
                n_TimeRel = c(n_TimeRel1, n_TimeRel2),
                b_Completed = c(b_Completed1, b_Completed2),
                b_Upcoming = c(b_Upcoming1, b_Upcoming2),
                TeamMemberList.n_Position1 = c(TeamMemberList.n_Position1, TeamMemberList.n_Position1.1),
                TeamMemberList.n_Position2 = c(TeamMemberList.n_Position2, TeamMemberList.n_Position2.1),
                TeamMemberList.n_Position3 = c(TeamMemberList.n_Position3, TeamMemberList.n_Position3.1),
                TeamMemberList.n_PersonID1 = c(TeamMemberList.n_PersonID1, TeamMemberList.n_PersonID1.1),
                TeamMemberList.n_PersonID2 = c(TeamMemberList.n_PersonID2, TeamMemberList.n_PersonID2.1),
                TeamMemberList.n_PersonID3 = c(TeamMemberList.n_PersonID3, TeamMemberList.n_PersonID3.1),
                TeamMemberList.c_Person1 = c(TeamMemberList.c_Person1, TeamMemberList.c_Person1.1),
                TeamMemberList.c_Person2 = c(TeamMemberList.c_Person2, TeamMemberList.c_Person2.1),
                TeamMemberList.c_Person3 = c(TeamMemberList.c_Person3, TeamMemberList.c_Person3.1),
                TeamMemberList.c_PersonFirstName1 = c(TeamMemberList.c_PersonFirstName1, TeamMemberList.c_PersonFirstName1.1),
                TeamMemberList.c_PersonFirstName2 = c(TeamMemberList.c_PersonFirstName2, TeamMemberList.c_PersonFirstName2.1),
                TeamMemberList.c_PersonFirstName3 = c(TeamMemberList.c_PersonFirstName3, TeamMemberList.c_PersonFirstName3.1),
                TeamMemberList.c_PersonLastName1 = c(TeamMemberList.c_PersonLastName1, TeamMemberList.c_PersonLastName1.1),
                TeamMemberList.c_PersonLastName2 = c(TeamMemberList.c_PersonLastName2, TeamMemberList.c_PersonLastName2.1),
                TeamMemberList.c_PersonLastName3 = c(TeamMemberList.c_PersonLastName3, TeamMemberList.c_PersonLastName3.1),
                TeamMemberList.c_PersonShort1 = c(TeamMemberList.c_PersonShort1, TeamMemberList.c_PersonShort1.1),
                TeamMemberList.c_PersonShort2 = c(TeamMemberList.c_PersonShort2, TeamMemberList.c_PersonShort2.1),
                TeamMemberList.c_PersonShort3 = c(TeamMemberList.c_PersonShort3, TeamMemberList.c_PersonShort3.1),
                TeamMemberList.c_Bib1 = c(TeamMemberList.c_Bib1, TeamMemberList.c_Bib1.1),
                TeamMemberList.c_Bib2 = c(TeamMemberList.c_Bib2, TeamMemberList.c_Bib2.1),
                TeamMemberList.c_Bib3 = c(TeamMemberList.c_Bib3, TeamMemberList.c_Bib3.1)
                ) # End of Summarize
  } else {
    Results <- Results %>% as.data.frame()
    # These Two should be empty lists, so unlist to get rid of them
    Results$TeamMemberList <- unlist(Results$TeamMemberList)
    Results$PhaseResultList <- unlist(Results$PhaseResultList)
  }

  # Write to CSV
  # Wrapped in a unique because we only need 1 filename
  # The team event stuff was being a little silly
  output_file_name <- unique(paste0("Output Folder/103 CSVs/", 
                           # Only the actual date, not the time of game
                           Event, "-", substr(Date, 1,10), ".csv"))
  
  write.csv(x = Results, 
           file = output_file_name,
           row.names = FALSE)
  cat(Event, "was a Success")
}