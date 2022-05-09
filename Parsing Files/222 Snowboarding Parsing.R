# Snowboarding Parsing

# Libraries
library(tidyverse)
library(jsonlite)
library(lubridate)

# Read in All File Names
# Code Stolen From:
# https://www.geeksforgeeks.org/read-all-files-in-directory-using-r/#:~:text=To%20list%20all%20files%20in,files%20in%20the%20specified%20directories.
all_files <- list.files(path = "Data/222-JSONs")
all_files <- paste0("Data/222-JSONs/", all_files)


for (i in 1:length(all_files)){
  
  json_file_name <- all_files[i]
  # Sanity Check
  print(json_file_name)
  
  # Read in the json file
  # Don't know how this works, but it does.
  # Stolen From Stack Overflow:
  # https://stackoverflow.com/questions/38074926/unable-to-parse-locally-stored-json-file-with-special-character-like-backslash
  raw_json <- fromJSON(gsub("\\\\","",readLines(json_file_name)))
  
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

  # Always Unlist Phase Result List b/c nothing there
  Results$PhaseResultList <- unlist(Results$PhaseResultList)
  
  # Two Different Types: Team and Individual
  if(str_detect(Event, pattern = "Team", negate = TRUE)) {
    # Individual Events Don't have a Team Member List
    Results$TeamMemberList <- unlist(Results$TeamMemberList)
    Full_Results <- Results
  } else {
    Full_Results <- Results %>% 
      summarize(n_ParticipantType = c(n_ParticipantType1,n_ParticipantType2, 
                                      n_ParticipantType3,n_ParticipantType4),
                n_ParticipantID = c(n_ParticipantID1,n_ParticipantID2, 
                                    n_ParticipantID3,n_ParticipantID4),
                c_Participant = c(c_Participant1,c_Participant2, 
                                  c_Participant3,c_Participant4),
                c_ParticipantShort = c(c_ParticipantShort1,c_ParticipantShort2, 
                                       c_ParticipantShort3,c_ParticipantShort4),
                c_ParticipantLastName = c(c_ParticipantLastName1,c_ParticipantLastName2, 
                                          c_ParticipantLastName3,c_ParticipantLastName4),
                c_Bib = c(c_Bib1,c_Bib2, c_Bib3,c_Bib4),
                n_StartOrder = c(n_StartOrder1,n_StartOrder2,n_StartOrder3,n_StartOrder4),
                NOC.n_ID = c(NOC.n_ID1, NOC.n_ID2, NOC.n_ID3,NOC.n_ID4),
                NOC.n_GeoID = c(NOC.n_GeoID1, NOC.n_GeoID2, NOC.n_GeoID3,NOC.n_GeoID4),
                NOC.c_Name = c(NOC.c_Name1, NOC.c_Name2, NOC.c_Name3,NOC.c_Name4),
                NOC.c_Short = c(NOC.c_Short1, NOC.c_Short2, NOC.c_Short3,NOC.c_Short4),
                n_Rank = c(n_Rank1, n_Rank2, n_Rank3, n_Rank4),
                n_RankSort = c(n_RankSort1, n_RankSort2, n_RankSort3,n_RankSort4),
                c_Rank = c(c_Rank1, c_Rank2, c_Rank3,c_Rank4),
                b_Completed = c(b_Completed1, b_Completed2, b_Completed3,b_Completed4),
                b_Upcoming = c(b_Upcoming1, b_Upcoming2, b_Upcoming3,b_Upcoming4),
                TeamMemberList.n_Position1 = c(TeamMemberList.n_Position1, TeamMemberList.n_Position1.1,
                                               TeamMemberList.n_Position1.2, TeamMemberList.n_Position1.3),
                TeamMemberList.n_Position2 = c(TeamMemberList.n_Position2, TeamMemberList.n_Position2.1,
                                               TeamMemberList.n_Position2.2, TeamMemberList.n_Position2.3),
                TeamMemberList.n_PersonID1 = c(TeamMemberList.n_PersonID1, TeamMemberList.n_PersonID1.1,
                                               TeamMemberList.n_PersonID1.2, TeamMemberList.n_PersonID1.3),
                TeamMemberList.n_PersonID2 = c(TeamMemberList.n_PersonID2, TeamMemberList.n_PersonID2.1,
                                               TeamMemberList.n_PersonID2.2, TeamMemberList.n_PersonID2.3),
                TeamMemberList.c_Person1 = c(TeamMemberList.c_Person1, TeamMemberList.c_Person1.1,
                                               TeamMemberList.c_Person1.2, TeamMemberList.c_Person1.3),
                TeamMemberList.c_Person2 = c(TeamMemberList.c_Person2, TeamMemberList.c_Person2.1,
                                               TeamMemberList.c_Person2.2, TeamMemberList.c_Person2.3),
                TeamMemberList.c_PersonFirstName1 = c(TeamMemberList.c_PersonFirstName1, TeamMemberList.c_Person1.1,
                                             TeamMemberList.c_PersonFirstName1.2, TeamMemberList.c_PersonFirstName1.3),
                TeamMemberList.c_PersonFirstName2 = c(TeamMemberList.c_PersonFirstName2, TeamMemberList.c_Person2.1,
                                             TeamMemberList.c_PersonFirstName2.2, TeamMemberList.c_PersonFirstName2.3),
                TeamMemberList.c_PersonLastName1 = c(TeamMemberList.c_PersonLastName1, TeamMemberList.c_Person1.1,
                                                      TeamMemberList.c_PersonLastName1.2, TeamMemberList.c_PersonLastName1.3),
                TeamMemberList.c_PersonLastName2 = c(TeamMemberList.c_PersonLastName2, TeamMemberList.c_Person2.1,
                                                      TeamMemberList.c_PersonLastName2.2, TeamMemberList.c_PersonLastName2.3),
                TeamMemberList.c_PersonShort1 = c(TeamMemberList.c_PersonShort1, TeamMemberList.c_Person1.1,
                                                      TeamMemberList.c_PersonShort1.2, TeamMemberList.c_PersonShort1.3),
                TeamMemberList.c_PersonShort2 = c(TeamMemberList.c_PersonShort2, TeamMemberList.c_Person2.1,
                                                      TeamMemberList.c_PersonShort2.2, TeamMemberList.c_PersonShort2.3),
                TeamMemberList.c_Bib1 = c(TeamMemberList.c_Bib1, TeamMemberList.c_Person1.1,
                                                      TeamMemberList.c_Bib1.2, TeamMemberList.c_Bib1.3),
                TeamMemberList.c_Bib2 = c(TeamMemberList.c_Bib2, TeamMemberList.c_Person2.1,
                                                      TeamMemberList.c_Bib2.2, TeamMemberList.c_Bib2.3),
                c_Result = c(c_Result1, c_Result2, c_Result3, c_Result4)
                
                )
  }
  
  # Write to CSV
  # Wrapped in a unique because we only need 1 filename
  # The team event stuff was being a little silly
  output_file_name <- unique(paste0("Data/222-CSVs/", 
                                    MatchID, ".csv"))
  
  write.csv(x = Full_Results, 
            file = output_file_name,
            row.names = FALSE)
  
  # Print that it worked
  print(paste(Event, "was a Success", "/n"))
}
