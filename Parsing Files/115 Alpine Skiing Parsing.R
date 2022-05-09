# Alpine Skiing Parsing

# Libraries
library(tidyverse)
library(jsonlite)
library(lubridate)

# Read in All File Names
# Code Stolen From:
# https://www.geeksforgeeks.org/read-all-files-in-directory-using-r/#:~:text=To%20list%20all%20files%20in,files%20in%20the%20specified%20directories.
all_files <- list.files(path = "Data/115-JSONs")
all_files <- paste0("Data/115-JSONs/", all_files)

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
  
  
  # Get Results
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
  
  # These Two should be empty lists, so unlist to get rid of them
  Results$TeamMemberList <- unlist(Results$TeamMemberList)
  Results$PhaseResultList <- unlist(Results$PhaseResultList)
  
  if(str_detect(Event, pattern = "Team")) {
    # Make it so there is only 1 team per row rather than 2
    Results <- Results %>% 
      summarize(ParticipantType = c(n_ParticipantType1, n_ParticipantType2),
                ParticipantID = c(n_ParticipantID1, n_ParticipantID2),
                Participant = c(c_Participant1, c_Participant2),
                ParticipantShort = c(c_ParticipantShort1, c_ParticipantShort2),
                ParticipantLastName = c(c_ParticipantLastName1, c_ParticipantLastName2),
                c_Bib = c(c_Bib1, c_Bib2),
                StartOrder = c(n_StartOrder1, n_StartOrder2),
                NOCID = c(NOC.n_ID1, NOC.n_ID2),
                NOCGeoID = c(NOC.n_GeoID1, NOC.n_GeoID2),
                NOCName = c(NOC.c_Name1, NOC.c_Name2),
                NOCShort = c(NOC.c_Short1, NOC.c_Short2),
                n_Rank = c(n_Rank1, n_Rank2),
                n_RankSort = c(n_RankSort1, n_RankSort2),
                c_Rank = c(c_Rank1, c_Rank2),
                c_Result = c(c_Result1, c_Result2),
                c_ResultAbs = c(c_ResultAbs1, c_ResultAbs2),
                n_PointsAbs = c(n_PointsAbs1, n_PointsAbs2),
                c_ODF_QualificationMark = c(c_ODF_QualificationMark1, c_ODF_QualificationMark2),
                c_ResultInfo_1 = c(c_ResultInfo_11, c_ResultInfo_12),
                c_ResultInfo_2 = c(c_ResultInfo_21, c_ResultInfo_22),
                c_ResultInfo_3 = c(c_ResultInfo_31, c_ResultInfo_32),
                c_ResultInfo_4 = c(c_ResultInfo_41, c_ResultInfo_42),
                c_ResultInfo_5 = c(c_ResultInfo_51, c_ResultInfo_52),
                b_Completed = c(b_Completed1, b_Completed2),
                b_Upcoming = c(b_Upcoming1, b_Upcoming2),
                TeamMemberList.n_Position1 = c(TeamMemberList.n_Position1, TeamMemberList.n_Position1.1),
                TeamMemberList.n_Position2 = c(TeamMemberList.n_Position2, TeamMemberList.n_Position2.1),
                TeamMemberList.n_Position3 = c(TeamMemberList.n_Position3, TeamMemberList.n_Position3.1),
                TeamMemberList.n_Position4 = c(TeamMemberList.n_Position4, TeamMemberList.n_Position4.1),
                TeamMemberList.n_PersonID1 = c(TeamMemberList.n_PersonID1, TeamMemberList.n_PersonID1.1),
                TeamMemberList.n_PersonID2 = c(TeamMemberList.n_PersonID2, TeamMemberList.n_PersonID2.1),
                TeamMemberList.n_PersonID3 = c(TeamMemberList.n_PersonID3, TeamMemberList.n_PersonID3.1),
                TeamMemberList.n_PersonID4 = c(TeamMemberList.n_PersonID4, TeamMemberList.n_PersonID4.1),
                TeamMemberList.c_Person1 = c(TeamMemberList.c_Person1, TeamMemberList.c_Person1.1),
                TeamMemberList.c_Person2 = c(TeamMemberList.c_Person2, TeamMemberList.c_Person2.1),
                TeamMemberList.c_Person3 = c(TeamMemberList.c_Person3, TeamMemberList.c_Person3.1),
                TeamMemberList.c_Person4 = c(TeamMemberList.c_Person4, TeamMemberList.c_Person4.1),
                TeamMemberList.c_PersonFirstName1 = c(TeamMemberList.c_PersonFirstName1, TeamMemberList.c_PersonFirstName1.1),
                TeamMemberList.c_PersonFirstName2 = c(TeamMemberList.c_PersonFirstName2, TeamMemberList.c_PersonFirstName2.1),
                TeamMemberList.c_PersonFirstName3 = c(TeamMemberList.c_PersonFirstName3, TeamMemberList.c_PersonFirstName3.1),
                TeamMemberList.c_PersonFirstName4 = c(TeamMemberList.c_PersonFirstName4, TeamMemberList.c_PersonFirstName4.1),
                TeamMemberList.c_PersonLastName1 = c(TeamMemberList.c_PersonLastName1, TeamMemberList.c_PersonLastName1.1),
                TeamMemberList.c_PersonLastName2 = c(TeamMemberList.c_PersonLastName2, TeamMemberList.c_PersonLastName2.1),
                TeamMemberList.c_PersonLastName3 = c(TeamMemberList.c_PersonLastName3, TeamMemberList.c_PersonLastName3.1),
                TeamMemberList.c_PersonLastName4 = c(TeamMemberList.c_PersonLastName4, TeamMemberList.c_PersonLastName4.1),
                TeamMemberList.c_PersonShort1 = c(TeamMemberList.c_PersonShort1, TeamMemberList.c_PersonShort1.1),
                TeamMemberList.c_PersonShort2 = c(TeamMemberList.c_PersonShort2, TeamMemberList.c_PersonShort2.1),
                TeamMemberList.c_PersonShort3 = c(TeamMemberList.c_PersonShort3, TeamMemberList.c_PersonShort3.1),
                TeamMemberList.c_PersonShort4 = c(TeamMemberList.c_PersonShort4, TeamMemberList.c_PersonShort4.1),
                TeamMemberList.c_Bib1 = c(TeamMemberList.c_Bib1, TeamMemberList.c_Bib1.1),
                TeamMemberList.c_Bib2 = c(TeamMemberList.c_Bib2, TeamMemberList.c_Bib2.1),
                TeamMemberList.c_Bib3 = c(TeamMemberList.c_Bib3, TeamMemberList.c_Bib3.1),
                TeamMemberList.c_Bib4 = c(TeamMemberList.c_Bib4, TeamMemberList.c_Bib4.1)
      ) # End of Summarize
  }
  
  # Write to CSV
  # Wrapped in a unique because we only need 1 filename
  # The team event stuff was being a little silly
  output_file_name <- unique(paste0("Data/115-CSVs/", 
                                    MatchID, ".csv"))
  
  write.csv(x = Results, 
            file = output_file_name,
            row.names = FALSE)
  
  # Print that it worked
  print(paste(MatchID, "was a Success", "/n"))
  
}
