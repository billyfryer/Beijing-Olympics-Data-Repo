# Freestyle Skiing Parsing

# Libraries
library(tidyverse)
library(jsonlite)
library(lubridate)

# Read in All File Names
# Code Stolen From:
# https://www.geeksforgeeks.org/read-all-files-in-directory-using-r/#:~:text=To%20list%20all%20files%20in,files%20in%20the%20specified%20directories.
all_files <- list.files(path = "Data/221 Freestyle Skiing JSONs")


for (json_file_name in all_files){
  
  # Sanity Check
  print(json_file_name)
  
  # Read in the json file
  # Don't know how this works, but it does.
  # Stolen From Stack Overflow:
  # https://stackoverflow.com/questions/38074926/unable-to-parse-locally-stored-json-file-with-special-character-like-backslash
  file_path <- paste0("Data/221 Freestyle Skiing JSONs/", json_file_name)
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
  
  # Two Different Types: Team/Relay and Everything else
  if(str_detect(Event, pattern = "Team")) {
    
    Full_Results <- Results %>% 
      summarize(n_ParticipantType = c(n_ParticipantType1,n_ParticipantType2, 
                                    n_ParticipantType3,n_ParticipantType4,
                                    n_ParticipantType5,n_ParticipantType6),
                n_ParticipantID = c(n_ParticipantID1,n_ParticipantID2, 
                                    n_ParticipantID3,n_ParticipantID4,
                                    n_ParticipantID5,n_ParticipantID6),
                c_Participant = c(c_Participant1,c_Participant2, 
                                    c_Participant3,c_Participant4,
                                    c_Participant5,c_Participant6),
                c_ParticipantShort = c(c_ParticipantShort1,c_ParticipantShort2, 
                                  c_ParticipantShort3,c_ParticipantShort4,
                                  c_ParticipantShort5,c_ParticipantShort6),
                c_ParticipantLastName = c(c_ParticipantLastName1,c_ParticipantLastName2, 
                                       c_ParticipantLastName3,c_ParticipantLastName4,
                                       c_ParticipantLastName5,c_ParticipantLastName6),
                c_Bib = c(c_Bib1,c_Bib2, 
                            c_Bib3,c_Bib4,
                            c_Bib5,c_Bib6),
                n_StartOrder = c(n_StartOrder1,n_StartOrder2, 
                                    n_StartOrder3,n_StartOrder4,
                                    n_StartOrder5,n_StartOrder6),
                NOC.n_ID = c(NOC.n_ID1, NOC.n_ID2, NOC.n_ID3,
                             NOC.n_ID4, NOC.n_ID5, NOC.n_ID6),
                NOC.n_GeoID = c(NOC.n_GeoID1, NOC.n_GeoID2, NOC.n_GeoID3,
                             NOC.n_GeoID4, NOC.n_GeoID5, NOC.n_GeoID6),
                NOC.c_Name = c(NOC.c_Name1, NOC.c_Name2, NOC.c_Name3,
                               NOC.c_Name4, NOC.c_Name5, NOC.c_Name6),
                NOC.c_Short = c(NOC.c_Short1, NOC.c_Short2, NOC.c_Short3,
                               NOC.c_Short4, NOC.c_Short5, NOC.c_Short6),
                n_Rank = c(n_Rank1, n_Rank2, n_Rank3,
                           n_Rank4, n_Rank5, n_Rank6),
                n_RankSort = c(n_RankSort1, n_RankSort2, n_RankSort3,
                               n_RankSort4, n_RankSort5, n_RankSort6),
                c_Rank = c(c_Rank1, c_Rank2, c_Rank3,
                           c_Rank4, c_Rank5, c_Rank6),
                c_Result = c(c_Result1, c_Result2, c_Result3,
                           c_Result4, c_Result5, c_Result6),
                c_ResultAbs = c(c_ResultAbs1, c_ResultAbs2, c_ResultAbs3,
                                c_ResultAbs4, c_ResultAbs5, c_ResultAbs6),
                n_PointsAbs = c(n_PointsAbs1, n_PointsAbs2, n_PointsAbs3,
                                n_PointsAbs4, n_PointsAbs5, n_PointsAbs6),
                c_ODF_QualificationMark = c(c_ODF_QualificationMark1, c_ODF_QualificationMark2,
                                            c_ODF_QualificationMark3, c_ODF_QualificationMark4,
                                            c_ODF_QualificationMark5, c_ODF_QualificationMark6),
                b_Completed = c(b_Completed1, b_Completed2, b_Completed3,
                                b_Completed4, b_Completed5, b_Completed6),
                b_Upcoming = c(b_Upcoming1, b_Upcoming2, b_Upcoming3,
                                b_Upcoming4, b_Upcoming5, b_Upcoming6),
                n_BDF_VideoSessionID = c(n_BDF_VideoSessionID1, n_BDF_VideoSessionID2,
                                         n_BDF_VideoSessionID3, n_BDF_VideoSessionID4,
                                         n_BDF_VideoSessionID5, n_BDF_VideoSessionID6),
                TeamMemberList.n_Position1 = c(TeamMemberList.n_Position1, TeamMemberList.n_Position1.1,
                                               TeamMemberList.n_Position1.2, TeamMemberList.n_Position1.3,
                                               TeamMemberList.n_Position1.4, TeamMemberList.n_Position1.5),
                TeamMemberList.n_Position2 = c(TeamMemberList.n_Position2, TeamMemberList.n_Position2.1,
                                               TeamMemberList.n_Position2.2, TeamMemberList.n_Position2.3,
                                               TeamMemberList.n_Position2.4, TeamMemberList.n_Position2.5),
                TeamMemberList.n_Position3 = c(TeamMemberList.n_Position3, TeamMemberList.n_Position3.1,
                                               TeamMemberList.n_Position3.2, TeamMemberList.n_Position3.3,
                                               TeamMemberList.n_Position3.4, TeamMemberList.n_Position3.5),
                TeamMemberList.c_Person1 = c(TeamMemberList.c_Person1, TeamMemberList.c_Person1.1,
                                               TeamMemberList.c_Person1.2, TeamMemberList.c_Person1.3,
                                               TeamMemberList.c_Person1.4, TeamMemberList.c_Person1.5),
                TeamMemberList.c_Person2 = c(TeamMemberList.c_Person2, TeamMemberList.c_Person2.1,
                                               TeamMemberList.c_Person2.2, TeamMemberList.c_Person2.3,
                                               TeamMemberList.c_Person2.4, TeamMemberList.c_Person2.5),
                TeamMemberList.c_Person3 = c(TeamMemberList.c_Person3, TeamMemberList.c_Person3.1,
                                               TeamMemberList.c_Person3.2, TeamMemberList.c_Person3.3,
                                               TeamMemberList.c_Person3.4, TeamMemberList.c_Person3.5),
                TeamMemberList.c_PersonFirstName1 = c(TeamMemberList.c_PersonFirstName1, TeamMemberList.c_PersonFirstName1.1,
                                               TeamMemberList.c_PersonFirstName1.2, TeamMemberList.c_PersonFirstName1.3,
                                               TeamMemberList.c_PersonFirstName1.4, TeamMemberList.c_PersonFirstName1.5),
                TeamMemberList.c_PersonFirstName2 = c(TeamMemberList.c_PersonFirstName2, TeamMemberList.c_PersonFirstName2.1,
                                               TeamMemberList.c_PersonFirstName2.2, TeamMemberList.c_PersonFirstName2.3,
                                               TeamMemberList.c_PersonFirstName2.4, TeamMemberList.c_PersonFirstName2.5),
                TeamMemberList.c_PersonFirstName3 = c(TeamMemberList.c_PersonFirstName3, TeamMemberList.c_PersonFirstName3.1,
                                               TeamMemberList.c_PersonFirstName3.2, TeamMemberList.c_PersonFirstName3.3,
                                               TeamMemberList.c_PersonFirstName3.4, TeamMemberList.c_PersonFirstName3.5),
                TeamMemberList.c_PersonLastName1 = c(TeamMemberList.c_PersonLastName1, TeamMemberList.c_PersonLastName1.1,
                                               TeamMemberList.c_PersonLastName1.2, TeamMemberList.c_PersonLastName1.3,
                                               TeamMemberList.c_PersonLastName1.4, TeamMemberList.c_PersonLastName1.5),
                TeamMemberList.c_PersonLastName2 = c(TeamMemberList.c_PersonLastName2, TeamMemberList.c_PersonLastName2.1,
                                               TeamMemberList.c_PersonLastName2.2, TeamMemberList.c_PersonLastName2.3,
                                               TeamMemberList.c_PersonLastName2.4, TeamMemberList.c_PersonLastName2.5),
                TeamMemberList.c_PersonLastName3 = c(TeamMemberList.c_PersonLastName3, TeamMemberList.c_PersonLastName3.1,
                                               TeamMemberList.c_PersonLastName3.2, TeamMemberList.c_PersonLastName3.3,
                                               TeamMemberList.c_PersonLastName3.4, TeamMemberList.c_PersonLastName3.5),
                TeamMemberList.c_PersonShort1 = c(TeamMemberList.c_PersonShort1, TeamMemberList.c_PersonShort1.1,
                                               TeamMemberList.c_PersonShort1.2, TeamMemberList.c_PersonShort1.3,
                                               TeamMemberList.c_PersonShort1.4, TeamMemberList.c_PersonShort1.5),
                TeamMemberList.c_PersonShort2 = c(TeamMemberList.c_PersonShort2, TeamMemberList.c_PersonShort2.1,
                                               TeamMemberList.c_PersonShort2.2, TeamMemberList.c_PersonShort2.3,
                                               TeamMemberList.c_PersonShort2.4, TeamMemberList.c_PersonShort2.5),
                TeamMemberList.c_PersonShort3 = c(TeamMemberList.c_PersonShort3, TeamMemberList.c_PersonShort3.1,
                                               TeamMemberList.c_PersonShort3.2, TeamMemberList.c_PersonShort3.3,
                                               TeamMemberList.c_PersonShort3.4, TeamMemberList.c_PersonShort3.5),
                TeamMemberList.c_Bib1 = c(TeamMemberList.c_Bib1, TeamMemberList.c_Bib1.1,
                                                  TeamMemberList.c_Bib1.2, TeamMemberList.c_Bib1.3,
                                                  TeamMemberList.c_Bib1.4, TeamMemberList.c_Bib1.5),
                TeamMemberList.c_Bib2 = c(TeamMemberList.c_Bib2, TeamMemberList.c_Bib2.1,
                                                  TeamMemberList.c_Bib2.2, TeamMemberList.c_Bib2.3,
                                                  TeamMemberList.c_Bib2.4, TeamMemberList.c_Bib2.5),
                TeamMemberList.c_Bib3 = c(TeamMemberList.c_Bib3, TeamMemberList.c_Bib3.1,
                                                  TeamMemberList.c_Bib3.2, TeamMemberList.c_Bib3.3,
                                                  TeamMemberList.c_Bib3.4, TeamMemberList.c_Bib3.5),
                TeamMemberList.n_BDF_VideoSessionID1 = c(TeamMemberList.n_BDF_VideoSessionID1, TeamMemberList.n_BDF_VideoSessionID1.1,
                                          TeamMemberList.n_BDF_VideoSessionID1.2, TeamMemberList.n_BDF_VideoSessionID1.3,
                                          TeamMemberList.n_BDF_VideoSessionID1.4, TeamMemberList.n_BDF_VideoSessionID1.5),
                TeamMemberList.n_BDF_VideoSessionID2 = c(TeamMemberList.n_BDF_VideoSessionID2, TeamMemberList.n_BDF_VideoSessionID2.1,
                                          TeamMemberList.n_BDF_VideoSessionID2.2, TeamMemberList.n_BDF_VideoSessionID2.3,
                                          TeamMemberList.n_BDF_VideoSessionID2.4, TeamMemberList.n_BDF_VideoSessionID2.5),
                TeamMemberList.n_BDF_VideoSessionID3 = c(TeamMemberList.n_BDF_VideoSessionID3, TeamMemberList.n_BDF_VideoSessionID3.1,
                                          TeamMemberList.n_BDF_VideoSessionID3.2, TeamMemberList.n_BDF_VideoSessionID3.3,
                                          TeamMemberList.n_BDF_VideoSessionID3.4, TeamMemberList.n_BDF_VideoSessionID3.5)
                )
    
    Full_Results <- Full_Results %>% filter(!is.na(n_ParticipantType))
    
  } else {
    # Individual Events Don't have a Team Member List
    Results$TeamMemberList <- unlist(Results$TeamMemberList)
    Full_Results <- Results
  }
  
  # Write to CSV
  # Wrapped in a unique because we only need 1 filename
  # The team event stuff was being a little silly
  output_file_name <- unique(paste0("Data/221 CSVs/", 
                                    MatchID, ".csv"))
  
  write.csv(x = Full_Results, 
            file = output_file_name,
            row.names = FALSE)
  
  # Print that it worked
  print(paste(Event, "was a Success", "/n"))
}
