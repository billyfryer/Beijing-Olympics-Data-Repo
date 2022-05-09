# Speed Skating Parsing

# Libraries
library(tidyverse)
library(jsonlite)
library(lubridate)

# Read in All File Names
# Code Stolen From:
# https://www.geeksforgeeks.org/read-all-files-in-directory-using-r/#:~:text=To%20list%20all%20files%20in,files%20in%20the%20specified%20directories.
all_files <- list.files(path = "Data/103-JSONs")
url_starter <- "https://raw.githubusercontent.com/b4billy/Beijing-Olympics-Data-Repo/main/Data/103-JSONs/"
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
  
  # This should be an empty list, so unlist to get rid of it
  Results$PhaseResultList <- unlist(Results$PhaseResultList)
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
  
  
  # In Team Events, this gets a little messy, so separate
  #if(i == 11) {
  #  
  #  TeamMemberList <- Results$TeamMemberList %>% 
  #    as.data.frame() %>% 
  #    summarise(key = rep(0:7, each = 3),
  #              n_Position = c(n_Position,n_Position.1,n_Position.2,n_Position.3,
  #                             n_Position.4,n_Position.5,n_Position.6,n_Position.7),
  #              n_PersonID = c(n_PersonID,n_PersonID.1,n_PersonID.2,n_PersonID.3,
  #                             n_PersonID.4,n_PersonID.5,n_PersonID.6,n_PersonID.7),
  #              c_Person = c(c_Person,c_Person.1,c_Person.2,c_Person.3,
  #                             c_Person.4,c_Person.5,c_Person.6,c_Person.7),
  #              c_PersonFirstName = c(c_PersonFirstName,c_PersonFirstName.1,c_PersonFirstName.2,c_PersonFirstName.3,
  #                           c_PersonFirstName.4,c_PersonFirstName.5,c_PersonFirstName.6,c_PersonFirstName.7),
  #              c_PersonLastName = c(c_PersonLastName,c_PersonLastName.1,c_PersonLastName.2,c_PersonLastName.3,
  #                                    c_PersonLastName.4,c_PersonLastName.5,c_PersonLastName.6,c_PersonLastName.7),
  #              c_PersonShort = c(c_PersonShort,c_PersonShort.1,c_PersonShort.2,c_PersonShort.3,
  #                           c_PersonShort.4,c_PersonShort.5,c_PersonShort.6,c_PersonShort.7),
  #              c_Bib = c(c_Bib,c_Bib.1,c_Bib.2,c_Bib.3,
  #                           c_Bib.4,c_Bib.5,c_Bib.6,c_Bib.7)
  #              ) %>% 
  #    group_by(key) %>% 
  #    nest(n_Position = n_Position,
  #         n_PersonID = n_PersonID,
  #         c_Person = c_Person,
  #         c_PersonFirstName = c_PersonFirstName,
  #         c_PersonLastName = c_PersonLastName,
  #         c_PersonShort = c_PersonShort,
  #         c_Bib = c_Bib) %>% 
  #    ungroup() %>% 
  #    unnest_wider(col = -key) %>% 
  #    unnest_wider(col = -key, 
  #                 names_sep = ".")
  #  
  #  Results <- Results %>% 
  #    mutate(key = 0:7) %>% 
  #    select(-TeamMemberList) %>%
  #    left_join(., TeamMemberList, by = "key") %>% 
  #    select(-key)
  #  
  #} else if(i == 12) {
  #  # Separate Team Member List
  #  TeamMemberList <- Results %>% 
  #    select(contains("TeamMemberList")) %>% 
  #    summarize(n_Position1 = c(TeamMemberList.n_Position1, TeamMemberList.n_Position1.1),
  #              n_Position2 = c(TeamMemberList.n_Position2, TeamMemberList.n_Position2.1),
  #              n_Position3 = c(TeamMemberList.n_Position3, TeamMemberList.n_Position3.1),
  #              n_PersonID1 = c(TeamMemberList.n_PersonID1, TeamMemberList.n_PersonID1.1),
  #              n_PersonID2 = c(TeamMemberList.n_PersonID2, TeamMemberList.n_PersonID2.1),
  #              n_PersonID3 = c(TeamMemberList.n_PersonID3, TeamMemberList.n_PersonID3.1),
  #              c_Person1 = c(TeamMemberList.c_Person1, TeamMemberList.c_Person1.1),
  #              c_Person2 = c(TeamMemberList.c_Person2, TeamMemberList.c_Person2.1),
  #              c_Person3 = c(TeamMemberList.c_Person3, TeamMemberList.c_Person3.1),
  #              c_PersonFirstName1 = c(TeamMemberList.c_PersonFirstName1, TeamMemberList.c_PersonFirstName1.1),
  #              c_PersonFirstName2 = c(TeamMemberList.c_PersonFirstName2, TeamMemberList.c_PersonFirstName2.1),
  #              c_PersonFirstName3 = c(TeamMemberList.c_PersonFirstName3, TeamMemberList.c_PersonFirstName3.1),
  #              c_PersonLastName1 = c(TeamMemberList.c_PersonLastName1, TeamMemberList.c_PersonLastName1.1),
  #              c_PersonLastName2 = c(TeamMemberList.c_PersonLastName2, TeamMemberList.c_PersonLastName2.1),
  #              c_PersonLastName3 = c(TeamMemberList.c_PersonLastName3, TeamMemberList.c_PersonLastName3.1),
  #              c_PersonShort1 = c(TeamMemberList.c_PersonShort1, TeamMemberList.c_PersonShort1.1),
  #              c_PersonShort2 = c(TeamMemberList.c_PersonShort2, TeamMemberList.c_PersonShort2.1),
  #              c_PersonShort3 = c(TeamMemberList.c_PersonShort3, TeamMemberList.c_PersonShort3.1),
  #              c_Bib1 = c(TeamMemberList.c_Bib1, TeamMemberList.c_Bib1.1),
  #              c_Bib2 = c(TeamMemberList.c_Bib2, TeamMemberList.c_Bib2.1),
  #              c_Bib3 = c(TeamMemberList.c_Bib3, TeamMemberList.c_Bib3.1),
  #    ) %>% 
  #    mutate(key = 1:4)
  # 
  # Results <- Results %>% 
  #   select(!contains("TeamMemberList")) %>% 
  #   summarize(n_ParticipantType = c(n_ParticipantType1, n_ParticipantType2),
  #             n_ParticipantID = c(n_ParticipantID1, n_ParticipantID2),
  #             c_Participant = c(c_Participant1, c_Participant2),
  #             c_ParticipantShort = c(c_ParticipantShort1, c_ParticipantShort2),
  #             c_ParticipantLastName = c(c_ParticipantLastName1, c_ParticipantLastName2),
  #             n_StartOrder = c(n_StartOrder1, n_StartOrder2),
  #             NOC.n_ID = c(NOC.n_ID1, NOC.n_ID2),
  #             NOC.n_GeoID = c(NOC.n_GeoID1, NOC.n_GeoID2),
  #             NOC.c_Name = c(NOC.c_Name1, NOC.c_Name2),
  #             NOC.c_Short = c(NOC.c_Short1, NOC.c_Short2),
  #             n_Rank = c(n_Rank1, n_Rank2),
  #             n_RankSort = c(n_RankSort1, n_RankSort2),
  #             c_Rank = c(c_Rank1, c_Rank2),
  #             c_Result = c(c_Result1, c_Result2),
  #             c_ResultAbs = c(c_ResultAbs1, c_ResultAbs2),
  #             n_TimeRel = c(n_TimeRel1, n_TimeRel2),
  #             c_ODF_QualificationMark = c(c_ODF_QualificationMark1, c_ODF_QualificationMark2),
  #             b_Completed = c(b_Completed1, b_Completed2),
  #             b_Upcoming = c(b_Upcoming1, b_Upcoming2)
  #             ) %>% 
  #   mutate(key = 1:4)
  # 
  # Results <- left_join(Results, TeamMemberList, by = "key") %>% 
  #   select(-key)
  # } else {
  #   Results$TeamMemberList <- unlist(Results$TeamMemberList)
  #   Results <- Results %>% unnest_wider(NOC)
  # }
  
  # Write to CSV
  # Wrapped in a unique because we only need 1 filename
  # The team event stuff was being a little silly
  output_file_name <- unique(paste0("Data/103-CSVs/", 
                                    MatchID, ".csv"))
  
  write.csv(x = Results, 
            file = output_file_name,
            row.names = FALSE)
  
  # Print that it worked
  print(paste(MatchID, "was a Success", "/n"))
}
