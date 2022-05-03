# Hockey Parsing File
# Original Code Courtesy of Alyssa Longmuir

# Libraries
library(tidyverse)
library(jsonlite)
library(lubridate)

# Read in All File Names
# Code Stolen From:
# https://www.geeksforgeeks.org/read-all-files-in-directory-using-r/#:~:text=To%20list%20all%20files%20in,files%20in%20the%20specified%20directories.
all_files <- list.files(path = "Data/113 Hockey JSONs")

# Key DF for matching event and matchid
key <- data.frame(Event = c(),
                  MatchID = c(),
                  Team1 = c(),
                  Team2 = c())

for (json_file_name in all_files) {
  
  # Sanity Check
  print(json_file_name)
  
  # Read in the json file
  # Don't know how this works, but it does.
  # Stolen From Stack Overflow:
  # https://stackoverflow.com/questions/38074926/unable-to-parse-locally-stored-json-file-with-special-character-like-backslash
  file_path <- paste0("Data/113 Hockey JSONs/", json_file_name)
  raw_json <- fromJSON(gsub("\\\\","",readLines(file_path)))
  
  # Date of Match and Gender
  Date <- raw_json$MatchInfo$StartDateTime$c_Local

  # Gender is in the json file name
  Event <- raw_json$MatchInfo$GenderEvent$c_Name
  
  # Match ID
  MatchID <- str_remove(json_file_name, pattern = ".json")
  
  ###################################################
  # Team 1 Data

  # Team Name
  Team1 <- raw_json$MatchInfo$Competitor1$c_Short
  # Players
  team1_players <- raw_json$PersonStatistics$Competitor1PersonList
  ### Statistcs
  team1_stats <- team1_players$StatisticsList
  # Unlist
  team1_stats <- lapply(team1_stats, unlist)
  # Convert to a Data Frame
  team1_stats <- lapply(team1_stats, FUN = function(x){ data.frame(t(x),
                                                                   stringsAsFactors = F) })
  # Bind Rows Together to make a dataframe from all these lists 
  team1_stats<- do.call("bind_rows", team1_stats)
  # Collapse these columns a bit to have 1 row per player
  team1_stats <- team1_stats %>%
    summarise(c_Value1, c_Value2, c_Value3, c_Value4,
              c_Value5, c_Value6, c_Value7, c_Value8,
              c_Value9, c_Value10, c_Value11, c_Value12, c_Value13)
  # Add on Player names to Stats
  team1_stats <- cbind(team1_players,team1_stats)
  # Change Variable Names
  team1_stats<- team1_stats %>%
    summarise(ID = n_PersonID, 
              Tm = Team1, 
              First_Name = c_PersonFirstName, 
              Last_Name = c_PersonLastName, 
              No = c_ShirtNr,
              Pos = c_FunctionShort, 
              G = c_Value1,
              A = c_Value3, 
              SOG = c_Value2, 
              PIM = c_Value5, 
              TOI = c_Value7, 
              FOW = c_Value10, 
              FOL = c_Value11)
  
  ###################################################
  # Team 2 Data
  # Team Name
  Team2 <- raw_json$MatchInfo$Competitor2$c_Short
  # Players
  team2_players <- raw_json$PersonStatistics$Competitor2PersonList
  ### Statistcs
  team2_stats <- team2_players$StatisticsList
  # Unlist
  team2_stats <- lapply(team2_stats, unlist)
  # Convert to a Data Frame
  team2_stats <- lapply(team2_stats, FUN = function(x){ data.frame(t(x), 
                                                                   stringsAsFactors = F) })
  # Bind Rows Together to make a dataframe from all these lists 
  team2_stats<- do.call("bind_rows", team2_stats)
  # Collapse these columns a bit to have 1 row per player
  team2_stats <- team2_stats %>%
    summarise(c_Value1, c_Value2, c_Value3, c_Value4,
              c_Value5, c_Value6, c_Value7, c_Value8,
              c_Value9, c_Value10, c_Value11, c_Value12, c_Value13)
  # Add on Player names to Stats
  team2_stats <- cbind(team2_players,team2_stats)
  # Change Variable Names
  team2_stats<- team2_stats %>%
    summarise(ID = n_PersonID, 
              Tm = Team2, 
              First_Name = c_PersonFirstName, 
              Last_Name = c_PersonLastName, 
              No = c_ShirtNr,
              Pos = c_FunctionShort, 
              G = c_Value1,
              A = c_Value3, 
              SOG = c_Value2, 
              PIM = c_Value5, 
              TOI = c_Value7, 
              FOW = c_Value10, 
              FOL = c_Value11)
  ###################################################
  # Play By Play
  
  # From the Raw JSON, get the PBP
  pbp <- raw_json$PlayByPlay$ActionList
  # Unlist a little bit
  pbp <- lapply(pbp, unlist)
  # Convert to a Dataframe
  pbp <- lapply(pbp, FUN = function(x){ data.frame(t(x), 
                                                   stringsAsFactors = F) })
  
  
  ### Calculating Secondary Assists
  pbp_player3 <- (pbp$n_SubPerson2ID)
  pbp_event <- (pbp$c_Action)
  Secondary_Assists <- rbind(pbp_player3,pbp_event)
  
  # ONLY  IF nrow(Secondary_Assists > 1)
  if(nrow(Secondary_Assists) > 1){
  Secondary_Assists <- data.frame(t(Secondary_Assists)) %>%
    # Rename some variables
    summarise(ID = X1, 
              Event_Type = X2) %>%
    # Only Goals
    filter(Event_Type == "Goal") %>%
    # Group By Player ID
    group_by(ID) %>%
      # Get rid of NAs
     drop_na() %>%
      # Count the number of secondary assists per player
      mutate(A2 = (length(Event_Type))) %>%
      # Summarize Number of Secondary Assits by Player
      summarise(ID = as.numeric(ID),
                A2) %>%
      # 1 Row per player
      unique()
  } else {
    Secondary_Assists <- NULL
  }
  ###################################################
  # Combining into 1 DF for easy output
  # Combine Team 1 and Team 2 Stats
  Game_Statistics_Summary <- rbind(team1_stats,team2_stats)
  
  # Join on Secondary Assists Columns
  if(!is.null(Secondary_Assists)) {
    # If there are Secondary Assists in the game, join those
    Game_Statistics_Summary <- left_join(Game_Statistics_Summary,
                                       Secondary_Assists)
  } else {
    # Otherwise, join a lot of 0s,
    Game_Statistics_Summary$A2 <- 0
  }
  
  Game_Statistics_Summary <- Game_Statistics_Summary %>%
    # Replace NAs with 0s
    mutate_all(~replace(., is.na(.), 0))%>%
    # Make a bunch of columns numeric
    mutate(across(G:PIM,as.numeric))%>%
    mutate(across(FOW:FOL,as.numeric))%>%
    # A Few Last Minute Stat Changes
    summarise(No, First_Name, Last_Name, Tm, Pos, G, A,
              # Primary Assists = Total Assists - Secondary Assistd
              A1 = A-A2, 
              A2,
              # Points = Goals + Assists
              PTS = G+A, 
              SOG, PIM, TOI, FOW, FOL,
              # Games Started?
              GS = round(G +(.90*A1+(.66*A2)+(.10*SOG)+(.11*FOW)-(.11*FOL)-(.15*PIM)),2))
  
  
  # Write to CSV
  output_file_name <- paste0("Data/113 Hockey CSVs/", MatchID, ".csv")
  
  write.csv(x = Game_Statistics_Summary, 
          file = output_file_name,
          row.names = FALSE)
  
  # Update Key
  key <- rbind(key, c(Event, MatchID, Team1, Team2))
} # End of For Loop

# Names of key and output to a CSV
names(key) <- c("Event", "MatchID", "Team1", "Team 2")
key$MatchID <- as.numeric(key$MatchID)
key_file <- "Data/Match ID Keys/113 Match ID Key.csv"

write.csv(x = key, 
          file = key_file,
          row.names = FALSE)
