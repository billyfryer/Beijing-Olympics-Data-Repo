# Curling Parsing File

# Libraries
library(tidyverse)
library(jsonlite)
library(lubridate)

# Read in All File Names
# Code Stolen From:
# https://www.geeksforgeeks.org/read-all-files-in-directory-using-r/#:~:text=To%20list%20all%20files%20in,files%20in%20the%20specified%20directories.

all_files <- list.files(path = "Data/212 Curling JSONs",
                        # To make sure I grab only the relevant files 
                        pattern = "Tournament")

# Key DF for matching event and matchid
key <- data.frame(Event = c(),
                  MatchID = c(),
                  Team1 = c(),
                  Team2 = c())

for (json_file_name in all_files){

  # Sanity Check
  print(json_file_name)
  
  # Read in the json file
  # Don't know how this works, but it does.
  # Stolen From Stack Overflow:
  # https://stackoverflow.com/questions/38074926/unable-to-parse-locally-stored-json-file-with-special-character-like-backslash
  file_path <- paste0("Data/212 Curling JSONs/", json_file_name)
  raw_json <- fromJSON(gsub("\\\\","",readLines(file_path)))
  
  # Date of Match and Gender
  Date <- raw_json$MatchInfo$StartDateTime$c_Local
  # Gender is in the json file name
  Event <- str_split(json_file_name, 
                     pattern = "Match ID", 
                     n = 2)[[1]][1] %>% 
    # Trim Whitespace
    str_trim()
  # Get Match ID
  MatchID <- str_split(json_file_name, 
                       pattern = "Match ID", 
                       n = 2)[[1]][2] %>% 
    # Trim Whitespace
    str_trim() %>% 
    # Get rid of the .json part
    str_remove(pattern = ".json")
  
  ###################################################
  # Team 1 Data

  # Team Name
  Team1 <- raw_json$MatchInfo$Competitor1$c_Short
  # Players
  team1_players <- raw_json$PersonStatistics$Competitor1PersonList
  # Unnest Statistics List Column
  team1_players <- team1_players %>% unnest(cols = "StatisticsList")
  # Pivot Wider
  team1_stats <- team1_players %>% 
    # Make Data Frame Wider with column names ODF Code
    # Values are Value and Relative Value
    pivot_wider(names_from = c_ODF_Code,
                values_from = c(c_Value, c_ValueRelative)) %>% 
    # We only want one record per person
    group_by(n_PersonID) %>% 
    summarize(Team = Team1,
              c_Person = max(c_Person, na.rm = TRUE),
              c_PersonShort = max(c_PersonShort, na.rm = TRUE),
              c_PersonFirstName = max(c_PersonFirstName, na.rm = TRUE),
              c_PersonLastName = max(c_PersonLastName, na.rm = TRUE),
              c_FunctionShort = max(c_FunctionShort, na.rm = TRUE),
              n_Sort = max(n_Sort, na.rm = TRUE), 
              c_Value_GAME_SUCCESS = max(c_Value_GAME_SUCCESS, na.rm = TRUE),
              c_Value_CW = max(c_Value_CW, na.rm = TRUE), 
              c_Value_CCW = max(c_Value_CCW, na.rm = TRUE), 
              c_Value_DRAW = max(c_Value_DRAW, na.rm = TRUE),
              c_Value_TAKEOUT = max(c_Value_TAKEOUT, na.rm = TRUE),
              c_ValueRelative_GAME_SUCCESS = max(c_ValueRelative_GAME_SUCCESS, na.rm = TRUE),
              c_ValueRelative_CW = max(c_ValueRelative_CW, na.rm = TRUE), 
              c_ValueRelative_CCW = max(c_ValueRelative_CCW, na.rm = TRUE),
              c_ValueRelative_DRAW = max(c_ValueRelative_DRAW, na.rm = TRUE),
              c_ValueRelative_TAKEOUT = max(c_ValueRelative_TAKEOUT, na.rm = TRUE))

  
  ###################################################
  # Team 2 Data

  # Team Name
  Team2 <- raw_json$MatchInfo$Competitor2$c_Short
  # Players
  team2_players <- raw_json$PersonStatistics$Competitor2PersonList
  # Unnest Statistics List Column
  team2_players <- team2_players %>% unnest(cols = "StatisticsList")
  # Pivot Wider
  team2_stats <- team2_players %>% 
    # Make Data Frame Wider with column names ODF Code
    # Values are Value and Relative Value
    pivot_wider(names_from = c_ODF_Code,
                values_from = c(c_Value, c_ValueRelative)) %>% 
    # We only want one record per person
    group_by(n_PersonID) %>% 
    summarize(Team = Team2,
              c_Person = max(c_Person, na.rm = TRUE),
              c_PersonShort = max(c_PersonShort, na.rm = TRUE),
              c_PersonFirstName = max(c_PersonFirstName, na.rm = TRUE),
              c_PersonLastName = max(c_PersonLastName, na.rm = TRUE),
              c_FunctionShort = max(c_FunctionShort, na.rm = TRUE),
              n_Sort = max(n_Sort, na.rm = TRUE), 
              c_Value_GAME_SUCCESS = max(c_Value_GAME_SUCCESS, na.rm = TRUE),
              c_Value_CW = max(c_Value_CW, na.rm = TRUE), 
              c_Value_CCW = max(c_Value_CCW, na.rm = TRUE), 
              c_Value_DRAW = max(c_Value_DRAW, na.rm = TRUE),
              c_Value_TAKEOUT = max(c_Value_TAKEOUT, na.rm = TRUE),
              c_ValueRelative_GAME_SUCCESS = max(c_ValueRelative_GAME_SUCCESS, na.rm = TRUE),
              c_ValueRelative_CW = max(c_ValueRelative_CW, na.rm = TRUE), 
              c_ValueRelative_CCW = max(c_ValueRelative_CCW, na.rm = TRUE),
              c_ValueRelative_DRAW = max(c_ValueRelative_DRAW, na.rm = TRUE),
              c_ValueRelative_TAKEOUT = max(c_ValueRelative_TAKEOUT, na.rm = TRUE))
  
  ###################################################
  # Combining into 1 DF for easy output
  # Combine Team 1 and Team 2 Stats
  Game_Statistics_Summary <- rbind(team1_stats,team2_stats)
  
  
  Game_Statistics_Summary <- Game_Statistics_Summary %>%
    # Replace NAs with 0s
    mutate_all(~replace(., is.na(.), 0)) %>%
    # Make a bunch of columns numeric
    # This causes all the "-" in the relative stuff to be coerced to NAs
    # Which is fine, just need to suppress the warnings
    suppressWarnings(mutate(across(n_Sort:c_ValueRelative_TAKEOUT,as.numeric))) %>%
    # Replace NAs with 0s (Again)
    mutate_all(~replace(., is.na(.), 0))
  
  # Write to CSV
  output_file_name <- paste0("Data/212 Curling CSVs/", MatchID, ".csv")
  
  write.csv(x = Game_Statistics_Summary, 
            file = output_file_name,
            row.names = FALSE)
  
  # Update Key
  key <- rbind(key, c(Event, MatchID, Team1, Team2))
}

# Names of key and output to a CSV
names(key) <- c("Event", "MatchID", "Team1", "Team 2")
key$MatchID <- as.numeric(key$MatchID)
key_file <- "Data/Lookup CSVs/212 Lookup.csv"

write.csv(x = key, 
          file = key_file,
          row.names = FALSE)