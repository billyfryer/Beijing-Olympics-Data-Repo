# Curling Parsing File

# Libraries
library(tidyverse)
library(jsonlite)
library(lubridate)

# Read in All File Names
# Code Stolen From:
# https://www.geeksforgeeks.org/read-all-files-in-directory-using-r/#:~:text=To%20list%20all%20files%20in,files%20in%20the%20specified%20directories.
all_files <- list.files(path = "Data/212-JSONs")
url_starter <- "https://raw.githubusercontent.com/b4billy/Beijing-Olympics-Data-Repo/main/Data/212-JSONs/"
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
  Date <- raw_json$MatchInfo$StartDateTime$c_Local
  
  # Gender is in the json file name
  Event <- raw_json$MatchInfo$GenderEvent$c_Name
  
  # Match ID
  MatchID <- raw_json$MatchInfo$n_ID
  
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
  output_file_name <- paste0("Data/212-CSVs/", MatchID, ".csv")
  
  write.csv(x = Game_Statistics_Summary, 
            file = output_file_name,
            row.names = FALSE)
} # End of For Loop
