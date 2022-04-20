library(tidyverse)

### Getting Date list
source("get_date_list.R")
date_list <- get_date_list()
#write.csv(date_list, "Output Folder/Date List.csv",
#row.names = FALSE)

### Getting Schedule Matrix
source("get_schedule_matrix.R")
schedule_matrix <- get_schedule_matrix()
#write.csv(schedule_matrix, "Output Folder/Schedule Matrix.csv",
#row.names = FALSE)

### Getting Sport List
source("get_sport_list.R")
sport_list <- get_sport_list()
#write.csv(sport_list, "Output Folder/Sport List.csv",
#row.names = FALSE)