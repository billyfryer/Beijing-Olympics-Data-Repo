# Monobob Stats
source("get_sport_list.R")
source("get_sport_schedule.R")
source("get_sport_data.R")
library(tidyverse)

# Get Curling Sport ID
CurlingID <- get_sport_list() %>% 
  filter(c_Sport == "Curling") %>% 
  pull(n_SportID)

# Get US Curling Schedule
curling_schedule <- get_sport_schedule(CurlingID) %>% 
  .$DateList.EventPhaseMatchList %>% 
  do.call(bind_rows, .) %>% 
  filter(Gender$c_Short == "M") %>% 
  .$Match %>% 
  filter(is.na(c_KnockoutMatchName))
  #filter(Competitor1$c_Name == "United States" | Competitor2$c_Name == "United States")

#############################################
# Make data longer
#############################################
# Grab Competitor 1 Stuff
final_scores <- curling_schedule %>% 
  select(n_ID, Competitor1, Competitor2) %>% 
  unpack(cols = c(Competitor1, Competitor2),
         names_sep = "_") %>% 
  select(n_ID, Competitor1_c_Name, Competitor1_c_Result, 
         Competitor2_c_Name, Competitor2_c_Result)

# write.csv(final_scores,
#          file = "Data Sets/Curling Round Robin.csv",
#          row.names = FALSE)
