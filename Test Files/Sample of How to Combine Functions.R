# Sample of How to Use My Functions in Tandem
sport_list <- get_sport_list()
event_phase <- get_event_phase(221)

m_team_ski_jumping_data <- get_sport_data(sportId = 221, phaseId = 2425741)

scores <- m_team_ski_jumping_data$Result$PhaseList %>% 
  .[[2]] %>% 
  .$ParticipantList %>% 
  do.call(rbind, .) %>% 
  data.frame() %>% 
  select(c_Participant, c_Result)

scores$Participant <- unlist(scores$c_Participant)
scores$Result <- unlist(scores$c_Result) %>% as.numeric()

final_data <- scores %>% select(Participant, Result)

write.csv(final_data,
          file = "Freestyle Skiing Mixed Team Aerials Finals Scores.csv")
