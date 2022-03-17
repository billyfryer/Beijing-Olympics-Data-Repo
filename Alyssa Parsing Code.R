library(tidyverse)
library(jsonlite)
library(lubridate)




url_json <- paste0("https://api-gracenote.nbcolympics.com/svc/games_v2.svc/json/GetMatchResult_Extended?sportId=113&matchId=747026&languageCode=2")

raw_json <- jsonlite::fromJSON(url_json)


Team1 <- raw_json$MatchInfo$Competitor1$c_Short
Team2 <- raw_json$MatchInfo$Competitor2$c_Short
Date <- raw_json$MatchInfo$StartDateTime$c_Local

#team 1
team1_players <- raw_json$PersonStatistics$Competitor1PersonList
team1_stats <- team1_players$StatisticsList
team1_stats <- lapply(team1_stats, unlist)
team1_stats <- lapply(team1_stats, FUN = function(x){ data.frame(t(x), stringsAsFactors = F) })
team1_stats<- do.call("bind_rows", team1_stats)
team1_stats <- team1_stats %>%
  summarise(c_Value1,c_Value2,c_Value3,c_Value4,c_Value5,c_Value6,c_Value7,c_Value8,c_Value9,c_Value10,c_Value11,c_Value12,c_Value13)

team1_stats <- cbind(team1_players,team1_stats)

team1_stats<- team1_stats %>%
  summarise(ID = n_PersonID, Tm = Team1, First_Name = c_PersonFirstName, Last_Name = c_PersonLastName, No = c_ShirtNr, Pos = c_FunctionShort, G = c_Value1,A = c_Value3, SOG = c_Value2, PIM = c_Value5, TOI = c_Value7, FOW = c_Value10, FOL = c_Value11)

#Team 2

team2_players <- raw_json$PersonStatistics$Competitor2PersonList
team2_stats <- team2_players$StatisticsList
team2_stats <- lapply(team2_stats, unlist)
team2_stats <- lapply(team2_stats, FUN = function(x){ data.frame(t(x), stringsAsFactors = F) })
team2_stats<- do.call("bind_rows", team2_stats)
team2_stats <- team2_stats %>%
  summarise(c_Value1,c_Value2,c_Value3,c_Value4,c_Value5,c_Value6,c_Value7,c_Value8,c_Value9,c_Value10,c_Value11,c_Value12,c_Value13)

team2_stats <- cbind(team2_players,team2_stats)

team2_stats<- team2_stats %>%
  summarise(ID = n_PersonID, Tm = Team2, First_Name = c_PersonFirstName, Last_Name = c_PersonLastName, No = c_ShirtNr, Pos = c_FunctionShort, G = c_Value1,A = c_Value3, SOG = c_Value2, PIM = c_Value5, TOI = c_Value7, FOW = c_Value10, FOL = c_Value11)

#The secondary Assist Problem LLLLL

pbp <- raw_json$PlayByPlay$ActionList
pbp <- lapply(pbp, unlist)
pbp <- lapply(pbp, FUN = function(x){ data.frame(t(x), stringsAsFactors = F) })



pbp_player3 <- (pbp$n_SubPerson2ID)
pbp_event <- (pbp$c_Action)
Secondary_Assists <- rbind(pbp_player3,pbp_event)


Secondary_Assists <- data.frame(t(Secondary_Assists))%>%
  summarise(ID = X1, Event_Type = X2)%>%
  filter(Event_Type == "Goal")%>%
  group_by(ID)%>%
  drop_na()%>%
  mutate(A2 = (length(Event_Type)))%>%
  summarise(ID = as.numeric(ID),A2)%>%
  unique()



Game_Statistics_Summary <- rbind(team1_stats,team2_stats)

Game_Statistics_Summary <- left_join(Game_Statistics_Summary,Secondary_Assists)%>%
  mutate_all(~replace(., is.na(.), 0))%>%
  mutate(across(G:PIM,as.numeric))%>%
  mutate(across(FOW:FOL,as.numeric))%>%
  summarise(No, First_Name, Last_Name, Tm, Pos, G, A, A1 = A-A2, A2, PTS = G+A, SOG, PIM, TOI, FOW, FOL,GS = round(G +(.90*A1+(.66*A2)+(.10*SOG)+(.11*FOW)-(.11*FOL)-(.15*PIM)),2))



write.csv(Game_Statistics_Summary, paste0(Team1,Team2,Date,".csv"))








