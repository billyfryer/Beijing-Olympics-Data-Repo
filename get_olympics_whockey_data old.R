library(jsonlite)
library(magrittr)

get_olympics_whockey_data <- function(matchId) {
  # Sport ID is 113 for hockey
  url <- paste0("https://api-gracenote.nbcolympics.com/svc/games_v2.svc/json/GetMatchResult_Extended?sportId=113&matchId=",matchId,"&languageCode=2")
  jackpot <- jsonlite::read_json(url)
  
  # PlayByPlay
  pbp_data <- jackpot$PlayByPlay$ActionList %>%
    do.call(rbind, .) %>%
    data.frame()
  
  # PlayByPlay
  pbp_data <- jackpot$PlayByPlay$ActionList %>%
    do.call(rbind, .) %>%
    data.frame()
  
  # TeamStatistics
  team_statistics <- jackpot$TeamStatistics %>%
    do.call(rbind, .) %>%
    data.frame()
  
  # PersonStatistics
  # Team 1 Individual Stats
  team_1_ind_stats <- jackpot$PersonStatistics$Competitor1PersonList %>%
    do.call(rbind, .) %>%
    data.frame()
  
  # Team 2 Individual Stats
  team_2_ind_stats <- jackpot$PersonStatistics$Competitor2PersonList %>%
    do.call(rbind, .) %>%
    data.frame()
  
  # Ind Statistics Dictionary
  ind_stats_dictionary <- jackpot$PersonStatistics$StatisticList %>%
    do.call(rbind, .) %>%
    data.frame()
  
  # MatchList is Empty
  
  # Period List
  box_score <- jackpot$PeriodList %>%
    do.call(rbind, .) %>%
    data.frame()
  
  # LineUp
  officials <- jackpot$LineUp$OfficialList %>%
    do.call(rbind, .) %>%
    data.frame()
  
  team_1_coach <- jackpot$LineUp$Competitor1$CoachList %>%
    do.call(rbind, .) %>%
    data.frame()
  
  team_1_lineup <- jackpot$LineUp$Competitor1$LineUp %>%
    do.call(rbind, .) %>%
    data.frame()
  
  team_2_coach <- jackpot$LineUp$Competitor2$CoachList %>%
    do.call(rbind, .) %>%
    data.frame()
  
  team_2_lineup <- jackpot$LineUp$Competitor2$LineUp %>%
    do.call(rbind, .) %>%
    data.frame()
  
  # Preview is empty for this
  
  # MatchInfo
  match_info <- jackpot$MatchInfo %>%
    unlist() %>%
    data.frame()
  
  # Prep output list
  output <- list(PBP = pbp_data,
                 Team_Statistics = team_statistics,
                 Team_1_Ind_Stats = team_1_ind_stats,
                 Team_2_Ind_Stats = team_2_ind_stats,
                 Ind_Stats_Dictionary = ind_stats_dictionary,
                 Box_Score = box_score,
                 Officials = officials,
                 Team_1_Coach = team_1_coach,
                 Team_1_Lineup = team_1_lineup,
                 Team_2_Coach = team_2_coach,
                 Team_2_Lineup = team_2_lineup,
                 Match_Info = match_info
                 )
  
  return(output)
}

#' NOC Codes for My Abbreviations
#' CHN = China
#' CZE = Czech Republic
#' SUI = Switzerland
#' CAN = Canada
#' JPN = Japan
#' SWE = Sweden
#' USA = USA
#' FIN = FIN
#' ROC = Russian Olympic Committee
#' DEN = Denmark

#' USA vs ROC - 746568
#' USA vs FIN - 746569
#' ROC vs SUI - 746570
#' SUI vs USA - 746571
#' SUI vs CAN - 746572
#' CAN vs FIN - 746573
#' JPN vs SWE - 746574
#' CZE vs SWE - 746575
#' DEN vs CHN - 746576
#' CHN vs JPN - 746577
#' DEN vs JPN - 746578
#' CHN vs CZE - 746579
#' CAN vs ROC - 747024
#' FIN vs SUI - 747025
#' USA vs CAN - 747026
#' ROC vs FIN - 747027
#' CZE vs DEN - 747028
#' CHN vs SWE - 747029
#' JPN vs CZE - 747030
#' SWE vs DEN - 747031