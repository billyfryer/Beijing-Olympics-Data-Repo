# Get Match Data
library(jsonlite)

get_sport_data <- function(sportId, matchId = NA, phaseId = NA) {

  # Base URL
  if(!is.na(matchId)) {
  base_url <- "https://api-gracenote.nbcolympics.com/svc/games_v2.svc/json/GetMatchResult_Extended?sportId="
  } else {
  base_url <- "https://api-gracenote.nbcolympics.com/svc/games_v2.svc/json/GetResult_Extended?sportId="
  }
  
  # Attach Sport Id
  if(!is.na(sportId)) {
    url <- paste0(base_url, sportId)
  } else {
    stop("SportId required")
  }
  
  # Attach Match Id
  if(!is.na(matchId)) {
    url <- paste0(url, "&matchId=", matchId)
  }
  
  # Attach Phase Id
  if(!is.na(phaseId)) {
    url <- paste0(url, "&phaseId=",phaseId)
  }
  
  # Raise Error for having both phaseId and matchId
  if((!is.na(phaseId) & !is.na(matchId)) | (is.na(phaseId) & is.na(matchId))) {
    stop("Make sure you only have one of matchId and phaseId.")
  }
  
  # Final URL Created
  final_url <- paste0(url, "&languageCode=2")
  
  res <- httr::RETRY("GET", final_url)
  
  jackpot <- res %>%
    httr::content(encoding = "UTF-8")

  return(jackpot)
}

#' List of Possible Names
#' 
#' For PhaseId Events
#' ~~~~~~~~~~~~~~~~~~~~~~
#' PhaseInfo
#' Medals
#' Current
#' Preview
#' StartList
#' Result
#' Standings
#' Summary
#' Legend
#' 
#' For MatchId Events
#' ~~~~~~~~~~~~~~~~~~~~~~
#'  MatchInfo
#'  Preview
#'  LineUp
#'  PeriodList
#'  MatchList
#'  PlayByPlay
#'  TeamStatistics
#'  PersonStatistics
#' 
