data("LahmanData", package = "Lahman")

# for side panel use
stat_category_labels <- c(
  W = "Wins (W)",
  L = "Losses (L)",
  R = "Runs Scored (R)",
  RA = "Runs Allowed (RA)",
  HR = "Home Runs (HR)",
  SB = "Stolen Bases (SB)",
  ERA = "Earned Run Average (ERA)",
  Ghome = "Home Games (Ghome)",
  G = "Games Played (G)",
  AB = "At Bats (AB)",
  H = "Hits (H)",
  X2B = "Doubles (2B)",
  X3B = "Triples (3B)",
  BB = "Walks (BB)",
  SO = "Strikeouts (SO)",
  ER = "Earned Runs (ER)",
  HBP = "Hit By Pitch (HBP)",
  SF = "Sacrifice Flies (SF)",
  SV = "Saves (SV)",
  HA = "Hits Allowed (HA)",
  HRA = "Home Runs Allowed (HRA)",
  CG = "Complete Games (CG)",
  E = "Errors (E)",
  PPF = "Pitcher's Performance Factor (PPF)",
  BPF = "Bat Performance Factor (BPF)",
  CS = "Caught Stealing (CS)",
  SHO = "Shutouts (SHO)",
  IPouts = "Outs by Pitcher (IPouts)",
  BBA = "Walks Allowed (BBA)",
  SOA = "Strikeouts Allowed (SOA)",
  DP = "Double Plays (DP)",
  FP = "Fielding Percentage (FPCT)",
  attendance = "Attendance"
)

# filtering used lahman stats (used in server)
useful_team_stats <- c(
  "W", "L", "R", "RA", "HR", "SB", "ERA",
  "AB", "H", "X2B", "X3B", "BB", "SO",
  "ER", "HBP", "SF", "SV", "HA", "HRA",
  "CG", "E", "CS", "SHO", "IPouts", "BBA",
  "SOA", "DP", "FP", "attendance"
)

# added feature under team summary tab
get_top_players_for_stat <- function(year, team_id, stat) {
  batting_stats <- c("AB", "H", "X2B", "X3B", "HR", "BB", "SO", "HBP", "SF", "SB", "CS", "R")
  pitching_stats <- c("W", "L", "ERA", "ER", "SV", "CG", "SHO", "HRA", "HA", "BBA", "SO", "IPouts")
  fielding_stats <- c("E", "DP", "FP")
  
  if (stat %in% batting_stats) {
    df <- Lahman::Batting |>
      dplyr::filter(yearID == year, teamID == team_id) |>
      dplyr::group_by(playerID) |>
      dplyr::summarise(
        stat_value = sum(.data[[stat]], na.rm = TRUE),
        .groups = "drop"
      )
    
  } else if (stat %in% pitching_stats) {
    df <- Lahman::Pitching |>
      dplyr::filter(yearID == year, teamID == team_id) |>
      dplyr::group_by(playerID) |>
      dplyr::summarise(
        stat_value = if (stat == "ERA") {
          mean(ERA, na.rm = TRUE)
        } else {
          sum(.data[[stat]], na.rm = TRUE)
        },
        .groups = "drop"
      )
    
  } else if (stat %in% fielding_stats) {
    # map team-level stat name to player-level fielding column
    fielding_col <- if (stat == "FP") "FPct" else stat
    
    df <- Lahman::Fielding |>
      dplyr::filter(yearID == year, teamID == team_id) |>
      dplyr::group_by(playerID) |>
      dplyr::summarise(
        stat_value = if (stat == "FP") {
          mean(.data[[fielding_col]], na.rm = TRUE)
        } else {
          sum(.data[[fielding_col]], na.rm = TRUE)
        },
        .groups = "drop"
      )
    
  } else {
    return(NULL)
  }
  
  df |>
    dplyr::left_join(
      Lahman::People |>
        dplyr::select(playerID, nameFirst, nameLast),
      by = "playerID"
    ) |>
    dplyr::mutate(player_name = paste(nameFirst, nameLast)) |>
    dplyr::arrange(dplyr::desc(stat_value)) |>
    dplyr::slice_head(n = 5) |>
    dplyr::select(Player = player_name, Value = stat_value)
}

# for teams that changed names
team_name_overrides <- c(
  ATH = "Athletics"
)

standardize_team_name <- function(team_id, raw_name) {
  team_id <- as.character(team_id)
  raw_name <- as.character(raw_name)
  
  if (!is.na(team_id) && team_id %in% names(team_name_overrides)) {
    return(unname(team_name_overrides[team_id]))
  }
  
  raw_name
}

# this is for the plots specifically
get_previous_year_team_id <- function(team_id, year) {
  team_id <- as.character(team_id)
  
  if (team_id == "ATH" && year >= 2025) {
    return("OAK")
  }
  
  team_id
}
