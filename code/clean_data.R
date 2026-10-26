library(dplyr)
library(readr)


games <- read_csv(
  "data/raw_data/tatum_brown_game_data.csv",
  show_col_types = FALSE
)


games_clean <- games |>
  filter(
    !is.na(minutes),
    minutes > 0,
    !is.na(points)
  )



season_summary <- games_clean |>
  group_by(
    athlete_display_name,
    season,
    season_label,
    season_type_name
  ) |>
  summarise(
    
    # Games
    games = n(),
    
    # Minutes
    mpg = mean(
      minutes,
      na.rm = TRUE
    ),
    
    pts = mean(
      points,
      na.rm = TRUE
    ),
    
    reb = mean(
      rebounds,
      na.rm = TRUE
    ),
    
    ast = mean(
      assists,
      na.rm = TRUE
    ),
    
    stl = mean(
      steals,
      na.rm = TRUE
    ),
    
    blk = mean(
      blocks,
      na.rm = TRUE
    ),
    
    tov = mean(
      turnovers,
      na.rm = TRUE
    ),
    
    
    fgm = sum(
      field_goals_made,
      na.rm = TRUE
    ),
    
    fga = sum(
      field_goals_attempted,
      na.rm = TRUE
    ),
    
    three_pm = sum(
      three_point_field_goals_made,
      na.rm = TRUE
    ),
    
    three_pa = sum(
      three_point_field_goals_attempted,
      na.rm = TRUE
    ),
    
    ftm = sum(
      free_throws_made,
      na.rm = TRUE
    ),
    
    fta = sum(
      free_throws_attempted,
      na.rm = TRUE
    ),
    
    total_points = sum(
      points,
      na.rm = TRUE
    ),
    
    plus_minus = mean(
      plus_minus,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  )



season_summary <- season_summary |>
  mutate(
    
    fg_pct = if_else(
      fga > 0,
      fgm / fga,
      NA_real_
    ),
    
    three_pct = if_else(
      three_pa > 0,
      three_pm / three_pa,
      NA_real_
    ),
    
    ft_pct = if_else(
      fta > 0,
      ftm / fta,
      NA_real_
    ),
    
    ts_pct = if_else(
      fga + 0.44 * fta > 0,
      total_points /
        (2 * (fga + 0.44 * fta)),
      NA_real_
    )
  )


season_summary <- season_summary |>
  select(
    
    player = athlete_display_name,
    
    season,
    season_label,
    season_type = season_type_name,
    
    games,
    mpg,
    
    pts,
    reb,
    ast,
    stl,
    blk,
    tov,
    
    fg_pct,
    three_pct,
    ft_pct,
    ts_pct,
    
    plus_minus
  ) |>
  
  arrange(
    season,
    season_type,
    player
  )



write_csv(
  season_summary,
  "data/processed_data/tatum_brown_season_summary.csv"
)


print(
  season_summary,
  n = Inf
)


message("DATA CLEANING COMPLETE")

message(
  "Rows saved: ",
  nrow(season_summary)
)

message(
  "\nSaved to:"
)

message(
  "data/processed_data/tatum_brown_season_summary.csv"
)