library(hoopR)
library(dplyr)
library(readr)

dir.create(
  "data/raw_data",
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  "data/processed_data",
  recursive = TRUE,
  showWarnings = FALSE
)



seasons <- 2018:2026



# Download NBA player box scores


nba_box <- load_nba_player_box(
  seasons = seasons
)


message(
  "Downloaded ",
  nrow(nba_box),
  " total player-game observations."
)



celtics_stars <- nba_box |>
  filter(
    athlete_display_name %in% c(
      "Jayson Tatum",
      "Jaylen Brown"
    )
  )



celtics_stars <- celtics_stars |>
  filter(
    season_type %in% c(2, 3)
  ) |>
  mutate(
    season_type_name = case_when(
      season_type == 2 ~ "Regular Season",
      season_type == 3 ~ "Playoffs",
      TRUE ~ NA_character_
    )
  )


celtics_stars <- celtics_stars |>
  mutate(
    season_label = paste0(
      season - 1,
      "-",
      substr(
        season,
        3,
        4
      )
    )
  )


message("\nPlayers found:")

print(
  celtics_stars |>
    count(
      season,
      season_label,
      athlete_display_name,
      season_type_name
    ) |>
    arrange(
      season,
      athlete_display_name,
      season_type_name
    )
)




write_csv(
  celtics_stars,
  "data/raw_data/tatum_brown_game_data.csv"
)





message("\n======================================")
message("DATA DOWNLOAD COMPLETE")

message(
  "Rows saved: ",
  nrow(celtics_stars)
)

message(
  "Tatum rows: ",
  sum(
    celtics_stars$athlete_display_name ==
      "Jayson Tatum"
  )
)

message(
  "Brown rows: ",
  sum(
    celtics_stars$athlete_display_name ==
      "Jaylen Brown"
  )
)


message(
  "\nSaved to:"
)

message(
  "data/raw_data/tatum_brown_game_data.csv"
)