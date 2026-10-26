library(hoopR)
library(dplyr)
library(readr)
library(plotly)
library(htmlwidgets)


seasons <- 2018:2026

players <- c(
  "Jayson Tatum",
  "Jaylen Brown"
)

advanced_list <- list()

for (yr in seasons) {
  
  message("Downloading advanced stats for ", yr, "...")
  
  fn_args <- names(formals(hoopR::bref_players_stats))
  
  call_args <- list(
    table = "advanced"
  )
  
  if ("season" %in% fn_args) {
    call_args$season <- yr
  } else if ("year" %in% fn_args) {
    call_args$year <- yr
  }
  
  temp <- do.call(
    hoopR::bref_players_stats,
    call_args
  )
  
  temp$season <- yr
  
  advanced_list[[as.character(yr)]] <- temp
  
  Sys.sleep(3)
}


advanced_raw <- bind_rows(advanced_list)


print(names(advanced_raw))



possible_player_cols <- c(
  "player",
  "player_name",
  "name"
)

player_col <- possible_player_cols[
  possible_player_cols %in% names(advanced_raw)
][1]

if (is.na(player_col)) {
  stop("Could not find player name column.")
}


advanced <- advanced_raw |>
  filter(
    .data[[player_col]] %in% players
  )

get_column <- function(df, possible_names) {
  
  found <- possible_names[
    possible_names %in% names(df)
  ]
  
  if (length(found) == 0) {
    return(rep(NA_real_, nrow(df)))
  }
  
  suppressWarnings(
    as.numeric(df[[found[1]]])
  )
}



advanced_clean <- advanced |>
  mutate(
    
    player = .data[[player_col]],
    
    per = get_column(
      advanced,
      c("per", "player_efficiency_rating")
    ),
    
    ts_pct = get_column(
      advanced,
      c("ts_pct", "true_shooting_pct")
    ),
    
    usg_pct = get_column(
      advanced,
      c("usg_pct", "usage_pct")
    ),
    
    ws = get_column(
      advanced,
      c("ws", "win_shares")
    ),
    
    ws_per_48 = get_column(
      advanced,
      c(
        "ws_per_48",
        "ws_48",
        "win_shares_per_48"
      )
    ),
    
    obpm = get_column(
      advanced,
      c("obpm", "offensive_box_plus_minus")
    ),
    
    dbpm = get_column(
      advanced,
      c("dbpm", "defensive_box_plus_minus")
    ),
    
    bpm = get_column(
      advanced,
      c("bpm", "box_plus_minus")
    ),
    
    vorp = get_column(
      advanced,
      c(
        "vorp",
        "value_over_replacement_player"
      )
    )
  ) |>
  
  mutate(
    season_label = paste0(
      season - 1,
      "-",
      substr(season, 3, 4)
    )
  ) |>
  
  select(
    player,
    season,
    season_label,
    per,
    ts_pct,
    usg_pct,
    ws,
    ws_per_48,
    obpm,
    dbpm,
    bpm,
    vorp
  ) |>
  
  arrange(
    season,
    player
  )


print(
  advanced_clean,
  n = Inf
)


write_csv(
  advanced_clean,
  "data/processed_data/tatum_brown_advanced_stats.csv"
)


metrics <- list(
  
  list(
    column = "per",
    label = "Player Efficiency Rating",
    short = "PER"
  ),
  
  list(
    column = "ws",
    label = "Win Shares",
    short = "WS"
  ),
  
  list(
    column = "ws_per_48",
    label = "Win Shares per 48 Minutes",
    short = "WS/48"
  ),
  
  list(
    column = "bpm",
    label = "Box Plus/Minus",
    short = "BPM"
  ),
  
  list(
    column = "vorp",
    label = "Value Over Replacement Player",
    short = "VORP"
  ),
  
  list(
    column = "usg_pct",
    label = "Usage Percentage",
    short = "USG%"
  )
)



fig_adv <- plot_ly()

trace_info <- list()
trace_number <- 0


for (metric in metrics) {
  
  for (player_now in players) {
    
    trace_number <- trace_number + 1
    
    temp <- advanced_clean |>
      filter(
        player == player_now
      ) |>
      arrange(season)
    
    starting_visible <- (
      metric$column == "per"
    )
    
    fig_adv <- fig_adv |>
      add_trace(
        
        x = temp$season_label,
        
        y = temp[[metric$column]],
        
        type = "scatter",
        
        mode = "lines+markers",
        
        name = player_now,
        
        visible = starting_visible,
        
        customdata = temp$season_label,
        
        hovertemplate = paste0(
          "<b>", player_now, "</b>",
          "<br>Season: %{x}",
          "<br>", metric$short,
          ": %{y:.2f}",
          "<extra></extra>"
        )
      )
    
    trace_info[[trace_number]] <- list(
      metric = metric$column
    )
  }
}




buttons <- list()

for (metric in metrics) {
  
  visible_vector <- sapply(
    trace_info,
    function(x) {
      x$metric == metric$column
    }
  )
  
  buttons[[length(buttons) + 1]] <- list(
    
    method = "update",
    
    args = list(
      
      list(
        visible = visible_vector
      ),
      
      list(
        
        title = paste0(
          "Jayson Tatum vs. Jaylen Brown: ",
          metric$label
        ),
        
        yaxis = list(
          title = metric$label
        )
      )
    ),
    
    label = metric$short
  )
}



fig_adv <- fig_adv |>
  layout(
    
    title = "Jayson Tatum vs. Jaylen Brown: Player Efficiency Rating",
    
    xaxis = list(
      title = "Season"
    ),
    
    yaxis = list(
      title = "Player Efficiency Rating"
    ),
    
    hovermode = "x",
    
    legend = list(
      orientation = "h",
      x = 0.5,
      xanchor = "center",
      y = -0.2
    ),
    
    margin = list(
      t = 120,
      b = 100
    ),
    
    updatemenus = list(
      
      list(
        type = "dropdown",
        
        direction = "down",
        
        x = 0,
        
        y = 1.15,
        
        active = 0,
        
        buttons = buttons
      )
    )
  )



fig_adv



saveWidget(
  fig_adv,
  "figures/tatum_brown_advanced_interactive.html",
  selfcontained = TRUE
)


message("\n======================================")
message("ADVANCED STATS COMPLETE")
message("======================================")

message(
  "Saved data: ",
  "data/processed_data/tatum_brown_advanced_stats.csv"
)

message(
  "Saved chart: ",
  "figures/tatum_brown_advanced_interactive.html"
)