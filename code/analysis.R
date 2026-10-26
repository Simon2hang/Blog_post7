library(dplyr)
library(readr)
library(plotly)
library(htmlwidgets)


stats <- read_csv(
  "data/processed_data/tatum_brown_season_summary.csv",
  show_col_types = FALSE
) |>
  arrange(season, player)

stats_plot <- stats |>
  mutate(
    ts_pct_plot = ts_pct * 100,
    three_pct_plot = three_pct * 100
  )

season_order <- stats_plot |>
  arrange(season) |>
  distinct(season, season_label) |>
  pull(season_label)


metrics <- list(
  
  list(
    column = "pts",
    label = "Points Per Game",
    short = "PTS",
    suffix = "",
    digits = ".1f"
  ),
  
  list(
    column = "reb",
    label = "Rebounds Per Game",
    short = "REB",
    suffix = "",
    digits = ".1f"
  ),
  
  list(
    column = "ast",
    label = "Assists Per Game",
    short = "AST",
    suffix = "",
    digits = ".1f"
  ),
  
  list(
    column = "ts_pct_plot",
    label = "True Shooting Percentage",
    short = "TS%",
    suffix = "%",
    digits = ".1f"
  ),
  
  list(
    column = "three_pct_plot",
    label = "Three-Point Percentage",
    short = "3P%",
    suffix = "%",
    digits = ".1f"
  ),
  
  list(
    column = "plus_minus",
    label = "Average Plus/Minus",
    short = "+/-",
    suffix = "",
    digits = ".1f"
  )
)

season_types <- c(
  "Regular Season",
  "Playoffs"
)

players <- c(
  "Jayson Tatum",
  "Jaylen Brown"
)


fig <- plot_ly()

trace_info <- list()

trace_number <- 0

for (season_type_now in season_types) {
  
  for (metric in metrics) {
    
    for (player_now in players) {
      
      trace_number <- trace_number + 1
      
      temp <- stats_plot |>
        filter(
          season_type == season_type_now,
          player == player_now
        ) |>
        arrange(season)
      
      starting_visible <- (
        season_type_now == "Regular Season" &
          metric$column == "pts"
      )
      
      hover_text <- paste0(
        "<b>", player_now, "</b>",
        "<br>Season: %{x}",
        "<br>", metric$short, ": %{y:",
        metric$digits, "}",
        metric$suffix,
        "<br>Games: %{customdata}",
        "<extra></extra>"
      )
      
      fig <- fig |>
        add_trace(
          x = temp$season_label,
          y = temp[[metric$column]],
          customdata = temp$games,
          type = "scatter",
          mode = "lines+markers",
          name = player_now,
          visible = starting_visible,
          hovertemplate = hover_text
        )
      
      trace_info[[trace_number]] <- list(
        season_type = season_type_now,
        metric = metric$column
      )
    }
  }
}


# Dropdown

buttons <- list()

for (season_type_now in season_types) {
  
  for (metric in metrics) {
    
    visible_vector <- sapply(
      trace_info,
      function(x) {
        x$season_type == season_type_now &
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
            metric$label,
            " — ",
            season_type_now
          ),
          
          yaxis = list(
            title = metric$label
          )
        )
      ),
      
      label = paste0(
        season_type_now,
        " — ",
        metric$short
      )
    )
  }
}



fig <- fig |>
  layout(
    
    title = "Jayson Tatum vs. Jaylen Brown: Points Per Game — Regular Season",
    
    xaxis = list(
      title = "Season",
      categoryorder = "array",
      categoryarray = season_order
    ),
    
    yaxis = list(
      title = "Points Per Game"
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


fig


saveWidget(
  fig,
  "figures/tatum_brown_interactive.html",
  selfcontained = TRUE
)


overall_comparison <- stats_plot |>
  group_by(
    player,
    season_type
  ) |>
  summarise(
    
    seasons = n(),
    
    avg_pts = mean(pts, na.rm = TRUE),
    
    avg_reb = mean(reb, na.rm = TRUE),
    
    avg_ast = mean(ast, na.rm = TRUE),
    
    avg_ts_pct = mean(
      ts_pct_plot,
      na.rm = TRUE
    ),
    
    avg_three_pct = mean(
      three_pct_plot,
      na.rm = TRUE
    ),
    
    avg_plus_minus = mean(
      plus_minus,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  )

print(overall_comparison)

write_csv(
  overall_comparison,
  "data/processed_data/overall_comparison.csv"
)