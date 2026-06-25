#
#
#
#
#
#
#
#| message: false
#| warning: false

library(tidyverse)
library(plotly)
library(htmltools)

games <- read_csv("Games.csv")

score_counts <- games |>
  select(home = homeScore, away = awayScore,
         homeTeam = hometeamName, awayTeam = awayteamName,
         date = gameDate) |>
  drop_na() |>
  filter(home >= 50, away >= 50, home <= 200, away <= 200) |>  # remove bad rows
  mutate(
    date    = as.Date(date),
    matchup = paste0(format(date, "%b %d, %Y"), " — ", homeTeam, " vs ", awayTeam)
  ) |>
  group_by(home, away) |>
  summarise(
    n_games  = n(),
    matchups = {
      u <- sort(unique(matchup))
      if (length(u) > 10) paste0(paste(u[1:10], collapse = "<br>"), "<br>… and ", length(u) - 10, " more")
      else paste(u, collapse = "<br>")
    },
    .groups = "drop"
  ) |>
  mutate(
    hover = paste0(
      "<b>Home ", home, "  –  Away ", away, "</b><br>",
      "Times occurred: ", n_games, "<br><br>",
      matchups
    )
  )

n_unique <- nrow(score_counts)
year_min <- format(min(as.Date(games$gameDate), na.rm = TRUE), "%Y")
year_max <- format(max(as.Date(games$gameDate), na.rm = TRUE), "%Y")

fig <- plot_ly(
  data = score_counts,
  x    = ~home,
  y    = ~away,
  type = "scatter",
  mode = "markers",
  marker = list(
    color      = ~log1p(n_games),
    colorscale = list(
      list(0,    "#0d0221"),
      list(0.2,  "#1a1a5e"),
      list(0.4,  "#6a0572"),
      list(0.6,  "#c0392b"),
      list(0.8,  "#f39c12"),
      list(1,    "#fffb96")
    ),
    cmin     = 0,
    cmax     = log1p(max(score_counts$n_games)),
    size     = 6,
    opacity  = 1,
    line     = list(width = 0),
    colorbar = list(
      title     = list(text = "Games", font = list(color = "#aaaacc", size = 11)),
      tickfont  = list(color = "#aaaacc", size = 10),
      thickness = 14,
      len       = 0.55,
      tickvals  = log1p(c(1, 5, 25, 100, 500)),
      ticktext  = c("1", "5", "25", "100", "500+")
    )
  ),
  text      = ~hover,
  hoverinfo = "text"
) |>
  add_segments(
    x = 60, xend = 190, y = 60, yend = 190,
    line       = list(color = "rgba(0,0,0,0.15)", width = 1, dash = "dash"),
    hoverinfo  = "skip",
    showlegend = FALSE,
    inherit    = FALSE
  ) |>
  layout(
    paper_bgcolor = "#ffffff",
    plot_bgcolor  = "#f8f9fa",
    font = list(color = "#333333", family = "monospace"),
    xaxis = list(
      title      = list(text = "Home PTS", font = list(color = "#555555", size = 16)),
      tickfont   = list(color = "#666666", size = 14),
      tickangle  = 0,
      gridcolor  = "#e0e0e0",
      zerolinecolor = "#d0d0d0",
      dtick      = 10,
      range      = c(58, 195),      # tight, data-driven range
      fixedrange = FALSE
    ),
    yaxis = list(
      title      = list(text = "Visitor PTS", font = list(color = "#555555", size = 16)),
      tickfont   = list(color = "#666666", size = 14),
      gridcolor  = "#e0e0e0",
      zerolinecolor = "#d0d0d0",
      dtick      = 10,
      range      = c(58, 195),
      scaleanchor = "x",
      scaleratio  = 1,
      fixedrange  = FALSE
    ),
    margin = list(t = 100, r = 50, b = 100, l = 100),
    height = 700,
    hoverlabel = list(
      bgcolor     = "#f0f0f5",
      bordercolor = "#999999",
      font        = list(color = "#333333", size = 12, family = "monospace")
    )
  ) |>
  config(
    displayModeBar          = TRUE,
    modeBarButtonsToRemove  = c("select2d", "lasso2d", "autoScale2d"),
    displaylogo             = FALSE,
    scrollZoom              = TRUE
  )

# Add title and subtitle annotations to the plot
fig <- fig |>
  layout(
    annotations = list(
      # Main title
      list(
        text = "<b>NBA Scorigami</b>",
        x = 0.5, xref = "paper",
        y = 1.12, yref = "paper",
        showarrow = FALSE,
        font = list(size = 24, color = "#333333"),
        xanchor = "center", yanchor = "top"
      ),
      # Subtitle about home team advantage
      list(
        text = "<i>Home teams have a significant advantage: most games cluster below the diagonal line</i>",
        x = 0.5, xref = "paper",
        y = 1.06, yref = "paper",
        showarrow = FALSE,
        font = list(size = 14, color = "#666666"),
        xanchor = "center", yanchor = "top"
      )
    )
  )

browsable(
  tagList(
    tags$div(
      style = "margin-bottom: 30px;",
      fig
    ),
    tags$div(
      style = "margin-bottom: 40px; text-align: center; color: #888888; font-family: monospace; font-size: 12px;",
      paste0("Data: NBA games from ", year_min, " to ", year_max, " | Hover over dots to see game details | ", 
             format(n_unique, big.mark = ","), " unique final scores")
    )
  )
)
```
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
