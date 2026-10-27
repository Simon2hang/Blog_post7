# Blog Post 7: Tatum or Brown?

This project compares **Jayson Tatum and Jaylen Brown** to explore whether the Boston Celtics made the right decision by keeping Tatum and trading Brown.

The analysis uses:

- Traditional statistics such as PTS, REB, AST, TS%, 3P%, and +/-
- Advanced statistics including PER, WS, WS/48, BPM, VORP, and USG%
- Interactive Plotly visualizations for comparing both players across seasons

## Data

NBA game data were collected using the `hoopR` R package.

Advanced statistics were collected from Basketball-Reference through `hoopR`.

## Files

```text
code/
  get_data.R
  clean_data.R
  analysis.R
  advanced_stats.R

data/
  raw_data/
  processed_data/

figures/
  jtjb.avif
  tatum_brown_interactive.html
  tatum_brown_advanced_interactive.html
```

## Main Takeaway

The results generally favor **Jayson Tatum**, especially in advanced metrics such as BPM and WS/48. Jaylen Brown had a strong 2025–26 season while Tatum was injured, but the longer-term data suggest that Tatum was the stronger overall player.
