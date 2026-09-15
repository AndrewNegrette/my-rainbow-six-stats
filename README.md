# What Should I Play? My Rainbow Six Siege Stats

An R Shiny dashboard for exploring your personal Rainbow Six Siege performance across operators, maps, platforms, and ranked play.

## Live Dashboard

[Open the live Rainbow Six Siege dashboard](https://01a0a772-e943-53c5-ff74-d5b91fd9e0e2.share.connect.posit.cloud/)

## Features

- Operator Decision Matrix: rounds played versus win rate, with K/D shown by point size
- KPI cards for rounds, win rate, K/D, kills, assists, and aces
- Minimum round filter to reduce misleading small sample rankings
- Map Advantage chart showing defender win rate minus attacker win rate
- PC vs. PlayStation operator comparison for all games or ranked play
- PC vs. PlayStation and ranked vs. all-game comparisons
- Transparent recommendation table based on win rate, K/D, and usage

## Run locally

1. Install R and RStudio.
2. Install the packages:

```r
install.packages("pacman")
pacman::p_load ("shiny", "readxl", "dplyr", "tidyr",
                "ggplot2", "scales", "janitor", "ggrepel")
```

3. The included workbook is already in the `data/` folder.
4. Open `app.R` in RStudio and click **Run App**.

## Data caveat

The workbook contains aggregate summaries rather than match-level observations. The dashboard therefore emphasizes comparisons and sample size. It does not claim to measure the general Siege meta or establish that an operator causes better results.
