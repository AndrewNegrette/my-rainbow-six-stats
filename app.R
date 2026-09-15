
pacman::p_load ("shiny", "readxl", "dplyr", "tidyr",
                "ggplot2", "scales", "janitor", "ggrepel")

DATA_FILE <- file.path("data", "My R6 Data.xlsx")

read_stats <- function(sheet) {
  read_excel(DATA_FILE, sheet = sheet, .name_repair = "unique") |>
    janitor::clean_names()
}

operator_sheets <- c(
  "Overall" = "Op_all",
  "PC (All Games)" = "Op_PC_all",
  "PlayStation (All Games)" = "Op_PSN_all",
  "PC (Ranked)" = "Op_PC_ranked",
  "PlayStation (Ranked)" = "Op_PSN_ranked"
)

map_sheets <- c(
  "Overall" = "Map_all",
  "PC (Ranked)" = "Map_PC_ranked",
  "PlayStation (Ranked)" = "Map_PSN_ranked"
)

operator_data <- read_stats(operator_sheets[[1]])
map_data <- read_stats(map_sheets[[1]])

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { background: #f3f5f7; color: #263238; }
      .app-header { background: linear-gradient(135deg, #18232c, #304b5a); color: white; padding: 26px 30px; margin: -15px -15px 24px -15px; box-shadow: 0 3px 10px rgba(0,0,0,.18); }
      .app-header h1 { margin: 0 0 6px 0; font-size: 30px; font-weight: 700; }
      .app-header p { margin: 0; color: #d7e3e8; font-size: 15px; }
      .sidebarPanel { background: white; border-radius: 10px; padding: 20px; box-shadow: 0 2px 8px rgba(31,45,61,.10); }
      .main-panel { background: white; border-radius: 10px; padding: 18px 22px 22px 22px; box-shadow: 0 2px 8px rgba(31,45,61,.10); }
      .nav-tabs > li > a { color: #48616d; font-weight: 600; }
      .nav-tabs > li.active > a { color: #e07a2d; border-top: 3px solid #e07a2d; }
      .form-group label { color: #344b55; font-weight: 600; }
      .irs-bar, .irs-from, .irs-to, .irs-single { background: #e07a2d; border-color: #e07a2d; }
      .irs-line { background: #d8e0e4; border-color: #d8e0e4; }
      .table { background: white; }
      .table > thead > tr > th { background: #304b5a; color: white; }
      .kpi-row { display: flex; gap: 12px; flex-wrap: wrap; margin-bottom: 20px; }
      .kpi-card { background: white; border-left: 5px solid #304b5a; border-radius: 8px; padding: 13px 16px; min-width: 125px; flex: 1; box-shadow: 0 2px 8px rgba(31,45,61,.10); }
      .kpi-label { color: #657780; font-size: 12px; text-transform: uppercase; letter-spacing: .04em; }
      .kpi-value { color: #263238; font-size: 24px; font-weight: 700; margin-top: 3px; }
      .control-section { border-top: 1px solid #d8e0e4; margin-top: 18px; padding-top: 14px; }
      .control-section h4 { color: #304b5a; margin-top: 0; font-weight: 700; }
    "))
  ),
  div(class = "app-header",
      h1("Who Should I Play?"),
      p("My personal Rainbow Six Siege performance dashboard.")),
  sidebarLayout(
    sidebarPanel(
      selectInput("operator_view", "Operator dataset", choices = names(operator_sheets)),
      selectInput("role_filter", "Operator role", choices = c("All roles", "Attacker", "Defender")),
      selectInput(
        "metric", "Chart metric",
        choices = c(
          "Win rate" = "win_percent",
          "K/D" = "kd",
          "Assists per 100 rounds" = "assists_rate",
          "Aces per 100 rounds" = "aces_rate",
          "Team kills per 100 rounds" = "tks_rate"
        )
      ),
      sliderInput("min_rounds", "Minimum rounds played", min = 0, max = 500, value = 30, step = 5),
      selectInput("map_view", "Map dataset", choices = names(map_sheets)),
      div(class = "control-section",
          h4("Platform Comparison"),
          selectInput("platform_view", "Dataset", choices = c("All Games" = "all", "Ranked" = "ranked")),
          sliderInput("platform_n", "Operators to compare", min = 8, max = 20, value = 12, step = 1)
      ),
      hr(),
      h4("Explore your stats"),
      p("Look for operators toward the upper right: strong results and meaningful usage. Point size represents K/D."),
      width = 3
    ),
      mainPanel(
        div(class = "main-panel",
            uiOutput("kpis"),
            tabsetPanel(
              tabPanel("Operator Decision Matrix", plotOutput("operator_plot", height = "750px")),
              tabPanel("Map Advantage", plotOutput("map_plot", height = "750px")),
              tabPanel("Platform Comparison", plotOutput("platform_plot", height = "750px")),
              tabPanel("Recommendations", tableOutput("recommendations"), br(), textOutput("recommendation_note")),
              tabPanel("Data Notes", verbatimTextOutput("data_notes"))
            )
        )
      )
  )
)

server <- function(input, output, session) {
  selected_operators <- reactive({
    df <- read_stats(operator_sheets[[input$operator_view]]) |>
      filter(input$role_filter == "All roles" | tools::toTitleCase(operator_type) == input$role_filter) |>
      filter(rounds_played >= input$min_rounds) |>
      mutate(
        operator = tools::toTitleCase(operator_id),
        role = tools::toTitleCase(operator_type)
      )
    df$metric_value <- switch(input$metric,
      win_percent = df$win_percent,
      kd = df$kd,
      assists_rate = 100 * df$assists / df$rounds_played,
      aces_rate = 100 * df$aces / df$rounds_played,
      tks_rate = 100 * df$tks / df$rounds_played
    )
    df
  })

  selected_maps <- reactive({
    read_stats(map_sheets[[input$map_view]]) |>
      filter(!is.na(map), !is.na(atk_win_percent), !is.na(def_win_percent)) |>
      mutate(
        defender_advantage = def_win_percent - atk_win_percent,
        map = factor(map, levels = map[order(defender_advantage)])
      )
  })

  platform_operators <- reactive({
    suffix <- if (input$platform_view == "ranked") "ranked" else "all"
    pc_sheet <- if (suffix == "ranked") "Op_PC_ranked" else "Op_PC_all"
    psn_sheet <- if (suffix == "ranked") "Op_PSN_ranked" else "Op_PSN_all"
    pc <- read_stats(pc_sheet) |> mutate(platform = "PC")
    psn <- read_stats(psn_sheet) |> mutate(platform = "PlayStation")
    bind_rows(pc, psn) |>
      filter(rounds_played >= input$min_rounds) |>
      mutate(operator = tools::toTitleCase(operator_id), role = tools::toTitleCase(operator_type))
  })

  output$kpis <- renderUI({
    df <- selected_operators()
    validate(need(nrow(df) > 0, "No operators meet the selected minimum round filter."))
    total_rounds <- sum(df$rounds_played, na.rm = TRUE)
    total_wins <- sum(df$wins, na.rm = TRUE)
    total_kills <- sum(df$kills, na.rm = TRUE)
    total_deaths <- sum(df$deaths, na.rm = TRUE)
    kpi <- function(label, value) div(class = "kpi-card", div(class = "kpi-label", label), div(class = "kpi-value", value))
    div(class = "kpi-row",
        kpi("Rounds", comma(total_rounds)),
        kpi("Win rate", percent(total_wins / total_rounds, accuracy = 0.1)),
        kpi("K/D", number(total_kills / total_deaths, accuracy = 0.01)),
        kpi("Kills", comma(total_kills)),
        kpi("Assists", comma(sum(df$assists, na.rm = TRUE))),
        kpi("Aces", comma(sum(df$aces, na.rm = TRUE))))
  })

  output$operator_plot <- renderPlot({
    df <- selected_operators() |> filter(!is.na(metric_value))
    validate(need(nrow(df) > 0, "No operators meet the selected minimum round filter."))

    metric_is_percent <- input$metric %in% c("win_percent")
    metric_label <- switch(input$metric,
      win_percent = "Win rate",
      kd = "K/D",
      assists_rate = "Assists per 100 rounds",
      aces_rate = "Aces per 100 rounds",
      tks_rate = "Team kills per 100 rounds"
    )
    reference_line <- if (input$metric == "win_percent") {
      geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey60")
    } else {
      NULL
    }
    ggplot(df, aes(x = rounds_played, y = metric_value, size = kd, color = role, label = operator)) +
      reference_line +
      geom_point(alpha = 0.8) +
      ggrepel::geom_text_repel(
        size = 3.1,
        box.padding = 0.45,
        point.padding = 0.25,
        force = 2,
        min.segment.length = 0,
        show.legend = FALSE,
        max.overlaps = Inf
      ) +
      scale_x_continuous(labels = comma) +
      scale_y_continuous(
        labels = if (metric_is_percent) percent_format(accuracy = 1) else number_format(accuracy = 0.01),
        limits = if (metric_is_percent) c(0, 1) else c(0, NA)
      ) +
      scale_size_continuous(name = "K/D", range = c(2, 12)) +
      scale_color_manual(values = c("Attacker" = "#e07a2d", "Defender" = "#457b9d")) +
      labs(
        title = paste(input$operator_view, "operators:", metric_label),
        subtitle = "Point size represents K/D. Use the minimum round filter to reduce small sample surprises.",
        x = "Rounds played", y = metric_label, color = "Role"
      ) +
      theme_minimal(base_size = 15) +
      theme(legend.position = "bottom", plot.title = element_text(face = "bold"))
  })

  output$map_plot <- renderPlot({
    df <- selected_maps()
    ggplot(df, aes(x = map, y = defender_advantage, fill = defender_advantage > 0)) +
      geom_col() +
      geom_hline(yintercept = 0, color = "grey30") +
      coord_flip() +
      scale_y_continuous(labels = percent_format(accuracy = 1)) +
      scale_fill_manual(values = c("TRUE" = "#457b9d", "FALSE" = "#e07a2d"), guide = "none") +
      labs(
        title = paste(input$map_view, "map advantage"),
        subtitle = "Positive values indicate a higher defender win rate than attacker win rate",
        x = NULL, y = "Defender win rate − attacker win rate"
      ) +
      theme_minimal(base_size = 15) +
      theme(plot.title = element_text(face = "bold"))
  })

  output$platform_plot <- renderPlot({
    df <- platform_operators()
    validate(need(nrow(df) > 0, "No platform operator rows meet the selected minimum round filter."))
    comparison <- df |>
      group_by(operator) |>
      filter(n_distinct(platform) == 2) |>
      mutate(total_rounds = sum(rounds_played, na.rm = TRUE)) |>
      ungroup() |>
      slice_max(total_rounds, n = input$platform_n, with_ties = FALSE) |>
      group_by(operator) |>
      summarise(
        pc = win_percent[platform == "PC"][1],
        playstation = win_percent[platform == "PlayStation"][1],
        total_rounds = first(total_rounds),
        .groups = "drop"
      ) |>
      mutate(operator = reorder(operator, (pc + playstation) / 2))
    validate(need(nrow(comparison) > 0, "No operators have data on both platforms under the current filter."))

    ggplot(comparison, aes(y = operator)) +
      geom_segment(aes(x = pc, xend = playstation, yend = operator), color = "#b9c4c9", linewidth = 2) +
      geom_point(aes(x = pc), color = "#304b5a", size = 5) +
      geom_point(aes(x = playstation), color = "#e07a2d", size = 5) +
      scale_x_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
      labs(
        title = paste("PC vs. PlayStation:", if (input$platform_view == "ranked") "Ranked" else "All Games"),
        subtitle = paste("Top", input$platform_n, "most played operators with data on both platforms • dark = PC, orange = PlayStation"),
        x = "Win rate", y = NULL
      ) +
      theme_minimal(base_size = 15) +
      theme(plot.title = element_text(face = "bold"), panel.grid.major.y = element_blank())
  })

  output$recommendations <- renderTable({
    selected_operators() |>
      mutate(
        reliability_score = 0.5 * win_percent + 0.3 * pmin(kd / 2, 1) + 0.2 * pmin(rounds_played / 500, 1)
      ) |>
      arrange(desc(reliability_score)) |>
      transmute(
        Operator = operator,
        Role = role,
        Rounds = rounds_played,
        `Win rate` = percent(win_percent, accuracy = 0.1),
        `K/D` = round(kd, 2),
        `Reliability score` = round(reliability_score, 3)
      ) |>
      slice_head(n = 10)
  }, striped = TRUE, bordered = TRUE, hover = TRUE, spacing = "s")

  output$recommendation_note <- renderText({
    "The reliability score is a transparent project metric, not an official rating."
  })

  output$data_notes <- renderPrint({
    cat(
      "This dashboard uses the aggregate tables in My R6 Data.xlsx.\n",
      "Operator rankings are filtered by minimum rounds to reduce small sample surprises.\n",
      "The workbook does not contain match dates or match-level rows, so this project focuses on comparisons and decision support rather than time trends.\n",
      "Win percentages and K/D should be interpreted as personal performance summaries, not causal evidence about operator strength.\n",
      sep = ""
    )
  })
}

shinyApp(ui, server)
