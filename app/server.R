# server code is gonna go here for now (will eventually be moved to an R-script)

# server/back end code for the app
server <- function(input, output, session) {
  
  # ---------- helpers ----------
  superhero_plot_theme <- function() {
    theme_minimal() +
      theme(
        plot.background = element_rect(fill = "#2b3e50", color = NA),
        panel.background = element_rect(fill = "#2b3e50", color = NA),
        panel.grid.major = element_line(color = "#4e5d6c", linewidth = 0.4),
        panel.grid.minor = element_line(color = "#3e4c59", linewidth = 0.3),
        text = element_text(color = "white"),
        axis.text = element_text(color = "white"),
        axis.title = element_text(color = "white", face = "bold"),
        plot.title = element_text(color = "white", face = "bold", hjust = 0.5),
        legend.background = element_rect(fill = "#2b3e50", color = NA),
        legend.key = element_rect(fill = "#2b3e50", color = NA),
        legend.text = element_text(color = "white"),
        legend.title = element_text(color = "white", face = "bold")
      )
  }
  
  pretty_stat <- function(stat) {
    ifelse(stat %in% names(stat_category_labels), yes = stat_category_labels[stat], stat)
  }
  
  year_int <- reactive({
    req(input$year_choice)
    yr <- suppressWarnings(as.integer(input$year_choice))
    req(!is.na(yr))
    yr
  })
  
  teams_year <- reactive({
    subset(Lahman::Teams, yearID == year_int())
  })
  
  team_row <- reactive({
    req(input$team_choice)
    df <- teams_year()
    row <- df[df$teamID == input$team_choice, , drop = FALSE]
    req(nrow(row) >= 1)
    row[1, , drop = FALSE]
  })
  
  ranked_dot_plot <- function(df, stat, year, selected_team) {
    validate(
      need(stat %in% names(df), "Selected stat isn't available for this year."),
      need(is.numeric(df[[stat]]), "Selected stat isn't numeric, can't plot.")
    )
    
    d <- df[, c("teamID", "name", stat)]
    names(d)[3] <- "value"
    
    d <- d[is.finite(d$value), , drop = FALSE]
    validate(need(nrow(d) > 1, "Not enough data to plot."))
    
    d <- d[order(d$value, decreasing = TRUE), , drop = FALSE]
    d$rank <- seq_len(nrow(d))
    
    pretty_label <- pretty_stat(stat)
    
    d$hover_text <- paste0(
      "Team: ", d$name, " (", d$teamID, ")",
      "<br>Year: ", year,
      "<br>", pretty_label, ": ", d$value,
      "<br>Rank: ", d$rank
    )
    
    ggplot(d, aes(x = value, y = rank, text = hover_text)) +
      geom_point(color = "white", size = 2, alpha = 0.8) +
      geom_point(
        data = subset(d, teamID == selected_team),
        color = "#f39c12",
        size = 4
      ) +
      geom_text(
        data = subset(d, teamID == selected_team),
        aes(label = teamID),
        color = "#f39c12",
        nudge_x = 0.08 * diff(range(d$value, na.rm = TRUE)),
        size = 4,
        fontface = "bold"
      ) +
      geom_segment(
        data = subset(d, teamID == selected_team),
        aes(x = min(d$value), xend = value, y = rank, yend = rank),
        color = "#f39c12",
        linewidth = 0.6,
        alpha = 0.5
      ) + 
      scale_y_reverse() +
      labs(
        title = paste0(year, " — ", pretty_label),
        x = pretty_label,
        y = "League Rank (1 = most)"
      ) +
      superhero_plot_theme()
  }
  
  get_available_stats <- function(row_df) {
    num_cols <- names(row_df)[vapply(row_df, is.numeric, logical(1))]
    intersect(useful_team_stats, num_cols)
  }
  
  # ---------- sidebar inputs ----------
  observeEvent(year_int(), {
    df <- teams_year()
    
    # avoid duplicate teamIDs if present
    df <- df[!duplicated(df$teamID), , drop = FALSE]
    
    team_labels <- paste0(df$name, " (", df$teamID, ")")
    team_values <- df$teamID
    names(team_values) <- team_labels
    
    updateSelectizeInput(
      session,
      "team_choice",
      choices = team_values,
      selected = character(0),
      server = TRUE
    )
    
    updateSelectizeInput(
      session,
      "stat_choice",
      choices = character(0),
      selected = character(0),
      server = TRUE
    )
  })
  
  available_stats <- reactive({
    req(input$team_choice)
    row <- team_row()
    
    get_available_stats(row)
  })
  
  observeEvent(input$team_choice, {
    stats <- available_stats()
    req(length(stats) > 0)
    
    default_stat <- if ("W" %in% stats) "W" else stats[1]
    
    pretty_stats <- stats
    names(pretty_stats) <- pretty_stat(stats)
    
    updateSelectizeInput(
      session,
      "stat_choice",
      choices = pretty_stats,
      selected = default_stat,
      server = TRUE
    )
  })
  
  # ---------- card header ----------
  output$plot_section_table_title <- renderUI({
    req(input$team_choice)
    
    row <- team_row()
    team_name <- row$name
    
    tags$h4(
      paste0("Team Summary for ", team_name, " (", input$team_choice, ") — ", year_int()),
      style = "font-weight:700; margin-top:10px;"
    )
  })
  
  output$card_title <- renderUI({
    req(input$team_choice)
    
    stat_txt <- if (!is.null(input$stat_choice) && nzchar(input$stat_choice)) {
      paste0(" | Stat: ", pretty_stat(input$stat_choice))
    } else {
      ""
    }
    
    tags$strong(
      paste0("Year: ", year_int(), " | Team: ", input$team_choice, stat_txt)
    )
  })
  
  output$plot_section_title <- renderUI({
    req(input$stat_choice)
    
    tags$h4(
      paste("League Comparison for", pretty_stat(input$stat_choice)),
      style = "font-weight:700; margin-top:20px;"
    )
  })
  
  output$plot_distrib <- renderUI({
    req(input$stat_choice)
    
    tags$h4(
      paste("League Distribution Comparison for", pretty_stat(input$stat_choice)),
      style = "font-weight:700; margin-top:20px;"
    )
  })
  # ---------- core team stats ----------
  output$team_stats_tbl <- renderTable({
    row <- team_row()
    
    wanted <- c("W", "L", "R", "RA", "HR", "SB", "ERA")
    cols <- intersect(wanted, names(row))
    req(length(cols) > 0)
    
    data.frame(
      Statistics  = pretty_stat(cols),
      Totals = vapply(cols, function(x) row[[x]][[1]], numeric(1)),
      row.names = NULL
    )
  }, striped = TRUE, bordered = TRUE, spacing = "s")
  
  # ---------- ranked dot plots ----------
  output$stat_plot_year <- renderPlotly({
    req(input$team_choice, input$stat_choice)
    df <- teams_year()
    
    ggplotly(
      ranked_dot_plot(df, input$stat_choice, year_int(), input$team_choice),
      tooltip = "text") %>% config(displayModeBar = FALSE)
  })
  
  output$stat_plot_prev <- renderPlotly({
    req(input$team_choice, input$stat_choice)
    prev_year <- year_int() - 1
    
    df_prev <- subset(Lahman::Teams, yearID == prev_year)
    validate(need(nrow(df_prev) > 0, "No data available for prior year."))
    
    ggplotly(
      ranked_dot_plot(df_prev, input$stat_choice, prev_year, input$team_choice),
      tooltip = "text") %>% config(displayModeBar = FALSE)
  })
  
  output$stat_distrib_year <- renderPlot({
    req(input$team_choice, input$stat_choice)
    df <- teams_year()
    
    validate(
      need(input$stat_choice %in% names(df), "Selected stat isn't available for this year."),
      need(is.numeric(df[[input$stat_choice]]), "Selected stat isn't numeric.")
    )
    
    output$stat_distrib_year <- renderPlot({
      req(input$team_choice, input$stat_choice)
      df <- teams_year()
      
      validate(
        need(input$stat_choice %in% names(df), "Selected stat isn't available for this year."),
        need(is.numeric(df[[input$stat_choice]]), "Selected stat isn't numeric.")
      )
      
      team_value <- df[[input$stat_choice]][df$teamID == input$team_choice]
      req(length(team_value) > 0)
      team_value <- team_value[1]
      
      mean_value <- mean(df[[input$stat_choice]], na.rm = TRUE)
      median_value <- median(df[[input$stat_choice]], na.rm = TRUE)
      
      percentile <- round(
        mean(df[[input$stat_choice]] <= team_value, na.rm = TRUE) * 100
      )
      
      ggplot(df, aes(x = .data[[input$stat_choice]])) +
        geom_density(
          fill = "white",
          color = "#f39c12",
          alpha = 0.2
        ) +
        geom_vline(
          xintercept = team_value,
          color = "#f39c12",
          linewidth = 1.2
        ) +
        geom_vline(
          xintercept = mean_value,
          color = "white",
          linetype = "dashed"
        ) +
        geom_vline(
          xintercept = median_value,
          color = "#00bc8c",
          linetype = "dashed"
        ) +
        labs(
          x = pretty_stat(input$stat_choice),
          y = "Density",
          title = paste("Distribution of", pretty_stat(input$stat_choice), "in", year_int()),
          subtitle = paste(
            "Selected team percentile:", percentile,
            "| Mean = white dashed",
            "| Median = green dashed"
          )
        ) +
        superhero_plot_theme()
    })
    
    output$stat_distrib_prev <- renderPlot({
      req(input$team_choice, input$stat_choice)
      prev_year <- year_int() - 1
      
      df_prev <- subset(Lahman::Teams, yearID == prev_year)
      
      validate(
        need(nrow(df_prev) > 0, "No data available for prior year."),
        need(input$stat_choice %in% names(df_prev), "Selected stat isn't available for prior year."),
        need(is.numeric(df_prev[[input$stat_choice]]), "Selected stat isn't numeric.")
      )
      
      team_value <- df_prev[[input$stat_choice]][df_prev$teamID == input$team_choice]
      req(length(team_value) > 0)
      team_value <- team_value[1]
      
      mean_value <- mean(df_prev[[input$stat_choice]], na.rm = TRUE)
      median_value <- median(df_prev[[input$stat_choice]], na.rm = TRUE)
      
      percentile <- round(
        mean(df_prev[[input$stat_choice]] <= team_value, na.rm = TRUE) * 100
      )
      
      ggplot(df_prev, aes(x = .data[[input$stat_choice]])) +
        geom_density(
          fill = "white",
          color = "#f39c12",
          alpha = 0.2
        ) +
        geom_vline(
          xintercept = team_value,
          color = "#f39c12",
          linewidth = 1.2
        ) +
        geom_vline(
          xintercept = mean_value,
          color = "white",
          linetype = "dashed"
        ) +
        geom_vline(
          xintercept = median_value,
          color = "#00bc8c",
          linetype = "dashed"
        ) +
        labs(
          x = pretty_stat(input$stat_choice),
          y = "Density",
          title = paste("Distribution of", pretty_stat(input$stat_choice), "in", prev_year),
          subtitle = paste(
            "Selected team percentile:", percentile,
            "| Mean = white dashed",
            "| Median = green dashed"
          )
        ) +
        superhero_plot_theme()
    })
  })
  
  # ---------- notes section ----------
  
  output$notes_section <- renderUI({
    includeMarkdown("notes.md")
  })
  
  # ---------- team vs team tab -------
  
  observeEvent(list(year_int(), input$team_choice), {
    req(input$team_choice)
    
    df <- teams_year()
    df <- df[!duplicated(df$teamID), , drop = FALSE]
    
    # exclude the currently selected team
    df <- df[df$teamID != input$team_choice, , drop = FALSE]
    
    compare_labels <- paste0(df$name, " (", df$teamID, ")")
    compare_values <- df$teamID
    names(compare_values) <- compare_labels
    
    updateSelectizeInput(
      session,
      "team_compare_choice",
      choices = compare_values,
      selected = character(0),
      server = TRUE
    )
  })
  
  output$team_compare_summary <- renderUI({
    req(input$team_choice, input$team_compare_choice)
    
    df <- teams_year()
    
    row1 <- df[df$teamID == input$team_choice, , drop = FALSE][1, , drop = FALSE]
    row2 <- df[df$teamID == input$team_compare_choice, , drop = FALSE][1, , drop = FALSE]
    
    team1_name <- row1$name
    team2_name <- row2$name
    
    stat <- input$stat_choice
    pretty_label <- pretty_stat(stat)
    
    val1 <- row1[[stat]][[1]]
    val2 <- row2[[stat]][[1]]
    diff_val <- round(val1 - val2, 2)
    
    comparison_text <- if (diff_val > 0) {
      paste0(team1_name, " lead ", team2_name, " by ", diff_val, " in ", pretty_label, ".")
    } else if (diff_val < 0) {
      paste0(team2_name, " lead ", team1_name, " by ", abs(diff_val), " in ", pretty_label, ".")
    } else {
      paste0(team1_name, " and ", team2_name, " are tied in ", pretty_label, ".")
    }
    
    tagList(
      tags$h4(
        paste0("Comparison: ", team1_name, " (", input$team_choice, ") vs ",
               team2_name, " (", input$team_compare_choice, ")"),
        style = "font-weight:700;"
      ),
      tags$p(comparison_text)
    )
  })
  
  output$team_compare_dumbbell <- renderPlot({
    req(input$team_choice, input$team_compare_choice)
    
    df <- teams_year()
    
    core_stats <- c("W", "L", "R", "RA", "HR", "SB", "ERA")
    core_stats <- core_stats[core_stats %in% names(df)]
    
    get_percentile <- function(vec, value) {
      round(mean(vec <= value, na.rm = TRUE) * 100, 1)
    }
    
    row1 <- df[df$teamID == input$team_choice, ][1, ]
    row2 <- df[df$teamID == input$team_compare_choice, ][1, ]
    
    compare_df <- data.frame(
      stat = core_stats,
      team1 = sapply(core_stats, function(s) {
        get_percentile(df[[s]], row1[[s]])
      }),
      team2 = sapply(core_stats, function(s) {
        get_percentile(df[[s]], row2[[s]])
      })
    )
    
    compare_df$stat <- factor(
      pretty_stat(compare_df$stat),
      levels = rev(pretty_stat(core_stats))
    )
    
    ggplot(compare_df, aes(y = stat)) +
      
      # connecting line
      geom_segment(
        aes(x = team1, xend = team2, yend = stat),
        color = "gray70",
        linewidth = 1
      ) +
      
      # team 1
      geom_point(
        aes(x = team1),
        color = "#f39c12",
        size = 3
      ) +
      
      # team 2
      geom_point(
        aes(x = team2),
        color = "#00bc8c",
        size = 3
      ) +
      
      labs(
        title = "Team Comparison (Percentiles)",
        subtitle = "Higher = better relative to league",
        x = "Percentile",
        y = NULL
      ) +
      
      xlim(0, 100) +
      superhero_plot_theme()
  })
}