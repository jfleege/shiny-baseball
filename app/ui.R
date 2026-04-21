# ui/display code here
ui <- page_sidebar(
  
  # title
  title = tags$span(
    style = "font-weight:700; font-size:1.6rem;",
    "Baseball Data and Analytics — Courtesy of the Lahman Package"),
  
  # CSS stuff for notes section
  tags$style(HTML("
  .definition-label {
    color: #f39c12;
    font-weight: 700;
  }
  
  .team-card-table {
    width: 100%;
  }
  
  .team-card-table table {
    width: 100% !important;
  }
  
  .team-card-table th,
  .team-card-table td {
    padding: 10px 14px;
  }
")),
  
  # left sidebar
  sidebar = sidebar(
    position = "left",
    
    # year selection first  
    selectInput(
      "year_choice",
      tags$h5(style = "font-weight:700;", "Select a Year to View:"),
      choices = sort(unique(Lahman::Teams$yearID), decreasing = TRUE)
    ),
    
    # team selection second (choices get updated from server)
    selectizeInput(
      "team_choice",
      tags$h5(style = "font-weight:700;", "Choose a Team to View:"),
      choices = NULL,
      options = list(placeholder = "Select a year first")
    ),
    
    # stat/metric (choices get updated from server)
    selectizeInput(
      "stat_choice",
      tags$h5(style = "font-weight:700;", "Select a Stat to View:"),
      choices = NULL,
      options = list(placeholder = "Select a team first")
    )
  ),
  
  # main page / card code here
  layout_columns(
    col_widths = c(8, 4),
    
    # main page / card code here
    card(
      card_header(uiOutput("card_title")),
      
      navset_card_tab(
        nav_panel(
          "Dashboard",
          card_body(
            uiOutput("plot_section_table_title"),
            
            tableOutput("team_stats_tbl"),
            
            uiOutput("top_players_section"),
            
            uiOutput("plot_section_title"),
            
            uiOutput("current_year_label"),
            fluidRow(
              column(6, plotlyOutput("stat_plot_year", height = "320px")),
              column(6, plotOutput("stat_distrib_year", height = "320px"))
            ),
            
            tags$br(),
            
            uiOutput("previous_year_label"),
            fluidRow(
              column(6, plotlyOutput("stat_plot_prev", height = "320px")),
              column(6, plotOutput("stat_distrib_prev", height = "320px"))
            )
          )
        ),
        
        nav_panel(
          "Team vs Team",
          card_body(
            
            # comparison selector
            selectizeInput(
              "team_compare_choice",
              tags$h5(style = "font-weight:700;", "Select a Team to Compare With:"),
              choices = NULL,
              options = list(placeholder = "Select a Team to Compare With")
            ),
            
            tags$br(),
            
            uiOutput("team_compare_summary"),
            
            tags$br(),
            
            fluidRow(
              column(
                6,
                div(
                  style = "
    background-color: #2b3e50;
    padding: 20px;
    border-radius: 12px;
    margin-right: 10px;
  ",
                  
                  uiOutput("team1_summary_title"),
                  
                  div(
                    class = "team-card-table",
                    tableOutput("team1_stats_tbl")
                  ),
                  
                  tags$div(style = "margin-top:15px;"),
                  
                  uiOutput("team1_top_players_section")
                )
              ),
              
              column(
                6,
                div(
                  style = "
    background-color: #2b3e50;
    padding: 20px;
    border-radius: 12px;
    margin-left: 10px;
  ",
                  
                  uiOutput("team2_summary_title"),
                  
                  div(
                    class = "team-card-table",
                    tableOutput("team2_stats_tbl")
                  ),
                  
                  tags$div(style = "margin-top:15px;"),
                  
                  uiOutput("team2_top_players_section")
                )
              )
            )
          )
        )
      )
    ),
    
    # right sidebar (for notes.md)
    card(
      card_header(tags$h4("Help Guide", style = "font-weight:700;")),
      card_body(
        div(
          style = "overflow-y: auto; max-height: 80vh;",
          uiOutput("notes_section")
        )
      )
    )
  ),
  
  theme = bs_theme(
    bootswatch = "superhero"
  )
)