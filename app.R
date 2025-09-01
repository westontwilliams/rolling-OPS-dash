library(shiny)
library(rvest)
library(dplyr)
library(stringr)
library(DT)
library(ggplot2)
library(zoo)
library(httr)
library(jsonlite)

hitters_api <- "https://www.fangraphs.com/api/leaders/major-league/data?age=&pos=all&stats=bat&lg=all&qual=1&season=2025&season1=2025&startdate=2025-03-01&enddate=2025-11-01&month=0&hand=&team=0%2Cto&pageitems=2000000000&pagenum=1&ind=0&rost=0&players=&type=8&postseason=&sortdir=default&sortstat=PlayerName"
r <- GET(hitters_api)
hitters <- fromJSON(content(r, as = "text"))
hitters <- as.data.frame(hitters)

teams_api <- "https://www.fangraphs.com/api/leaders/major-league/data?age=&pos=all&stats=bat&lg=all&qual=1&season=2025&season1=2025&startdate=2025-03-01&enddate=2025-11-01&month=0&hand=&team=0%2Cts&pageitems=2000000000&pagenum=1&ind=0&rost=0&players=&type=8&postseason=&sortdir=asc&sortstat=TeamNameAbb"
r <- GET(teams_api)
teams <- fromJSON(content(r, as = "text"))
teams <- as.data.frame(teams)

league_api <- "https://www.fangraphs.com/api/leaders/major-league/data?age=&pos=all&stats=bat&lg=all&qual=0&season=2025&season1=2025&startdate=2025-03-01&enddate=2025-11-01&month=0&hand=&team=0%2Css&pageitems=30&pagenum=1&ind=0&rost=0&players=&type=8&postseason=&sortdir=default&sortstat=WAR"
r <- GET(league_api)
league <- fromJSON(content(r, as = "text"))
league <- as.data.frame(league)

ui <- fluidPage(
  titlePanel("Rolling OPS Tracker"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput("team", "Choose a Team:", choices = c("", teams$data.TeamName)),
      actionButton("clear_team", "Clear Team Selection"),
      selectizeInput("player", "Choose a Player:", choices = c("", hitters$data.PlayerName)),
      actionButton("clear_player", "Clear Player Selection")
    ),
    mainPanel(
      textOutput("selection"),
      DTOutput("game_log"),
      plotOutput("ops_plot")
    )
  )
)

server <- function(input, output, session) {
  filtered_players <- reactive({
    req(input$team)
    tid <- teams$data.teamid[teams$data.TeamName == input$team]
    
    hitters %>%
      filter(data.teamid == tid) %>%
      pull(data.PlayerName)
  })
  
  observe({
    updateSelectInput(session, "player",
                      choices = c("", filtered_players()),
                      selected = "")
  })
  
  observeEvent(input$clear_team, {
    updateSelectInput(session, "team", selected = "")
    updateSelectInput(session, "player",
                      choices = c("", sort(hitters$data.PlayerName)),
                      selected = "")
  })
  
  observeEvent(input$clear_player, {
    updateSelectInput(session, "player", selected = "")
  })
  
  output$selection <- renderText({
    req(input$player)
    player_teams <- hitters %>%
      filter(data.PlayerName == input$player) %>%
      pull(data.TeamName) %>%
      unique() %>%
      paste(collapse = "/")
    paste0(input$player, " (", player_teams, "): Last 10 Games")
  })
  
  player_game_log <- reactive({
    req(input$player)
    pids <- hitters$data.playerid[hitters$data.PlayerName == input$player]
    positions <- hitters$data.position[hitters$data.PlayerName == input$player]
    
    all_logs <- lapply(seq_along(pids), function(i) {
      log_api <- paste0("https://www.fangraphs.com/api/players/game-log?",
                        "playerid=", pids[i],
                        "&position=", URLencode(positions[i]),
                        "&type=0")
      r <- GET(log_api)
      log <- fromJSON(content(r, as = "text"))
      log <- as.data.frame(log)
      colnames(log) <- gsub("^mlb\\.", "", colnames(log))
      log
    })
    
    combined_log <- bind_rows(all_logs) %>%
      slice(-1) %>%
      mutate(Date = str_extract(Date, "(?<=\\>).*?(?=\\<)")) %>%
      filter(!is.na(Date) & Date != "" & Date < as.Date("2040-01-01")) %>%
      distinct()
    
    combined_log$Date <- as.Date(combined_log$Date)
    combined_log
  })
  
  output$game_log <- renderDT({
    req(player_game_log())
    log_subset <- player_game_log() %>%
      select(Date, PA, AB, R, H, RBI, BB, HR, SB) %>%
      head(10)
    datatable(log_subset, options = list(
      dom = 't',
      paging = FALSE,
      ordering = FALSE,
      info = FALSE
    ),
    rownames = FALSE )
  })
  
  output$ops_plot <- renderPlot({
    req(player_game_log())
    log_subset <- player_game_log()
    log_subset$Date <- as.Date(log_subset$Date)
    log_subset$OPS <- as.numeric(log_subset$OPS)
    log_subset <- log_subset %>%
      mutate(weighted_ops = OPS*PA) %>% 
      arrange(Date) %>%
      mutate(rolling_weighted_ops = zoo::rollsum(weighted_ops, k = 10, fill = NA, align = "right")) %>% 
      mutate(sum_pa = zoo::rollsum(PA, k=10, fill = NA, align = "right")) %>% 
      mutate(rolling_ops = rolling_weighted_ops/sum_pa)
    season_ops <- hitters %>%
      filter(data.PlayerName == input$player) %>%
      summarize(season_ops = sum(as.numeric(data.OPS) * as.numeric(data.PA), na.rm = TRUE) / sum(as.numeric(data.PA), na.rm = TRUE)) %>%
      pull(season_ops)
    ggplot(log_subset, aes(x = Date, y = rolling_ops, color = rolling_ops)) +
      geom_line(size = 1) +
      geom_point() +
      scale_color_gradient2(low = "blue", mid = "gray", high = "red", midpoint = league$data.OPS, limits = c(0,2)) +
      geom_hline(yintercept = season_ops, linetype = "dashed", color = "dodgerblue2", size = 0.8) +
      geom_hline(yintercept = league$data.OPS, linetype = "dashed", color = "black", size = 0.8) +
      annotate("text", x = min(log_subset$Date), y = 2.0, label = paste0(" Season OPS: ", sprintf("%.3f", season_ops)), fontface = "bold", hjust = 0.1, color = "dodgerblue2") +
      annotate("text", x = min(log_subset$Date), y = 1.9, label = paste0(" League OPS: ", sprintf("%.3f", league$data.OPS)), fontface = "bold", hjust = 0.1, color = "black") +
      labs(
        title = paste0("10-Game Rolling OPS for ", input$player)
      ) +
      xlab("") +
      ylab("") +
      ylim(0, 2) +
      theme_classic(base_size = 14) +
      guides(color = "none")
  })
}

shinyApp(ui, server)