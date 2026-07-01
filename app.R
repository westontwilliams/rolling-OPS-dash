library(shiny)
library(rvest)
library(dplyr)
library(stringr)
library(DT)
library(ggplot2)
library(zoo)
library(httr)
library(jsonlite)

hitters_api <- "https://statsapi.mlb.com/api/v1/stats?stats=season&group=hitting&season=2026&sportIds=1&gameType=R&limit=2000&playerPool=all"
r <- GET(hitters_api)
hitters_raw <- fromJSON(content(r, as = "text"), flatten = TRUE)
hitters <- hitters_raw$stats$splits[[1]]
colnames(hitters) <- gsub("^stat\\.", "", colnames(hitters))
hitters <- hitters %>% mutate(
  PA = as.numeric(plateAppearances),
  OPS = as.numeric(ops)
)

teams_api <- "https://statsapi.mlb.com/api/v1/teams?sportId=1&season=2026&activeStatus=Yes"
r <- GET(teams_api)
teams <- fromJSON(content(r, as = "text"), flatten = TRUE)$teams

league_ops <- sum(hitters$OPS * hitters$PA, na.rm = TRUE) /
  sum(hitters$PA, na.rm = TRUE)

teams <- teams %>% arrange(abbreviation)
team_choices <- setNames(teams$abbreviation, teams$abbreviation)

all_players <- sort(unique(hitters$player.fullName))

ui <- fluidPage(
  lang = "en",
  titlePanel("Rolling OPS Tracker"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput("team", "Choose a Team:", choices = c("", team_choices)),
      actionButton("clear_team", "Clear Team Selection"),
      selectizeInput("player", "Choose a Player:", choices = c("", all_players)),
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
    tid <- teams$id[teams$abbreviation == input$team]
    
    hitters %>%
      filter(team.id == tid) %>%
      pull(player.fullName) %>%
      sort()
  })
  
  observe({
    updateSelectInput(session, "player",
                      choices = c("", filtered_players()),
                      selected = "")
  })
  
  observeEvent(input$clear_team, {
    updateSelectInput(session, "team", selected = "")
    updateSelectInput(session, "player",
                      choices = c("", sort(hitters$player.fullName)),
                      selected = "")
  })
  
  observeEvent(input$clear_player, {
    updateSelectInput(session, "player", selected = "")
  })
  
  output$selection <- renderText({
    req(input$player)
    player_team_ids <- hitters %>%
      filter(player.fullName == input$player) %>%
      pull(team.id) %>%
      unique()
    player_teams <- teams$abbreviation[teams$id %in% player_team_ids] %>%
      paste(collapse = "/")
    paste0(input$player, " (", player_teams, "): Last 10 Games")
  })
  
  player_game_log <- reactive({
    req(input$player)
    pids <- hitters$player.id[hitters$player.fullName == input$player]
    
    all_logs <- lapply(seq_along(pids), function(i) {
      log_api <- paste0("https://statsapi.mlb.com/api/v1/people/", pids,
                        "/stats?stats=gameLog&group=hitting&season=2026")
      r <- GET(log_api)
      log_raw <- fromJSON(content(r, as = "text"), flatten = TRUE)
      log <- log_raw$stats$splits[[1]]
      colnames(log) <- gsub("^stat\\.", "", colnames(log))
      log
    })
    
    combined_log <- bind_rows(all_logs) %>%
      filter(!is.na(date) & date != "") %>%
      distinct() %>%
      mutate(
        Date = as.Date(date),
        PA = as.numeric(plateAppearances),
        AB = as.numeric(atBats),
        R = as.numeric(runs),
        H = as.numeric(hits),
        RBI = as.numeric(rbi),
        BB = as.numeric(baseOnBalls),
        HR = as.numeric(homeRuns),
        SB = as.numeric(stolenBases),
        HBP = as.numeric(hitByPitch),
        SF = as.numeric(sacFlies),
        TB = as.numeric(totalBases),
        OPS = as.numeric(ops)
      )
    
    combined_log
  })
  
  output$game_log <- renderDT({
    req(player_game_log())
    log_subset <- player_game_log() %>%
      select(Date, PA, AB, R, H, RBI, BB, HR, SB) %>%
      arrange(desc(Date)) %>%
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
    log_subset <- player_game_log() %>%
      arrange(Date) %>%
      mutate(
        roll_AB  = zoo::rollsum(AB,  k = 10, fill = NA, align = "right"),
        roll_H   = zoo::rollsum(H,   k = 10, fill = NA, align = "right"),
        roll_BB  = zoo::rollsum(BB,  k = 10, fill = NA, align = "right"),
        roll_HBP = zoo::rollsum(HBP, k = 10, fill = NA, align = "right"),
        roll_SF  = zoo::rollsum(SF,  k = 10, fill = NA, align = "right"),
        roll_TB  = zoo::rollsum(TB,  k = 10, fill = NA, align = "right"),
        obp_denom = roll_AB + roll_BB + roll_HBP + roll_SF,
        rolling_ops = ifelse(
          roll_AB == 0 | obp_denom == 0, NA,
          (roll_H + roll_BB + roll_HBP) / obp_denom + roll_TB / roll_AB
        )
      ) %>%
      filter(!is.na(rolling_ops))
    
    season_ops <- hitters %>%
      filter(player.fullName == input$player) %>%
      summarize(season_ops = sum(OPS * PA, na.rm = TRUE) / sum(PA, na.rm = TRUE)) %>%
      pull(season_ops)
    
    ggplot(log_subset, aes(x = Date, y = rolling_ops, color = rolling_ops)) +
      geom_line(linewidth = 1) +
      geom_point() +
      scale_color_gradient2(low = "blue", mid = "gray", high = "red", midpoint = league_ops, limits = c(0,2)) +
      geom_hline(yintercept = season_ops, linetype = "dashed", color = "dodgerblue2", linewidth = 0.8) +
      geom_hline(yintercept = league_ops, linetype = "dashed", color = "black", linewidth = 0.8) +
      annotate("text", x = min(log_subset$Date), y = 2.0, label = paste0(" Season OPS: ", sprintf("%.3f", season_ops)), fontface = "bold", hjust = 0.1, color = "dodgerblue2") +
      annotate("text", x = min(log_subset$Date), y = 1.9, label = paste0(" League OPS: ", sprintf("%.3f", league_ops)), fontface = "bold", hjust = 0.1, color = "black") +
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