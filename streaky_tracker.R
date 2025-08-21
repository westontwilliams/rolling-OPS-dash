library(shiny)
library(rvest)
library(dplyr)
library(stringr)
library(DT)
library(ggplot2)
library(zoo)

hitters_api <- "https://www.fangraphs.com/api/leaders/major-league/data?age=&pos=all&stats=bat&lg=all&qual=1&season=2025&season1=2025&startdate=2025-03-01&enddate=2025-11-01&month=0&hand=&team=0%2Cto&pageitems=2000000000&pagenum=1&ind=0&rost=0&players=&type=8&postseason=&sortdir=default&sortstat=PlayerName"
r <- GET(hitters_api)
hitters <- fromJSON(content(r, as = "text"))
hitters <- as.data.frame(hitters)

teams_api <- "https://www.fangraphs.com/api/leaders/major-league/data?age=&pos=all&stats=bat&lg=all&qual=1&season=2025&season1=2025&startdate=2025-03-01&enddate=2025-11-01&month=0&hand=&team=0%2Cts&pageitems=2000000000&pagenum=1&ind=0&rost=0&players=&type=8&postseason=&sortdir=asc&sortstat=TeamNameAbb"
r <- GET(teams_api)
teams <- fromJSON(content(r, as = "text"))
teams <- as.data.frame(teams)

ui <- fluidPage(
  titlePanel("Select an MLB Team"),
  sidebarLayout(
    sidebarPanel(
      selectInput("team", "Choose a Team:", choices = c("", teams$data.TeamName)),
      selectInput("player", "Choose a Player:", choices = c("", hitters$data.PlayerName))
    ),
    mainPanel(
      textOutput("selection"),
      DTOutput("game_log"),
      plotOutput("wrc_plot")
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
  
  output$selection <- renderText({
    req(input$player)
    player_team <- hitters$data.TeamName[hitters$data.PlayerName == input$player]
    paste0(player_team, " - ", input$player)
  })
  
  player_game_log <- reactive({
    req(input$player)
    pid <- hitters$data.playerid[hitters$data.PlayerName == input$player]
    pos <- hitters$data.position[hitters$data.PlayerName == input$player]
    
    log_api <- paste0("https://www.fangraphs.com/api/players/game-log?",
      "playerid=", pid,
      "&position=", URLencode(pos),
      "&type=0"
    )
    r <- GET(log_api)
    log <- fromJSON(content(r, as = "text"))
    log <- as.data.frame(log)
    colnames(log) <- gsub("^mlb\\.", "", colnames(log))
    log
  })
  
  output$game_log <- renderDT({
    req(player_game_log())
    log_subset <- player_game_log() %>%
      slice(-1) %>% 
      mutate(Date = str_extract(Date, "(?<=\\>).*?(?=\\<)")) %>% 
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
  
  output$wrc_plot <- renderPlot({
    req(player_game_log())
    log_subset <- player_game_log() %>%
      slice(-1) %>% 
      mutate(Date = str_extract(Date, "(?<=\\>).*?(?=\\<)"))
    log_subset$Date <- as.Date(log_subset$Date)
    log_subset$WRC. <- as.numeric(log_subset$wRC.)
    log_subset <- log_subset %>%
      arrange(Date) %>%
      mutate(rolling_wrc = zoo::rollmean(wRC., k = 10, fill = NA, align = "right"))
    ggplot(log_subset, aes(x = Date, y = rolling_wrc)) +
      geom_line(color = "steelblue", size = 1) +
      geom_point(color = "darkred") +
      labs(
        title = paste0("10-Game Rolling wRC+ for ", input$player),
        x = "Date",
        y = "Rolling wRC+"
      ) +
      theme_minimal(base_size = 14)
  })
}

shinyApp(ui, server)