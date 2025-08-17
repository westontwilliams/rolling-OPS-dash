library(shiny)
library(rvest)
library(dplyr)
library(stringr)

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
      textOutput("selection")
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
    req(input$team, input$player)
    paste0("You selected: ", input$team, ", ", input$player)
  })
}

shinyApp(ui, server)