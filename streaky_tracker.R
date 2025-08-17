library(shiny)
library(rvest)
library(dplyr)
library(stringr)

hitters_api <- "https://www.fangraphs.com/api/leaders/major-league/data?age=&pos=all&stats=bat&lg=all&qual=1&season=2025&season1=2025&startdate=2025-03-01&enddate=2025-11-01&month=0&hand=&team=0%2Cto&pageitems=2000000000&pagenum=1&ind=0&rost=0&players=&type=8&postseason=&sortdir=default&sortstat=PlayerName"
r <- GET(hitters_api)
hitters <- fromJSON(content(r, as = "text"))
hitters <- as.data.frame(hitters)
hitters_final <- hitters %>%
  select(data.PlayerName, data.TeamName, data.playerid, data.teamid)

teams <- c("Atlanta Braves", "Boston Red Sox")

ui <- fluidPage(
  titlePanel("Select an MLB Team"),
  sidebarLayout(
    sidebarPanel(
      selectInput("team", "Choose a Team:", choices = c("", teams)),
      selectInput("player", "Choose a Player:", choices = c("", hitters_final$data.PlayerName))
    ),
    mainPanel(
      textOutput("selection")
    )
  )
)

server <- function(input, output) {
  output$selection <- renderText({
    paste0("You selected: ", input$team, ", ", input$player)
  })
}

shinyApp(ui, server)