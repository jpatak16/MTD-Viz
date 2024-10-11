library(pacman)
library(shiny)
library(magrittr)
library(dplyr)
library(stringr)
library(gt)
library(gtExtras)
library(googlesheets4)

gs4_auth(path = ".secrets/asu-mtd-c191882d31e3.json")

ss <- gs4_get("https://docs.google.com/spreadsheets/d/13TtpNrsgFBv9UR4s-mw15uDd17UruiV9OZM6fWs03kE/edit?usp=sharing")

hoopR_espn_nba_teams <- read.csv("espn_nba_teams.csv")
nba_players = read.csv("nba_players.csv")

nba_teams <- hoopR_espn_nba_teams |> 
  pull(display_name)

# Define UI for application that draws a histogram
ui = navbarPage("2024 NBA Trade Deadline Competition", fluid = TRUE,
                tabPanel(
                  "Trade Entry",
                  tags$style(type="text/css",
                  ".shiny-output-error { visibility: hidden; }",
                  ".shiny-output-error:before { visibility: hidden; }",
                  ".modal-dialog { width: fit-content !important; }"),
                  fluidRow(
                    column(
                      9, 
                      h1(span("2024 NBA Trade Deadline Competition", 
                              style = 'color:rgba(169,20,20,128);')), 
                      h1(span("Trade Entry", 
                              style = 'font-size: 60px; font-weight: bold;'))
                    ),
                    column(
                      3, 
                      img(src="ASU-NTDC-Logo.png", height = 180, width = 240)
                    )
                  ),
                  tags$hr(style="border-color:rgba(169,20,20,128);"),
                  fluidRow(
                    column(
                      6, 
                      selectizeInput(
                        "teamsInvolved", 
                        label = "Teams Involved", 
                        choices = nba_teams, 
                        multiple = T
                      )
                    ),
                    column(
                      2, 
                      actionButton(
                        "submit", 
                        "Submit For Approval"
                      )
                    ),
                    column(
                      2, 
                      actionButton(
                        "clearpg", 
                        "Clear All Input"
                      )
                    )
                  ),
                  h2(" ")
                ),
              )

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  transaction_log = reactiveVal(
      value = read_sheet("https://docs.google.com/spreadsheets/d/13TtpNrsgFBv9UR4s-mw15uDd17UruiV9OZM6fWs03kE/edit?usp=sharing")
    )

  #clear page on button click
  observeEvent(input$clearpg, {
    session$reload()
  })


}


shinyApp(ui, server)
