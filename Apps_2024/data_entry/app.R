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

nba_teams <- hoopR_espn_nba_teams |> 
  pull(display_name)


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
                  tags$hr(style="border-color:rgba(169,20,20,128); margin-bottom:0px"),
                  fluidRow(
                    style = "background-color: #dddddd; padding-top: 20px; padding-bottom: 20px; display: flex; align-items: center;",
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
                  # Conditional Panel for 1 Team
                  fluidRow(
                    conditionalPanel(
                      condition = "input.teamsInvolved.length == 1",
                      column(
                        12,
                        div(
                          style = "text-align: center;",
                          htmlOutput("team1_logo"),
                          h4("Trading Away:")
                        )
                      )
                    )
                  ),
                  # Conditional Panel for 2 Teams
                  fluidRow(
                    conditionalPanel(
                      condition = "input.teamsInvolved.length == 2",
                      column(
                        6,
                        div(
                          style = "text-align: center; border-right: 2px solid black; height: 100%;",
                          htmlOutput("team1_logo2")
                        ),
                        column(
                          8,
                          h4("Players Traded:"),
                          uiOutput("t1p1"),
                          uiOutput("t1p2")
                        ),
                        column(
                          4,
                          h4("To:"),
                          uiOutput("t1p1_to")
                        )
                      ),
                      column(
                        6,
                        div(
                          style = "text-align: center;",
                          htmlOutput("team2_logo"),
                          h4("Trading Away:")
                        )
                      )
                    )
                  ),
                  # Conditional Panel for 3 Teams
                  fluidRow(
                    conditionalPanel(
                      condition = "input.teamsInvolved.length == 3",
                      column(
                        4,
                        div(
                          style = "text-align: center; border-right: 2px solid black; height: 100%;",
                          htmlOutput("team1_logo3"),
                          h4("Trading Away:")
                        )
                      ),
                      column(
                        4,
                        div(
                          style = "text-align: center; border-right: 2px solid black; height: 100%;",
                          htmlOutput("team2_logo3"),
                          h4("Trading Away:")
                        )
                      ),
                      column(
                        4,
                        div(
                          style = "text-align: center;",
                          htmlOutput("team3_logo"),
                          h4("Trading Away:")
                        )
                      )
                    )
                  )
                )
)


server <- function(input, output, session) {
  
  transaction_log = reactiveVal(
      value = read_sheet(
        "https://docs.google.com/spreadsheets/d/13TtpNrsgFBv9UR4s-mw15uDd17UruiV9OZM6fWs03kE/edit?usp=sharing",
        sheet = "All_TL"
        )
    )

  # Clear page on button click
  observeEvent(input$clearpg, {
    session$reload()
  })
  
  # Logo outputs for teams
  logo1_url <- reactive(
      hoopR_espn_nba_teams |>
        filter(
          display_name == input$teamsInvolved[[1]]
        ) |>
        pull(logo)
    )
  
  output$team1_logo = output$team1_logo2 = output$team1_logo3 = renderText({
      paste0(
        '<img src ="', 
        logo1_url(), 
        '" style="width:75px;">'
      )
    })
  
  logo2_url <- reactive(
    hoopR_espn_nba_teams |>
      filter(
        display_name == input$teamsInvolved[[2]]
      ) |>
      pull(logo)
  )
  
  output$team2_logo = output$team2_logo3 = renderText({
    paste0(
      '<img src ="', 
      logo2_url(), 
      '" style="width:75px;">'
    )
  })
  
  logo3_url <- reactive(
    hoopR_espn_nba_teams |>
      filter(
        display_name == input$teamsInvolved[[3]]
      ) |>
      pull(logo)
  )
  
  output$team3_logo = renderText({
    paste0(
      '<img src ="', 
      logo3_url(), 
      '" style="width:75px;">'
    )
  })
  
  ########## UI Outputs for Inputs
  
  ### Traded Player Inputs
  output$t1p1 <- renderUI({
    if(FALSE){}
    else{
      selectizeInput(
        "t1p1", 
        "", 
        choices = read_sheet(
            ss,
            sheet = hoopR_espn_nba_teams |>
              filter(
                display_name == input$teamsInvolved[[1]]
              ) |>
              pull(abbreviation)
          ) |>
          mutate(
            Player = ifelse(is.na(Player), "", Player)
          ) |>
          pull(Player), 
        options = list(create = TRUE)
      )
    }
  })
  output$t1p2 <- renderUI({
    if(input$t1p1 == " " | input$t1p1 == ""){}
    else{
      selectizeInput(
        "t1p2", 
        "", 
        choices = read_sheet(
          ss,
          sheet = hoopR_espn_nba_teams |>
            filter(
              display_name == input$teamsInvolved[[1]]
            ) |>
            pull(abbreviation)
        ) |>
          mutate(
            Player = ifelse(is.na(Player), "", Player)
          ) |>
          pull(Player), 
        options = list(create = TRUE)
      )
    }
  })

}


shinyApp(ui, server)
