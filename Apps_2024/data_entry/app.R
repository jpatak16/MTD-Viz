library(pacman)
library(shiny)
library(shinyWidgets)
library(magrittr)
library(dplyr)
library(stringr)
library(gt)
library(gtExtras)
library(googlesheets4)

gs4_auth(path = ".secrets/asu-mtd-c191882d31e3.json")

ss <- gs4_get("https://docs.google.com/spreadsheets/d/13TtpNrsgFBv9UR4s-mw15uDd17UruiV9OZM6fWs03kE/edit?usp=sharing")

hoopR_espn_nba_teams <- read.csv("espn_nba_teams.csv") |>
  mutate(
    dropdown_logo = paste0("<img src='", logo, "' width=20px><div class='jhr'></div></img>")
  )

nba_teams <- hoopR_espn_nba_teams |> 
  pull(display_name)


ui = fluidPage(
  tags$style(HTML("
    .side-by-side-row {
      display: flex;
      flex-wrap: nowrap;  /* Prevent wrapping on narrow screens */
    }
  ")),
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
        fluidRow(
          class = "side-by-side-row",
          column(
            8,
            h4("Players Traded:"),
            uiOutput("t1p1_2t"),
            conditionalPanel(condition = "!(input.t1p1== ' '||input.t1p1=='')", uiOutput("t1p2_2t")),
            conditionalPanel(condition = "!(input.t1p2== ' '||input.t1p2=='')", uiOutput("t1p3_2t")),
            conditionalPanel(condition = "!(input.t1p3== ' '||input.t1p3=='')", uiOutput("t1p4_2t")),
            conditionalPanel(condition = "!(input.t1p4== ' '||input.t1p4=='')", uiOutput("t1p5_2t")),
            conditionalPanel(condition = "!(input.t1p5== ' '||input.t1p5=='')", uiOutput("t1p6_2t")),
            conditionalPanel(condition = "!(input.t1p6== ' '||input.t1p6=='')", uiOutput("t1p7_2t")),
            conditionalPanel(condition = "!(input.t1p7== ' '||input.t1p7=='')", uiOutput("t1p8_2t")),
            conditionalPanel(condition = "!(input.t1p8== ' '||input.t1p8=='')", uiOutput("t1p9_2t")),
            conditionalPanel(condition = "!(input.t1p9== ' '||input.t1p9=='')", uiOutput("t1p10_2t")),
            h4("Other Assets Traded:")
          ),
          column(
            2,
            h4("To:"),
            uiOutput("t1p1_2t_to"),
            conditionalPanel(condition = "!(input.t1p1== ' '||input.t1p1=='')", uiOutput("t1p2_2t_to")),
            conditionalPanel(condition = "!(input.t1p2== ' '||input.t1p2=='')", uiOutput("t1p3_2t_to")),
            conditionalPanel(condition = "!(input.t1p3== ' '||input.t1p3=='')", uiOutput("t1p4_2t_to")),
            conditionalPanel(condition = "!(input.t1p4== ' '||input.t1p4=='')", uiOutput("t1p5_2t_to")),
            conditionalPanel(condition = "!(input.t1p5== ' '||input.t1p5=='')", uiOutput("t1p6_2t_to")),
            conditionalPanel(condition = "!(input.t1p6== ' '||input.t1p6=='')", uiOutput("t1p7_2t_to")),
            conditionalPanel(condition = "!(input.t1p7== ' '||input.t1p7=='')", uiOutput("t1p8_2t_to")),
            conditionalPanel(condition = "!(input.t1p8== ' '||input.t1p8=='')", uiOutput("t1p9_2t_to")),
            conditionalPanel(condition = "!(input.t1p9== ' '||input.t1p9=='')", uiOutput("t1p10_2t_to"))
          )
        ),
      ),
      column(
        6,
        div(
          style = "text-align: center;",
          htmlOutput("team2_logo"),
        ),
        fluidRow(
          class = "side-by-side-row",
          column(
            8,
            h4("Players Traded:"),
            uiOutput("t2p1_2t"),
            conditionalPanel(condition = "!(input.t2p1== ' '||input.t2p1=='')", uiOutput("t2p2_2t")),
            conditionalPanel(condition = "!(input.t2p2== ' '||input.t2p2=='')", uiOutput("t2p3_2t")),
            conditionalPanel(condition = "!(input.t2p3== ' '||input.t2p3=='')", uiOutput("t2p4_2t")),
            conditionalPanel(condition = "!(input.t2p4== ' '||input.t2p4=='')", uiOutput("t2p5_2t")),
            conditionalPanel(condition = "!(input.t2p5== ' '||input.t2p5=='')", uiOutput("t2p6_2t")),
            conditionalPanel(condition = "!(input.t2p6== ' '||input.t2p6=='')", uiOutput("t2p7_2t")),
            conditionalPanel(condition = "!(input.t2p7== ' '||input.t2p7=='')", uiOutput("t2p8_2t")),
            conditionalPanel(condition = "!(input.t2p8== ' '||input.t2p8=='')", uiOutput("t2p9_2t")),
            conditionalPanel(condition = "!(input.t2p9== ' '||input.t2p9=='')", uiOutput("t2p10_2t")),
            h4("Other Assets Traded:")
          ),
          column(
            2,
            h4("To:"),
            uiOutput("t2p1_2t_to"),
            conditionalPanel(condition = "!(input.t2p1== ' '||input.t2p1=='')", uiOutput("t2p2_2t_to")),
            conditionalPanel(condition = "!(input.t2p2== ' '||input.t2p2=='')", uiOutput("t2p3_2t_to")),
            conditionalPanel(condition = "!(input.t2p3== ' '||input.t2p3=='')", uiOutput("t2p4_2t_to")),
            conditionalPanel(condition = "!(input.t2p4== ' '||input.t2p4=='')", uiOutput("t2p5_2t_to")),
            conditionalPanel(condition = "!(input.t2p5== ' '||input.t2p5=='')", uiOutput("t2p6_2t_to")),
            conditionalPanel(condition = "!(input.t2p6== ' '||input.t2p6=='')", uiOutput("t2p7_2t_to")),
            conditionalPanel(condition = "!(input.t2p7== ' '||input.t2p7=='')", uiOutput("t2p8_2t_to")),
            conditionalPanel(condition = "!(input.t2p8== ' '||input.t2p8=='')", uiOutput("t2p9_2t_to")),
            conditionalPanel(condition = "!(input.t2p9== ' '||input.t2p9=='')", uiOutput("t2p10_2t_to"))
          )
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
  # Team 1
  output$t1p1_2t = output$t1p1_3t = renderUI({
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
  })
  output$t1p2_2t = output$t1p2_3t = renderUI({
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
  })
  output$t1p3_2t = output$t1p3_3t = renderUI({
      selectizeInput(
        "t1p3", 
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
  })
  output$t1p4_2t = output$t1p4_3t = renderUI({
      selectizeInput(
        "t1p4", 
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
  })
  output$t1p5_2t = output$t1p5_3t = renderUI({
      selectizeInput(
        "t1p5", 
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
  })
  output$t1p6_2t = output$t1p6_3t = renderUI({
      selectizeInput(
        "t1p6", 
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
  })
  output$t1p7_2t = output$t1p7_3t = renderUI({
      selectizeInput(
        "t1p7", 
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
  })
  output$t1p8_2t = output$t1p8_3t = renderUI({
      selectizeInput(
        "t1p8", 
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
  })
  output$t1p9_2t = output$t1p9_3t = renderUI({
      selectizeInput(
        "t1p9", 
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
  })
  output$t1p10_2t = output$t1p10_3t = renderUI({
      selectizeInput(
        "t1p10", 
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
  })
  
  # Team 2 
  output$t2p1_2t = output$t2p1_3t = renderUI({
      selectizeInput(
        "t2p1", 
        "", 
        choices = read_sheet(
          ss,
          sheet = hoopR_espn_nba_teams |>
            filter(
              display_name == input$teamsInvolved[[2]]
            ) |>
            pull(abbreviation)
        ) |>
          mutate(
            Player = ifelse(is.na(Player), "", Player)
          ) |>
          pull(Player), 
        options = list(create = TRUE)
      )
  })
  output$t2p2_2t = output$t2p2_3t = renderUI({
      selectizeInput(
        "t2p2", 
        "", 
        choices = read_sheet(
          ss,
          sheet = hoopR_espn_nba_teams |>
            filter(
              display_name == input$teamsInvolved[[2]]
            ) |>
            pull(abbreviation)
        ) |>
          mutate(
            Player = ifelse(is.na(Player), "", Player)
          ) |>
          pull(Player), 
        options = list(create = TRUE)
      )
  })
  output$t2p3_2t = output$t2p3_3t = renderUI({
      selectizeInput(
        "t2p3", 
        "", 
        choices = read_sheet(
          ss,
          sheet = hoopR_espn_nba_teams |>
            filter(
              display_name == input$teamsInvolved[[2]]
            ) |>
            pull(abbreviation)
        ) |>
          mutate(
            Player = ifelse(is.na(Player), "", Player)
          ) |>
          pull(Player), 
        options = list(create = TRUE)
      )
  })
  output$t2p4_2t = output$t2p4_3t = renderUI({
      selectizeInput(
        "t2p4", 
        "", 
        choices = read_sheet(
          ss,
          sheet = hoopR_espn_nba_teams |>
            filter(
              display_name == input$teamsInvolved[[2]]
            ) |>
            pull(abbreviation)
        ) |>
          mutate(
            Player = ifelse(is.na(Player), "", Player)
          ) |>
          pull(Player), 
        options = list(create = TRUE)
      )
  })
  output$t2p5_2t = output$t2p5_3t = renderUI({
      selectizeInput(
        "t2p5", 
        "", 
        choices = read_sheet(
          ss,
          sheet = hoopR_espn_nba_teams |>
            filter(
              display_name == input$teamsInvolved[[2]]
            ) |>
            pull(abbreviation)
        ) |>
          mutate(
            Player = ifelse(is.na(Player), "", Player)
          ) |>
          pull(Player), 
        options = list(create = TRUE)
      )
  })
  output$t2p6_2t = output$t2p6_3t = renderUI({
      selectizeInput(
        "t2p6", 
        "", 
        choices = read_sheet(
          ss,
          sheet = hoopR_espn_nba_teams |>
            filter(
              display_name == input$teamsInvolved[[2]]
            ) |>
            pull(abbreviation)
        ) |>
          mutate(
            Player = ifelse(is.na(Player), "", Player)
          ) |>
          pull(Player), 
        options = list(create = TRUE)
      )
  })
  output$t2p7_2t = output$t2p7_3t = renderUI({
      selectizeInput(
        "t2p7", 
        "", 
        choices = read_sheet(
          ss,
          sheet = hoopR_espn_nba_teams |>
            filter(
              display_name == input$teamsInvolved[[2]]
            ) |>
            pull(abbreviation)
        ) |>
          mutate(
            Player = ifelse(is.na(Player), "", Player)
          ) |>
          pull(Player), 
        options = list(create = TRUE)
      )
  })
  output$t2p8_2t = output$t2p8_3t = renderUI({
      selectizeInput(
        "t2p8", 
        "", 
        choices = read_sheet(
          ss,
          sheet = hoopR_espn_nba_teams |>
            filter(
              display_name == input$teamsInvolved[[2]]
            ) |>
            pull(abbreviation)
        ) |>
          mutate(
            Player = ifelse(is.na(Player), "", Player)
          ) |>
          pull(Player), 
        options = list(create = TRUE)
      )
  })
  output$t2p9_2t = output$t2p9_3t = renderUI({
      selectizeInput(
        "t2p9", 
        "", 
        choices = read_sheet(
          ss,
          sheet = hoopR_espn_nba_teams |>
            filter(
              display_name == input$teamsInvolved[[2]]
            ) |>
            pull(abbreviation)
        ) |>
          mutate(
            Player = ifelse(is.na(Player), "", Player)
          ) |>
          pull(Player), 
        options = list(create = TRUE)
      )
  })
  output$t2p10_2t = output$t2p10_3t = renderUI({
      selectizeInput(
        "t2p10", 
        "", 
        choices = read_sheet(
          ss,
          sheet = hoopR_espn_nba_teams |>
            filter(
              display_name == input$teamsInvolved[[2]]
            ) |>
            pull(abbreviation)
        ) |>
          mutate(
            Player = ifelse(is.na(Player), "", Player)
          ) |>
          pull(Player), 
        options = list(create = TRUE)
      )
  })
  
  ### Traded Played To Team Input
  # Team 1
  output$t1p1_2t_to = output$t1p1_3t_to = renderUI({
      pickerInput(
        "t1p1_to", 
        "", 
        choices = input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]],
        choicesOpt = list(
          content = hoopR_espn_nba_teams |>
            filter(
              display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]
            ) |>
            pull(dropdown_logo)
        ),
        options = list(`dropdown-align-center` = TRUE)
      )
  })
  output$t1p2_2t_to = output$t1p2_3t_to = renderUI({
      pickerInput(
        "t1p2_to", 
        "", 
        choices = input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]],
        choicesOpt = list(
          content = hoopR_espn_nba_teams |>
            filter(
              display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]
            ) |>
            pull(dropdown_logo)
        ),
        options = list(`dropdown-align-center` = TRUE)
      )
  })
  output$t1p3_2t_to = output$t1p3_3t_to = renderUI({
      pickerInput(
        "t1p3_to", 
        "", 
        choices = input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]],
        choicesOpt = list(
          content = hoopR_espn_nba_teams |>
            filter(
              display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]
            ) |>
            pull(dropdown_logo)
        ),
        options = list(`dropdown-align-center` = TRUE)
      )
  })
  output$t1p4_2t_to = output$t1p4_3t_to = renderUI({
      pickerInput(
        "t1p4_to", 
        "", 
        choices = input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]],
        choicesOpt = list(
          content = hoopR_espn_nba_teams |>
            filter(
              display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]
            ) |>
            pull(dropdown_logo)
        ),
        options = list(`dropdown-align-center` = TRUE)
      )
  })
  output$t1p5_2t_to = output$t1p5_3t_to = renderUI({
      pickerInput(
        "t1p5_to", 
        "", 
        choices = input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]],
        choicesOpt = list(
          content = hoopR_espn_nba_teams |>
            filter(
              display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]
            ) |>
            pull(dropdown_logo)
        ),
        options = list(`dropdown-align-center` = TRUE)
      )
  })
  output$t1p6_2t_to = output$t1p6_3t_to = renderUI({
      pickerInput(
        "t1p6_to", 
        "", 
        choices = input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]],
        choicesOpt = list(
          content = hoopR_espn_nba_teams |>
            filter(
              display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]
            ) |>
            pull(dropdown_logo)
        ),
        options = list(`dropdown-align-center` = TRUE)
      )
  })
  output$t1p7_2t_to = output$t1p7_3t_to = renderUI({
      pickerInput(
        "t1p7_to", 
        "", 
        choices = input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]],
        choicesOpt = list(
          content = hoopR_espn_nba_teams |>
            filter(
              display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]
            ) |>
            pull(dropdown_logo)
        ),
        options = list(`dropdown-align-center` = TRUE)
      )
  })
  output$t1p8_2t_to = output$t1p8_3t_to = renderUI({
      pickerInput(
        "t1p8_to", 
        "", 
        choices = input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]],
        choicesOpt = list(
          content = hoopR_espn_nba_teams |>
            filter(
              display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]
            ) |>
            pull(dropdown_logo)
        ),
        options = list(`dropdown-align-center` = TRUE)
      )
  })
  output$t1p9_2t_to = output$t1p9_3t_to = renderUI({
      pickerInput(
        "t1p9_to", 
        "", 
        choices = input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]],
        choicesOpt = list(
          content = hoopR_espn_nba_teams |>
            filter(
              display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]
            ) |>
            pull(dropdown_logo)
        ),
        options = list(`dropdown-align-center` = TRUE)
      )
  })
  output$t1p10_2t_to = output$t1p10_3t_to = renderUI({
      pickerInput(
        "t1p10_to", 
        "", 
        choices = input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]],
        choicesOpt = list(
          content = hoopR_espn_nba_teams |>
            filter(
              display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]
            ) |>
            pull(dropdown_logo)
        ),
        options = list(`dropdown-align-center` = TRUE)
      )
  })
  
  # Team 2
  output$t2p1_2t_to = output$t2p1_3t_to = renderUI({
      pickerInput(
        "t2p1_to", 
        "", 
        choices = input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]],
        choicesOpt = list(
          content = hoopR_espn_nba_teams |>
            filter(
              display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]
            ) |>
            pull(dropdown_logo)
        ),
        options = list(`dropdown-align-center` = TRUE)
      )
  })
  output$t2p2_2t_to = output$t2p2_3t_to = renderUI({
      pickerInput(
        "t2p2_to", 
        "", 
        choices = input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]],
        choicesOpt = list(
          content = hoopR_espn_nba_teams |>
            filter(
              display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]
            ) |>
            pull(dropdown_logo)
        ),
        options = list(`dropdown-align-center` = TRUE)
      )
  })
  output$t2p3_2t_to = output$t2p3_3t_to = renderUI({
      pickerInput(
        "t2p3_to", 
        "", 
        choices = input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]],
        choicesOpt = list(
          content = hoopR_espn_nba_teams |>
            filter(
              display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]
            ) |>
            pull(dropdown_logo)
        ),
        options = list(`dropdown-align-center` = TRUE)
      )
  })
  output$t2p4_2t_to = output$t2p4_3t_to = renderUI({
      pickerInput(
        "t2p4_to", 
        "", 
        choices = input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]],
        choicesOpt = list(
          content = hoopR_espn_nba_teams |>
            filter(
              display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]
            ) |>
            pull(dropdown_logo)
        ),
        options = list(`dropdown-align-center` = TRUE)
      )
  })
  output$t2p5_2t_to = output$t2p5_3t_to = renderUI({
      pickerInput(
        "t2p5_to", 
        "", 
        choices = input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]],
        choicesOpt = list(
          content = hoopR_espn_nba_teams |>
            filter(
              display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]
            ) |>
            pull(dropdown_logo)
        ),
        options = list(`dropdown-align-center` = TRUE)
      )
  })
  output$t2p6_2t_to = output$t2p6_3t_to = renderUI({
      pickerInput(
        "t2p6_to", 
        "", 
        choices = input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]],
        choicesOpt = list(
          content = hoopR_espn_nba_teams |>
            filter(
              display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]
            ) |>
            pull(dropdown_logo)
        ),
        options = list(`dropdown-align-center` = TRUE)
      )
  })
  output$t2p7_2t_to = output$t2p7_3t_to = renderUI({
      pickerInput(
        "t2p7_to", 
        "", 
        choices = input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]],
        choicesOpt = list(
          content = hoopR_espn_nba_teams |>
            filter(
              display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]
            ) |>
            pull(dropdown_logo)
        ),
        options = list(`dropdown-align-center` = TRUE)
      )
  })
  output$t2p8_2t_to = output$t2p8_3t_to = renderUI({
      pickerInput(
        "t2p8_to", 
        "", 
        choices = input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]],
        choicesOpt = list(
          content = hoopR_espn_nba_teams |>
            filter(
              display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]
            ) |>
            pull(dropdown_logo)
        ),
        options = list(`dropdown-align-center` = TRUE)
      )
  })
  output$t2p9_2t_to = output$t2p9_3t_to = renderUI({
      pickerInput(
        "t2p9_to", 
        "", 
        choices = input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]],
        choicesOpt = list(
          content = hoopR_espn_nba_teams |>
            filter(
              display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]
            ) |>
            pull(dropdown_logo)
        ),
        options = list(`dropdown-align-center` = TRUE)
      )
  })
  output$t2p10_2t_to = output$t2p10_3t_to = renderUI({
      pickerInput(
        "t2p10_to", 
        "", 
        choices = input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]],
        choicesOpt = list(
          content = hoopR_espn_nba_teams |>
            filter(
              display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]
            ) |>
            pull(dropdown_logo)
        ),
        options = list(`dropdown-align-center` = TRUE)
      )
  })

}


shinyApp(ui, server)
