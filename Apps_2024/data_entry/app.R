library(pacman)
library(shiny)
library(bslib)
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

ui = bootstrapPage(
  theme = bs_theme(version = 5, bootswatch = "journal"),
  # Title Header
  div(
    class = "title",
    img(
      src="ASU-NTDC-Logo.png", 
      width = "100vw",
      style = "display: block; margin-left: auto; margin-right: auto;"
    ),
    div("2024 NBA Trade Deadline Competition", 
         style = 'color:rgba(169,20,20,128); text-align: center'), 
    div("Trade Entry", 
         style = 'font-weight: bold; text-align: center')
  ),
  tags$hr(style="border-color:rgba(169,20,20,128); margin-bottom:0px"),
  # Input/Button Box
  div(
    class = "row",
    style = "background-color: #dddddd; padding-top: 10px; padding-bottom: 20px; align-items: center; padding-left: 10px",
    div(
      class = "col-md-5",
      selectizeInput(
        "teamsInvolved", 
        label = "Teams Involved", 
        choices = nba_teams, 
        multiple = T,
        width = "95%"
      )
    ),
    div(
      class = "col-md-5",
      div(
        class = "button-container",
        style = "margin-bottom: 10px; margin-top: 10px",
        actionButton(
          "submit", 
          "Submit Trade"
        ),
        actionButton(
          "clearpg", 
          "Clear Input"
        )
      )
    )
  ),
  # Two Teams Panel
  conditionalPanel(
    condition = "input.teamsInvolved.length == 2",
    div(
      class = "row",
      # Team 1
      div(
        class = "col-md-6",
        style = "text-align: center; border-right: 2px solid black; border-top: 2px solid black; padding-left: 1.2em; padding-right: 1.2em;",
        h5(""),
        htmlOutput("team1_logo2"),
        h5(""),
        # Players Traded
        div(
          class = "row",
          div(
            class = "col-xs-8",
            style = "text-align: left; width: 60%; padding-left: 1.2em; padding-right: .6em;",
            h5("Players Traded:"),
            uiOutput("t1p1_2t"),
            conditionalPanel(condition = "!(input.t1p1== ' '||input.t1p1=='')", uiOutput("t1p2_2t")),
            conditionalPanel(condition = "!(input.t1p2== ' '||input.t1p2=='')", uiOutput("t1p3_2t")),
            conditionalPanel(condition = "!(input.t1p3== ' '||input.t1p3=='')", uiOutput("t1p4_2t")),
            conditionalPanel(condition = "!(input.t1p4== ' '||input.t1p4=='')", uiOutput("t1p5_2t")),
            conditionalPanel(condition = "!(input.t1p5== ' '||input.t1p5=='')", uiOutput("t1p6_2t")),
            conditionalPanel(condition = "!(input.t1p6== ' '||input.t1p6=='')", uiOutput("t1p7_2t")),
            conditionalPanel(condition = "!(input.t1p7== ' '||input.t1p7=='')", uiOutput("t1p8_2t")),
            conditionalPanel(condition = "!(input.t1p8== ' '||input.t1p8=='')", uiOutput("t1p9_2t")),
            conditionalPanel(condition = "!(input.t1p9== ' '||input.t1p9=='')", uiOutput("t1p10_2t"))
          ),
          div(
            class = "col-xs-4",
            style = "text-align: left; width: 38%; padding-left: .6em; padding-right: 1.2em;",
            h5("To:"),
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
        # Other Assets Traded
        div(
          class = "row",
          div(
            class = "col-xs-5",
            style = "text-align: left; width: 40%; padding-left: 1.2em; padding-right: .6em;",
            h5("Other Assets:", style = "font-size: 1.1rem;"),
            uiOutput("t1a1_2t"),
            conditionalPanel(condition = "!(input.t1a1== ' '||input.t1a1=='')", uiOutput("t1a2_2t")),
            conditionalPanel(condition = "!(input.t1a2== ' '||input.t1a2=='')", uiOutput("t1a3_2t")),
            conditionalPanel(condition = "!(input.t1a3== ' '||input.t1a3=='')", uiOutput("t1a4_2t")),
            conditionalPanel(condition = "!(input.t1a4== ' '||input.t1a4=='')", uiOutput("t1a5_2t")),
            conditionalPanel(condition = "!(input.t1a5== ' '||input.t1a5=='')", uiOutput("t1a6_2t")),
            conditionalPanel(condition = "!(input.t1a6== ' '||input.t1a6=='')", uiOutput("t1a7_2t")),
            conditionalPanel(condition = "!(input.t1a7== ' '||input.t1a7=='')", uiOutput("t1a8_2t")),
            conditionalPanel(condition = "!(input.t1a8== ' '||input.t1a8=='')", uiOutput("t1a9_2t")),
            conditionalPanel(condition = "!(input.t1a9== ' '||input.t1a9=='')", uiOutput("t1a10_2t"))
          ),
          div(
            class = "col-xs-3",
            style = "text-align: left; width: 25%; padding-left: .6em; padding-right: .6em;",
            h5("To:", style = "font-size: 1.1rem;"),
            uiOutput("t1a1_2t_to"),
            conditionalPanel(condition = "!(input.t1a1== ' '||input.t1a1=='')", uiOutput("t1a2_2t_to")),
            conditionalPanel(condition = "!(input.t1a2== ' '||input.t1a2=='')", uiOutput("t1a3_2t_to")),
            conditionalPanel(condition = "!(input.t1a3== ' '||input.t1a3=='')", uiOutput("t1a4_2t_to")),
            conditionalPanel(condition = "!(input.t1a4== ' '||input.t1a4=='')", uiOutput("t1a5_2t_to")),
            conditionalPanel(condition = "!(input.t1a5== ' '||input.t1a5=='')", uiOutput("t1a6_2t_to")),
            conditionalPanel(condition = "!(input.t1a6== ' '||input.t1a6=='')", uiOutput("t1a7_2t_to")),
            conditionalPanel(condition = "!(input.t1a7== ' '||input.t1a7=='')", uiOutput("t1a8_2t_to")),
            conditionalPanel(condition = "!(input.t1a8== ' '||input.t1a8=='')", uiOutput("t1a9_2t_to")),
            conditionalPanel(condition = "!(input.t1a9== ' '||input.t1a9=='')", uiOutput("t1a10_2t_to"))
          ),
          div(
            class = "col-xs-2",
            style = "text-align: left; width: 35%; padding-left: .6em; padding-right: 1.2em;",
            h5("Add Protection?", style = "font-size: 1.1rem; width: 110%;"),
            uiOutput("t1a1_2t_pro"),
            conditionalPanel(condition = "!(input.t1a1== ' '||input.t1a1=='')", uiOutput("t1a2_2t_pro")),
            conditionalPanel(condition = "!(input.t1a2== ' '||input.t1a2=='')", uiOutput("t1a3_2t_pro")),
            conditionalPanel(condition = "!(input.t1a3== ' '||input.t1a3=='')", uiOutput("t1a4_2t_pro")),
            conditionalPanel(condition = "!(input.t1a4== ' '||input.t1a4=='')", uiOutput("t1a5_2t_pro")),
            conditionalPanel(condition = "!(input.t1a5== ' '||input.t1a5=='')", uiOutput("t1a6_2t_pro")),
            conditionalPanel(condition = "!(input.t1a6== ' '||input.t1a6=='')", uiOutput("t1a7_2t_pro")),
            conditionalPanel(condition = "!(input.t1a7== ' '||input.t1a7=='')", uiOutput("t1a8_2t_pro")),
            conditionalPanel(condition = "!(input.t1a8== ' '||input.t1a8=='')", uiOutput("t1a9_2t_pro")),
            conditionalPanel(condition = "!(input.t1a9== ' '||input.t1a9=='')", uiOutput("t1a10_2t_pro"))
          )
        )
      ),
      # Team 2
      div(
        class = "col-md-6",
        style = "text-align: center; border-top: 2px solid black; padding-left: 1.2em; padding-right: 1.2em;",
        h5(""),
        htmlOutput("team2_logo"),
        h5(""),
        # Players Traded
        div(
          class = "row",
          div(
            class = "col-xs-8",
            style = "text-align: left; width: 60%; padding-left: 1.2em; padding-right: .6em;",
            h5("Players Traded:"),
            uiOutput("t2p1_2t"),
            conditionalPanel(condition = "!(input.t2p1== ' '||input.t2p1=='')", uiOutput("t2p2_2t")),
            conditionalPanel(condition = "!(input.t2p2== ' '||input.t2p2=='')", uiOutput("t2p3_2t")),
            conditionalPanel(condition = "!(input.t2p3== ' '||input.t2p3=='')", uiOutput("t2p4_2t")),
            conditionalPanel(condition = "!(input.t2p4== ' '||input.t2p4=='')", uiOutput("t2p5_2t")),
            conditionalPanel(condition = "!(input.t2p5== ' '||input.t2p5=='')", uiOutput("t2p6_2t")),
            conditionalPanel(condition = "!(input.t2p6== ' '||input.t2p6=='')", uiOutput("t2p7_2t")),
            conditionalPanel(condition = "!(input.t2p7== ' '||input.t2p7=='')", uiOutput("t2p8_2t")),
            conditionalPanel(condition = "!(input.t2p8== ' '||input.t2p8=='')", uiOutput("t2p9_2t")),
            conditionalPanel(condition = "!(input.t2p9== ' '||input.t2p9=='')", uiOutput("t2p10_2t"))
          ),
          div(
            class = "col-xs-4",
            style = "text-align: left; width: 38%; padding-left: .6em; padding-right: 1.2em;",
            h5("To:"),
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
        ),
        # Other Assets Traded
        div(
          class = "row",
          div(
            class = "col-xs-5",
            style = "text-align: left; width: 40%; padding-left: 1.2em; padding-right: .6em;",
            h5("Other Assets:", style = "font-size: 1.1rem;"),
            uiOutput("t2a1_2t"),
            conditionalPanel(condition = "!(input.t2a1== ' '||input.t2a1=='')", uiOutput("t2a2_2t")),
            conditionalPanel(condition = "!(input.t2a2== ' '||input.t2a2=='')", uiOutput("t2a3_2t")),
            conditionalPanel(condition = "!(input.t2a3== ' '||input.t2a3=='')", uiOutput("t2a4_2t")),
            conditionalPanel(condition = "!(input.t2a4== ' '||input.t2a4=='')", uiOutput("t2a5_2t")),
            conditionalPanel(condition = "!(input.t2a5== ' '||input.t2a5=='')", uiOutput("t2a6_2t")),
            conditionalPanel(condition = "!(input.t2a6== ' '||input.t2a6=='')", uiOutput("t2a7_2t")),
            conditionalPanel(condition = "!(input.t2a7== ' '||input.t2a7=='')", uiOutput("t2a8_2t")),
            conditionalPanel(condition = "!(input.t2a8== ' '||input.t2a8=='')", uiOutput("t2a9_2t")),
            conditionalPanel(condition = "!(input.t2a9== ' '||input.t2a9=='')", uiOutput("t2a10_2t"))
          ),
          div(
            class = "col-xs-3",
            style = "text-align: left; width: 25%; padding-left: .6em; padding-right: .6em;",
            h5("To:", style = "font-size: 1.1rem;"),
            uiOutput("t2a1_2t_to"),
            conditionalPanel(condition = "!(input.t2a1== ' '||input.t2a1=='')", uiOutput("t2a2_2t_to")),
            conditionalPanel(condition = "!(input.t2a2== ' '||input.t2a2=='')", uiOutput("t2a3_2t_to")),
            conditionalPanel(condition = "!(input.t2a3== ' '||input.t2a3=='')", uiOutput("t2a4_2t_to")),
            conditionalPanel(condition = "!(input.t2a4== ' '||input.t2a4=='')", uiOutput("t2a5_2t_to")),
            conditionalPanel(condition = "!(input.t2a5== ' '||input.t2a5=='')", uiOutput("t2a6_2t_to")),
            conditionalPanel(condition = "!(input.t2a6== ' '||input.t2a6=='')", uiOutput("t2a7_2t_to")),
            conditionalPanel(condition = "!(input.t2a7== ' '||input.t2a7=='')", uiOutput("t2a8_2t_to")),
            conditionalPanel(condition = "!(input.t2a8== ' '||input.t2a8=='')", uiOutput("t2a9_2t_to")),
            conditionalPanel(condition = "!(input.t2a9== ' '||input.t2a9=='')", uiOutput("t2a10_2t_to"))
          ),
          div(
            class = "col-xs-2",
            style = "text-align: left; width: 35%; padding-left: .6em; padding-right: 1.2em;",
            h5("Add Protection?", style = "font-size: 1.1rem; width: 110%;"),
            uiOutput("t2a1_2t_pro"),
            conditionalPanel(condition = "!(input.t2a1== ' '||input.t2a1=='')", uiOutput("t2a2_2t_pro")),
            conditionalPanel(condition = "!(input.t2a2== ' '||input.t2a2=='')", uiOutput("t2a3_2t_pro")),
            conditionalPanel(condition = "!(input.t2a3== ' '||input.t2a3=='')", uiOutput("t2a4_2t_pro")),
            conditionalPanel(condition = "!(input.t2a4== ' '||input.t2a4=='')", uiOutput("t2a5_2t_pro")),
            conditionalPanel(condition = "!(input.t2a5== ' '||input.t2a5=='')", uiOutput("t2a6_2t_pro")),
            conditionalPanel(condition = "!(input.t2a6== ' '||input.t2a6=='')", uiOutput("t2a7_2t_pro")),
            conditionalPanel(condition = "!(input.t2a7== ' '||input.t2a7=='')", uiOutput("t2a8_2t_pro")),
            conditionalPanel(condition = "!(input.t2a8== ' '||input.t2a8=='')", uiOutput("t2a9_2t_pro")),
            conditionalPanel(condition = "!(input.t2a9== ' '||input.t2a9=='')", uiOutput("t2a10_2t_pro"))
          )
        )
      ),
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
  
  # Player List for teamsInvolved
  team1_players <- reactive(
    if(length(input$teamsInvolved) < 1){}
    else{
      read_sheet(
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
        pull(Player)
    }
  )
  
  team2_players <- reactive(
    if(length(input$teamsInvolved) < 2){}
    else{
      read_sheet(
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
        pull(Player)
    }
  )
  
  team3_players <- reactive(
    if(length(input$teamsInvolved) < 3){}
    else{
      read_sheet(
        ss,
        sheet = hoopR_espn_nba_teams |>
          filter(
            display_name == input$teamsInvolved[[3]]
          ) |>
          pull(abbreviation)
      ) |>
        mutate(
          Player = ifelse(is.na(Player), "", Player)
        ) |>
        pull(Player)
    }
  )
  
  team4_players <- reactive(
    if(length(input$teamsInvolved) < 4){}
    else{
      read_sheet(
        ss,
        sheet = hoopR_espn_nba_teams |>
          filter(
            display_name == input$teamsInvolved[[4]]
          ) |>
          pull(abbreviation)
      ) |>
        mutate(
          Player = ifelse(is.na(Player), "", Player)
        ) |>
        pull(Player)
    }
  )
  
  team5_players <- reactive(
    if(length(input$teamsInvolved) < 5){}
    else{
      read_sheet(
        ss,
        sheet = hoopR_espn_nba_teams |>
          filter(
            display_name == input$teamsInvolved[[5]]
          ) |>
          pull(abbreviation)
      ) |>
        mutate(
          Player = ifelse(is.na(Player), "", Player)
        ) |>
        pull(Player)
    }
  )
  
  ########## UI Outputs for Inputs
  
  ### Traded Player Inputs
  # Team 1
  output$t1p1_2t = output$t1p1_3t = renderUI({
    selectizeInput(
      "t1p1", 
      "", 
      choices = team1_players(), 
      options = list(create = TRUE)
    )
  })
  output$t1p2_2t = output$t1p2_3t = renderUI({
    selectizeInput(
      "t1p2", 
      "", 
      choices = team1_players(), 
      options = list(create = TRUE)
    )
  })
  output$t1p3_2t = output$t1p3_3t = renderUI({
    selectizeInput(
      "t1p3", 
      "", 
      choices = team1_players(), 
      options = list(create = TRUE)
    )
  })
  output$t1p4_2t = output$t1p4_3t = renderUI({
    selectizeInput(
      "t1p4", 
      "", 
      choices = team1_players(), 
      options = list(create = TRUE)
    )
  })
  output$t1p5_2t = output$t1p5_3t = renderUI({
    selectizeInput(
      "t1p5", 
      "", 
      choices = team1_players(), 
      options = list(create = TRUE)
    )
  })
  output$t1p6_2t = output$t1p6_3t = renderUI({
    selectizeInput(
      "t1p6", 
      "", 
      choices = team1_players(), 
      options = list(create = TRUE)
    )
  })
  output$t1p7_2t = output$t1p7_3t = renderUI({
    selectizeInput(
      "t1p7", 
      "", 
      choices = team1_players(), 
      options = list(create = TRUE)
    )
  })
  output$t1p8_2t = output$t1p8_3t = renderUI({
    selectizeInput(
      "t1p8", 
      "", 
      choices = team1_players(), 
      options = list(create = TRUE)
    )
  })
  output$t1p9_2t = output$t1p9_3t = renderUI({
    selectizeInput(
      "t1p9", 
      "", 
      choices = team1_players(), 
      options = list(create = TRUE)
    )
  })
  output$t1p10_2t = output$t1p10_3t = renderUI({
    selectizeInput(
      "t1p10", 
      "", 
      choices = team1_players(), 
      options = list(create = TRUE)
    )
  })
  
  # Team 2 
  output$t2p1_2t = output$t2p1_3t = renderUI({
    selectizeInput(
      "t2p1", 
      "", 
      choices = team2_players(), 
      options = list(create = TRUE)
    )
  })
  output$t2p2_2t = output$t2p2_3t = renderUI({
    selectizeInput(
      "t2p2", 
      "", 
      choices = team2_players(), 
      options = list(create = TRUE)
    )
  })
  output$t2p3_2t = output$t2p3_3t = renderUI({
    selectizeInput(
      "t2p3", 
      "", 
      choices = team2_players(), 
      options = list(create = TRUE)
    )
  })
  output$t2p4_2t = output$t2p4_3t = renderUI({
    selectizeInput(
      "t2p4", 
      "", 
      choices = team2_players(), 
      options = list(create = TRUE)
    )
  })
  output$t2p5_2t = output$t2p5_3t = renderUI({
    selectizeInput(
      "t2p5", 
      "", 
      choices = team2_players(), 
      options = list(create = TRUE)
    )
  })
  output$t2p6_2t = output$t2p6_3t = renderUI({
    selectizeInput(
      "t2p6", 
      "", 
      choices = team2_players(), 
      options = list(create = TRUE)
    )
  })
  output$t2p7_2t = output$t2p7_3t = renderUI({
    selectizeInput(
      "t2p7", 
      "", 
      choices = team2_players(), 
      options = list(create = TRUE)
    )
  })
  output$t2p8_2t = output$t2p8_3t = renderUI({
    selectizeInput(
      "t2p8", 
      "", 
      choices = team2_players(), 
      options = list(create = TRUE)
    )
  })
  output$t2p9_2t = output$t2p9_3t = renderUI({
    selectizeInput(
      "t2p9", 
      "", 
      choices = team2_players(), 
      options = list(create = TRUE)
    )
  })
  output$t2p10_2t = output$t2p10_3t = renderUI({
    selectizeInput(
      "t2p10", 
      "", 
      choices = team2_players(), 
      options = list(create = TRUE)
    )
  })
  
  ### Traded Asset Input
  # Team 1
  output$t1a1_2t = output$t1a1_3t = renderUI({
    textInput(
      "t1a1", 
      ""
    )
  })
  output$t1a2_2t = output$t1a2_3t = renderUI({
    textInput(
      "t1a2", 
      ""
    )
  })
  output$t1a3_2t = output$t1a3_3t = renderUI({
    textInput(
      "t1a3", 
      ""
    )
  })
  output$t1a4_2t = output$t1a4_3t = renderUI({
    textInput(
      "t1a4", 
      ""
    )
  })
  output$t1a5_2t = output$t1a5_3t = renderUI({
    textInput(
      "t1a5", 
      ""
    )
  })
  output$t1a6_2t = output$t1a6_3t = renderUI({
    textInput(
      "t1a6", 
      ""
    )
  })
  output$t1a7_2t = output$t1a7_3t = renderUI({
    textInput(
      "t1a7", 
      ""
    )
  })
  output$t1a8_2t = output$t1a8_3t = renderUI({
    textInput(
      "t1a8", 
      ""
    )
  })
  output$t1a9_2t = output$t1a9_3t = renderUI({
    textInput(
      "t1a9", 
      ""
    )
  })
  output$t1a10_2t = output$t1a10_3t = renderUI({
    textInput(
      "t1a10", 
      ""
    )
  })
  
  # Team 2
  output$t2a1_2t = output$t2a1_3t = renderUI({
    textInput(
      "t2a1", 
      ""
    )
  })
  output$t2a2_2t = output$t2a2_3t = renderUI({
    textInput(
      "t2a2", 
      ""
    )
  })
  output$t2a3_2t = output$t2a3_3t = renderUI({
    textInput(
      "t2a3", 
      ""
    )
  })
  output$t2a4_2t = output$t2a4_3t = renderUI({
    textInput(
      "t2a4", 
      ""
    )
  })
  output$t2a5_2t = output$t2a5_3t = renderUI({
    textInput(
      "t2a5", 
      ""
    )
  })
  output$t2a6_2t = output$t2a6_3t = renderUI({
    textInput(
      "t2a6", 
      ""
    )
  })
  output$t2a7_2t = output$t2a7_3t = renderUI({
    textInput(
      "t2a7", 
      ""
    )
  })
  output$t2a8_2t = output$t2a8_3t = renderUI({
    textInput(
      "t2a8", 
      ""
    )
  })
  output$t2a9_2t = output$t2a9_3t = renderUI({
    textInput(
      "t2a9", 
      ""
    )
  })
  output$t2a10_2t = output$t2a10_3t = renderUI({
    textInput(
      "t2a10", 
      ""
    )
  })
  
  ### Traded Player To Team Input
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
  
  ### Traded Asset to Team Input
  # Team 1
  output$t1a1_2t_to = output$t1a1_3t_to = renderUI({
    div(
      style = "margin-bottom: -0.05rem;",
      pickerInput(
        "t1a1_to", 
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
    )
  })
  output$t1a2_2t_to = output$t1a2_3t_to = renderUI({
    div(
      style = "margin-bottom: -0.05rem;",
      pickerInput(
        "t1a2_to", 
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
    )
  })
  output$t1a3_2t_to = output$t1a3_3t_to = renderUI({
    div(
      style = "margin-bottom: -0.05rem;",
      pickerInput(
        "t1a3_to", 
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
    )
  })
  output$t1a4_2t_to = output$t1a4_3t_to = renderUI({
    div(
      style = "margin-bottom: -0.05rem;",
      pickerInput(
        "t1a4_to", 
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
    )
  })
  output$t1a5_2t_to = output$t1a5_3t_to = renderUI({
    div(
      style = "margin-bottom: -0.05rem;",
      pickerInput(
        "t1a5_to", 
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
    )
  })
  output$t1a6_2t_to = output$t1a6_3t_to = renderUI({
    div(
      style = "margin-bottom: -0.05rem;",
      pickerInput(
        "t1a6_to", 
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
    )
  })
  output$t1a7_2t_to = output$t1a7_3t_to = renderUI({
    div(
      style = "margin-bottom: -0.05rem;",
      pickerInput(
        "t1a7_to", 
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
    )
  })
  output$t1a8_2t_to = output$t1a8_3t_to = renderUI({
    div(
      style = "margin-bottom: -0.05rem;",
      pickerInput(
        "t1a8_to", 
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
    )
  })
  output$t1a9_2t_to = output$t1a9_3t_to = renderUI({
    div(
      style = "margin-bottom: -0.05rem;",
      pickerInput(
        "t1a9_to", 
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
    )
  })
  output$t1a10_2t_to = output$t1a10_3t_to = renderUI({
    div(
      style = "margin-bottom: -0.05rem;",
      pickerInput(
        "t1a10_to", 
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
    )
  })
  
  # Team 2
  output$t2a1_2t_to = output$t2a1_3t_to = renderUI({
    div(
      style = "margin-bottom: -0.05rem;",
      pickerInput(
        "t2a1_to", 
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
    )
  })
  output$t2a2_2t_to = output$t2a2_3t_to = renderUI({
    div(
      style = "margin-bottom: -0.05rem;",
      pickerInput(
        "t2a2_to", 
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
    )
  })
  output$t2a3_2t_to = output$t2a3_3t_to = renderUI({
    div(
      style = "margin-bottom: -0.05rem;",
      pickerInput(
        "t2a3_to", 
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
    )
  })
  output$t2a4_2t_to = output$t2a4_3t_to = renderUI({
    div(
      style = "margin-bottom: -0.05rem;",
      pickerInput(
        "t2a4_to", 
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
    )
  })
  output$t2a5_2t_to = output$t2a5_3t_to = renderUI({
    div(
      style = "margin-bottom: -0.05rem;",
      pickerInput(
        "t2a5_to", 
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
    )
  })
  output$t2a6_2t_to = output$t2a6_3t_to = renderUI({
    div(
      style = "margin-bottom: -0.05rem;",
      pickerInput(
        "t2a6_to", 
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
    )
  })
  output$t2a7_2t_to = output$t2a7_3t_to = renderUI({
    div(
      style = "margin-bottom: -0.05rem;",
      pickerInput(
        "t2a7_to", 
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
    )
  })
  output$t2a8_2t_to = output$t2a8_3t_to = renderUI({
    div(
      style = "margin-bottom: -0.05rem;",
      pickerInput(
        "t2a8_to", 
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
    )
  })
  output$t2a9_2t_to = output$t2a9_3t_to = renderUI({
    div(
      style = "margin-bottom: -0.05rem;",
      pickerInput(
        "t2a9_to", 
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
    )
  })
  output$t2a10_2t_to = output$t2a10_3t_to = renderUI({
    div(
      style = "margin-bottom: -0.05rem;",
      pickerInput(
        "t2a10_to", 
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
    )
  })
  
  # Traded Asset Protection Input
  # Team 1
  output$t1a1_2t_pro= output$t1a1_3t_pro = renderUI({
    textInput(
      "t1a1_pro", 
      ""
    )
  })
  output$t1a2_2t_pro= output$t1a2_3t_pro = renderUI({
    textInput(
      "t1a2_pro", 
      ""
    )
  })
  output$t1a3_2t_pro= output$t1a3_3t_pro = renderUI({
    textInput(
      "t1a3_pro", 
      ""
    )
  })
  output$t1a4_2t_pro= output$t1a4_3t_pro = renderUI({
    textInput(
      "t1a4_pro", 
      ""
    )
  })
  output$t1a5_2t_pro= output$t1a5_3t_pro = renderUI({
    textInput(
      "t1a5_pro", 
      ""
    )
  })
  output$t1a6_2t_pro= output$t1a6_3t_pro = renderUI({
    textInput(
      "t1a6_pro", 
      ""
    )
  })
  output$t1a7_2t_pro= output$t1a7_3t_pro = renderUI({
    textInput(
      "t1a7_pro", 
      ""
    )
  })
  output$t1a8_2t_pro= output$t1a8_3t_pro = renderUI({
    textInput(
      "t1a8_pro", 
      ""
    )
  })
  output$t1a9_2t_pro= output$t1a9_3t_pro = renderUI({
    textInput(
      "t1a9_pro", 
      ""
    )
  })
  output$t1a10_2t_pro= output$t1a10_3t_pro = renderUI({
    textInput(
      "t1a10_pro", 
      ""
    )
  })
  
  # Team 2
  output$t2a1_2t_pro= output$t2a1_3t_pro = renderUI({
    textInput(
      "t2a1_pro", 
      ""
    )
  })
  output$t2a2_2t_pro= output$t2a2_3t_pro = renderUI({
    textInput(
      "t2a2_pro", 
      ""
    )
  })
  output$t2a3_2t_pro= output$t2a3_3t_pro = renderUI({
    textInput(
      "t2a3_pro", 
      ""
    )
  })
  output$t2a4_2t_pro= output$t2a4_3t_pro = renderUI({
    textInput(
      "t2a4_pro", 
      ""
    )
  })
  output$t2a5_2t_pro= output$t2a5_3t_pro = renderUI({
    textInput(
      "t2a5_pro", 
      ""
    )
  })
  output$t2a6_2t_pro= output$t2a6_3t_pro = renderUI({
    textInput(
      "t2a6_pro", 
      ""
    )
  })
  output$t2a7_2t_pro= output$t2a7_3t_pro = renderUI({
    textInput(
      "t2a7_pro", 
      ""
    )
  })
  output$t2a8_2t_pro= output$t2a8_3t_pro = renderUI({
    textInput(
      "t2a8_pro", 
      ""
    )
  })
  output$t2a9_2t_pro= output$t2a9_3t_pro = renderUI({
    textInput(
      "t2a9_pro", 
      ""
    )
  })
  output$t2a10_2t_pro= output$t2a10_3t_pro = renderUI({
    textInput(
      "t2a10_pro", 
      ""
    )
  })
  
  
}


shinyApp(ui, server)