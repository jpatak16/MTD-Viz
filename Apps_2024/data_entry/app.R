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
  tags$hr(style="border-width: 2px; border-color:rgba(169,20,20,128); margin-bottom:0px;"),
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
  # If no teams are selected
  conditionalPanel(
    condition = "input.teamsInvolved.length == 0",
    div(
      class = "col-xs-12",
      style = "text-align: center; border-top: 2px solid black; padding-left: 1.2em; padding-right: 1.2em;",
      h2("Welcome to the 2024 ASU Mock Trade Deadline Competition!", style = "margin-bottom: 1em;"),
      p("During competiton hours, please enter your trades using this portal. Once the trade is entered, you will recieve a transaction ID for the trade. Please save this ID because once you leave that screen, you will not be able to see it again.", style = "margin-bottom: 1em;"),
      p("Submitting a trade on this portal does NOT make it official. You and one representivie from each team involved in the trade must take your transaction ID to the trade checkers. It will then be reviewed, and if confirmed legal by them, will then become offical.", style = "margin-bottom: 1em;"),
      h3("Thank You!")
    )
  ),
  # One Team Panel
  conditionalPanel(
    condition = "input.teamsInvolved.length == 1",
    div(
      class = "row",
      div(
        class = "col-md-12",
        style = "text-align: center; border-right: 2px solid black; border-top: 2px solid black; padding-left: 1.2em; padding-right: 1.2em;",
        h5(""),
        htmlOutput("team1_logo"),
        h5(""),
        # Players Traded
        div(
          class = "row",
          div(
            class = "col-2",
            conditionalPanel(
              condition = "input.t1p1_1t== 'HiddenPanel'",
              h3("_")
            )
          ),
          div(
            class = "col-4",
            style = "text-align: left; padding-left: 1.2em; padding-right: 0.6em;",
            h5("Player:"),
            uiOutput("t1p1_1t"),
            conditionalPanel(condition = "!(input.t1p1_1t== ' '||input.t1p1_1t=='')", uiOutput("t1p2_1t")),
            conditionalPanel(condition = "!(input.t1p2_1t== ' '||input.t1p2_1t=='')", uiOutput("t1p3_1t")),
            conditionalPanel(condition = "!(input.t1p3_1t== ' '||input.t1p3_1t=='')", uiOutput("t1p4_1t")),
            conditionalPanel(condition = "!(input.t1p4_1t== ' '||input.t1p4_1t=='')", uiOutput("t1p5_1t")),
            conditionalPanel(condition = "!(input.t1p5_1t== ' '||input.t1p5_1t=='')", uiOutput("t1p6_1t")),
            conditionalPanel(condition = "!(input.t1p6_1t== ' '||input.t1p6_1t=='')", uiOutput("t1p7_1t")),
            conditionalPanel(condition = "!(input.t1p7_1t== ' '||input.t1p7_1t=='')", uiOutput("t1p8_1t")),
            conditionalPanel(condition = "!(input.t1p8_1t== ' '||input.t1p8_1t=='')", uiOutput("t1p9_1t")),
            conditionalPanel(condition = "!(input.t1p9_1t== ' '||input.t1p9_1t=='')", uiOutput("t1p10_1t"))
          ),
          div(
            class = "col-4",
            style = "text-align: left; width: 38%; padding-left: .6em; padding-right: 1.2em;",
            h5("To:"),
            uiOutput("t1p1_1t_to"),
            conditionalPanel(condition = "!(input.t1p1_1t== ' '||input.t1p1_1t=='')", uiOutput("t1p2_1t_to")),
            conditionalPanel(condition = "!(input.t1p2_1t== ' '||input.t1p2_1t=='')", uiOutput("t1p3_1t_to")),
            conditionalPanel(condition = "!(input.t1p3_1t== ' '||input.t1p3_1t=='')", uiOutput("t1p4_1t_to")),
            conditionalPanel(condition = "!(input.t1p4_1t== ' '||input.t1p4_1t=='')", uiOutput("t1p5_1t_to")),
            conditionalPanel(condition = "!(input.t1p5_1t== ' '||input.t1p5_1t=='')", uiOutput("t1p6_1t_to")),
            conditionalPanel(condition = "!(input.t1p6_1t== ' '||input.t1p6_1t=='')", uiOutput("t1p7_1t_to")),
            conditionalPanel(condition = "!(input.t1p7_1t== ' '||input.t1p7_1t=='')", uiOutput("t1p8_1t_to")),
            conditionalPanel(condition = "!(input.t1p8_1t== ' '||input.t1p8_1t=='')", uiOutput("t1p9_1t_to")),
            conditionalPanel(condition = "!(input.t1p9_1t== ' '||input.t1p9_1t=='')", uiOutput("t1p10_1t_to"))
          ),
          div(
            class = "col-2",
            conditionalPanel(
              condition = "input.t1p1_1t== 'HiddenPanel'",
              h3("_")
            )
          )
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
            conditionalPanel(condition = "!(input.t1p1_2t== ' '||input.t1p1_2t=='')", uiOutput("t1p2_2t")),
            conditionalPanel(condition = "!(input.t1p2_2t== ' '||input.t1p2_2t=='')", uiOutput("t1p3_2t")),
            conditionalPanel(condition = "!(input.t1p3_2t== ' '||input.t1p3_2t=='')", uiOutput("t1p4_2t")),
            conditionalPanel(condition = "!(input.t1p4_2t== ' '||input.t1p4_2t=='')", uiOutput("t1p5_2t")),
            conditionalPanel(condition = "!(input.t1p5_2t== ' '||input.t1p5_2t=='')", uiOutput("t1p6_2t")),
            conditionalPanel(condition = "!(input.t1p6_2t== ' '||input.t1p6_2t=='')", uiOutput("t1p7_2t")),
            conditionalPanel(condition = "!(input.t1p7_2t== ' '||input.t1p7_2t=='')", uiOutput("t1p8_2t")),
            conditionalPanel(condition = "!(input.t1p8_2t== ' '||input.t1p8_2t=='')", uiOutput("t1p9_2t")),
            conditionalPanel(condition = "!(input.t1p9_2t== ' '||input.t1p9_2t=='')", uiOutput("t1p10_2t"))
          ),
          div(
            class = "col-xs-4",
            style = "text-align: left; width: 38%; padding-left: .6em; padding-right: 1.2em;",
            h5("To:"),
            uiOutput("t1p1_2t_to"),
            conditionalPanel(condition = "!(input.t1p1_2t== ' '||input.t1p1_2t=='')", uiOutput("t1p2_2t_to")),
            conditionalPanel(condition = "!(input.t1p2_2t== ' '||input.t1p2_2t=='')", uiOutput("t1p3_2t_to")),
            conditionalPanel(condition = "!(input.t1p3_2t== ' '||input.t1p3_2t=='')", uiOutput("t1p4_2t_to")),
            conditionalPanel(condition = "!(input.t1p4_2t== ' '||input.t1p4_2t=='')", uiOutput("t1p5_2t_to")),
            conditionalPanel(condition = "!(input.t1p5_2t== ' '||input.t1p5_2t=='')", uiOutput("t1p6_2t_to")),
            conditionalPanel(condition = "!(input.t1p6_2t== ' '||input.t1p6_2t=='')", uiOutput("t1p7_2t_to")),
            conditionalPanel(condition = "!(input.t1p7_2t== ' '||input.t1p7_2t=='')", uiOutput("t1p8_2t_to")),
            conditionalPanel(condition = "!(input.t1p8_2t== ' '||input.t1p8_2t=='')", uiOutput("t1p9_2t_to")),
            conditionalPanel(condition = "!(input.t1p9_2t== ' '||input.t1p9_2t=='')", uiOutput("t1p10_2t_to"))
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
            conditionalPanel(condition = "!(input.t1a1_2t== ' '||input.t1a1_2t=='')", uiOutput("t1a2_2t")),
            conditionalPanel(condition = "!(input.t1a2_2t== ' '||input.t1a2_2t=='')", uiOutput("t1a3_2t")),
            conditionalPanel(condition = "!(input.t1a3_2t== ' '||input.t1a3_2t=='')", uiOutput("t1a4_2t")),
            conditionalPanel(condition = "!(input.t1a4_2t== ' '||input.t1a4_2t=='')", uiOutput("t1a5_2t")),
            conditionalPanel(condition = "!(input.t1a5_2t== ' '||input.t1a5_2t=='')", uiOutput("t1a6_2t")),
            conditionalPanel(condition = "!(input.t1a6_2t== ' '||input.t1a6_2t=='')", uiOutput("t1a7_2t")),
            conditionalPanel(condition = "!(input.t1a7_2t== ' '||input.t1a7_2t=='')", uiOutput("t1a8_2t")),
            conditionalPanel(condition = "!(input.t1a8_2t== ' '||input.t1a8_2t=='')", uiOutput("t1a9_2t")),
            conditionalPanel(condition = "!(input.t1a9_2t== ' '||input.t1a9_2t=='')", uiOutput("t1a10_2t"))
          ),
          div(
            class = "col-xs-3",
            style = "text-align: left; width: 25%; padding-left: .6em; padding-right: .6em;",
            h5("To:", style = "font-size: 1.1rem;"),
            uiOutput("t1a1_2t_to"),
            conditionalPanel(condition = "!(input.t1a1_2t== ' '||input.t1a1_2t=='')", uiOutput("t1a2_2t_to")),
            conditionalPanel(condition = "!(input.t1a2_2t== ' '||input.t1a2_2t=='')", uiOutput("t1a3_2t_to")),
            conditionalPanel(condition = "!(input.t1a3_2t== ' '||input.t1a3_2t=='')", uiOutput("t1a4_2t_to")),
            conditionalPanel(condition = "!(input.t1a4_2t== ' '||input.t1a4_2t=='')", uiOutput("t1a5_2t_to")),
            conditionalPanel(condition = "!(input.t1a5_2t== ' '||input.t1a5_2t=='')", uiOutput("t1a6_2t_to")),
            conditionalPanel(condition = "!(input.t1a6_2t== ' '||input.t1a6_2t=='')", uiOutput("t1a7_2t_to")),
            conditionalPanel(condition = "!(input.t1a7_2t== ' '||input.t1a7_2t=='')", uiOutput("t1a8_2t_to")),
            conditionalPanel(condition = "!(input.t1a8_2t== ' '||input.t1a8_2t=='')", uiOutput("t1a9_2t_to")),
            conditionalPanel(condition = "!(input.t1a9_2t== ' '||input.t1a9_2t=='')", uiOutput("t1a10_2t_to"))
          ),
          div(
            class = "col-xs-2",
            style = "text-align: left; width: 35%; padding-left: .6em; padding-right: 1.2em;",
            h5("Add Protection?", style = "font-size: 1.1rem; width: 110%;"),
            uiOutput("t1a1_2t_pro"),
            conditionalPanel(condition = "!(input.t1a1_2t== ' '||input.t1a1_2t=='')", uiOutput("t1a2_2t_pro")),
            conditionalPanel(condition = "!(input.t1a2_2t== ' '||input.t1a2_2t=='')", uiOutput("t1a3_2t_pro")),
            conditionalPanel(condition = "!(input.t1a3_2t== ' '||input.t1a3_2t=='')", uiOutput("t1a4_2t_pro")),
            conditionalPanel(condition = "!(input.t1a4_2t== ' '||input.t1a4_2t=='')", uiOutput("t1a5_2t_pro")),
            conditionalPanel(condition = "!(input.t1a5_2t== ' '||input.t1a5_2t=='')", uiOutput("t1a6_2t_pro")),
            conditionalPanel(condition = "!(input.t1a6_2t== ' '||input.t1a6_2t=='')", uiOutput("t1a7_2t_pro")),
            conditionalPanel(condition = "!(input.t1a7_2t== ' '||input.t1a7_2t=='')", uiOutput("t1a8_2t_pro")),
            conditionalPanel(condition = "!(input.t1a8_2t== ' '||input.t1a8_2t=='')", uiOutput("t1a9_2t_pro")),
            conditionalPanel(condition = "!(input.t1a9_2t== ' '||input.t1a9_2t=='')", uiOutput("t1a10_2t_pro"))
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
            conditionalPanel(condition = "!(input.t2p1_2t== ' '||input.t2p1_2t=='')", uiOutput("t2p2_2t")),
            conditionalPanel(condition = "!(input.t2p2_2t== ' '||input.t2p2_2t=='')", uiOutput("t2p3_2t")),
            conditionalPanel(condition = "!(input.t2p3_2t== ' '||input.t2p3_2t=='')", uiOutput("t2p4_2t")),
            conditionalPanel(condition = "!(input.t2p4_2t== ' '||input.t2p4_2t=='')", uiOutput("t2p5_2t")),
            conditionalPanel(condition = "!(input.t2p5_2t== ' '||input.t2p5_2t=='')", uiOutput("t2p6_2t")),
            conditionalPanel(condition = "!(input.t2p6_2t== ' '||input.t2p6_2t=='')", uiOutput("t2p7_2t")),
            conditionalPanel(condition = "!(input.t2p7_2t== ' '||input.t2p7_2t=='')", uiOutput("t2p8_2t")),
            conditionalPanel(condition = "!(input.t2p8_2t== ' '||input.t2p8_2t=='')", uiOutput("t2p9_2t")),
            conditionalPanel(condition = "!(input.t2p9_2t== ' '||input.t2p9_2t=='')", uiOutput("t2p10_2t"))
          ),
          div(
            class = "col-xs-4",
            style = "text-align: left; width: 38%; padding-left: .6em; padding-right: 1.2em;",
            h5("To:"),
            uiOutput("t2p1_2t_to"),
            conditionalPanel(condition = "!(input.t2p1_2t== ' '||input.t2p1_2t=='')", uiOutput("t2p2_2t_to")),
            conditionalPanel(condition = "!(input.t2p2_2t== ' '||input.t2p2_2t=='')", uiOutput("t2p3_2t_to")),
            conditionalPanel(condition = "!(input.t2p3_2t== ' '||input.t2p3_2t=='')", uiOutput("t2p4_2t_to")),
            conditionalPanel(condition = "!(input.t2p4_2t== ' '||input.t2p4_2t=='')", uiOutput("t2p5_2t_to")),
            conditionalPanel(condition = "!(input.t2p5_2t== ' '||input.t2p5_2t=='')", uiOutput("t2p6_2t_to")),
            conditionalPanel(condition = "!(input.t2p6_2t== ' '||input.t2p6_2t=='')", uiOutput("t2p7_2t_to")),
            conditionalPanel(condition = "!(input.t2p7_2t== ' '||input.t2p7_2t=='')", uiOutput("t2p8_2t_to")),
            conditionalPanel(condition = "!(input.t2p8_2t== ' '||input.t2p8_2t=='')", uiOutput("t2p9_2t_to")),
            conditionalPanel(condition = "!(input.t2p9_2t== ' '||input.t2p9_2t=='')", uiOutput("t2p10_2t_to"))
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
            conditionalPanel(condition = "!(input.t2a1_2t== ' '||input.t2a1_2t=='')", uiOutput("t2a2_2t")),
            conditionalPanel(condition = "!(input.t2a2_2t== ' '||input.t2a2_2t=='')", uiOutput("t2a3_2t")),
            conditionalPanel(condition = "!(input.t2a3_2t== ' '||input.t2a3_2t=='')", uiOutput("t2a4_2t")),
            conditionalPanel(condition = "!(input.t2a4_2t== ' '||input.t2a4_2t=='')", uiOutput("t2a5_2t")),
            conditionalPanel(condition = "!(input.t2a5_2t== ' '||input.t2a5_2t=='')", uiOutput("t2a6_2t")),
            conditionalPanel(condition = "!(input.t2a6_2t== ' '||input.t2a6_2t=='')", uiOutput("t2a7_2t")),
            conditionalPanel(condition = "!(input.t2a7_2t== ' '||input.t2a7_2t=='')", uiOutput("t2a8_2t")),
            conditionalPanel(condition = "!(input.t2a8_2t== ' '||input.t2a8_2t=='')", uiOutput("t2a9_2t")),
            conditionalPanel(condition = "!(input.t2a9_2t== ' '||input.t2a9_2t=='')", uiOutput("t2a10_2t"))
          ),
          div(
            class = "col-xs-3",
            style = "text-align: left; width: 25%; padding-left: .6em; padding-right: .6em;",
            h5("To:", style = "font-size: 1.1rem;"),
            uiOutput("t2a1_2t_to"),
            conditionalPanel(condition = "!(input.t2a1_2t== ' '||input.t2a1_2t=='')", uiOutput("t2a2_2t_to")),
            conditionalPanel(condition = "!(input.t2a2_2t== ' '||input.t2a2_2t=='')", uiOutput("t2a3_2t_to")),
            conditionalPanel(condition = "!(input.t2a3_2t== ' '||input.t2a3_2t=='')", uiOutput("t2a4_2t_to")),
            conditionalPanel(condition = "!(input.t2a4_2t== ' '||input.t2a4_2t=='')", uiOutput("t2a5_2t_to")),
            conditionalPanel(condition = "!(input.t2a5_2t== ' '||input.t2a5_2t=='')", uiOutput("t2a6_2t_to")),
            conditionalPanel(condition = "!(input.t2a6_2t== ' '||input.t2a6_2t=='')", uiOutput("t2a7_2t_to")),
            conditionalPanel(condition = "!(input.t2a7_2t== ' '||input.t2a7_2t=='')", uiOutput("t2a8_2t_to")),
            conditionalPanel(condition = "!(input.t2a8_2t== ' '||input.t2a8_2t=='')", uiOutput("t2a9_2t_to")),
            conditionalPanel(condition = "!(input.t2a9_2t== ' '||input.t2a9_2t=='')", uiOutput("t2a10_2t_to"))
          ),
          div(
            class = "col-xs-2",
            style = "text-align: left; width: 35%; padding-left: .6em; padding-right: 1.2em;",
            h5("Add Protection?", style = "font-size: 1.1rem; width: 110%;"),
            uiOutput("t2a1_2t_pro"),
            conditionalPanel(condition = "!(input.t2a1_2t== ' '||input.t2a1_2t=='')", uiOutput("t2a2_2t_pro")),
            conditionalPanel(condition = "!(input.t2a2_2t== ' '||input.t2a2_2t=='')", uiOutput("t2a3_2t_pro")),
            conditionalPanel(condition = "!(input.t2a3_2t== ' '||input.t2a3_2t=='')", uiOutput("t2a4_2t_pro")),
            conditionalPanel(condition = "!(input.t2a4_2t== ' '||input.t2a4_2t=='')", uiOutput("t2a5_2t_pro")),
            conditionalPanel(condition = "!(input.t2a5_2t== ' '||input.t2a5_2t=='')", uiOutput("t2a6_2t_pro")),
            conditionalPanel(condition = "!(input.t2a6_2t== ' '||input.t2a6_2t=='')", uiOutput("t2a7_2t_pro")),
            conditionalPanel(condition = "!(input.t2a7_2t== ' '||input.t2a7_2t=='')", uiOutput("t2a8_2t_pro")),
            conditionalPanel(condition = "!(input.t2a8_2t== ' '||input.t2a8_2t=='')", uiOutput("t2a9_2t_pro")),
            conditionalPanel(condition = "!(input.t2a9_2t== ' '||input.t2a9_2t=='')", uiOutput("t2a10_2t_pro"))
          )
        )
      )
    )
  ),
  # Three Teams Panel
  conditionalPanel(
    condition = "input.teamsInvolved.length == 3",
    div(
      class = "row",
      # Team 1
      div(
        class = "col-lg-4 col-md-6",
        style = "text-align: center; border-right: 2px solid black; border-top: 2px solid black; padding-left: 1.2em; padding-right: 1.2em;",
        h5(""),
        htmlOutput("team1_logo3"),
        h5(""),
        # Players Traded
        div(
          class = "row",
          div(
            class = "col-xs-8",
            style = "text-align: left; width: 60%; padding-left: 1.2em; padding-right: .6em; white-space: nowrap;",
            h5("Players Traded:"),
            uiOutput("t1p1_3t"),
            conditionalPanel(condition = "!(input.t1p1_3t== ' '||input.t1p1_3t=='')", uiOutput("t1p2_3t")),
            conditionalPanel(condition = "!(input.t1p2_3t== ' '||input.t1p2_3t=='')", uiOutput("t1p3_3t")),
            conditionalPanel(condition = "!(input.t1p3_3t== ' '||input.t1p3_3t=='')", uiOutput("t1p4_3t")),
            conditionalPanel(condition = "!(input.t1p4_3t== ' '||input.t1p4_3t=='')", uiOutput("t1p5_3t")),
            conditionalPanel(condition = "!(input.t1p5_3t== ' '||input.t1p5_3t=='')", uiOutput("t1p6_3t")),
            conditionalPanel(condition = "!(input.t1p6_3t== ' '||input.t1p6_3t=='')", uiOutput("t1p7_3t")),
            conditionalPanel(condition = "!(input.t1p7_3t== ' '||input.t1p7_3t=='')", uiOutput("t1p8_3t")),
            conditionalPanel(condition = "!(input.t1p8_3t== ' '||input.t1p8_3t=='')", uiOutput("t1p9_3t")),
            conditionalPanel(condition = "!(input.t1p9_3t== ' '||input.t1p9_3t=='')", uiOutput("t1p10_3t"))
          ),
          div(
            class = "col-xs-4",
            style = "text-align: left; width: 38%; padding-left: .6em; padding-right: 1.2em;",
            h5("To:"),
            uiOutput("t1p1_3t_to"),
            conditionalPanel(condition = "!(input.t1p1_3t== ' '||input.t1p1_3t=='')", uiOutput("t1p2_3t_to")),
            conditionalPanel(condition = "!(input.t1p2_3t== ' '||input.t1p2_3t=='')", uiOutput("t1p3_3t_to")),
            conditionalPanel(condition = "!(input.t1p3_3t== ' '||input.t1p3_3t=='')", uiOutput("t1p4_3t_to")),
            conditionalPanel(condition = "!(input.t1p4_3t== ' '||input.t1p4_3t=='')", uiOutput("t1p5_3t_to")),
            conditionalPanel(condition = "!(input.t1p5_3t== ' '||input.t1p5_3t=='')", uiOutput("t1p6_3t_to")),
            conditionalPanel(condition = "!(input.t1p6_3t== ' '||input.t1p6_3t=='')", uiOutput("t1p7_3t_to")),
            conditionalPanel(condition = "!(input.t1p7_3t== ' '||input.t1p7_3t=='')", uiOutput("t1p8_3t_to")),
            conditionalPanel(condition = "!(input.t1p8_3t== ' '||input.t1p8_3t=='')", uiOutput("t1p9_3t_to")),
            conditionalPanel(condition = "!(input.t1p9_3t== ' '||input.t1p9_3t=='')", uiOutput("t1p10_3t_to"))
          )
        ),
        # Other Assets Traded
        div(
          class = "row",
          div(
            class = "col-xs-5",
            style = "text-align: left; width: 40%; padding-left: 1.2em; padding-right: .6em;",
            h5("Other Assets:", style = "font-size: 1.1rem;"),
            uiOutput("t1a1_3t"),
            conditionalPanel(condition = "!(input.t1a1_3t== ' '||input.t1a1_3t=='')", uiOutput("t1a2_3t")),
            conditionalPanel(condition = "!(input.t1a2_3t== ' '||input.t1a2_3t=='')", uiOutput("t1a3_3t")),
            conditionalPanel(condition = "!(input.t1a3_3t== ' '||input.t1a3_3t=='')", uiOutput("t1a4_3t")),
            conditionalPanel(condition = "!(input.t1a4_3t== ' '||input.t1a4_3t=='')", uiOutput("t1a5_3t")),
            conditionalPanel(condition = "!(input.t1a5_3t== ' '||input.t1a5_3t=='')", uiOutput("t1a6_3t")),
            conditionalPanel(condition = "!(input.t1a6_3t== ' '||input.t1a6_3t=='')", uiOutput("t1a7_3t")),
            conditionalPanel(condition = "!(input.t1a7_3t== ' '||input.t1a7_3t=='')", uiOutput("t1a8_3t")),
            conditionalPanel(condition = "!(input.t1a8_3t== ' '||input.t1a8_3t=='')", uiOutput("t1a9_3t")),
            conditionalPanel(condition = "!(input.t1a9_3t== ' '||input.t1a9_3t=='')", uiOutput("t1a10_3t"))
          ),
          div(
            class = "col-xs-3",
            style = "text-align: left; width: 25%; padding-left: .6em; padding-right: .6em;",
            h5("To:", style = "font-size: 1.1rem;"),
            uiOutput("t1a1_3t_to"),
            conditionalPanel(condition = "!(input.t1a1_3t== ' '||input.t1a1_3t=='')", uiOutput("t1a2_3t_to")),
            conditionalPanel(condition = "!(input.t1a2_3t== ' '||input.t1a2_3t=='')", uiOutput("t1a3_3t_to")),
            conditionalPanel(condition = "!(input.t1a3_3t== ' '||input.t1a3_3t=='')", uiOutput("t1a4_3t_to")),
            conditionalPanel(condition = "!(input.t1a4_3t== ' '||input.t1a4_3t=='')", uiOutput("t1a5_3t_to")),
            conditionalPanel(condition = "!(input.t1a5_3t== ' '||input.t1a5_3t=='')", uiOutput("t1a6_3t_to")),
            conditionalPanel(condition = "!(input.t1a6_3t== ' '||input.t1a6_3t=='')", uiOutput("t1a7_3t_to")),
            conditionalPanel(condition = "!(input.t1a7_3t== ' '||input.t1a7_3t=='')", uiOutput("t1a8_3t_to")),
            conditionalPanel(condition = "!(input.t1a8_3t== ' '||input.t1a8_3t=='')", uiOutput("t1a9_3t_to")),
            conditionalPanel(condition = "!(input.t1a9_3t== ' '||input.t1a9_3t=='')", uiOutput("t1a10_3t_to"))
          ),
          div(
            class = "col-xs-2",
            style = "text-align: left; width: 35%; padding-left: .6em; padding-right: 1.2em;",
            h5("Add Protection?", style = "font-size: 1.1rem; width: 110%; white-space: nowrap"),
            uiOutput("t1a1_3t_pro"),
            conditionalPanel(condition = "!(input.t1a1_3t== ' '||input.t1a1_3t=='')", uiOutput("t1a2_3t_pro")),
            conditionalPanel(condition = "!(input.t1a2_3t== ' '||input.t1a2_3t=='')", uiOutput("t1a3_3t_pro")),
            conditionalPanel(condition = "!(input.t1a3_3t== ' '||input.t1a3_3t=='')", uiOutput("t1a4_3t_pro")),
            conditionalPanel(condition = "!(input.t1a4_3t== ' '||input.t1a4_3t=='')", uiOutput("t1a5_3t_pro")),
            conditionalPanel(condition = "!(input.t1a5_3t== ' '||input.t1a5_3t=='')", uiOutput("t1a6_3t_pro")),
            conditionalPanel(condition = "!(input.t1a6_3t== ' '||input.t1a6_3t=='')", uiOutput("t1a7_3t_pro")),
            conditionalPanel(condition = "!(input.t1a7_3t== ' '||input.t1a7_3t=='')", uiOutput("t1a8_3t_pro")),
            conditionalPanel(condition = "!(input.t1a8_3t== ' '||input.t1a8_3t=='')", uiOutput("t1a9_3t_pro")),
            conditionalPanel(condition = "!(input.t1a9_3t== ' '||input.t1a9_3t=='')", uiOutput("t1a10_3t_pro"))
          )
        )
      ),
      # Team 2
      div(
        class = "col-lg-4 col-md-6",
        style = "text-align: center; border-right: 2px solid black; border-top: 2px solid black; padding-left: 1.2em; padding-right: 1.2em;",
        h5(""),
        htmlOutput("team2_logo3"),
        h5(""),
        # Players Traded
        div(
          class = "row",
          div(
            class = "col-xs-8",
            style = "text-align: left; width: 60%; padding-left: 1.2em; padding-right: .6em; white-space: nowrap;",
            h5("Players Traded:"),
            uiOutput("t2p1_3t"),
            conditionalPanel(condition = "!(input.t2p1_3t== ' '||input.t2p1_3t=='')", uiOutput("t2p2_3t")),
            conditionalPanel(condition = "!(input.t2p2_3t== ' '||input.t2p2_3t=='')", uiOutput("t2p3_3t")),
            conditionalPanel(condition = "!(input.t2p3_3t== ' '||input.t2p3_3t=='')", uiOutput("t2p4_3t")),
            conditionalPanel(condition = "!(input.t2p4_3t== ' '||input.t2p4_3t=='')", uiOutput("t2p5_3t")),
            conditionalPanel(condition = "!(input.t2p5_3t== ' '||input.t2p5_3t=='')", uiOutput("t2p6_3t")),
            conditionalPanel(condition = "!(input.t2p6_3t== ' '||input.t2p6_3t=='')", uiOutput("t2p7_3t")),
            conditionalPanel(condition = "!(input.t2p7_3t== ' '||input.t2p7_3t=='')", uiOutput("t2p8_3t")),
            conditionalPanel(condition = "!(input.t2p8_3t== ' '||input.t2p8_3t=='')", uiOutput("t2p9_3t")),
            conditionalPanel(condition = "!(input.t2p9_3t== ' '||input.t2p9_3t=='')", uiOutput("t2p10_3t"))
          ),
          div(
            class = "col-xs-4",
            style = "text-align: left; width: 38%; padding-left: .6em; padding-right: 1.2em;",
            h5("To:"),
            uiOutput("t2p1_3t_to"),
            conditionalPanel(condition = "!(input.t2p1_3t== ' '||input.t2p1_3t=='')", uiOutput("t2p2_3t_to")),
            conditionalPanel(condition = "!(input.t2p2_3t== ' '||input.t2p2_3t=='')", uiOutput("t2p3_3t_to")),
            conditionalPanel(condition = "!(input.t2p3_3t== ' '||input.t2p3_3t=='')", uiOutput("t2p4_3t_to")),
            conditionalPanel(condition = "!(input.t2p4_3t== ' '||input.t2p4_3t=='')", uiOutput("t2p5_3t_to")),
            conditionalPanel(condition = "!(input.t2p5_3t== ' '||input.t2p5_3t=='')", uiOutput("t2p6_3t_to")),
            conditionalPanel(condition = "!(input.t2p6_3t== ' '||input.t2p6_3t=='')", uiOutput("t2p7_3t_to")),
            conditionalPanel(condition = "!(input.t2p7_3t== ' '||input.t2p7_3t=='')", uiOutput("t2p8_3t_to")),
            conditionalPanel(condition = "!(input.t2p8_3t== ' '||input.t2p8_3t=='')", uiOutput("t2p9_3t_to")),
            conditionalPanel(condition = "!(input.t2p9_3t== ' '||input.t2p9_3t=='')", uiOutput("t2p10_3t_to"))
          )
        ),
        # Other Assets Traded
        div(
          class = "row",
          div(
            class = "col-xs-5",
            style = "text-align: left; width: 40%; padding-left: 1.2em; padding-right: .6em;",
            h5("Other Assets:", style = "font-size: 1.1rem;"),
            uiOutput("t2a1_3t"),
            conditionalPanel(condition = "!(input.t2a1_3t== ' '||input.t2a1_3t=='')", uiOutput("t2a2_3t")),
            conditionalPanel(condition = "!(input.t2a2_3t== ' '||input.t2a2_3t=='')", uiOutput("t2a3_3t")),
            conditionalPanel(condition = "!(input.t2a3_3t== ' '||input.t2a3_3t=='')", uiOutput("t2a4_3t")),
            conditionalPanel(condition = "!(input.t2a4_3t== ' '||input.t2a4_3t=='')", uiOutput("t2a5_3t")),
            conditionalPanel(condition = "!(input.t2a5_3t== ' '||input.t2a5_3t=='')", uiOutput("t2a6_3t")),
            conditionalPanel(condition = "!(input.t2a6_3t== ' '||input.t2a6_3t=='')", uiOutput("t2a7_3t")),
            conditionalPanel(condition = "!(input.t2a7_3t== ' '||input.t2a7_3t=='')", uiOutput("t2a8_3t")),
            conditionalPanel(condition = "!(input.t2a8_3t== ' '||input.t2a8_3t=='')", uiOutput("t2a9_3t")),
            conditionalPanel(condition = "!(input.t2a9_3t== ' '||input.t2a9_3t=='')", uiOutput("t2a10_3t"))
          ),
          div(
            class = "col-xs-3",
            style = "text-align: left; width: 25%; padding-left: .6em; padding-right: .6em;",
            h5("To:", style = "font-size: 1.1rem;"),
            uiOutput("t2a1_3t_to"),
            conditionalPanel(condition = "!(input.t2a1_3t== ' '||input.t2a1_3t=='')", uiOutput("t2a2_3t_to")),
            conditionalPanel(condition = "!(input.t2a2_3t== ' '||input.t2a2_3t=='')", uiOutput("t2a3_3t_to")),
            conditionalPanel(condition = "!(input.t2a3_3t== ' '||input.t2a3_3t=='')", uiOutput("t2a4_3t_to")),
            conditionalPanel(condition = "!(input.t2a4_3t== ' '||input.t2a4_3t=='')", uiOutput("t2a5_3t_to")),
            conditionalPanel(condition = "!(input.t2a5_3t== ' '||input.t2a5_3t=='')", uiOutput("t2a6_3t_to")),
            conditionalPanel(condition = "!(input.t2a6_3t== ' '||input.t2a6_3t=='')", uiOutput("t2a7_3t_to")),
            conditionalPanel(condition = "!(input.t2a7_3t== ' '||input.t2a7_3t=='')", uiOutput("t2a8_3t_to")),
            conditionalPanel(condition = "!(input.t2a8_3t== ' '||input.t2a8_3t=='')", uiOutput("t2a9_3t_to")),
            conditionalPanel(condition = "!(input.t2a9_3t== ' '||input.t2a9_3t=='')", uiOutput("t2a10_3t_to"))
          ),
          div(
            class = "col-xs-2",
            style = "text-align: left; width: 35%; padding-left: .6em; padding-right: 1.2em; white-space: nowrap;",
            h5("Add Protection?", style = "font-size: 1.1rem; width: 110%;"),
            uiOutput("t2a1_3t_pro"),
            conditionalPanel(condition = "!(input.t2a1_3t== ' '||input.t2a1_3t=='')", uiOutput("t2a2_3t_pro")),
            conditionalPanel(condition = "!(input.t2a2_3t== ' '||input.t2a2_3t=='')", uiOutput("t2a3_3t_pro")),
            conditionalPanel(condition = "!(input.t2a3_3t== ' '||input.t2a3_3t=='')", uiOutput("t2a4_3t_pro")),
            conditionalPanel(condition = "!(input.t2a4_3t== ' '||input.t2a4_3t=='')", uiOutput("t2a5_3t_pro")),
            conditionalPanel(condition = "!(input.t2a5_3t== ' '||input.t2a5_3t=='')", uiOutput("t2a6_3t_pro")),
            conditionalPanel(condition = "!(input.t2a6_3t== ' '||input.t2a6_3t=='')", uiOutput("t2a7_3t_pro")),
            conditionalPanel(condition = "!(input.t2a7_3t== ' '||input.t2a7_3t=='')", uiOutput("t2a8_3t_pro")),
            conditionalPanel(condition = "!(input.t2a8_3t== ' '||input.t2a8_3t=='')", uiOutput("t2a9_3t_pro")),
            conditionalPanel(condition = "!(input.t2a9_3t== ' '||input.t2a9_3t=='')", uiOutput("t2a10_3t_pro"))
          )
        )
      ),
      # Team 3
      div(
        class = "col-lg-4 col-md-6",
        style = "text-align: center; border-right: 2px solid black; border-top: 2px solid black; padding-left: 1.2em; padding-right: 1.2em;",
        h5(""),
        htmlOutput("team3_logo"),
        h5(""),
        # Players Traded
        div(
          class = "row",
          div(
            class = "col-xs-8",
            style = "text-align: left; width: 60%; padding-left: 1.2em; padding-right: .6em; white-space: nowrap;",
            h5("Players Traded:"),
            uiOutput("t3p1_3t"),
            conditionalPanel(condition = "!(input.t3p1_3t== ' '||input.t3p1_3t=='')", uiOutput("t3p2_3t")),
            conditionalPanel(condition = "!(input.t3p2_3t== ' '||input.t3p2_3t=='')", uiOutput("t3p3_3t")),
            conditionalPanel(condition = "!(input.t3p3_3t== ' '||input.t3p3_3t=='')", uiOutput("t3p4_3t")),
            conditionalPanel(condition = "!(input.t3p4_3t== ' '||input.t3p4_3t=='')", uiOutput("t3p5_3t")),
            conditionalPanel(condition = "!(input.t3p5_3t== ' '||input.t3p5_3t=='')", uiOutput("t3p6_3t")),
            conditionalPanel(condition = "!(input.t3p6_3t== ' '||input.t3p6_3t=='')", uiOutput("t3p7_3t")),
            conditionalPanel(condition = "!(input.t3p7_3t== ' '||input.t3p7_3t=='')", uiOutput("t3p8_3t")),
            conditionalPanel(condition = "!(input.t3p8_3t== ' '||input.t3p8_3t=='')", uiOutput("t3p9_3t")),
            conditionalPanel(condition = "!(input.t3p9_3t== ' '||input.t3p9_3t=='')", uiOutput("t3p10_3t"))
          ),
          div(
            class = "col-xs-4",
            style = "text-align: left; width: 38%; padding-left: .6em; padding-right: 1.2em;",
            h5("To:"),
            uiOutput("t3p1_3t_to"),
            conditionalPanel(condition = "!(input.t3p1_3t== ' '||input.t3p1_3t=='')", uiOutput("t3p2_3t_to")),
            conditionalPanel(condition = "!(input.t3p2_3t== ' '||input.t3p2_3t=='')", uiOutput("t3p3_3t_to")),
            conditionalPanel(condition = "!(input.t3p3_3t== ' '||input.t3p3_3t=='')", uiOutput("t3p4_3t_to")),
            conditionalPanel(condition = "!(input.t3p4_3t== ' '||input.t3p4_3t=='')", uiOutput("t3p5_3t_to")),
            conditionalPanel(condition = "!(input.t3p5_3t== ' '||input.t3p5_3t=='')", uiOutput("t3p6_3t_to")),
            conditionalPanel(condition = "!(input.t3p6_3t== ' '||input.t3p6_3t=='')", uiOutput("t3p7_3t_to")),
            conditionalPanel(condition = "!(input.t3p7_3t== ' '||input.t3p7_3t=='')", uiOutput("t3p8_3t_to")),
            conditionalPanel(condition = "!(input.t3p8_3t== ' '||input.t3p8_3t=='')", uiOutput("t3p9_3t_to")),
            conditionalPanel(condition = "!(input.t3p9_3t== ' '||input.t3p9_3t=='')", uiOutput("t3p10_3t_to"))
          )
        ),
        # Other Assets Traded
        div(
          class = "row",
          div(
            class = "col-xs-5",
            style = "text-align: left; width: 40%; padding-left: 1.2em; padding-right: .6em;",
            h5("Other Assets:", style = "font-size: 1.1rem;"),
            uiOutput("t3a1_3t"),
            conditionalPanel(condition = "!(input.t3a1_3t== ' '||input.t3a1_3t=='')", uiOutput("t3a2_3t")),
            conditionalPanel(condition = "!(input.t3a2_3t== ' '||input.t3a2_3t=='')", uiOutput("t3a3_3t")),
            conditionalPanel(condition = "!(input.t3a3_3t== ' '||input.t3a3_3t=='')", uiOutput("t3a4_3t")),
            conditionalPanel(condition = "!(input.t3a4_3t== ' '||input.t3a4_3t=='')", uiOutput("t3a5_3t")),
            conditionalPanel(condition = "!(input.t3a5_3t== ' '||input.t3a5_3t=='')", uiOutput("t3a6_3t")),
            conditionalPanel(condition = "!(input.t3a6_3t== ' '||input.t3a6_3t=='')", uiOutput("t3a7_3t")),
            conditionalPanel(condition = "!(input.t3a7_3t== ' '||input.t3a7_3t=='')", uiOutput("t3a8_3t")),
            conditionalPanel(condition = "!(input.t3a8_3t== ' '||input.t3a8_3t=='')", uiOutput("t3a9_3t")),
            conditionalPanel(condition = "!(input.t3a9_3t== ' '||input.t3a9_3t=='')", uiOutput("t3a10_3t"))
          ),
          div(
            class = "col-xs-3",
            style = "text-align: left; width: 25%; padding-left: .6em; padding-right: .6em;",
            h5("To:", style = "font-size: 1.1rem;"),
            uiOutput("t3a1_3t_to"),
            conditionalPanel(condition = "!(input.t3a1_3t== ' '||input.t3a1_3t=='')", uiOutput("t3a2_3t_to")),
            conditionalPanel(condition = "!(input.t3a2_3t== ' '||input.t3a2_3t=='')", uiOutput("t3a3_3t_to")),
            conditionalPanel(condition = "!(input.t3a3_3t== ' '||input.t3a3_3t=='')", uiOutput("t3a4_3t_to")),
            conditionalPanel(condition = "!(input.t3a4_3t== ' '||input.t3a4_3t=='')", uiOutput("t3a5_3t_to")),
            conditionalPanel(condition = "!(input.t3a5_3t== ' '||input.t3a5_3t=='')", uiOutput("t3a6_3t_to")),
            conditionalPanel(condition = "!(input.t3a6_3t== ' '||input.t3a6_3t=='')", uiOutput("t3a7_3t_to")),
            conditionalPanel(condition = "!(input.t3a7_3t== ' '||input.t3a7_3t=='')", uiOutput("t3a8_3t_to")),
            conditionalPanel(condition = "!(input.t3a8_3t== ' '||input.t3a8_3t=='')", uiOutput("t3a9_3t_to")),
            conditionalPanel(condition = "!(input.t3a9_3t== ' '||input.t3a9_3t=='')", uiOutput("t3a10_3t_to"))
          ),
          div(
            class = "col-xs-2",
            style = "text-align: left; width: 35%; padding-left: .6em; padding-right: 1.2em;",
            h5("Add Protection?", style = "font-size: 1.1rem; width: 110%; white-space: nowrap;"),
            uiOutput("t3a1_3t_pro"),
            conditionalPanel(condition = "!(input.t3a1_3t== ' '||input.t3a1_3t=='')", uiOutput("t3a2_3t_pro")),
            conditionalPanel(condition = "!(input.t3a2_3t== ' '||input.t3a2_3t=='')", uiOutput("t3a3_3t_pro")),
            conditionalPanel(condition = "!(input.t3a3_3t== ' '||input.t3a3_3t=='')", uiOutput("t3a4_3t_pro")),
            conditionalPanel(condition = "!(input.t3a4_3t== ' '||input.t3a4_3t=='')", uiOutput("t3a5_3t_pro")),
            conditionalPanel(condition = "!(input.t3a5_3t== ' '||input.t3a5_3t=='')", uiOutput("t3a6_3t_pro")),
            conditionalPanel(condition = "!(input.t3a6_3t== ' '||input.t3a6_3t=='')", uiOutput("t3a7_3t_pro")),
            conditionalPanel(condition = "!(input.t3a7_3t== ' '||input.t3a7_3t=='')", uiOutput("t3a8_3t_pro")),
            conditionalPanel(condition = "!(input.t3a8_3t== ' '||input.t3a8_3t=='')", uiOutput("t3a9_3t_pro")),
            conditionalPanel(condition = "!(input.t3a9_3t== ' '||input.t3a9_3t=='')", uiOutput("t3a10_3t_pro"))
          )
        )
      )
    )
  ),
  # Four Team Panel
  conditionalPanel(
    condition = "input.teamsInvolved.length == 4",
    div(
      class = "row",
      # Team 1
      div(
        class = "col-md-6",
        style = "text-align: center; border-right: 2px solid black; border-top: 2px solid black; padding-left: 1.2em; padding-right: 1.2em;",
        h5(""),
        htmlOutput("team1_logo4"),
        h5(""),
        # Players Traded
        div(
          class = "row",
          div(
            class = "col-xs-8",
            style = "text-align: left; width: 60%; padding-left: 1.2em; padding-right: .6em;",
            h5("Players Traded:"),
            uiOutput("t1p1_4t"),
            conditionalPanel(condition = "!(input.t1p1_4t== ' '||input.t1p1_4t=='')", uiOutput("t1p2_4t")),
            conditionalPanel(condition = "!(input.t1p2_4t== ' '||input.t1p2_4t=='')", uiOutput("t1p3_4t")),
            conditionalPanel(condition = "!(input.t1p3_4t== ' '||input.t1p3_4t=='')", uiOutput("t1p4_4t")),
            conditionalPanel(condition = "!(input.t1p4_4t== ' '||input.t1p4_4t=='')", uiOutput("t1p5_4t")),
            conditionalPanel(condition = "!(input.t1p5_4t== ' '||input.t1p5_4t=='')", uiOutput("t1p6_4t")),
            conditionalPanel(condition = "!(input.t1p6_4t== ' '||input.t1p6_4t=='')", uiOutput("t1p7_4t")),
            conditionalPanel(condition = "!(input.t1p7_4t== ' '||input.t1p7_4t=='')", uiOutput("t1p8_4t")),
            conditionalPanel(condition = "!(input.t1p8_4t== ' '||input.t1p8_4t=='')", uiOutput("t1p9_4t")),
            conditionalPanel(condition = "!(input.t1p9_4t== ' '||input.t1p9_4t=='')", uiOutput("t1p10_4t"))
          ),
          div(
            class = "col-xs-4",
            style = "text-align: left; width: 38%; padding-left: .6em; padding-right: 1.2em;",
            h5("To:"),
            uiOutput("t1p1_4t_to"),
            conditionalPanel(condition = "!(input.t1p1_4t== ' '||input.t1p1_4t=='')", uiOutput("t1p2_4t_to")),
            conditionalPanel(condition = "!(input.t1p2_4t== ' '||input.t1p2_4t=='')", uiOutput("t1p3_4t_to")),
            conditionalPanel(condition = "!(input.t1p3_4t== ' '||input.t1p3_4t=='')", uiOutput("t1p4_4t_to")),
            conditionalPanel(condition = "!(input.t1p4_4t== ' '||input.t1p4_4t=='')", uiOutput("t1p5_4t_to")),
            conditionalPanel(condition = "!(input.t1p5_4t== ' '||input.t1p5_4t=='')", uiOutput("t1p6_4t_to")),
            conditionalPanel(condition = "!(input.t1p6_4t== ' '||input.t1p6_4t=='')", uiOutput("t1p7_4t_to")),
            conditionalPanel(condition = "!(input.t1p7_4t== ' '||input.t1p7_4t=='')", uiOutput("t1p8_4t_to")),
            conditionalPanel(condition = "!(input.t1p8_4t== ' '||input.t1p8_4t=='')", uiOutput("t1p9_4t_to")),
            conditionalPanel(condition = "!(input.t1p9_4t== ' '||input.t1p9_4t=='')", uiOutput("t1p10_4t_to"))
          )
        ),
        # Other Assets Traded
        div(
          class = "row",
          div(
            class = "col-xs-5",
            style = "text-align: left; width: 40%; padding-left: 1.2em; padding-right: .6em;",
            h5("Other Assets:", style = "font-size: 1.1rem;"),
            uiOutput("t1a1_4t"),
            conditionalPanel(condition = "!(input.t1a1_4t== ' '||input.t1a1_4t=='')", uiOutput("t1a2_4t")),
            conditionalPanel(condition = "!(input.t1a2_4t== ' '||input.t1a2_4t=='')", uiOutput("t1a3_4t")),
            conditionalPanel(condition = "!(input.t1a3_4t== ' '||input.t1a3_4t=='')", uiOutput("t1a4_4t")),
            conditionalPanel(condition = "!(input.t1a4_4t== ' '||input.t1a4_4t=='')", uiOutput("t1a5_4t")),
            conditionalPanel(condition = "!(input.t1a5_4t== ' '||input.t1a5_4t=='')", uiOutput("t1a6_4t")),
            conditionalPanel(condition = "!(input.t1a6_4t== ' '||input.t1a6_4t=='')", uiOutput("t1a7_4t")),
            conditionalPanel(condition = "!(input.t1a7_4t== ' '||input.t1a7_4t=='')", uiOutput("t1a8_4t")),
            conditionalPanel(condition = "!(input.t1a8_4t== ' '||input.t1a8_4t=='')", uiOutput("t1a9_4t")),
            conditionalPanel(condition = "!(input.t1a9_4t== ' '||input.t1a9_4t=='')", uiOutput("t1a10_4t"))
          ),
          div(
            class = "col-xs-3",
            style = "text-align: left; width: 25%; padding-left: .6em; padding-right: .6em;",
            h5("To:", style = "font-size: 1.1rem;"),
            uiOutput("t1a1_4t_to"),
            conditionalPanel(condition = "!(input.t1a1_4t== ' '||input.t1a1_4t=='')", uiOutput("t1a2_4t_to")),
            conditionalPanel(condition = "!(input.t1a2_4t== ' '||input.t1a2_4t=='')", uiOutput("t1a3_4t_to")),
            conditionalPanel(condition = "!(input.t1a3_4t== ' '||input.t1a3_4t=='')", uiOutput("t1a4_4t_to")),
            conditionalPanel(condition = "!(input.t1a4_4t== ' '||input.t1a4_4t=='')", uiOutput("t1a5_4t_to")),
            conditionalPanel(condition = "!(input.t1a5_4t== ' '||input.t1a5_4t=='')", uiOutput("t1a6_4t_to")),
            conditionalPanel(condition = "!(input.t1a6_4t== ' '||input.t1a6_4t=='')", uiOutput("t1a7_4t_to")),
            conditionalPanel(condition = "!(input.t1a7_4t== ' '||input.t1a7_4t=='')", uiOutput("t1a8_4t_to")),
            conditionalPanel(condition = "!(input.t1a8_4t== ' '||input.t1a8_4t=='')", uiOutput("t1a9_4t_to")),
            conditionalPanel(condition = "!(input.t1a9_4t== ' '||input.t1a9_4t=='')", uiOutput("t1a10_4t_to"))
          ),
          div(
            class = "col-xs-2",
            style = "text-align: left; width: 35%; padding-left: .6em; padding-right: 1.2em;",
            h5("Add Protection?", style = "font-size: 1.1rem; width: 110%;"),
            uiOutput("t1a1_4t_pro"),
            conditionalPanel(condition = "!(input.t1a1_4t== ' '||input.t1a1_4t=='')", uiOutput("t1a2_4t_pro")),
            conditionalPanel(condition = "!(input.t1a2_4t== ' '||input.t1a2_4t=='')", uiOutput("t1a3_4t_pro")),
            conditionalPanel(condition = "!(input.t1a3_4t== ' '||input.t1a3_4t=='')", uiOutput("t1a4_4t_pro")),
            conditionalPanel(condition = "!(input.t1a4_4t== ' '||input.t1a4_4t=='')", uiOutput("t1a5_4t_pro")),
            conditionalPanel(condition = "!(input.t1a5_4t== ' '||input.t1a5_4t=='')", uiOutput("t1a6_4t_pro")),
            conditionalPanel(condition = "!(input.t1a6_4t== ' '||input.t1a6_4t=='')", uiOutput("t1a7_4t_pro")),
            conditionalPanel(condition = "!(input.t1a7_4t== ' '||input.t1a7_4t=='')", uiOutput("t1a8_4t_pro")),
            conditionalPanel(condition = "!(input.t1a8_4t== ' '||input.t1a8_4t=='')", uiOutput("t1a9_4t_pro")),
            conditionalPanel(condition = "!(input.t1a9_4t== ' '||input.t1a9_4t=='')", uiOutput("t1a10_4t_pro"))
          )
        )
      ),
      # Team 2
      div(
        class = "col-md-6",
        style = "text-align: center; border-right: 2px solid black; border-top: 2px solid black; padding-left: 1.2em; padding-right: 1.2em;",
        h5(""),
        htmlOutput("team2_logo4"),
        h5(""),
        # Players Traded
        div(
          class = "row",
          div(
            class = "col-xs-8",
            style = "text-align: left; width: 60%; padding-left: 1.2em; padding-right: .6em;",
            h5("Players Traded:"),
            uiOutput("t2p1_4t"),
            conditionalPanel(condition = "!(input.t2p1_4t== ' '||input.t2p1_4t=='')", uiOutput("t2p2_4t")),
            conditionalPanel(condition = "!(input.t2p2_4t== ' '||input.t2p2_4t=='')", uiOutput("t2p3_4t")),
            conditionalPanel(condition = "!(input.t2p3_4t== ' '||input.t2p3_4t=='')", uiOutput("t2p4_4t")),
            conditionalPanel(condition = "!(input.t2p4_4t== ' '||input.t2p4_4t=='')", uiOutput("t2p5_4t")),
            conditionalPanel(condition = "!(input.t2p5_4t== ' '||input.t2p5_4t=='')", uiOutput("t2p6_4t")),
            conditionalPanel(condition = "!(input.t2p6_4t== ' '||input.t2p6_4t=='')", uiOutput("t2p7_4t")),
            conditionalPanel(condition = "!(input.t2p7_4t== ' '||input.t2p7_4t=='')", uiOutput("t2p8_4t")),
            conditionalPanel(condition = "!(input.t2p8_4t== ' '||input.t2p8_4t=='')", uiOutput("t2p9_4t")),
            conditionalPanel(condition = "!(input.t2p9_4t== ' '||input.t2p9_4t=='')", uiOutput("t2p10_4t"))
          ),
          div(
            class = "col-xs-4",
            style = "text-align: left; width: 38%; padding-left: .6em; padding-right: 1.2em;",
            h5("To:"),
            uiOutput("t2p1_4t_to"),
            conditionalPanel(condition = "!(input.t2p1_4t== ' '||input.t2p1_4t=='')", uiOutput("t2p2_4t_to")),
            conditionalPanel(condition = "!(input.t2p2_4t== ' '||input.t2p2_4t=='')", uiOutput("t2p3_4t_to")),
            conditionalPanel(condition = "!(input.t2p3_4t== ' '||input.t2p3_4t=='')", uiOutput("t2p4_4t_to")),
            conditionalPanel(condition = "!(input.t2p4_4t== ' '||input.t2p4_4t=='')", uiOutput("t2p5_4t_to")),
            conditionalPanel(condition = "!(input.t2p5_4t== ' '||input.t2p5_4t=='')", uiOutput("t2p6_4t_to")),
            conditionalPanel(condition = "!(input.t2p6_4t== ' '||input.t2p6_4t=='')", uiOutput("t2p7_4t_to")),
            conditionalPanel(condition = "!(input.t2p7_4t== ' '||input.t2p7_4t=='')", uiOutput("t2p8_4t_to")),
            conditionalPanel(condition = "!(input.t2p8_4t== ' '||input.t2p8_4t=='')", uiOutput("t2p9_4t_to")),
            conditionalPanel(condition = "!(input.t2p9_4t== ' '||input.t2p9_4t=='')", uiOutput("t2p10_4t_to"))
          )
        ),
        # Other Assets Traded
        div(
          class = "row",
          div(
            class = "col-xs-5",
            style = "text-align: left; width: 40%; padding-left: 1.2em; padding-right: .6em;",
            h5("Other Assets:", style = "font-size: 1.1rem;"),
            uiOutput("t2a1_4t"),
            conditionalPanel(condition = "!(input.t2a1_4t== ' '||input.t2a1_4t=='')", uiOutput("t2a2_4t")),
            conditionalPanel(condition = "!(input.t2a2_4t== ' '||input.t2a2_4t=='')", uiOutput("t2a3_4t")),
            conditionalPanel(condition = "!(input.t2a3_4t== ' '||input.t2a3_4t=='')", uiOutput("t2a4_4t")),
            conditionalPanel(condition = "!(input.t2a4_4t== ' '||input.t2a4_4t=='')", uiOutput("t2a5_4t")),
            conditionalPanel(condition = "!(input.t2a5_4t== ' '||input.t2a5_4t=='')", uiOutput("t2a6_4t")),
            conditionalPanel(condition = "!(input.t2a6_4t== ' '||input.t2a6_4t=='')", uiOutput("t2a7_4t")),
            conditionalPanel(condition = "!(input.t2a7_4t== ' '||input.t2a7_4t=='')", uiOutput("t2a8_4t")),
            conditionalPanel(condition = "!(input.t2a8_4t== ' '||input.t2a8_4t=='')", uiOutput("t2a9_4t")),
            conditionalPanel(condition = "!(input.t2a9_4t== ' '||input.t2a9_4t=='')", uiOutput("t2a10_4t"))
          ),
          div(
            class = "col-xs-3",
            style = "text-align: left; width: 25%; padding-left: .6em; padding-right: .6em;",
            h5("To:", style = "font-size: 1.1rem;"),
            uiOutput("t2a1_4t_to"),
            conditionalPanel(condition = "!(input.t2a1_4t== ' '||input.t2a1_4t=='')", uiOutput("t2a2_4t_to")),
            conditionalPanel(condition = "!(input.t2a2_4t== ' '||input.t2a2_4t=='')", uiOutput("t2a3_4t_to")),
            conditionalPanel(condition = "!(input.t2a3_4t== ' '||input.t2a3_4t=='')", uiOutput("t2a4_4t_to")),
            conditionalPanel(condition = "!(input.t2a4_4t== ' '||input.t2a4_4t=='')", uiOutput("t2a5_4t_to")),
            conditionalPanel(condition = "!(input.t2a5_4t== ' '||input.t2a5_4t=='')", uiOutput("t2a6_4t_to")),
            conditionalPanel(condition = "!(input.t2a6_4t== ' '||input.t2a6_4t=='')", uiOutput("t2a7_4t_to")),
            conditionalPanel(condition = "!(input.t2a7_4t== ' '||input.t2a7_4t=='')", uiOutput("t2a8_4t_to")),
            conditionalPanel(condition = "!(input.t2a8_4t== ' '||input.t2a8_4t=='')", uiOutput("t2a9_4t_to")),
            conditionalPanel(condition = "!(input.t2a9_4t== ' '||input.t2a9_4t=='')", uiOutput("t2a10_4t_to"))
          ),
          div(
            class = "col-xs-2",
            style = "text-align: left; width: 35%; padding-left: .6em; padding-right: 1.2em;",
            h5("Add Protection?", style = "font-size: 1.1rem; width: 110%;"),
            uiOutput("t2a1_4t_pro"),
            conditionalPanel(condition = "!(input.t2a1_4t== ' '||input.t2a1_4t=='')", uiOutput("t2a2_4t_pro")),
            conditionalPanel(condition = "!(input.t2a2_4t== ' '||input.t2a2_4t=='')", uiOutput("t2a3_4t_pro")),
            conditionalPanel(condition = "!(input.t2a3_4t== ' '||input.t2a3_4t=='')", uiOutput("t2a4_4t_pro")),
            conditionalPanel(condition = "!(input.t2a4_4t== ' '||input.t2a4_4t=='')", uiOutput("t2a5_4t_pro")),
            conditionalPanel(condition = "!(input.t2a5_4t== ' '||input.t2a5_4t=='')", uiOutput("t2a6_4t_pro")),
            conditionalPanel(condition = "!(input.t2a6_4t== ' '||input.t2a6_4t=='')", uiOutput("t2a7_4t_pro")),
            conditionalPanel(condition = "!(input.t2a7_4t== ' '||input.t2a7_4t=='')", uiOutput("t2a8_4t_pro")),
            conditionalPanel(condition = "!(input.t2a8_4t== ' '||input.t2a8_4t=='')", uiOutput("t2a9_4t_pro")),
            conditionalPanel(condition = "!(input.t2a9_4t== ' '||input.t2a9_4t=='')", uiOutput("t2a10_4t_pro"))
          )
        )
      ),
      # Team 3
      div(
        class = "col-md-6",
        style = "text-align: center; border-right: 2px solid black; border-top: 2px solid black; padding-left: 1.2em; padding-right: 1.2em;",
        h5(""),
        htmlOutput("team3_logo4"),
        h5(""),
        # Players Traded
        div(
          class = "row",
          div(
            class = "col-xs-8",
            style = "text-align: left; width: 60%; padding-left: 1.2em; padding-right: .6em;",
            h5("Players Traded:"),
            uiOutput("t3p1_4t"),
            conditionalPanel(condition = "!(input.t3p1_4t== ' '||input.t3p1_4t=='')", uiOutput("t3p2_4t")),
            conditionalPanel(condition = "!(input.t3p2_4t== ' '||input.t3p2_4t=='')", uiOutput("t3p3_4t")),
            conditionalPanel(condition = "!(input.t3p3_4t== ' '||input.t3p3_4t=='')", uiOutput("t3p4_4t")),
            conditionalPanel(condition = "!(input.t3p4_4t== ' '||input.t3p4_4t=='')", uiOutput("t3p5_4t")),
            conditionalPanel(condition = "!(input.t3p5_4t== ' '||input.t3p5_4t=='')", uiOutput("t3p6_4t")),
            conditionalPanel(condition = "!(input.t3p6_4t== ' '||input.t3p6_4t=='')", uiOutput("t3p7_4t")),
            conditionalPanel(condition = "!(input.t3p7_4t== ' '||input.t3p7_4t=='')", uiOutput("t3p8_4t")),
            conditionalPanel(condition = "!(input.t3p8_4t== ' '||input.t3p8_4t=='')", uiOutput("t3p9_4t")),
            conditionalPanel(condition = "!(input.t3p9_4t== ' '||input.t3p9_4t=='')", uiOutput("t3p10_4t"))
          ),
          div(
            class = "col-xs-4",
            style = "text-align: left; width: 38%; padding-left: .6em; padding-right: 1.2em;",
            h5("To:"),
            uiOutput("t3p1_4t_to"),
            conditionalPanel(condition = "!(input.t3p1_4t== ' '||input.t3p1_4t=='')", uiOutput("t3p2_4t_to")),
            conditionalPanel(condition = "!(input.t3p2_4t== ' '||input.t3p2_4t=='')", uiOutput("t3p3_4t_to")),
            conditionalPanel(condition = "!(input.t3p3_4t== ' '||input.t3p3_4t=='')", uiOutput("t3p4_4t_to")),
            conditionalPanel(condition = "!(input.t3p4_4t== ' '||input.t3p4_4t=='')", uiOutput("t3p5_4t_to")),
            conditionalPanel(condition = "!(input.t3p5_4t== ' '||input.t3p5_4t=='')", uiOutput("t3p6_4t_to")),
            conditionalPanel(condition = "!(input.t3p6_4t== ' '||input.t3p6_4t=='')", uiOutput("t3p7_4t_to")),
            conditionalPanel(condition = "!(input.t3p7_4t== ' '||input.t3p7_4t=='')", uiOutput("t3p8_4t_to")),
            conditionalPanel(condition = "!(input.t3p8_4t== ' '||input.t3p8_4t=='')", uiOutput("t3p9_4t_to")),
            conditionalPanel(condition = "!(input.t3p9_4t== ' '||input.t3p9_4t=='')", uiOutput("t3p10_4t_to"))
          )
        ),
        # Other Assets Traded
        div(
          class = "row",
          div(
            class = "col-xs-5",
            style = "text-align: left; width: 40%; padding-left: 1.2em; padding-right: .6em;",
            h5("Other Assets:", style = "font-size: 1.1rem;"),
            uiOutput("t3a1_4t"),
            conditionalPanel(condition = "!(input.t3a1_4t== ' '||input.t3a1_4t=='')", uiOutput("t3a2_4t")),
            conditionalPanel(condition = "!(input.t3a2_4t== ' '||input.t3a2_4t=='')", uiOutput("t3a3_4t")),
            conditionalPanel(condition = "!(input.t3a3_4t== ' '||input.t3a3_4t=='')", uiOutput("t3a4_4t")),
            conditionalPanel(condition = "!(input.t3a4_4t== ' '||input.t3a4_4t=='')", uiOutput("t3a5_4t")),
            conditionalPanel(condition = "!(input.t3a5_4t== ' '||input.t3a5_4t=='')", uiOutput("t3a6_4t")),
            conditionalPanel(condition = "!(input.t3a6_4t== ' '||input.t3a6_4t=='')", uiOutput("t3a7_4t")),
            conditionalPanel(condition = "!(input.t3a7_4t== ' '||input.t3a7_4t=='')", uiOutput("t3a8_4t")),
            conditionalPanel(condition = "!(input.t3a8_4t== ' '||input.t3a8_4t=='')", uiOutput("t3a9_4t")),
            conditionalPanel(condition = "!(input.t3a9_4t== ' '||input.t3a9_4t=='')", uiOutput("t3a10_4t"))
          ),
          div(
            class = "col-xs-3",
            style = "text-align: left; width: 25%; padding-left: .6em; padding-right: .6em;",
            h5("To:", style = "font-size: 1.1rem;"),
            uiOutput("t3a1_4t_to"),
            conditionalPanel(condition = "!(input.t3a1_4t== ' '||input.t3a1_4t=='')", uiOutput("t3a2_4t_to")),
            conditionalPanel(condition = "!(input.t3a2_4t== ' '||input.t3a2_4t=='')", uiOutput("t3a3_4t_to")),
            conditionalPanel(condition = "!(input.t3a3_4t== ' '||input.t3a3_4t=='')", uiOutput("t3a4_4t_to")),
            conditionalPanel(condition = "!(input.t3a4_4t== ' '||input.t3a4_4t=='')", uiOutput("t3a5_4t_to")),
            conditionalPanel(condition = "!(input.t3a5_4t== ' '||input.t3a5_4t=='')", uiOutput("t3a6_4t_to")),
            conditionalPanel(condition = "!(input.t3a6_4t== ' '||input.t3a6_4t=='')", uiOutput("t3a7_4t_to")),
            conditionalPanel(condition = "!(input.t3a7_4t== ' '||input.t3a7_4t=='')", uiOutput("t3a8_4t_to")),
            conditionalPanel(condition = "!(input.t3a8_4t== ' '||input.t3a8_4t=='')", uiOutput("t3a9_4t_to")),
            conditionalPanel(condition = "!(input.t3a9_4t== ' '||input.t3a9_4t=='')", uiOutput("t3a10_4t_to"))
          ),
          div(
            class = "col-xs-2",
            style = "text-align: left; width: 35%; padding-left: .6em; padding-right: 1.2em;",
            h5("Add Protection?", style = "font-size: 1.1rem; width: 110%;"),
            uiOutput("t3a1_4t_pro"),
            conditionalPanel(condition = "!(input.t3a1_4t== ' '||input.t3a1_4t=='')", uiOutput("t3a2_4t_pro")),
            conditionalPanel(condition = "!(input.t3a2_4t== ' '||input.t3a2_4t=='')", uiOutput("t3a3_4t_pro")),
            conditionalPanel(condition = "!(input.t3a3_4t== ' '||input.t3a3_4t=='')", uiOutput("t3a4_4t_pro")),
            conditionalPanel(condition = "!(input.t3a4_4t== ' '||input.t3a4_4t=='')", uiOutput("t3a5_4t_pro")),
            conditionalPanel(condition = "!(input.t3a5_4t== ' '||input.t3a5_4t=='')", uiOutput("t3a6_4t_pro")),
            conditionalPanel(condition = "!(input.t3a6_4t== ' '||input.t3a6_4t=='')", uiOutput("t3a7_4t_pro")),
            conditionalPanel(condition = "!(input.t3a7_4t== ' '||input.t3a7_4t=='')", uiOutput("t3a8_4t_pro")),
            conditionalPanel(condition = "!(input.t3a8_4t== ' '||input.t3a8_4t=='')", uiOutput("t3a9_4t_pro")),
            conditionalPanel(condition = "!(input.t3a9_4t== ' '||input.t3a9_4t=='')", uiOutput("t3a10_4t_pro"))
          )
        )
      ),
      # Team 4
      div(
        class = "col-md-6",
        style = "text-align: center; border-right: 2px solid black; border-top: 2px solid black; padding-left: 1.2em; padding-right: 1.2em;",
        h5(""),
        htmlOutput("team4_logo"),
        h5(""),
        # Players Traded
        div(
          class = "row",
          div(
            class = "col-xs-8",
            style = "text-align: left; width: 60%; padding-left: 1.2em; padding-right: .6em;",
            h5("Players Traded:"),
            uiOutput("t4p1_4t"),
            conditionalPanel(condition = "!(input.t4p1_4t== ' '||input.t4p1_4t=='')", uiOutput("t4p2_4t")),
            conditionalPanel(condition = "!(input.t4p2_4t== ' '||input.t4p2_4t=='')", uiOutput("t4p3_4t")),
            conditionalPanel(condition = "!(input.t4p3_4t== ' '||input.t4p3_4t=='')", uiOutput("t4p4_4t")),
            conditionalPanel(condition = "!(input.t4p4_4t== ' '||input.t4p4_4t=='')", uiOutput("t4p5_4t")),
            conditionalPanel(condition = "!(input.t4p5_4t== ' '||input.t4p5_4t=='')", uiOutput("t4p6_4t")),
            conditionalPanel(condition = "!(input.t4p6_4t== ' '||input.t4p6_4t=='')", uiOutput("t4p7_4t")),
            conditionalPanel(condition = "!(input.t4p7_4t== ' '||input.t4p7_4t=='')", uiOutput("t4p8_4t")),
            conditionalPanel(condition = "!(input.t4p8_4t== ' '||input.t4p8_4t=='')", uiOutput("t4p9_4t")),
            conditionalPanel(condition = "!(input.t4p9_4t== ' '||input.t4p9_4t=='')", uiOutput("t4p10_4t"))
          ),
          div(
            class = "col-xs-4",
            style = "text-align: left; width: 38%; padding-left: .6em; padding-right: 1.2em;",
            h5("To:"),
            uiOutput("t4p1_4t_to"),
            conditionalPanel(condition = "!(input.t4p1_4t== ' '||input.t4p1_4t=='')", uiOutput("t4p2_4t_to")),
            conditionalPanel(condition = "!(input.t4p2_4t== ' '||input.t4p2_4t=='')", uiOutput("t4p3_4t_to")),
            conditionalPanel(condition = "!(input.t4p3_4t== ' '||input.t4p3_4t=='')", uiOutput("t4p4_4t_to")),
            conditionalPanel(condition = "!(input.t4p4_4t== ' '||input.t4p4_4t=='')", uiOutput("t4p5_4t_to")),
            conditionalPanel(condition = "!(input.t4p5_4t== ' '||input.t4p5_4t=='')", uiOutput("t4p6_4t_to")),
            conditionalPanel(condition = "!(input.t4p6_4t== ' '||input.t4p6_4t=='')", uiOutput("t4p7_4t_to")),
            conditionalPanel(condition = "!(input.t4p7_4t== ' '||input.t4p7_4t=='')", uiOutput("t4p8_4t_to")),
            conditionalPanel(condition = "!(input.t4p8_4t== ' '||input.t4p8_4t=='')", uiOutput("t4p9_4t_to")),
            conditionalPanel(condition = "!(input.t4p9_4t== ' '||input.t4p9_4t=='')", uiOutput("t4p10_4t_to"))
          )
        ),
        # Other Assets Traded
        div(
          class = "row",
          div(
            class = "col-xs-5",
            style = "text-align: left; width: 40%; padding-left: 1.2em; padding-right: .6em;",
            h5("Other Assets:", style = "font-size: 1.1rem;"),
            uiOutput("t4a1_4t"),
            conditionalPanel(condition = "!(input.t4a1_4t== ' '||input.t4a1_4t=='')", uiOutput("t4a2_4t")),
            conditionalPanel(condition = "!(input.t4a2_4t== ' '||input.t4a2_4t=='')", uiOutput("t4a3_4t")),
            conditionalPanel(condition = "!(input.t4a3_4t== ' '||input.t4a3_4t=='')", uiOutput("t4a4_4t")),
            conditionalPanel(condition = "!(input.t4a4_4t== ' '||input.t4a4_4t=='')", uiOutput("t4a5_4t")),
            conditionalPanel(condition = "!(input.t4a5_4t== ' '||input.t4a5_4t=='')", uiOutput("t4a6_4t")),
            conditionalPanel(condition = "!(input.t4a6_4t== ' '||input.t4a6_4t=='')", uiOutput("t4a7_4t")),
            conditionalPanel(condition = "!(input.t4a7_4t== ' '||input.t4a7_4t=='')", uiOutput("t4a8_4t")),
            conditionalPanel(condition = "!(input.t4a8_4t== ' '||input.t4a8_4t=='')", uiOutput("t4a9_4t")),
            conditionalPanel(condition = "!(input.t4a9_4t== ' '||input.t4a9_4t=='')", uiOutput("t4a10_4t"))
          ),
          div(
            class = "col-xs-3",
            style = "text-align: left; width: 25%; padding-left: .6em; padding-right: .6em;",
            h5("To:", style = "font-size: 1.1rem;"),
            uiOutput("t4a1_4t_to"),
            conditionalPanel(condition = "!(input.t4a1_4t== ' '||input.t4a1_4t=='')", uiOutput("t4a2_4t_to")),
            conditionalPanel(condition = "!(input.t4a2_4t== ' '||input.t4a2_4t=='')", uiOutput("t4a3_4t_to")),
            conditionalPanel(condition = "!(input.t4a3_4t== ' '||input.t4a3_4t=='')", uiOutput("t4a4_4t_to")),
            conditionalPanel(condition = "!(input.t4a4_4t== ' '||input.t4a4_4t=='')", uiOutput("t4a5_4t_to")),
            conditionalPanel(condition = "!(input.t4a5_4t== ' '||input.t4a5_4t=='')", uiOutput("t4a6_4t_to")),
            conditionalPanel(condition = "!(input.t4a6_4t== ' '||input.t4a6_4t=='')", uiOutput("t4a7_4t_to")),
            conditionalPanel(condition = "!(input.t4a7_4t== ' '||input.t4a7_4t=='')", uiOutput("t4a8_4t_to")),
            conditionalPanel(condition = "!(input.t4a8_4t== ' '||input.t4a8_4t=='')", uiOutput("t4a9_4t_to")),
            conditionalPanel(condition = "!(input.t4a9_4t== ' '||input.t4a9_4t=='')", uiOutput("t4a10_4t_to"))
          ),
          div(
            class = "col-xs-2",
            style = "text-align: left; width: 35%; padding-left: .6em; padding-right: 1.2em;",
            h5("Add Protection?", style = "font-size: 1.1rem; width: 110%;"),
            uiOutput("t4a1_4t_pro"),
            conditionalPanel(condition = "!(input.t4a1_4t== ' '||input.t4a1_4t=='')", uiOutput("t4a2_4t_pro")),
            conditionalPanel(condition = "!(input.t4a2_4t== ' '||input.t4a2_4t=='')", uiOutput("t4a3_4t_pro")),
            conditionalPanel(condition = "!(input.t4a3_4t== ' '||input.t4a3_4t=='')", uiOutput("t4a4_4t_pro")),
            conditionalPanel(condition = "!(input.t4a4_4t== ' '||input.t4a4_4t=='')", uiOutput("t4a5_4t_pro")),
            conditionalPanel(condition = "!(input.t4a5_4t== ' '||input.t4a5_4t=='')", uiOutput("t4a6_4t_pro")),
            conditionalPanel(condition = "!(input.t4a6_4t== ' '||input.t4a6_4t=='')", uiOutput("t4a7_4t_pro")),
            conditionalPanel(condition = "!(input.t4a7_4t== ' '||input.t4a7_4t=='')", uiOutput("t4a8_4t_pro")),
            conditionalPanel(condition = "!(input.t4a8_4t== ' '||input.t4a8_4t=='')", uiOutput("t4a9_4t_pro")),
            conditionalPanel(condition = "!(input.t4a9_4t== ' '||input.t4a9_4t=='')", uiOutput("t4a10_4t_pro"))
          )
        )
      )
    )
  ),
  conditionalPanel(
    condition = "input.teamsInvolved.length >= 5",
    div(
      class = "col-xs-12",
      style = "text-align: center; border-top: 2px solid black; padding-left: 1.2em; padding-right: 1.2em;",
      h2(""),
      h2("For all trades that involve 5 or more teams, please see the event staff for help entering it into the system."),
      h2(""),
      h2("Thank You")
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
  
  output$team1_logo = output$team1_logo2 = output$team1_logo3 = output$team1_logo4 = output$team1_logo5 = output$team1_logo6 = renderText({
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
  
  output$team2_logo = output$team2_logo3 = output$team2_logo4 = output$team2_logo5 = output$team2_logo6 = renderText({
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
  
  output$team3_logo = output$team3_logo4 = output$team3_logo5 = output$team3_logo6 = renderText({
    paste0(
      '<img src ="', 
      logo3_url(), 
      '" style="width:75px;">'
    )
  })
  
  logo4_url <- reactive(
    hoopR_espn_nba_teams |>
      filter(
        display_name == input$teamsInvolved[[4]]
      ) |>
      pull(logo)
  )
  
  output$team4_logo = output$team4_logo5 = output$team4_logo6 = renderText({
    paste0(
      '<img src ="', 
      logo4_url(), 
      '" style="width:75px;">'
    )
  })
  
  ########## UI Outputs for Inputs
  
  ### Traded Player Inputs
  # Team 1
  output$t1p1_1t = renderUI({
    if(length(input$teamsInvolved) == 1){
      textInput(
        "t1p1_1t", 
        ""
      )
    }
  })
  output$t1p2_1t = renderUI({
    if(length(input$teamsInvolved) == 1){
      textInput(
        "t1p2_1t", 
        ""
      )
    }
  })
  output$t1p3_1t = renderUI({
    if(length(input$teamsInvolved) == 1){
      textInput(
        "t1p3_1t", 
        ""
      )
    }
  })
  output$t1p4_1t = renderUI({
    if(length(input$teamsInvolved) == 1){
      textInput(
        "t1p4_1t", 
        ""
      )
    }
  })
  output$t1p5_1t = renderUI({
    if(length(input$teamsInvolved) == 1){
      textInput(
        "t1p5_1t", 
        ""
      )
    }
  })
  output$t1p6_1t = renderUI({
    if(length(input$teamsInvolved) == 1){
      textInput(
        "t1p6_1t", 
        ""
      )
    }
  })
  output$t1p7_1t = renderUI({
    if(length(input$teamsInvolved) == 1){
      textInput(
        "t1p7_1t", 
        ""
      )
    }
  })
  output$t1p8_1t = renderUI({
    if(length(input$teamsInvolved) == 1){
      textInput(
        "t1p8_1t", 
        ""
      )
    }
  })
  output$t1p9_1t = renderUI({
    if(length(input$teamsInvolved) == 1){
      textInput(
        "t1p9_1t", 
        ""
      )
    }
  })
  output$t1p10_1t = renderUI({
    if(length(input$teamsInvolved) == 1){
      textInput(
        "t1p10_1t", 
        ""
      )
    }
  })
  
  output$t1p1_2t = renderUI({
    if(length(input$teamsInvolved) == 2){
      textInput(
        "t1p1_2t", 
        ""
      )
    }
  })
  output$t1p2_2t = renderUI({
    if(length(input$teamsInvolved) == 2){
      textInput(
        "t1p2_2t", 
        ""
      )
    }
  })
  output$t1p3_2t = renderUI({
    if(length(input$teamsInvolved) == 2){
      textInput(
        "t1p3_2t", 
        ""
      )
    }
  })
  output$t1p4_2t = renderUI({
    if(length(input$teamsInvolved) == 2){
      textInput(
        "t1p4_2t", 
        ""
      )
    }
  })
  output$t1p5_2t = renderUI({
    if(length(input$teamsInvolved) == 2){
      textInput(
        "t1p5_2t", 
        ""
      )
    }
  })
  output$t1p6_2t = renderUI({
    if(length(input$teamsInvolved) == 2){
      textInput(
        "t1p6_2t", 
        ""
      )
    }
  })
  output$t1p7_2t = renderUI({
    if(length(input$teamsInvolved) == 2){
      textInput(
        "t1p7_2t", 
        ""
      )
    }
  })
  output$t1p8_2t = renderUI({
    if(length(input$teamsInvolved) == 2){
      textInput(
        "t1p8_2t", 
        ""
      )
    }
  })
  output$t1p9_2t = renderUI({
    if(length(input$teamsInvolved) == 2){
      textInput(
        "t1p9_2t", 
        ""
      )
    }
  })
  output$t1p10_2t = renderUI({
    if(length(input$teamsInvolved) == 2){
      textInput(
        "t1p10_2t", 
        ""
      )
    }
  })
  
  output$t1p1_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t1p1_3t", 
        ""
      )
    }
  })
  output$t1p2_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t1p2_3t", 
        ""
      )
    }
  })
  output$t1p3_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t1p3_3t", 
        ""
      )
    }
  })
  output$t1p4_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t1p4_3t", 
        ""
      )
    }
  })
  output$t1p5_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t1p5_3t", 
        ""
      )
    }
  })
  output$t1p6_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t1p6_3t", 
        ""
      )
    }
  })
  output$t1p7_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t1p7_3t", 
        ""
      )
    }
  })
  output$t1p8_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t1p8_3t", 
        ""
      )
    }
  })
  output$t1p9_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t1p9_3t", 
        ""
      )
    }
  })
  output$t1p10_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t1p10_3t", 
        ""
      )
    }
  })
  
  output$t1p1_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t1p1_4t", 
        ""
      )
    }
  })
  output$t1p2_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t1p2_4t", 
        ""
      )
    }
  })
  output$t1p3_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t1p3_4t", 
        ""
      )
    }
  })
  output$t1p4_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t1p4_4t", 
        ""
      )
    }
  })
  output$t1p5_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t1p5_4t", 
        ""
      )
    }
  })
  output$t1p6_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t1p6_4t", 
        ""
      )
    }
  })
  output$t1p7_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t1p7_4t", 
        ""
      )
    }
  })
  output$t1p8_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t1p8_4t", 
        ""
      )
    }
  })
  output$t1p9_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t1p9_4t", 
        ""
      )
    }
  })
  output$t1p10_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t1p10_4t", 
        ""
      )
    }
  })
  
  # Team 2 
  output$t2p1_2t = renderUI({
    if(length(input$teamsInvolved) == 2){
      textInput(
        "t2p1_2t", 
        ""
      )
    }
  })
  output$t2p2_2t = renderUI({
    if(length(input$teamsInvolved) == 2){
      textInput(
        "t2p2_2t", 
        ""
      )
    }
  })
  output$t2p3_2t = renderUI({
    if(length(input$teamsInvolved) == 2){
      textInput(
        "t2p3_2t", 
        ""
      )
    }
  })
  output$t2p4_2t = renderUI({
    if(length(input$teamsInvolved) == 2){
      textInput(
        "t2p4_2t", 
        ""
      )
    }
  })
  output$t2p5_2t = renderUI({
    if(length(input$teamsInvolved) == 2){
      textInput(
        "t2p5_2t", 
        ""
      )
    }
  })
  output$t2p6_2t = renderUI({
    if(length(input$teamsInvolved) == 2){
      textInput(
        "t2p6_2t", 
        ""
      )
    }
  })
  output$t2p7_2t = renderUI({
    if(length(input$teamsInvolved) == 2){
      textInput(
        "t2p7_2t", 
        ""
      )
    }
  })
  output$t2p8_2t = renderUI({
    if(length(input$teamsInvolved) == 2){
      textInput(
        "t2p8_2t", 
        ""
      )
    }
  })
  output$t2p9_2t = renderUI({
    if(length(input$teamsInvolved) == 2){
      textInput(
        "t2p9_2t", 
        ""
      )
    }
  })
  output$t2p10_2t = renderUI({
    if(length(input$teamsInvolved) == 2){
      textInput(
        "t2p10_2t", 
        ""
      )
    }
  })
  
  output$t2p1_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t2p1_3t", 
        ""
      )
    }
  })
  output$t2p2_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t2p2_3t", 
        ""
      )
    }
  })
  output$t2p3_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t2p3_3t", 
        ""
      )
    }
  })
  output$t2p4_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t2p4_3t", 
        ""
      )
    }
  })
  output$t2p5_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t2p5_3t", 
        ""
      )
    }
  })
  output$t2p6_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t2p6_3t", 
        ""
      )
    }
  })
  output$t2p7_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t2p7_3t", 
        ""
      )
    }
  })
  output$t2p8_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t2p8_3t", 
        ""
      )
    }
  })
  output$t2p9_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t2p9_3t", 
        ""
      )
    }
  })
  output$t2p10_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t2p10_3t", 
        ""
      )
    }
  })
  
  output$t2p1_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t2p1_4t", 
        ""
      )
    }
  })
  output$t2p2_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t2p2_4t", 
        ""
      )
    }
  })
  output$t2p3_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t2p3_4t", 
        ""
      )
    }
  })
  output$t2p4_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t2p4_4t", 
        ""
      )
    }
  })
  output$t2p5_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t2p5_4t", 
        ""
      )
    }
  })
  output$t2p6_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t2p6_4t", 
        ""
      )
    }
  })
  output$t2p7_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t2p7_4t", 
        ""
      )
    }
  })
  output$t2p8_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t2p8_4t", 
        ""
      )
    }
  })
  output$t2p9_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t2p9_4t", 
        ""
      )
    }
  })
  output$t2p10_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t2p10_4t", 
        ""
      )
    }
  })
  
  # Team 3
  output$t3p1_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t3p1_3t", 
        ""
      )
    }
  })
  output$t3p2_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t3p2_3t", 
        ""
      )
    }
  })
  output$t3p3_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t3p3_3t", 
        ""
      )
    }
  })
  output$t3p4_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t3p4_3t", 
        ""
      )
    }
  })
  output$t3p5_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t3p5_3t", 
        ""
      )
    }
  })
  output$t3p6_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t3p6_3t", 
        ""
      )
    }
  })
  output$t3p7_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t3p7_3t", 
        ""
      )
    }
  })
  output$t3p8_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t3p8_3t", 
        ""
      )
    }
  })
  output$t3p9_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t3p9_3t", 
        ""
      )
    }
  })
  output$t3p10_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t3p10_3t", 
        ""
      )
    }
  })
  
  output$t3p1_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t3p1_4t", 
        ""
      )
    }
  })
  output$t3p2_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t3p2_4t", 
        ""
      )
    }
  })
  output$t3p3_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t3p3_4t", 
        ""
      )
    }
  })
  output$t3p4_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t3p4_4t", 
        ""
      )
    }
  })
  output$t3p5_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t3p5_4t", 
        ""
      )
    }
  })
  output$t3p6_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t3p6_4t", 
        ""
      )
    }
  })
  output$t3p7_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t3p7_4t", 
        ""
      )
    }
  })
  output$t3p8_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t3p8_4t", 
        ""
      )
    }
  })
  output$t3p9_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t3p9_4t", 
        ""
      )
    }
  })
  output$t3p10_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t3p10_4t", 
        ""
      )
    }
  })
  
  # Team 4
  output$t4p1_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t4p1_4t", 
        ""
      )
    }
  })
  output$t4p2_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t4p2_4t", 
        ""
      )
    }
  })
  output$t4p3_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t4p3_4t", 
        ""
      )
    }
  })
  output$t4p4_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t4p4_4t", 
        ""
      )
    }
  })
  output$t4p5_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t4p5_4t", 
        ""
      )
    }
  })
  output$t4p6_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t4p6_4t", 
        ""
      )
    }
  })
  output$t4p7_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t4p7_4t", 
        ""
      )
    }
  })
  output$t4p8_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t4p8_4t", 
        ""
      )
    }
  })
  output$t4p9_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t4p9_4t", 
        ""
      )
    }
  })
  output$t4p10_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t4p10_4t", 
        ""
      )
    }
  })
  
  ### Traded Asset Input
  # Team 1
  output$t1a1_2t = renderUI({
    if(length(input$teamsInvolved) == 2){
      textInput(
        "t1a1_2t", 
        ""
      ) 
    }
  })
  output$t1a2_2t = renderUI({
    if(length(input$teamsInvolved) == 2){
      textInput(
        "t1a2_2t", 
        ""
      ) 
    }
  })
  output$t1a3_2t = renderUI({
    if(length(input$teamsInvolved) == 2){
      textInput(
        "t1a3_2t", 
        ""
      ) 
    }
  })
  output$t1a4_2t = renderUI({
    if(length(input$teamsInvolved) == 2){
      textInput(
        "t1a4_2t", 
        ""
      ) 
    }
  })
  output$t1a5_2t = renderUI({
    if(length(input$teamsInvolved) == 2){
      textInput(
        "t1a5_2t", 
        ""
      ) 
    }
  })
  output$t1a6_2t = renderUI({
    if(length(input$teamsInvolved) == 2){
      textInput(
        "t1a6_2t", 
        ""
      ) 
    }
  })
  output$t1a7_2t = renderUI({
    if(length(input$teamsInvolved) == 2){
      textInput(
        "t1a7_2t", 
        ""
      ) 
    }
  })
  output$t1a8_2t = renderUI({
    if(length(input$teamsInvolved) == 2){
      textInput(
        "t1a8_2t", 
        ""
      ) 
    }
  })
  output$t1a9_2t = renderUI({
    if(length(input$teamsInvolved) == 2){
      textInput(
        "t1a9_2t", 
        ""
      ) 
    }
  })
  output$t1a10_2t = renderUI({
    if(length(input$teamsInvolved) == 2){
      textInput(
        "t1a10_2t", 
        ""
      ) 
    }
  })
  
  output$t1a1_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t1a1_3t", 
        ""
      ) 
    }
  })
  output$t1a2_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t1a2_3t", 
        ""
      ) 
    }
  })
  output$t1a3_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t1a3_3t", 
        ""
      ) 
    }
  })
  output$t1a4_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t1a4_3t", 
        ""
      ) 
    }
  })
  output$t1a5_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t1a5_3t", 
        ""
      ) 
    }
  })
  output$t1a6_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t1a6_3t", 
        ""
      ) 
    }
  })
  output$t1a7_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t1a7_3t", 
        ""
      ) 
    }
  })
  output$t1a8_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t1a8_3t", 
        ""
      ) 
    }
  })
  output$t1a9_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t1a9_3t", 
        ""
      ) 
    }
  })
  output$t1a10_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t1a10_3t", 
        ""
      ) 
    }
  })
  
  output$t1a1_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t1a1_4t", 
        ""
      ) 
    }
  })
  output$t1a2_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t1a2_4t", 
        ""
      ) 
    }
  })
  output$t1a3_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t1a3_4t", 
        ""
      ) 
    }
  })
  output$t1a4_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t1a4_4t", 
        ""
      ) 
    }
  })
  output$t1a5_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t1a5_4t", 
        ""
      ) 
    }
  })
  output$t1a6_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t1a6_4t", 
        ""
      ) 
    }
  })
  output$t1a7_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t1a7_4t", 
        ""
      ) 
    }
  })
  output$t1a8_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t1a8_4t", 
        ""
      ) 
    }
  })
  output$t1a9_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t1a9_4t", 
        ""
      ) 
    }
  })
  output$t1a10_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t1a10_4t", 
        ""
      ) 
    }
  })
  
  # Team 2
  output$t2a1_2t = renderUI({
    if(length(input$teamsInvolved) == 2){
      textInput(
        "t2a1_2t", 
        ""
      ) 
    }
  })
  output$t2a2_2t = renderUI({
    if(length(input$teamsInvolved) == 2){
      textInput(
        "t2a2_2t", 
        ""
      ) 
    }
  })
  output$t2a3_2t = renderUI({
    if(length(input$teamsInvolved) == 2){
      textInput(
        "t2a3_2t", 
        ""
      ) 
    }
  })
  output$t2a4_2t = renderUI({
    if(length(input$teamsInvolved) == 2){
      textInput(
        "t2a4_2t", 
        ""
      ) 
    }
  })
  output$t2a5_2t = renderUI({
    if(length(input$teamsInvolved) == 2){
      textInput(
        "t2a5_2t", 
        ""
      ) 
    }
  })
  output$t2a6_2t = renderUI({
    if(length(input$teamsInvolved) == 2){
      textInput(
        "t2a6_2t", 
        ""
      ) 
    }
  })
  output$t2a7_2t = renderUI({
    if(length(input$teamsInvolved) == 2){
      textInput(
        "t2a7_2t", 
        ""
      ) 
    }
  })
  output$t2a8_2t = renderUI({
    if(length(input$teamsInvolved) == 2){
      textInput(
        "t2a8_2t", 
        ""
      ) 
    }
  })
  output$t2a9_2t = renderUI({
    if(length(input$teamsInvolved) == 2){
      textInput(
        "t2a9_2t", 
        ""
      ) 
    }
  })
  output$t2a10_2t = renderUI({
    if(length(input$teamsInvolved) == 2){
      textInput(
        "t2a10_2t", 
        ""
      ) 
    }
  })
  
  output$t2a1_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t2a1_3t", 
        ""
      ) 
    }
  })
  output$t2a2_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t2a2_3t", 
        ""
      ) 
    }
  })
  output$t2a3_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t2a3_3t", 
        ""
      ) 
    }
  })
  output$t2a4_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t2a4_3t", 
        ""
      ) 
    }
  })
  output$t2a5_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t2a5_3t", 
        ""
      ) 
    }
  })
  output$t2a6_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t2a6_3t", 
        ""
      ) 
    }
  })
  output$t2a7_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t2a7_3t", 
        ""
      ) 
    }
  })
  output$t2a8_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t2a8_3t", 
        ""
      ) 
    }
  })
  output$t2a9_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t2a9_3t", 
        ""
      ) 
    }
  })
  output$t2a10_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t2a10_3t", 
        ""
      ) 
    }
  })
  
  output$t2a1_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t2a1_4t", 
        ""
      ) 
    }
  })
  output$t2a2_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t2a2_4t", 
        ""
      ) 
    }
  })
  output$t2a3_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t2a3_4t", 
        ""
      ) 
    }
  })
  output$t2a4_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t2a4_4t", 
        ""
      ) 
    }
  })
  output$t2a5_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t2a5_4t", 
        ""
      ) 
    }
  })
  output$t2a6_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t2a6_4t", 
        ""
      ) 
    }
  })
  output$t2a7_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t2a7_4t", 
        ""
      ) 
    }
  })
  output$t2a8_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t2a8_4t", 
        ""
      ) 
    }
  })
  output$t2a9_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t2a9_4t", 
        ""
      ) 
    }
  })
  output$t2a10_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t2a10_4t", 
        ""
      ) 
    }
  })
  
  # Team 3
  output$t3a1_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t3a1_3t", 
        ""
      ) 
    }
  })
  output$t3a2_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t3a2_3t", 
        ""
      ) 
    }
  })
  output$t3a3_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t3a3_3t", 
        ""
      ) 
    }
  })
  output$t3a4_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t3a4_3t", 
        ""
      ) 
    }
  })
  output$t3a5_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t3a5_3t", 
        ""
      ) 
    }
  })
  output$t3a6_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t3a6_3t", 
        ""
      ) 
    }
  })
  output$t3a7_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t3a7_3t", 
        ""
      ) 
    }
  })
  output$t3a8_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t3a8_3t", 
        ""
      ) 
    }
  })
  output$t3a9_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t3a9_3t", 
        ""
      ) 
    }
  })
  output$t3a10_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t3a10_3t", 
        ""
      ) 
    }
  })
  
  output$t3a1_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t3a1_4t", 
        ""
      ) 
    }
  })
  output$t3a2_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t3a2_4t", 
        ""
      ) 
    }
  })
  output$t3a3_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t3a3_4t", 
        ""
      ) 
    }
  })
  output$t3a4_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t3a4_4t", 
        ""
      ) 
    }
  })
  output$t3a5_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t3a5_4t", 
        ""
      ) 
    }
  })
  output$t3a6_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t3a6_4t", 
        ""
      ) 
    }
  })
  output$t3a7_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t3a7_4t", 
        ""
      ) 
    }
  })
  output$t3a8_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t3a8_4t", 
        ""
      ) 
    }
  })
  output$t3a9_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t3a9_4t", 
        ""
      ) 
    }
  })
  output$t3a10_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t3a10_4t", 
        ""
      ) 
    }
  })
  
  # Team 4
  output$t4a1_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t4a1_4t", 
        ""
      ) 
    }
  })
  output$t4a2_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t4a2_4t", 
        ""
      ) 
    }
  })
  output$t4a3_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t4a3_4t", 
        ""
      ) 
    }
  })
  output$t4a4_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t4a4_4t", 
        ""
      ) 
    }
  })
  output$t4a5_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t4a5_4t", 
        ""
      ) 
    }
  })
  output$t4a6_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t4a6_4t", 
        ""
      ) 
    }
  })
  output$t4a7_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t4a7_4t", 
        ""
      ) 
    }
  })
  output$t4a8_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t4a8_4t", 
        ""
      ) 
    }
  })
  output$t4a9_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t4a9_4t", 
        ""
      ) 
    }
  })
  output$t4a10_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t4a10_4t", 
        ""
      ) 
    }
  })
  
  ### Traded Player To Team Input
  # Team 1
  output$t1p1_1t_to = renderUI({
    if(length(input$teamsInvolved) == 1){
      pickerInput(
        "t1p1_1t_to", 
        "", 
        choices = c("Waive"),
        options = list(`dropdown-align-center` = TRUE),
      )
    }
  })
  output$t1p2_1t_to = renderUI({
    if(length(input$teamsInvolved) == 1){
      pickerInput(
        "t1p2_1t_to", 
        "", 
        choices = c("Waive"),
        options = list(`dropdown-align-center` = TRUE),
      )
    }
  })
  output$t1p3_1t_to = renderUI({
    if(length(input$teamsInvolved) == 1){
      pickerInput(
        "t1p3_1t_to", 
        "", 
        choices = c("Waive"),
        options = list(`dropdown-align-center` = TRUE),
      )
    }
  })
  output$t1p4_1t_to = renderUI({
    if(length(input$teamsInvolved) == 1){
      pickerInput(
        "t1p4_1t_to", 
        "", 
        choices = c("Waive"),
        options = list(`dropdown-align-center` = TRUE),
      )
    }
  })
  output$t1p5_1t_to = renderUI({
    if(length(input$teamsInvolved) == 1){
      pickerInput(
        "t1p5_1t_to", 
        "", 
        choices = c("Waive"),
        options = list(`dropdown-align-center` = TRUE),
      )
    }
  })
  output$t1p6_1t_to = renderUI({
    if(length(input$teamsInvolved) == 1){
      pickerInput(
        "t1p6_1t_to", 
        "", 
        choices = c("Waive"),
        options = list(`dropdown-align-center` = TRUE),
      )
    }
  })
  output$t1p7_1t_to = renderUI({
    if(length(input$teamsInvolved) == 1){
      pickerInput(
        "t1p7_1t_to", 
        "", 
        choices = c("Waive"),
        options = list(`dropdown-align-center` = TRUE),
      )
    }
  })
  output$t1p8_1t_to = renderUI({
    if(length(input$teamsInvolved) == 1){
      pickerInput(
        "t1p8_1t_to", 
        "", 
        choices = c("Waive"),
        options = list(`dropdown-align-center` = TRUE),
      )
    }
  })
  output$t1p9_1t_to = renderUI({
    if(length(input$teamsInvolved) == 1){
      pickerInput(
        "t1p9_1t_to", 
        "", 
        choices = c("Waive"),
        options = list(`dropdown-align-center` = TRUE),
      )
    }
  })
  output$t1p10_1t_to = renderUI({
    if(length(input$teamsInvolved) == 1){
      pickerInput(
        "t1p10_1t_to", 
        "", 
        choices = c("Waive"),
        options = list(`dropdown-align-center` = TRUE),
      )
    }
  })
  
  output$t1p1_2t_to = renderUI({
    if(length(input$teamsInvolved) == 2){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t1p1_2t_to", 
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
    }
  })
  output$t1p2_2t_to = renderUI({
    if(length(input$teamsInvolved) == 2){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t1p2_2t_to", 
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
    }
  })
  output$t1p3_2t_to = renderUI({
    if(length(input$teamsInvolved) == 2){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t1p3_2t_to", 
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
    }
  })
  output$t1p4_2t_to = renderUI({
    if(length(input$teamsInvolved) == 2){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t1p4_2t_to", 
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
    }
  })
  output$t1p5_2t_to = renderUI({
    if(length(input$teamsInvolved) == 2){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t1p5_2t_to", 
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
    }
  })
  output$t1p6_2t_to = renderUI({
    if(length(input$teamsInvolved) == 2){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t1p6_2t_to", 
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
    }
  })
  output$t1p7_2t_to = renderUI({
    if(length(input$teamsInvolved) == 2){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t1ap7_2t_to", 
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
    }
  })
  output$t1p8_2t_to = renderUI({
    if(length(input$teamsInvolved) == 2){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t1ap8_2t_to", 
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
    }
  })
  output$t1p9_2t_to = renderUI({
    if(length(input$teamsInvolved) == 2){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t1p9_2t_to", 
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
    }
  })
  output$t1p10_2t_to = renderUI({
    if(length(input$teamsInvolved) == 2){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t1p10_2t_to", 
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
    }
  })
  
  output$t1p1_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      pickerInput(
        "t1p1_3t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]) |> sort(),
        choicesOpt = list(
          content = c(
            "",
            hoopR_espn_nba_teams |>
              filter(
                display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]
              ) |>
              pull(dropdown_logo)
          )
        ),
        options = list(`dropdown-align-center` = TRUE,
                       placeholder = "Team"),
        selected = ""
      )
    }
  })
  output$t1p2_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      pickerInput(
        "t1p2_3t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]) |> sort(),
        choicesOpt = list(
          content = c(
            "",
            hoopR_espn_nba_teams |>
              filter(
                display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]
              ) |>
              pull(dropdown_logo)
          )
        ),
        options = list(`dropdown-align-center` = TRUE,
                       placeholder = "Team"),
        selected = ""
      )
    }
  })
  output$t1p3_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      pickerInput(
        "t1p3_3t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]) |> sort(),
        choicesOpt = list(
          content = c(
            "",
            hoopR_espn_nba_teams |>
              filter(
                display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]
              ) |>
              pull(dropdown_logo)
          )
        ),
        options = list(`dropdown-align-center` = TRUE,
                       placeholder = "Team"),
        selected = ""
      )
    }
  })
  output$t1p4_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      pickerInput(
        "t1p4_3t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]) |> sort(),
        choicesOpt = list(
          content = c(
            "",
            hoopR_espn_nba_teams |>
              filter(
                display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]
              ) |>
              pull(dropdown_logo)
          )
        ),
        options = list(`dropdown-align-center` = TRUE,
                       placeholder = "Team"),
        selected = ""
      )
    }
  })
  output$t1p5_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      pickerInput(
        "t1p5_3t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]) |> sort(),
        choicesOpt = list(
          content = c(
            "",
            hoopR_espn_nba_teams |>
              filter(
                display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]
              ) |>
              pull(dropdown_logo)
          )
        ),
        options = list(`dropdown-align-center` = TRUE,
                       placeholder = "Team"),
        selected = ""
      )
    }
  })
  output$t1p6_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      pickerInput(
        "t1p6_3t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]) |> sort(),
        choicesOpt = list(
          content = c(
            "",
            hoopR_espn_nba_teams |>
              filter(
                display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]
              ) |>
              pull(dropdown_logo)
          )
        ),
        options = list(`dropdown-align-center` = TRUE,
                       placeholder = "Team"),
        selected = ""
      )
    }
  })
  output$t1p7_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      pickerInput(
        "t1p7_3t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]) |> sort(),
        choicesOpt = list(
          content = c(
            "",
            hoopR_espn_nba_teams |>
              filter(
                display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]
              ) |>
              pull(dropdown_logo)
          )
        ),
        options = list(`dropdown-align-center` = TRUE,
                       placeholder = "Team"),
        selected = ""
      )
    }
  })
  output$t1p8_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      pickerInput(
        "t1p8_3t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]) |> sort(),
        choicesOpt = list(
          content = c(
            "",
            hoopR_espn_nba_teams |>
              filter(
                display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]
              ) |>
              pull(dropdown_logo)
          )
        ),
        options = list(`dropdown-align-center` = TRUE,
                       placeholder = "Team"),
        selected = ""
      )
    }
  })
  output$t1p9_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      pickerInput(
        "t1p9_3t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]) |> sort(),
        choicesOpt = list(
          content = c(
            "",
            hoopR_espn_nba_teams |>
              filter(
                display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]
              ) |>
              pull(dropdown_logo)
          )
        ),
        options = list(`dropdown-align-center` = TRUE,
                       placeholder = "Team"),
        selected = ""
      )
    }
  })
  output$t1p10_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      pickerInput(
        "t1p10_3t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]) |> sort(),
        choicesOpt = list(
          content = c(
            "",
            hoopR_espn_nba_teams |>
              filter(
                display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]
              ) |>
              pull(dropdown_logo)
          )
        ),
        options = list(`dropdown-align-center` = TRUE,
                       placeholder = "Team"),
        selected = ""
      )
    }
  })
  
  output$t1p1_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      pickerInput(
        "t1p1_4t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]) |> sort(),
        choicesOpt = list(
          content = c(
            "",
            hoopR_espn_nba_teams |>
              filter(
                display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]
              ) |>
              pull(dropdown_logo)
          )
        ),
        options = list(`dropdown-align-center` = TRUE,
                       placeholder = "Team"),
        selected = ""
      )
    }
  })
  output$t1p2_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      pickerInput(
        "t1p2_4t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]) |> sort(),
        choicesOpt = list(
          content = c(
            "",
            hoopR_espn_nba_teams |>
              filter(
                display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]
              ) |>
              pull(dropdown_logo)
          )
        ),
        options = list(`dropdown-align-center` = TRUE,
                       placeholder = "Team"),
        selected = ""
      )
    }
  })
  output$t1p3_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      pickerInput(
        "t1p3_4t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]) |> sort(),
        choicesOpt = list(
          content = c(
            "",
            hoopR_espn_nba_teams |>
              filter(
                display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]
              ) |>
              pull(dropdown_logo)
          )
        ),
        options = list(`dropdown-align-center` = TRUE,
                       placeholder = "Team"),
        selected = ""
      )
    }
  })
  output$t1p4_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      pickerInput(
        "t1p4_4t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]) |> sort(),
        choicesOpt = list(
          content = c(
            "",
            hoopR_espn_nba_teams |>
              filter(
                display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]
              ) |>
              pull(dropdown_logo)
          )
        ),
        options = list(`dropdown-align-center` = TRUE,
                       placeholder = "Team"),
        selected = ""
      )
    }
  })
  output$t1p5_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      pickerInput(
        "t1p5_4t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]) |> sort(),
        choicesOpt = list(
          content = c(
            "",
            hoopR_espn_nba_teams |>
              filter(
                display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]
              ) |>
              pull(dropdown_logo)
          )
        ),
        options = list(`dropdown-align-center` = TRUE,
                       placeholder = "Team"),
        selected = ""
      )
    }
  })
  output$t1p6_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      pickerInput(
        "t1p6_4t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]) |> sort(),
        choicesOpt = list(
          content = c(
            "",
            hoopR_espn_nba_teams |>
              filter(
                display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]
              ) |>
              pull(dropdown_logo)
          )
        ),
        options = list(`dropdown-align-center` = TRUE,
                       placeholder = "Team"),
        selected = ""
      )
    }
  })
  output$t1p7_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      pickerInput(
        "t1p7_4t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]) |> sort(),
        choicesOpt = list(
          content = c(
            "",
            hoopR_espn_nba_teams |>
              filter(
                display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]
              ) |>
              pull(dropdown_logo)
          )
        ),
        options = list(`dropdown-align-center` = TRUE,
                       placeholder = "Team"),
        selected = ""
      )
    }
  })
  output$t1p8_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      pickerInput(
        "t1p8_4t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]) |> sort(),
        choicesOpt = list(
          content = c(
            "",
            hoopR_espn_nba_teams |>
              filter(
                display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]
              ) |>
              pull(dropdown_logo)
          )
        ),
        options = list(`dropdown-align-center` = TRUE,
                       placeholder = "Team"),
        selected = ""
      )
    }
  })
  output$t1p9_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      pickerInput(
        "t1p9_4t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]) |> sort(),
        choicesOpt = list(
          content = c(
            "",
            hoopR_espn_nba_teams |>
              filter(
                display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]
              ) |>
              pull(dropdown_logo)
          )
        ),
        options = list(`dropdown-align-center` = TRUE,
                       placeholder = "Team"),
        selected = ""
      )
    }
  })
  output$t1p10_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      pickerInput(
        "t1p10_4t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]) |> sort(),
        choicesOpt = list(
          content = c(
            "",
            hoopR_espn_nba_teams |>
              filter(
                display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]
              ) |>
              pull(dropdown_logo)
          )
        ),
        options = list(`dropdown-align-center` = TRUE,
                       placeholder = "Team"),
        selected = ""
      )
    }
  })
  
  # Team 2
  output$t2p1_2t_to = renderUI({
    if(length(input$teamsInvolved) == 2){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t2p1_2t_to", 
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
    }
  })
  output$t2p2_2t_to = renderUI({
    if(length(input$teamsInvolved) == 2){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t2p2_2t_to", 
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
    }
  })
  output$t2p3_2t_to = renderUI({
    if(length(input$teamsInvolved) == 2){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t2p3_2t_to", 
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
    }
  })
  output$t2p4_2t_to = renderUI({
    if(length(input$teamsInvolved) == 2){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t2p4_2t_to", 
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
    }
  })
  output$t2p5_2t_to = renderUI({
    if(length(input$teamsInvolved) == 2){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t2p5_2t_to", 
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
    }
  })
  output$t2p6_2t_to = renderUI({
    if(length(input$teamsInvolved) == 2){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t2p6_2t_to", 
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
    }
  })
  output$t2p7_2t_to = renderUI({
    if(length(input$teamsInvolved) == 2){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t2ap7_2t_to", 
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
    }
  })
  output$t2p8_2t_to = renderUI({
    if(length(input$teamsInvolved) == 2){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t2ap8_2t_to", 
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
    }
  })
  output$t2p9_2t_to = renderUI({
    if(length(input$teamsInvolved) == 2){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t2p9_2t_to", 
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
    }
  })
  output$t2p10_2t_to = renderUI({
    if(length(input$teamsInvolved) == 2){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t2p10_2t_to", 
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
    }
  })
  
  output$t2p1_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      pickerInput(
        "t2p1_3t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]) |> sort(),
        choicesOpt = list(
          content = c(
            "",
            hoopR_espn_nba_teams |>
              filter(
                display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]
              ) |>
              pull(dropdown_logo)
          )
        ),
        options = list(`dropdown-align-center` = TRUE,
                       placeholder = "Team"),
        selected = ""
      )
    }
  })
  output$t2p2_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      pickerInput(
        "t2p2_3t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]) |> sort(),
        choicesOpt = list(
          content = c(
            "",
            hoopR_espn_nba_teams |>
              filter(
                display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]
              ) |>
              pull(dropdown_logo)
          )
        ),
        options = list(`dropdown-align-center` = TRUE,
                       placeholder = "Team"),
        selected = ""
      )
    }
  })
  output$t2p3_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      pickerInput(
        "t2p3_3t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]) |> sort(),
        choicesOpt = list(
          content = c(
            "",
            hoopR_espn_nba_teams |>
              filter(
                display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]
              ) |>
              pull(dropdown_logo)
          )
        ),
        options = list(`dropdown-align-center` = TRUE,
                       placeholder = "Team"),
        selected = ""
      )
    }
  })
  output$t2p4_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      pickerInput(
        "t2p4_3t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]) |> sort(),
        choicesOpt = list(
          content = c(
            "",
            hoopR_espn_nba_teams |>
              filter(
                display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]
              ) |>
              pull(dropdown_logo)
          )
        ),
        options = list(`dropdown-align-center` = TRUE,
                       placeholder = "Team"),
        selected = ""
      )
    }
  })
  output$t2p5_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      pickerInput(
        "t2p5_3t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]) |> sort(),
        choicesOpt = list(
          content = c(
            "",
            hoopR_espn_nba_teams |>
              filter(
                display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]
              ) |>
              pull(dropdown_logo)
          )
        ),
        options = list(`dropdown-align-center` = TRUE,
                       placeholder = "Team"),
        selected = ""
      )
    }
  })
  output$t2p6_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      pickerInput(
        "t2p6_3t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]) |> sort(),
        choicesOpt = list(
          content = c(
            "",
            hoopR_espn_nba_teams |>
              filter(
                display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]
              ) |>
              pull(dropdown_logo)
          )
        ),
        options = list(`dropdown-align-center` = TRUE,
                       placeholder = "Team"),
        selected = ""
      )
    }
  })
  output$t2p7_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      pickerInput(
        "t2p7_3t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]) |> sort(),
        choicesOpt = list(
          content = c(
            "",
            hoopR_espn_nba_teams |>
              filter(
                display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]
              ) |>
              pull(dropdown_logo)
          )
        ),
        options = list(`dropdown-align-center` = TRUE,
                       placeholder = "Team"),
        selected = ""
      )
    }
  })
  output$t2p8_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      pickerInput(
        "t2p8_3t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]) |> sort(),
        choicesOpt = list(
          content = c(
            "",
            hoopR_espn_nba_teams |>
              filter(
                display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]
              ) |>
              pull(dropdown_logo)
          )
        ),
        options = list(`dropdown-align-center` = TRUE,
                       placeholder = "Team"),
        selected = ""
      )
    }
  })
  output$t2p9_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      pickerInput(
        "t2p9_3t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]) |> sort(),
        choicesOpt = list(
          content = c(
            "",
            hoopR_espn_nba_teams |>
              filter(
                display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]
              ) |>
              pull(dropdown_logo)
          )
        ),
        options = list(`dropdown-align-center` = TRUE,
                       placeholder = "Team"),
        selected = ""
      )
    }
  })
  output$t2p10_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      pickerInput(
        "t2p10_3t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]) |> sort(),
        choicesOpt = list(
          content = c(
            "",
            hoopR_espn_nba_teams |>
              filter(
                display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]
              ) |>
              pull(dropdown_logo)
          )
        ),
        options = list(`dropdown-align-center` = TRUE,
                       placeholder = "Team"),
        selected = ""
      )
    }
  })
  
  output$t2p1_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      pickerInput(
        "t2p1_4t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]) |> sort(),
        choicesOpt = list(
          content = c(
            "",
            hoopR_espn_nba_teams |>
              filter(
                display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]
              ) |>
              pull(dropdown_logo)
          )
        ),
        options = list(`dropdown-align-center` = TRUE,
                       placeholder = "Team"),
        selected = ""
      )
    }
  })
  output$t2p2_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      pickerInput(
        "t2p2_4t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]) |> sort(),
        choicesOpt = list(
          content = c(
            "",
            hoopR_espn_nba_teams |>
              filter(
                display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]
              ) |>
              pull(dropdown_logo)
          )
        ),
        options = list(`dropdown-align-center` = TRUE,
                       placeholder = "Team"),
        selected = ""
      )
    }
  })
  output$t2p3_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      pickerInput(
        "t2p3_4t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]) |> sort(),
        choicesOpt = list(
          content = c(
            "",
            hoopR_espn_nba_teams |>
              filter(
                display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]
              ) |>
              pull(dropdown_logo)
          )
        ),
        options = list(`dropdown-align-center` = TRUE,
                       placeholder = "Team"),
        selected = ""
      )
    }
  })
  output$t2p4_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      pickerInput(
        "t2p4_4t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]) |> sort(),
        choicesOpt = list(
          content = c(
            "",
            hoopR_espn_nba_teams |>
              filter(
                display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]
              ) |>
              pull(dropdown_logo)
          )
        ),
        options = list(`dropdown-align-center` = TRUE,
                       placeholder = "Team"),
        selected = ""
      )
    }
  })
  output$t2p5_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      pickerInput(
        "t2p5_4t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]) |> sort(),
        choicesOpt = list(
          content = c(
            "",
            hoopR_espn_nba_teams |>
              filter(
                display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]
              ) |>
              pull(dropdown_logo)
          )
        ),
        options = list(`dropdown-align-center` = TRUE,
                       placeholder = "Team"),
        selected = ""
      )
    }
  })
  output$t2p6_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      pickerInput(
        "t2p6_4t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]) |> sort(),
        choicesOpt = list(
          content = c(
            "",
            hoopR_espn_nba_teams |>
              filter(
                display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]
              ) |>
              pull(dropdown_logo)
          )
        ),
        options = list(`dropdown-align-center` = TRUE,
                       placeholder = "Team"),
        selected = ""
      )
    }
  })
  output$t2p7_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      pickerInput(
        "t2p7_4t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]) |> sort(),
        choicesOpt = list(
          content = c(
            "",
            hoopR_espn_nba_teams |>
              filter(
                display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]
              ) |>
              pull(dropdown_logo)
          )
        ),
        options = list(`dropdown-align-center` = TRUE,
                       placeholder = "Team"),
        selected = ""
      )
    }
  })
  output$t2p8_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      pickerInput(
        "t2p8_4t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]) |> sort(),
        choicesOpt = list(
          content = c(
            "",
            hoopR_espn_nba_teams |>
              filter(
                display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]
              ) |>
              pull(dropdown_logo)
          )
        ),
        options = list(`dropdown-align-center` = TRUE,
                       placeholder = "Team"),
        selected = ""
      )
    }
  })
  output$t2p9_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      pickerInput(
        "t2p9_4t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]) |> sort(),
        choicesOpt = list(
          content = c(
            "",
            hoopR_espn_nba_teams |>
              filter(
                display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]
              ) |>
              pull(dropdown_logo)
          )
        ),
        options = list(`dropdown-align-center` = TRUE,
                       placeholder = "Team"),
        selected = ""
      )
    }
  })
  output$t2p10_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      pickerInput(
        "t2p10_4t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]) |> sort(),
        choicesOpt = list(
          content = c(
            "",
            hoopR_espn_nba_teams |>
              filter(
                display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]
              ) |>
              pull(dropdown_logo)
          )
        ),
        options = list(`dropdown-align-center` = TRUE,
                       placeholder = "Team"),
        selected = ""
      )
    }
  })
  
  # Team 3
  output$t3p1_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      pickerInput(
        "t3p1_3t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]) |> sort(),
        choicesOpt = list(
          content = c(
            "",
            hoopR_espn_nba_teams |>
              filter(
                display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]
              ) |>
              pull(dropdown_logo)
          )
        ),
        options = list(`dropdown-align-center` = TRUE,
                       placeholder = "Team"),
        selected = ""
      )
    }
  })
  output$t3p2_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      pickerInput(
        "t3p2_3t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]) |> sort(),
        choicesOpt = list(
          content = c(
            "",
            hoopR_espn_nba_teams |>
              filter(
                display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]
              ) |>
              pull(dropdown_logo)
          )
        ),
        options = list(`dropdown-align-center` = TRUE,
                       placeholder = "Team"),
        selected = ""
      )
    }
  })
  output$t3p3_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      pickerInput(
        "t3p3_3t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]) |> sort(),
        choicesOpt = list(
          content = c(
            "",
            hoopR_espn_nba_teams |>
              filter(
                display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]
              ) |>
              pull(dropdown_logo)
          )
        ),
        options = list(`dropdown-align-center` = TRUE,
                       placeholder = "Team"),
        selected = ""
      )
    }
  })
  output$t3p4_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      pickerInput(
        "t3p4_3t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]) |> sort(),
        choicesOpt = list(
          content = c(
            "",
            hoopR_espn_nba_teams |>
              filter(
                display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]
              ) |>
              pull(dropdown_logo)
          )
        ),
        options = list(`dropdown-align-center` = TRUE,
                       placeholder = "Team"),
        selected = ""
      )
    }
  })
  output$t3p5_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      pickerInput(
        "t3p5_3t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]) |> sort(),
        choicesOpt = list(
          content = c(
            "",
            hoopR_espn_nba_teams |>
              filter(
                display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]
              ) |>
              pull(dropdown_logo)
          )
        ),
        options = list(`dropdown-align-center` = TRUE,
                       placeholder = "Team"),
        selected = ""
      )
    }
  })
  output$t3p6_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      pickerInput(
        "t3p6_3t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]) |> sort(),
        choicesOpt = list(
          content = c(
            "",
            hoopR_espn_nba_teams |>
              filter(
                display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]
              ) |>
              pull(dropdown_logo)
          )
        ),
        options = list(`dropdown-align-center` = TRUE,
                       placeholder = "Team"),
        selected = ""
      )
    }
  })
  output$t3p7_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      pickerInput(
        "t3p7_3t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]) |> sort(),
        choicesOpt = list(
          content = c(
            "",
            hoopR_espn_nba_teams |>
              filter(
                display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]
              ) |>
              pull(dropdown_logo)
          )
        ),
        options = list(`dropdown-align-center` = TRUE,
                       placeholder = "Team"),
        selected = ""
      )
    }
  })
  output$t3p8_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      pickerInput(
        "t3p8_3t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]) |> sort(),
        choicesOpt = list(
          content = c(
            "",
            hoopR_espn_nba_teams |>
              filter(
                display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]
              ) |>
              pull(dropdown_logo)
          )
        ),
        options = list(`dropdown-align-center` = TRUE,
                       placeholder = "Team"),
        selected = ""
      )
    }
  })
  output$t3p9_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      pickerInput(
        "t3p9_3t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]) |> sort(),
        choicesOpt = list(
          content = c(
            "",
            hoopR_espn_nba_teams |>
              filter(
                display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]
              ) |>
              pull(dropdown_logo)
          )
        ),
        options = list(`dropdown-align-center` = TRUE,
                       placeholder = "Team"),
        selected = ""
      )
    }
  })
  output$t3p10_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      pickerInput(
        "t3p10_3t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]) |> sort(),
        choicesOpt = list(
          content = c(
            "",
            hoopR_espn_nba_teams |>
              filter(
                display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]
              ) |>
              pull(dropdown_logo)
          )
        ),
        options = list(`dropdown-align-center` = TRUE,
                       placeholder = "Team"),
        selected = ""
      )
    }
  })
  
  output$t3p1_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      pickerInput(
        "t3p1_4t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]) |> sort(),
        choicesOpt = list(
          content = c(
            "",
            hoopR_espn_nba_teams |>
              filter(
                display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]
              ) |>
              pull(dropdown_logo)
          )
        ),
        options = list(`dropdown-align-center` = TRUE,
                       placeholder = "Team"),
        selected = ""
      )
    }
  })
  output$t3p2_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      pickerInput(
        "t3p2_4t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]) |> sort(),
        choicesOpt = list(
          content = c(
            "",
            hoopR_espn_nba_teams |>
              filter(
                display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]
              ) |>
              pull(dropdown_logo)
          )
        ),
        options = list(`dropdown-align-center` = TRUE,
                       placeholder = "Team"),
        selected = ""
      )
    }
  })
  output$t3p3_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      pickerInput(
        "t3p3_4t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]) |> sort(),
        choicesOpt = list(
          content = c(
            "",
            hoopR_espn_nba_teams |>
              filter(
                display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]
              ) |>
              pull(dropdown_logo)
          )
        ),
        options = list(`dropdown-align-center` = TRUE,
                       placeholder = "Team"),
        selected = ""
      )
    }
  })
  output$t3p4_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      pickerInput(
        "t3p4_4t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]) |> sort(),
        choicesOpt = list(
          content = c(
            "",
            hoopR_espn_nba_teams |>
              filter(
                display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]
              ) |>
              pull(dropdown_logo)
          )
        ),
        options = list(`dropdown-align-center` = TRUE,
                       placeholder = "Team"),
        selected = ""
      )
    }
  })
  output$t3p5_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      pickerInput(
        "t3p5_4t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]) |> sort(),
        choicesOpt = list(
          content = c(
            "",
            hoopR_espn_nba_teams |>
              filter(
                display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]
              ) |>
              pull(dropdown_logo)
          )
        ),
        options = list(`dropdown-align-center` = TRUE,
                       placeholder = "Team"),
        selected = ""
      )
    }
  })
  output$t3p6_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      pickerInput(
        "t3p6_4t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]) |> sort(),
        choicesOpt = list(
          content = c(
            "",
            hoopR_espn_nba_teams |>
              filter(
                display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]
              ) |>
              pull(dropdown_logo)
          )
        ),
        options = list(`dropdown-align-center` = TRUE,
                       placeholder = "Team"),
        selected = ""
      )
    }
  })
  output$t3p7_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      pickerInput(
        "t3p7_4t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]) |> sort(),
        choicesOpt = list(
          content = c(
            "",
            hoopR_espn_nba_teams |>
              filter(
                display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]
              ) |>
              pull(dropdown_logo)
          )
        ),
        options = list(`dropdown-align-center` = TRUE,
                       placeholder = "Team"),
        selected = ""
      )
    }
  })
  output$t3p8_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      pickerInput(
        "t3p8_4t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]) |> sort(),
        choicesOpt = list(
          content = c(
            "",
            hoopR_espn_nba_teams |>
              filter(
                display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]
              ) |>
              pull(dropdown_logo)
          )
        ),
        options = list(`dropdown-align-center` = TRUE,
                       placeholder = "Team"),
        selected = ""
      )
    }
  })
  output$t3p9_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      pickerInput(
        "t3p9_4t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]) |> sort(),
        choicesOpt = list(
          content = c(
            "",
            hoopR_espn_nba_teams |>
              filter(
                display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]
              ) |>
              pull(dropdown_logo)
          )
        ),
        options = list(`dropdown-align-center` = TRUE,
                       placeholder = "Team"),
        selected = ""
      )
    }
  })
  output$t3p10_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      pickerInput(
        "t3p10_4t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]) |> sort(),
        choicesOpt = list(
          content = c(
            "",
            hoopR_espn_nba_teams |>
              filter(
                display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]
              ) |>
              pull(dropdown_logo)
          )
        ),
        options = list(`dropdown-align-center` = TRUE,
                       placeholder = "Team"),
        selected = ""
      )
    }
  })
  
  # Team 4
  output$t4p1_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      pickerInput(
        "t4p1_4t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[4]]]) |> sort(),
        choicesOpt = list(
          content = c(
            "",
            hoopR_espn_nba_teams |>
              filter(
                display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[4]]]
              ) |>
              pull(dropdown_logo)
          )
        ),
        options = list(`dropdown-align-center` = TRUE,
                       placeholder = "Team"),
        selected = ""
      )
    }
  })
  output$t4p2_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      pickerInput(
        "t4p2_4t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[4]]]) |> sort(),
        choicesOpt = list(
          content = c(
            "",
            hoopR_espn_nba_teams |>
              filter(
                display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[4]]]
              ) |>
              pull(dropdown_logo)
          )
        ),
        options = list(`dropdown-align-center` = TRUE,
                       placeholder = "Team"),
        selected = ""
      )
    }
  })
  output$t4p3_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      pickerInput(
        "t4p3_4t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[4]]]) |> sort(),
        choicesOpt = list(
          content = c(
            "",
            hoopR_espn_nba_teams |>
              filter(
                display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[4]]]
              ) |>
              pull(dropdown_logo)
          )
        ),
        options = list(`dropdown-align-center` = TRUE,
                       placeholder = "Team"),
        selected = ""
      )
    }
  })
  output$t4p4_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      pickerInput(
        "t4p4_4t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[4]]]) |> sort(),
        choicesOpt = list(
          content = c(
            "",
            hoopR_espn_nba_teams |>
              filter(
                display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[4]]]
              ) |>
              pull(dropdown_logo)
          )
        ),
        options = list(`dropdown-align-center` = TRUE,
                       placeholder = "Team"),
        selected = ""
      )
    }
  })
  output$t4p5_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      pickerInput(
        "t4p5_4t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[4]]]) |> sort(),
        choicesOpt = list(
          content = c(
            "",
            hoopR_espn_nba_teams |>
              filter(
                display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[4]]]
              ) |>
              pull(dropdown_logo)
          )
        ),
        options = list(`dropdown-align-center` = TRUE,
                       placeholder = "Team"),
        selected = ""
      )
    }
  })
  output$t4p6_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      pickerInput(
        "t4p6_4t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[4]]]) |> sort(),
        choicesOpt = list(
          content = c(
            "",
            hoopR_espn_nba_teams |>
              filter(
                display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[4]]]
              ) |>
              pull(dropdown_logo)
          )
        ),
        options = list(`dropdown-align-center` = TRUE,
                       placeholder = "Team"),
        selected = ""
      )
    }
  })
  output$t4p7_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      pickerInput(
        "t4p7_4t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[4]]]) |> sort(),
        choicesOpt = list(
          content = c(
            "",
            hoopR_espn_nba_teams |>
              filter(
                display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[4]]]
              ) |>
              pull(dropdown_logo)
          )
        ),
        options = list(`dropdown-align-center` = TRUE,
                       placeholder = "Team"),
        selected = ""
      )
    }
  })
  output$t4p8_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      pickerInput(
        "t4p8_4t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[4]]]) |> sort(),
        choicesOpt = list(
          content = c(
            "",
            hoopR_espn_nba_teams |>
              filter(
                display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[4]]]
              ) |>
              pull(dropdown_logo)
          )
        ),
        options = list(`dropdown-align-center` = TRUE,
                       placeholder = "Team"),
        selected = ""
      )
    }
  })
  output$t4p9_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      pickerInput(
        "t4p9_4t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[4]]]) |> sort(),
        choicesOpt = list(
          content = c(
            "",
            hoopR_espn_nba_teams |>
              filter(
                display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[4]]]
              ) |>
              pull(dropdown_logo)
          )
        ),
        options = list(`dropdown-align-center` = TRUE,
                       placeholder = "Team"),
        selected = ""
      )
    }
  })
  output$t4p10_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      pickerInput(
        "t4p10_4t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[4]]]) |> sort(),
        choicesOpt = list(
          content = c(
            "",
            hoopR_espn_nba_teams |>
              filter(
                display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[4]]]
              ) |>
              pull(dropdown_logo)
          )
        ),
        options = list(`dropdown-align-center` = TRUE,
                       placeholder = "Team"),
        selected = ""
      )
    }
  })
  
  
  ### Traded Asset to Team Input
  # Team 1
  output$t1a1_2t_to = renderUI({
    if(length(input$teamsInvolved) == 2){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t1a1_2t_to", 
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
    }
  })
  output$t1a2_2t_to = renderUI({
    if(length(input$teamsInvolved) == 2){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t1a2_2t_to", 
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
    }
  })
  output$t1a3_2t_to = renderUI({
    if(length(input$teamsInvolved) == 2){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t1a3_2t_to", 
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
    }
  })
  output$t1a4_2t_to = renderUI({
    if(length(input$teamsInvolved) == 2){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t1a4_2t_to", 
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
    }
  })
  output$t1a5_2t_to = renderUI({
    if(length(input$teamsInvolved) == 2){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t1a5_2t_to", 
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
    }
  })
  output$t1a6_2t_to = renderUI({
    if(length(input$teamsInvolved) == 2){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t1a6_2t_to", 
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
    }
  })
  output$t1a7_2t_to = renderUI({
    if(length(input$teamsInvolved) == 2){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t1a7_2t_to", 
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
    }
  })
  output$t1a8_2t_to = renderUI({
    if(length(input$teamsInvolved) == 2){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t1a8_2t_to", 
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
    }
  })
  output$t1a9_2t_to = renderUI({
    if(length(input$teamsInvolved) == 2){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t1a9_2t_to", 
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
    }
  })
  output$t1a10_2t_to = renderUI({
    if(length(input$teamsInvolved) == 2){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t1a10_2t_to", 
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
    }
  })
  
  output$t1a1_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      div(
        pickerInput(
          "t1a1_3t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]) |> sort(),
          choicesOpt = list(
            content = c(
              "",
              hoopR_espn_nba_teams |>
                filter(
                  display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]
                ) |>
                pull(dropdown_logo)
            )
          ),
          options = list(`dropdown-align-center` = TRUE,
                         placeholder = "Team"),
          selected = ""
        )
      )
    }
  })
  output$t1a2_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      div(
        pickerInput(
          "t1a2_3t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]) |> sort(),
          choicesOpt = list(
            content = c(
              "",
              hoopR_espn_nba_teams |>
                filter(
                  display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]
                ) |>
                pull(dropdown_logo)
            )
          ),
          options = list(`dropdown-align-center` = TRUE,
                         placeholder = "Team"),
          selected = ""
        )
      )
    }
  })
  output$t1a3_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      div(
        pickerInput(
          "t1a3_3t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]) |> sort(),
          choicesOpt = list(
            content = c(
              "",
              hoopR_espn_nba_teams |>
                filter(
                  display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]
                ) |>
                pull(dropdown_logo)
            )
          ),
          options = list(`dropdown-align-center` = TRUE,
                         placeholder = "Team"),
          selected = ""
        )
      )
    }
  })
  output$t1a4_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      div(
        pickerInput(
          "t1a4_3t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]) |> sort(),
          choicesOpt = list(
            content = c(
              "",
              hoopR_espn_nba_teams |>
                filter(
                  display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]
                ) |>
                pull(dropdown_logo)
            )
          ),
          options = list(`dropdown-align-center` = TRUE,
                         placeholder = "Team"),
          selected = ""
        )
      )
    }
  })
  output$t1a5_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      div(
        pickerInput(
          "t1a5_3t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]) |> sort(),
          choicesOpt = list(
            content = c(
              "",
              hoopR_espn_nba_teams |>
                filter(
                  display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]
                ) |>
                pull(dropdown_logo)
            )
          ),
          options = list(`dropdown-align-center` = TRUE,
                         placeholder = "Team"),
          selected = ""
        )
      )
    }
  })
  output$t1a6_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      div(
        pickerInput(
          "t1a6_3t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]) |> sort(),
          choicesOpt = list(
            content = c(
              "",
              hoopR_espn_nba_teams |>
                filter(
                  display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]
                ) |>
                pull(dropdown_logo)
            )
          ),
          options = list(`dropdown-align-center` = TRUE,
                         placeholder = "Team"),
          selected = ""
        )
      )
    }
  })
  output$t1a7_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      div(
        pickerInput(
          "t1a7_3t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]) |> sort(),
          choicesOpt = list(
            content = c(
              "",
              hoopR_espn_nba_teams |>
                filter(
                  display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]
                ) |>
                pull(dropdown_logo)
            )
          ),
          options = list(`dropdown-align-center` = TRUE,
                         placeholder = "Team"),
          selected = ""
        )
      )
    }
  })
  output$t1a8_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      div(
        pickerInput(
          "t1a8_3t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]) |> sort(),
          choicesOpt = list(
            content = c(
              "",
              hoopR_espn_nba_teams |>
                filter(
                  display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]
                ) |>
                pull(dropdown_logo)
            )
          ),
          options = list(`dropdown-align-center` = TRUE,
                         placeholder = "Team"),
          selected = ""
        )
      )
    }
  })
  output$t1a9_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      div(
        pickerInput(
          "t1a9_3t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]) |> sort(),
          choicesOpt = list(
            content = c(
              "",
              hoopR_espn_nba_teams |>
                filter(
                  display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]
                ) |>
                pull(dropdown_logo)
            )
          ),
          options = list(`dropdown-align-center` = TRUE,
                         placeholder = "Team"),
          selected = ""
        )
      )
    }
  })
  output$t1a10_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      div(
        pickerInput(
          "t1a10_3t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]) |> sort(),
          choicesOpt = list(
            content = c(
              "",
              hoopR_espn_nba_teams |>
                filter(
                  display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]
                ) |>
                pull(dropdown_logo)
            )
          ),
          options = list(`dropdown-align-center` = TRUE,
                         placeholder = "Team"),
          selected = ""
        )
      )
    }
  })
  
  output$t1a1_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      div(
        pickerInput(
          "t1a1_4t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]) |> sort(),
          choicesOpt = list(
            content = c(
              "",
              hoopR_espn_nba_teams |>
                filter(
                  display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]
                ) |>
                pull(dropdown_logo)
            )
          ),
          options = list(`dropdown-align-center` = TRUE,
                         placeholder = "Team"),
          selected = ""
        )
      )
    }
  })
  output$t1a2_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      div(
        pickerInput(
          "t1a2_4t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]) |> sort(),
          choicesOpt = list(
            content = c(
              "",
              hoopR_espn_nba_teams |>
                filter(
                  display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]
                ) |>
                pull(dropdown_logo)
            )
          ),
          options = list(`dropdown-align-center` = TRUE,
                         placeholder = "Team"),
          selected = ""
        )
      )
    }
  })
  output$t1a3_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      div(
        pickerInput(
          "t1a3_4t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]) |> sort(),
          choicesOpt = list(
            content = c(
              "",
              hoopR_espn_nba_teams |>
                filter(
                  display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]
                ) |>
                pull(dropdown_logo)
            )
          ),
          options = list(`dropdown-align-center` = TRUE,
                         placeholder = "Team"),
          selected = ""
        )
      )
    }
  })
  output$t1a4_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      div(
        pickerInput(
          "t1a4_4t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]) |> sort(),
          choicesOpt = list(
            content = c(
              "",
              hoopR_espn_nba_teams |>
                filter(
                  display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]
                ) |>
                pull(dropdown_logo)
            )
          ),
          options = list(`dropdown-align-center` = TRUE,
                         placeholder = "Team"),
          selected = ""
        )
      )
    }
  })
  output$t1a5_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      div(
        pickerInput(
          "t1a5_4t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]) |> sort(),
          choicesOpt = list(
            content = c(
              "",
              hoopR_espn_nba_teams |>
                filter(
                  display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]
                ) |>
                pull(dropdown_logo)
            )
          ),
          options = list(`dropdown-align-center` = TRUE,
                         placeholder = "Team"),
          selected = ""
        )
      )
    }
  })
  output$t1a6_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      div(
        pickerInput(
          "t1a6_4t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]) |> sort(),
          choicesOpt = list(
            content = c(
              "",
              hoopR_espn_nba_teams |>
                filter(
                  display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]
                ) |>
                pull(dropdown_logo)
            )
          ),
          options = list(`dropdown-align-center` = TRUE,
                         placeholder = "Team"),
          selected = ""
        )
      )
    }
  })
  output$t1a7_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      div(
        pickerInput(
          "t1a7_4t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]) |> sort(),
          choicesOpt = list(
            content = c(
              "",
              hoopR_espn_nba_teams |>
                filter(
                  display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]
                ) |>
                pull(dropdown_logo)
            )
          ),
          options = list(`dropdown-align-center` = TRUE,
                         placeholder = "Team"),
          selected = ""
        )
      )
    }
  })
  output$t1a8_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      div(
        pickerInput(
          "t1a8_4t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]) |> sort(),
          choicesOpt = list(
            content = c(
              "",
              hoopR_espn_nba_teams |>
                filter(
                  display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]
                ) |>
                pull(dropdown_logo)
            )
          ),
          options = list(`dropdown-align-center` = TRUE,
                         placeholder = "Team"),
          selected = ""
        )
      )
    }
  })
  output$t1a9_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      div(
        pickerInput(
          "t1a9_4t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]) |> sort(),
          choicesOpt = list(
            content = c(
              "",
              hoopR_espn_nba_teams |>
                filter(
                  display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]
                ) |>
                pull(dropdown_logo)
            )
          ),
          options = list(`dropdown-align-center` = TRUE,
                         placeholder = "Team"),
          selected = ""
        )
      )
    }
  })
  output$t1a10_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      div(
        pickerInput(
          "t1a10_4t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]) |> sort(),
          choicesOpt = list(
            content = c(
              "",
              hoopR_espn_nba_teams |>
                filter(
                  display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]
                ) |>
                pull(dropdown_logo)
            )
          ),
          options = list(`dropdown-align-center` = TRUE,
                         placeholder = "Team"),
          selected = ""
        )
      )
    }
  })
  
  # Team 2
  output$t2a1_2t_to = renderUI({
    if(length(input$teamsInvolved) == 2){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t2a1_2t_to", 
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
    }
  })
  output$t2a2_2t_to = renderUI({
    if(length(input$teamsInvolved) == 2){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t2a2_2t_to", 
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
    }
  })
  output$t2a3_2t_to = renderUI({
    if(length(input$teamsInvolved) == 2){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t2a3_2t_to", 
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
    }
  })
  output$t2a4_2t_to = renderUI({
    if(length(input$teamsInvolved) == 2){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t2a4_2t_to", 
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
    }
  })
  output$t2a5_2t_to = renderUI({
    if(length(input$teamsInvolved) == 2){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t2a5_2t_to", 
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
    }
  })
  output$t2a6_2t_to = renderUI({
    if(length(input$teamsInvolved) == 2){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t2a6_2t_to", 
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
    }
  })
  output$t2a7_2t_to = renderUI({
    if(length(input$teamsInvolved) == 2){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t2a7_2t_to", 
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
    }
  })
  output$t2a8_2t_to = renderUI({
    if(length(input$teamsInvolved) == 2){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t2a8_2t_to", 
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
    }
  })
  output$t2a9_2t_to = renderUI({
    if(length(input$teamsInvolved) == 2){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t2a9_2t_to", 
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
    }
  })
  output$t2a10_2t_to = renderUI({
    if(length(input$teamsInvolved) == 2){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t2a10_2t_to", 
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
    }
  })
  
  output$t2a1_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      div(
        pickerInput(
          "t2a1_3t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]) |> sort(),
          choicesOpt = list(
            content = c(
              "",
              hoopR_espn_nba_teams |>
                filter(
                  display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]
                ) |>
                pull(dropdown_logo)
            )
          ),
          options = list(`dropdown-align-center` = TRUE,
                         placeholder = "Team"),
          selected = ""
        )
      )
    }
  })
  output$t2a2_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      div(
        pickerInput(
          "t2a2_3t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]) |> sort(),
          choicesOpt = list(
            content = c(
              "",
              hoopR_espn_nba_teams |>
                filter(
                  display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]
                ) |>
                pull(dropdown_logo)
            )
          ),
          options = list(`dropdown-align-center` = TRUE,
                         placeholder = "Team"),
          selected = ""
        )
      )
    }
  })
  output$t2a3_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      div(
        pickerInput(
          "t2a3_3t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]) |> sort(),
          choicesOpt = list(
            content = c(
              "",
              hoopR_espn_nba_teams |>
                filter(
                  display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]
                ) |>
                pull(dropdown_logo)
            )
          ),
          options = list(`dropdown-align-center` = TRUE,
                         placeholder = "Team"),
          selected = ""
        )
      )
    }
  })
  output$t2a4_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      div(
        pickerInput(
          "t2a4_3t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]) |> sort(),
          choicesOpt = list(
            content = c(
              "",
              hoopR_espn_nba_teams |>
                filter(
                  display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]
                ) |>
                pull(dropdown_logo)
            )
          ),
          options = list(`dropdown-align-center` = TRUE,
                         placeholder = "Team"),
          selected = ""
        )
      )
    }
  })
  output$t2a5_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      div(
        pickerInput(
          "t2a5_3t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]) |> sort(),
          choicesOpt = list(
            content = c(
              "",
              hoopR_espn_nba_teams |>
                filter(
                  display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]
                ) |>
                pull(dropdown_logo)
            )
          ),
          options = list(`dropdown-align-center` = TRUE,
                         placeholder = "Team"),
          selected = ""
        )
      )
    }
  })
  output$t2a6_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      div(
        pickerInput(
          "t2a6_3t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]) |> sort(),
          choicesOpt = list(
            content = c(
              "",
              hoopR_espn_nba_teams |>
                filter(
                  display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]
                ) |>
                pull(dropdown_logo)
            )
          ),
          options = list(`dropdown-align-center` = TRUE,
                         placeholder = "Team"),
          selected = ""
        )
      )
    }
  })
  output$t2a7_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      div(
        pickerInput(
          "t2a7_3t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]) |> sort(),
          choicesOpt = list(
            content = c(
              "",
              hoopR_espn_nba_teams |>
                filter(
                  display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]
                ) |>
                pull(dropdown_logo)
            )
          ),
          options = list(`dropdown-align-center` = TRUE,
                         placeholder = "Team"),
          selected = ""
        )
      )
    }
  })
  output$t2a8_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      div(
        pickerInput(
          "t2a8_3t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]) |> sort(),
          choicesOpt = list(
            content = c(
              "",
              hoopR_espn_nba_teams |>
                filter(
                  display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]
                ) |>
                pull(dropdown_logo)
            )
          ),
          options = list(`dropdown-align-center` = TRUE,
                         placeholder = "Team"),
          selected = ""
        )
      )
    }
  })
  output$t2a9_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      div(
        pickerInput(
          "t2a9_3t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]) |> sort(),
          choicesOpt = list(
            content = c(
              "",
              hoopR_espn_nba_teams |>
                filter(
                  display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]
                ) |>
                pull(dropdown_logo)
            )
          ),
          options = list(`dropdown-align-center` = TRUE,
                         placeholder = "Team"),
          selected = ""
        )
      )
    }
  })
  output$t2a10_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      div(
        pickerInput(
          "t2a10_3t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]) |> sort(),
          choicesOpt = list(
            content = c(
              "",
              hoopR_espn_nba_teams |>
                filter(
                  display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]
                ) |>
                pull(dropdown_logo)
            )
          ),
          options = list(`dropdown-align-center` = TRUE,
                         placeholder = "Team"),
          selected = ""
        )
      )
    }
  })
  
  output$t2a1_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      div(
        pickerInput(
          "t2a1_4t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]) |> sort(),
          choicesOpt = list(
            content = c(
              "",
              hoopR_espn_nba_teams |>
                filter(
                  display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]
                ) |>
                pull(dropdown_logo)
            )
          ),
          options = list(`dropdown-align-center` = TRUE,
                         placeholder = "Team"),
          selected = ""
        )
      )
    }
  })
  output$t2a2_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      div(
        pickerInput(
          "t2a2_4t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]) |> sort(),
          choicesOpt = list(
            content = c(
              "",
              hoopR_espn_nba_teams |>
                filter(
                  display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]
                ) |>
                pull(dropdown_logo)
            )
          ),
          options = list(`dropdown-align-center` = TRUE,
                         placeholder = "Team"),
          selected = ""
        )
      )
    }
  })
  output$t2a3_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      div(
        pickerInput(
          "t2a3_4t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]) |> sort(),
          choicesOpt = list(
            content = c(
              "",
              hoopR_espn_nba_teams |>
                filter(
                  display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]
                ) |>
                pull(dropdown_logo)
            )
          ),
          options = list(`dropdown-align-center` = TRUE,
                         placeholder = "Team"),
          selected = ""
        )
      )
    }
  })
  output$t2a4_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      div(
        pickerInput(
          "t2a4_4t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]) |> sort(),
          choicesOpt = list(
            content = c(
              "",
              hoopR_espn_nba_teams |>
                filter(
                  display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]
                ) |>
                pull(dropdown_logo)
            )
          ),
          options = list(`dropdown-align-center` = TRUE,
                         placeholder = "Team"),
          selected = ""
        )
      )
    }
  })
  output$t2a5_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      div(
        pickerInput(
          "t2a5_4t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]) |> sort(),
          choicesOpt = list(
            content = c(
              "",
              hoopR_espn_nba_teams |>
                filter(
                  display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]
                ) |>
                pull(dropdown_logo)
            )
          ),
          options = list(`dropdown-align-center` = TRUE,
                         placeholder = "Team"),
          selected = ""
        )
      )
    }
  })
  output$t2a6_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      div(
        pickerInput(
          "t2a6_4t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]) |> sort(),
          choicesOpt = list(
            content = c(
              "",
              hoopR_espn_nba_teams |>
                filter(
                  display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]
                ) |>
                pull(dropdown_logo)
            )
          ),
          options = list(`dropdown-align-center` = TRUE,
                         placeholder = "Team"),
          selected = ""
        )
      )
    }
  })
  output$t2a7_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      div(
        pickerInput(
          "t2a7_4t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]) |> sort(),
          choicesOpt = list(
            content = c(
              "",
              hoopR_espn_nba_teams |>
                filter(
                  display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]
                ) |>
                pull(dropdown_logo)
            )
          ),
          options = list(`dropdown-align-center` = TRUE,
                         placeholder = "Team"),
          selected = ""
        )
      )
    }
  })
  output$t2a8_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      div(
        pickerInput(
          "t2a8_4t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]) |> sort(),
          choicesOpt = list(
            content = c(
              "",
              hoopR_espn_nba_teams |>
                filter(
                  display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]
                ) |>
                pull(dropdown_logo)
            )
          ),
          options = list(`dropdown-align-center` = TRUE,
                         placeholder = "Team"),
          selected = ""
        )
      )
    }
  })
  output$t2a9_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      div(
        pickerInput(
          "t2a9_4t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]) |> sort(),
          choicesOpt = list(
            content = c(
              "",
              hoopR_espn_nba_teams |>
                filter(
                  display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]
                ) |>
                pull(dropdown_logo)
            )
          ),
          options = list(`dropdown-align-center` = TRUE,
                         placeholder = "Team"),
          selected = ""
        )
      )
    }
  })
  output$t2a10_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      div(
        pickerInput(
          "t2a10_4t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]) |> sort(),
          choicesOpt = list(
            content = c(
              "",
              hoopR_espn_nba_teams |>
                filter(
                  display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]
                ) |>
                pull(dropdown_logo)
            )
          ),
          options = list(`dropdown-align-center` = TRUE,
                         placeholder = "Team"),
          selected = ""
        )
      )
    }
  })
  
  # Team 3
  output$t3a1_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      div(
        pickerInput(
          "t3a1_3t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]) |> sort(),
          choicesOpt = list(
            content = c(
              "",
              hoopR_espn_nba_teams |>
                filter(
                  display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]
                ) |>
                pull(dropdown_logo)
            )
          ),
          options = list(`dropdown-align-center` = TRUE,
                         placeholder = "Team"),
          selected = ""
        )
      )
    }
  })
  output$t3a2_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      div(
        pickerInput(
          "t3a2_3t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]) |> sort(),
          choicesOpt = list(
            content = c(
              "",
              hoopR_espn_nba_teams |>
                filter(
                  display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]
                ) |>
                pull(dropdown_logo)
            )
          ),
          options = list(`dropdown-align-center` = TRUE,
                         placeholder = "Team"),
          selected = ""
        )
      )
    }
  })
  output$t3a3_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      div(
        pickerInput(
          "t3a3_3t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]) |> sort(),
          choicesOpt = list(
            content = c(
              "",
              hoopR_espn_nba_teams |>
                filter(
                  display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]
                ) |>
                pull(dropdown_logo)
            )
          ),
          options = list(`dropdown-align-center` = TRUE,
                         placeholder = "Team"),
          selected = ""
        )
      )
    }
  })
  output$t3a4_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      div(
        pickerInput(
          "t3a4_3t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]) |> sort(),
          choicesOpt = list(
            content = c(
              "",
              hoopR_espn_nba_teams |>
                filter(
                  display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]
                ) |>
                pull(dropdown_logo)
            )
          ),
          options = list(`dropdown-align-center` = TRUE,
                         placeholder = "Team"),
          selected = ""
        )
      )
    }
  })
  output$t3a5_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      div(
        pickerInput(
          "t3a5_3t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]) |> sort(),
          choicesOpt = list(
            content = c(
              "",
              hoopR_espn_nba_teams |>
                filter(
                  display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]
                ) |>
                pull(dropdown_logo)
            )
          ),
          options = list(`dropdown-align-center` = TRUE,
                         placeholder = "Team"),
          selected = ""
        )
      )
    }
  })
  output$t3a6_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      div(
        pickerInput(
          "t3a6_3t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]) |> sort(),
          choicesOpt = list(
            content = c(
              "",
              hoopR_espn_nba_teams |>
                filter(
                  display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]
                ) |>
                pull(dropdown_logo)
            )
          ),
          options = list(`dropdown-align-center` = TRUE,
                         placeholder = "Team"),
          selected = ""
        )
      )
    }
  })
  output$t3a7_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      div(
        pickerInput(
          "t3a7_3t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]) |> sort(),
          choicesOpt = list(
            content = c(
              "",
              hoopR_espn_nba_teams |>
                filter(
                  display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]
                ) |>
                pull(dropdown_logo)
            )
          ),
          options = list(`dropdown-align-center` = TRUE,
                         placeholder = "Team"),
          selected = ""
        )
      )
    }
  })
  output$t3a8_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      div(
        pickerInput(
          "t3a8_3t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]) |> sort(),
          choicesOpt = list(
            content = c(
              "",
              hoopR_espn_nba_teams |>
                filter(
                  display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]
                ) |>
                pull(dropdown_logo)
            )
          ),
          options = list(`dropdown-align-center` = TRUE,
                         placeholder = "Team"),
          selected = ""
        )
      )
    }
  })
  output$t3a9_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      div(
        pickerInput(
          "t3a9_3t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]) |> sort(),
          choicesOpt = list(
            content = c(
              "",
              hoopR_espn_nba_teams |>
                filter(
                  display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]
                ) |>
                pull(dropdown_logo)
            )
          ),
          options = list(`dropdown-align-center` = TRUE,
                         placeholder = "Team"),
          selected = ""
        )
      )
    }
  })
  output$t3a10_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      div(
        pickerInput(
          "t3a10_3t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]) |> sort(),
          choicesOpt = list(
            content = c(
              "",
              hoopR_espn_nba_teams |>
                filter(
                  display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]
                ) |>
                pull(dropdown_logo)
            )
          ),
          options = list(`dropdown-align-center` = TRUE,
                         placeholder = "Team"),
          selected = ""
        )
      )
    }
  })
  
  output$t3a1_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      div(
        pickerInput(
          "t3a1_4t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]) |> sort(),
          choicesOpt = list(
            content = c(
              "",
              hoopR_espn_nba_teams |>
                filter(
                  display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]
                ) |>
                pull(dropdown_logo)
            )
          ),
          options = list(`dropdown-align-center` = TRUE,
                         placeholder = "Team"),
          selected = ""
        )
      )
    }
  })
  output$t3a2_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      div(
        pickerInput(
          "t3a2_4t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]) |> sort(),
          choicesOpt = list(
            content = c(
              "",
              hoopR_espn_nba_teams |>
                filter(
                  display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]
                ) |>
                pull(dropdown_logo)
            )
          ),
          options = list(`dropdown-align-center` = TRUE,
                         placeholder = "Team"),
          selected = ""
        )
      )
    }
  })
  output$t3a3_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      div(
        pickerInput(
          "t3a3_4t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]) |> sort(),
          choicesOpt = list(
            content = c(
              "",
              hoopR_espn_nba_teams |>
                filter(
                  display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]
                ) |>
                pull(dropdown_logo)
            )
          ),
          options = list(`dropdown-align-center` = TRUE,
                         placeholder = "Team"),
          selected = ""
        )
      )
    }
  })
  output$t3a4_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      div(
        pickerInput(
          "t3a4_4t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]) |> sort(),
          choicesOpt = list(
            content = c(
              "",
              hoopR_espn_nba_teams |>
                filter(
                  display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]
                ) |>
                pull(dropdown_logo)
            )
          ),
          options = list(`dropdown-align-center` = TRUE,
                         placeholder = "Team"),
          selected = ""
        )
      )
    }
  })
  output$t3a5_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      div(
        pickerInput(
          "t3a5_4t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]) |> sort(),
          choicesOpt = list(
            content = c(
              "",
              hoopR_espn_nba_teams |>
                filter(
                  display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]
                ) |>
                pull(dropdown_logo)
            )
          ),
          options = list(`dropdown-align-center` = TRUE,
                         placeholder = "Team"),
          selected = ""
        )
      )
    }
  })
  output$t3a6_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      div(
        pickerInput(
          "t3a6_4t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]) |> sort(),
          choicesOpt = list(
            content = c(
              "",
              hoopR_espn_nba_teams |>
                filter(
                  display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]
                ) |>
                pull(dropdown_logo)
            )
          ),
          options = list(`dropdown-align-center` = TRUE,
                         placeholder = "Team"),
          selected = ""
        )
      )
    }
  })
  output$t3a7_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      div(
        pickerInput(
          "t3a7_4t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]) |> sort(),
          choicesOpt = list(
            content = c(
              "",
              hoopR_espn_nba_teams |>
                filter(
                  display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]
                ) |>
                pull(dropdown_logo)
            )
          ),
          options = list(`dropdown-align-center` = TRUE,
                         placeholder = "Team"),
          selected = ""
        )
      )
    }
  })
  output$t3a8_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      div(
        pickerInput(
          "t3a8_4t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]) |> sort(),
          choicesOpt = list(
            content = c(
              "",
              hoopR_espn_nba_teams |>
                filter(
                  display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]
                ) |>
                pull(dropdown_logo)
            )
          ),
          options = list(`dropdown-align-center` = TRUE,
                         placeholder = "Team"),
          selected = ""
        )
      )
    }
  })
  output$t3a9_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      div(
        pickerInput(
          "t3a9_4t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]) |> sort(),
          choicesOpt = list(
            content = c(
              "",
              hoopR_espn_nba_teams |>
                filter(
                  display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]
                ) |>
                pull(dropdown_logo)
            )
          ),
          options = list(`dropdown-align-center` = TRUE,
                         placeholder = "Team"),
          selected = ""
        )
      )
    }
  })
  output$t3a10_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      div(
        pickerInput(
          "t3a10_4t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]) |> sort(),
          choicesOpt = list(
            content = c(
              "",
              hoopR_espn_nba_teams |>
                filter(
                  display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]
                ) |>
                pull(dropdown_logo)
            )
          ),
          options = list(`dropdown-align-center` = TRUE,
                         placeholder = "Team"),
          selected = ""
        )
      )
    }
  })
  
  # Team 4
  output$t4a1_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      div(
        pickerInput(
          "t4a1_4t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[4]]]) |> sort(),
          choicesOpt = list(
            content = c(
              "",
              hoopR_espn_nba_teams |>
                filter(
                  display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[4]]]
                ) |>
                pull(dropdown_logo)
            )
          ),
          options = list(`dropdown-align-center` = TRUE,
                         placeholder = "Team"),
          selected = ""
        )
      )
    }
  })
  output$t4a2_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      div(
        pickerInput(
          "t4a2_4t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[4]]]) |> sort(),
          choicesOpt = list(
            content = c(
              "",
              hoopR_espn_nba_teams |>
                filter(
                  display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[4]]]
                ) |>
                pull(dropdown_logo)
            )
          ),
          options = list(`dropdown-align-center` = TRUE,
                         placeholder = "Team"),
          selected = ""
        )
      )
    }
  })
  output$t4a3_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      div(
        pickerInput(
          "t4a3_4t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[4]]]) |> sort(),
          choicesOpt = list(
            content = c(
              "",
              hoopR_espn_nba_teams |>
                filter(
                  display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[4]]]
                ) |>
                pull(dropdown_logo)
            )
          ),
          options = list(`dropdown-align-center` = TRUE,
                         placeholder = "Team"),
          selected = ""
        )
      )
    }
  })
  output$t4a4_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      div(
        pickerInput(
          "t4a4_4t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[4]]]) |> sort(),
          choicesOpt = list(
            content = c(
              "",
              hoopR_espn_nba_teams |>
                filter(
                  display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[4]]]
                ) |>
                pull(dropdown_logo)
            )
          ),
          options = list(`dropdown-align-center` = TRUE,
                         placeholder = "Team"),
          selected = ""
        )
      )
    }
  })
  output$t4a5_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      div(
        pickerInput(
          "t4a5_4t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[4]]]) |> sort(),
          choicesOpt = list(
            content = c(
              "",
              hoopR_espn_nba_teams |>
                filter(
                  display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[4]]]
                ) |>
                pull(dropdown_logo)
            )
          ),
          options = list(`dropdown-align-center` = TRUE,
                         placeholder = "Team"),
          selected = ""
        )
      )
    }
  })
  output$t4a6_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      div(
        pickerInput(
          "t4a6_4t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[4]]]) |> sort(),
          choicesOpt = list(
            content = c(
              "",
              hoopR_espn_nba_teams |>
                filter(
                  display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[4]]]
                ) |>
                pull(dropdown_logo)
            )
          ),
          options = list(`dropdown-align-center` = TRUE,
                         placeholder = "Team"),
          selected = ""
        )
      )
    }
  })
  output$t4a7_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      div(
        pickerInput(
          "t4a7_4t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[4]]]) |> sort(),
          choicesOpt = list(
            content = c(
              "",
              hoopR_espn_nba_teams |>
                filter(
                  display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[4]]]
                ) |>
                pull(dropdown_logo)
            )
          ),
          options = list(`dropdown-align-center` = TRUE,
                         placeholder = "Team"),
          selected = ""
        )
      )
    }
  })
  output$t4a8_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      div(
        pickerInput(
          "t4a8_4t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[4]]]) |> sort(),
          choicesOpt = list(
            content = c(
              "",
              hoopR_espn_nba_teams |>
                filter(
                  display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[4]]]
                ) |>
                pull(dropdown_logo)
            )
          ),
          options = list(`dropdown-align-center` = TRUE,
                         placeholder = "Team"),
          selected = ""
        )
      )
    }
  })
  output$t4a9_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      div(
        pickerInput(
          "t4a9_4t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[4]]]) |> sort(),
          choicesOpt = list(
            content = c(
              "",
              hoopR_espn_nba_teams |>
                filter(
                  display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[4]]]
                ) |>
                pull(dropdown_logo)
            )
          ),
          options = list(`dropdown-align-center` = TRUE,
                         placeholder = "Team"),
          selected = ""
        )
      )
    }
  })
  output$t4a10_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      div(
        pickerInput(
          "t4a10_4t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[4]]]) |> sort(),
          choicesOpt = list(
            content = c(
              "",
              hoopR_espn_nba_teams |>
                filter(
                  display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[4]]]
                ) |>
                pull(dropdown_logo)
            )
          ),
          options = list(`dropdown-align-center` = TRUE,
                         placeholder = "Team"),
          selected = ""
        )
      )
    }
  })
  
  # Traded Asset Protection Input
  # Team 1
  output$t1a1_2t_pro = renderUI({
    if(length(input$teamsInvolved) == 2){
      textInput(
        "t1a1_2t_pro", 
        ""
      )
    }
  })
  output$t1a2_2t_pro = renderUI({
    if(length(input$teamsInvolved) == 2){
      textInput(
        "t1a2_2t_pro", 
        ""
      )
    }
  })
  output$t1a3_2t_pro = renderUI({
    if(length(input$teamsInvolved) == 2){
      textInput(
        "t1a3_2t_pro", 
        ""
      )
    }
  })
  output$t1a4_2t_pro = renderUI({
    if(length(input$teamsInvolved) == 2){
      textInput(
        "t1a4_2t_pro", 
        ""
      )
    }
  })
  output$t1a5_2t_pro = renderUI({
    if(length(input$teamsInvolved) == 2){
      textInput(
        "t1a5_2t_pro", 
        ""
      )
    }
  })
  output$t1a6_2t_pro = renderUI({
    if(length(input$teamsInvolved) == 2){
      textInput(
        "t1a6_2t_pro", 
        ""
      )
    }
  })
  output$t1a7_2t_pro = renderUI({
    if(length(input$teamsInvolved) == 2){
      textInput(
        "t1a7_2t_pro", 
        ""
      )
    }
  })
  output$t1a8_2t_pro = renderUI({
    if(length(input$teamsInvolved) == 2){
      textInput(
        "t1a8_2t_pro", 
        ""
      )
    }
  })
  output$t1a9_2t_pro = renderUI({
    if(length(input$teamsInvolved) == 2){
      textInput(
        "t1a9_2t_pro", 
        ""
      )
    }
  })
  output$t1a10_2t_pro = renderUI({
    if(length(input$teamsInvolved) == 2){
      textInput(
        "t1a10_2t_pro", 
        ""
      )
    }
  })
  
  output$t1a1_3t_pro = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t1a1_3t_pro", 
        ""
      )
    }
  })
  output$t1a2_3t_pro = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t1a2_3t_pro", 
        ""
      )
    }
  })
  output$t1a3_3t_pro = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t1a3_3t_pro", 
        ""
      )
    }
  })
  output$t1a4_3t_pro = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t1a4_3t_pro", 
        ""
      )
    }
  })
  output$t1a5_3t_pro = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t1a5_3t_pro", 
        ""
      )
    }
  })
  output$t1a6_3t_pro = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t1a6_3t_pro", 
        ""
      )
    }
  })
  output$t1a7_3t_pro = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t1a7_3t_pro", 
        ""
      )
    }
  })
  output$t1a8_3t_pro = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t1a8_3t_pro", 
        ""
      )
    }
  })
  output$t1a9_3t_pro = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t1a9_3t_pro", 
        ""
      )
    }
  })
  output$t1a10_3t_pro = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t1a10_3t_pro", 
        ""
      )
    }
  })
  
  output$t1a1_4t_pro = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t1a1_4t_pro", 
        ""
      )
    }
  })
  output$t1a2_4t_pro = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t1a2_4t_pro", 
        ""
      )
    }
  })
  output$t1a3_4t_pro = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t1a3_4t_pro", 
        ""
      )
    }
  })
  output$t1a4_4t_pro = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t1a4_4t_pro", 
        ""
      )
    }
  })
  output$t1a5_4t_pro = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t1a5_4t_pro", 
        ""
      )
    }
  })
  output$t1a6_4t_pro = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t1a6_4t_pro", 
        ""
      )
    }
  })
  output$t1a7_4t_pro = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t1a7_4t_pro", 
        ""
      )
    }
  })
  output$t1a8_4t_pro = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t1a8_4t_pro", 
        ""
      )
    }
  })
  output$t1a9_4t_pro = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t1a9_4t_pro", 
        ""
      )
    }
  })
  output$t1a10_4t_pro = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t1a10_4t_pro", 
        ""
      )
    }
  })
  
  # Team 2
  output$t2a1_2t_pro = renderUI({
    if(length(input$teamsInvolved) == 2){
      textInput(
        "t2a1_2t_pro", 
        ""
      )
    }
  })
  output$t2a2_2t_pro = renderUI({
    if(length(input$teamsInvolved) == 2){
      textInput(
        "t2a2_2t_pro", 
        ""
      )
    }
  })
  output$t2a3_2t_pro = renderUI({
    if(length(input$teamsInvolved) == 2){
      textInput(
        "t2a3_2t_pro", 
        ""
      )
    }
  })
  output$t2a4_2t_pro = renderUI({
    if(length(input$teamsInvolved) == 2){
      textInput(
        "t2a4_2t_pro", 
        ""
      )
    }
  })
  output$t2a5_2t_pro = renderUI({
    if(length(input$teamsInvolved) == 2){
      textInput(
        "t2a5_2t_pro", 
        ""
      )
    }
  })
  output$t2a6_2t_pro = renderUI({
    if(length(input$teamsInvolved) == 2){
      textInput(
        "t2a6_2t_pro", 
        ""
      )
    }
  })
  output$t2a7_2t_pro = renderUI({
    if(length(input$teamsInvolved) == 2){
      textInput(
        "t2a7_2t_pro", 
        ""
      )
    }
  })
  output$t2a8_2t_pro = renderUI({
    if(length(input$teamsInvolved) == 2){
      textInput(
        "t2a8_2t_pro", 
        ""
      )
    }
  })
  output$t2a9_2t_pro = renderUI({
    if(length(input$teamsInvolved) == 2){
      textInput(
        "t2a9_2t_pro", 
        ""
      )
    }
  })
  output$t2a10_2t_pro = renderUI({
    if(length(input$teamsInvolved) == 2){
      textInput(
        "t2a10_2t_pro", 
        ""
      )
    }
  })
  
  output$t2a1_3t_pro = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t2a1_3t_pro", 
        ""
      )
    }
  })
  output$t2a2_3t_pro = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t2a2_3t_pro", 
        ""
      )
    }
  })
  output$t2a3_3t_pro = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t2a3_3t_pro", 
        ""
      )
    }
  })
  output$t2a4_3t_pro = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t2a4_3t_pro", 
        ""
      )
    }
  })
  output$t2a5_3t_pro = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t2a5_3t_pro", 
        ""
      )
    }
  })
  output$t2a6_3t_pro = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t2a6_3t_pro", 
        ""
      )
    }
  })
  output$t2a7_3t_pro = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t2a7_3t_pro", 
        ""
      )
    }
  })
  output$t2a8_3t_pro = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t2a8_3t_pro", 
        ""
      )
    }
  })
  output$t2a9_3t_pro = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t2a9_3t_pro", 
        ""
      )
    }
  })
  output$t2a10_3t_pro = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t2a10_3t_pro", 
        ""
      )
    }
  })
  
  output$t2a1_4t_pro = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t2a1_4t_pro", 
        ""
      )
    }
  })
  output$t2a2_4t_pro = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t2a2_4t_pro", 
        ""
      )
    }
  })
  output$t2a3_4t_pro = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t2a3_4t_pro", 
        ""
      )
    }
  })
  output$t2a4_4t_pro = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t2a4_4t_pro", 
        ""
      )
    }
  })
  output$t2a5_4t_pro = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t2a5_4t_pro", 
        ""
      )
    }
  })
  output$t2a6_4t_pro = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t2a6_4t_pro", 
        ""
      )
    }
  })
  output$t2a7_4t_pro = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t2a7_4t_pro", 
        ""
      )
    }
  })
  output$t2a8_4t_pro = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t2a8_4t_pro", 
        ""
      )
    }
  })
  output$t2a9_4t_pro = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t2a9_4t_pro", 
        ""
      )
    }
  })
  output$t2a10_4t_pro = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t2a10_4t_pro", 
        ""
      )
    }
  })
  
  # Team 3
  output$t3a1_3t_pro = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t3a1_3t_pro", 
        ""
      )
    }
  })
  output$t3a2_3t_pro = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t3a2_3t_pro", 
        ""
      )
    }
  })
  output$t3a3_3t_pro = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t3a3_3t_pro", 
        ""
      )
    }
  })
  output$t3a4_3t_pro = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t3a4_3t_pro", 
        ""
      )
    }
  })
  output$t3a5_3t_pro = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t3a5_3t_pro", 
        ""
      )
    }
  })
  output$t3a6_3t_pro = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t3a6_3t_pro", 
        ""
      )
    }
  })
  output$t3a7_3t_pro = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t3a7_3t_pro", 
        ""
      )
    }
  })
  output$t3a8_3t_pro = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t3a8_3t_pro", 
        ""
      )
    }
  })
  output$t3a9_3t_pro = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t3a9_3t_pro", 
        ""
      )
    }
  })
  output$t3a10_3t_pro = renderUI({
    if(length(input$teamsInvolved) == 3){
      textInput(
        "t3a10_3t_pro", 
        ""
      )
    }
  })
  
  output$t3a1_4t_pro = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t3a1_4t_pro", 
        ""
      )
    }
  })
  output$t3a2_4t_pro = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t3a2_4t_pro", 
        ""
      )
    }
  })
  output$t3a3_4t_pro = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t3a3_4t_pro", 
        ""
      )
    }
  })
  output$t3a4_4t_pro = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t3a4_4t_pro", 
        ""
      )
    }
  })
  output$t3a5_4t_pro = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t3a5_4t_pro", 
        ""
      )
    }
  })
  output$t3a6_4t_pro = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t3a6_4t_pro", 
        ""
      )
    }
  })
  output$t3a7_4t_pro = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t3a7_4t_pro", 
        ""
      )
    }
  })
  output$t3a8_4t_pro = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t3a8_4t_pro", 
        ""
      )
    }
  })
  output$t3a9_4t_pro = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t3a9_4t_pro", 
        ""
      )
    }
  })
  output$t3a10_4t_pro = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t3a10_4t_pro", 
        ""
      )
    }
  })
  
  # Team 4
  output$t4a1_4t_pro = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t4a1_4t_pro", 
        ""
      )
    }
  })
  output$t4a2_4t_pro = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t4a2_4t_pro", 
        ""
      )
    }
  })
  output$t4a3_4t_pro = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t4a3_4t_pro", 
        ""
      )
    }
  })
  output$t4a4_4t_pro = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t4a4_4t_pro", 
        ""
      )
    }
  })
  output$t4a5_4t_pro = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t4a5_4t_pro", 
        ""
      )
    }
  })
  output$t4a6_4t_pro = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t4a6_4t_pro", 
        ""
      )
    }
  })
  output$t4a7_4t_pro = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t4a7_4t_pro", 
        ""
      )
    }
  })
  output$t4a8_4t_pro = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t4a8_4t_pro", 
        ""
      )
    }
  })
  output$t4a9_4t_pro = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t4a9_4t_pro", 
        ""
      )
    }
  })
  output$t4a10_4t_pro = renderUI({
    if(length(input$teamsInvolved) == 4){
      textInput(
        "t4a10_4t_pro", 
        ""
      )
    }
  })
  
  t1_name = reactive(
    if(length(input$teamsInvolved) >= 1){
      input$teamsInvolved[[1]]
    }
  )

  t2_name = reactive(
    if(length(input$teamsInvolved) >= 2){
      input$teamsInvolved[[2]]
    }
  )

  t3_name = reactive(
    if(length(input$teamsInvolved) >= 3){
      input$teamsInvolved[[3]]
    }
  )

  t4_name = reactive(
    if(length(input$teamsInvolved) >= 4){
      input$teamsInvolved[[4]]
    }
  )

  # Fill df for new proposed trade
  # Team 1
  t1_players = reactive(
    if(length(input$teamsInvolved) == 2){
      data.frame(
        asset = c(
          input$t1p1_2t,
          input$t1p2_2t,
          input$t1p3_2t,
          input$t1p4_2t,
          input$t1p5_2t,
          input$t1p6_2t,
          input$t1p7_2t,
          input$t1p8_2t,
          input$t1p9_2t,
          input$t1p10_2t
        ),
        away_from_team = rep(t1_name(), 10),
        to_team = c(
          input$t1p1_2t_to,
          input$t1p2_2t_to,
          input$t1p3_2t_to,
          input$t1p4_2t_to,
          input$t1p5_2t_to,
          input$t1p6_2t_to,
          input$t1p7_2t_to,
          input$t1p8_2t_to,
          input$t1p9_2t_to,
          input$t1p10_2t_to
        ),
        note = rep("", 10)
      )
    }
    else if(length(input$teamsInvolved) == 3){
      data.frame(
        asset = c(
          input$t1p1_3t,
          input$t1p2_3t,
          input$t1p3_3t,
          input$t1p4_3t,
          input$t1p5_3t,
          input$t1p6_3t,
          input$t1p7_3t,
          input$t1p8_3t,
          input$t1p9_3t,
          input$t1p10_3t
        ),
        away_from_team = rep(t1_name(), 10),
        to_team = c(
          input$t1p1_3t_to,
          input$t1p2_3t_to,
          input$t1p3_3t_to,
          input$t1p4_3t_to,
          input$t1p5_3t_to,
          input$t1p6_3t_to,
          input$t1p7_3t_to,
          input$t1p8_3t_to,
          input$t1p9_3t_to,
          input$t1p10_3t_to
        ),
        note = rep("", 10)
      )
    }
    else if(length(input$teamsInvolved) == 4){
      data.frame(
        asset = c(
          input$t1p1_4t,
          input$t1p2_4t,
          input$t1p3_4t,
          input$t1p4_4t,
          input$t1p5_4t,
          input$t1p6_4t,
          input$t1p7_4t,
          input$t1p8_4t,
          input$t1p9_4t,
          input$t1p10_4t
        ),
        away_from_team = rep(t1_name(), 10),
        to_team = c(
          input$t1p1_4t_to,
          input$t1p2_4t_to,
          input$t1p3_4t_to,
          input$t1p4_4t_to,
          input$t1p5_4t_to,
          input$t1p6_4t_to,
          input$t1p7_4t_to,
          input$t1p8_4t_to,
          input$t1p9_4t_to,
          input$t1p10_4t_to
        ),
        note = rep("", 10)
      )
    }
    else if(length(input$teamsInvolved) == 1){
      data.frame(
        asset = c(
          paste0(input$t1p1_1t, " Waived"),
          paste0(input$t1p2_1t, " Waived"),
          paste0(input$t1p3_1t, " Waived"),
          paste0(input$t1p4_1t, " Waived"),
          paste0(input$t1p5_1t, " Waived"),
          paste0(input$t1p6_1t, " Waived"),
          paste0(input$t1p7_1t, " Waived"),
          paste0(input$t1p8_1t, " Waived"),
          paste0(input$t1p9_1t, " Waived"),
          paste0(input$t1p10_1t, " Waived")
        ),
        away_from_team = rep(t1_name(), 10),
        to_team = rep(t1_name(), 10),
        note = rep("", 10)
      ) |>
      mutate(
        asset = ifelse(asset == " Waived", "", asset)
      )
    }
  )
  

  t1_picks = reactive(
    if(length(input$teamsInvolved) == 2){
      data.frame(
        asset = c(
          input$t1a1_2t,
          input$t1a2_2t,
          input$t1a3_2t,
          input$t1a4_2t,
          input$t1a5_2t,
          input$t1a6_2t,
          input$t1a7_2t,
          input$t1a8_2t,
          input$t1a9_2t,
          input$t1a10_2t
        ),
        away_from_team = rep(t1_name(), 10),
        to_team = c(
          input$t1a1_2t_to,
          input$t1a2_2t_to,
          input$t1a3_2t_to,
          input$t1a4_2t_to,
          input$t1a5_2t_to,
          input$t1a6_2t_to,
          input$t1a7_2t_to,
          input$t1a8_2t_to,
          input$t1a9_2t_to,
          input$t1a10_2t_to
        ),
        note = c(
          input$t1a1_2t_pro,
          input$t1a2_2t_pro,
          input$t1a3_2t_pro,
          input$t1a4_2t_pro,
          input$t1a5_2t_pro,
          input$t1a6_2t_pro,
          input$t1a7_2t_pro,
          input$t1a8_2t_pro,
          input$t1a9_2t_pro,
          input$t1a10_2t_pro
        )
      )
    }
    else if(length(input$teamsInvolved) == 3){
      data.frame(
        asset = c(
          input$t1a1_3t,
          input$t1a2_3t,
          input$t1a3_3t,
          input$t1a4_3t,
          input$t1a5_3t,
          input$t1a6_3t,
          input$t1a7_3t,
          input$t1a8_3t,
          input$t1a9_3t,
          input$t1a10_3t
        ),
        away_from_team = rep(t1_name(), 10),
        to_team = c(
          input$t1a1_3t_to,
          input$t1a2_3t_to,
          input$t1a3_3t_to,
          input$t1a4_3t_to,
          input$t1a5_3t_to,
          input$t1a6_3t_to,
          input$t1a7_3t_to,
          input$t1a8_3t_to,
          input$t1a9_3t_to,
          input$t1a10_3t_to
        ),
        note = c(
          input$t1a1_3t_pro,
          input$t1a2_3t_pro,
          input$t1a3_3t_pro,
          input$t1a4_3t_pro,
          input$t1a5_3t_pro,
          input$t1a6_3t_pro,
          input$t1a7_3t_pro,
          input$t1a8_3t_pro,
          input$t1a9_3t_pro,
          input$t1a10_3t_pro
        )
      )
    }
    else if(length(input$teamsInvolved) == 4){
      data.frame(
        asset = c(
          input$t1a1_4t,
          input$t1a2_4t,
          input$t1a3_4t,
          input$t1a4_4t,
          input$t1a5_4t,
          input$t1a6_4t,
          input$t1a7_4t,
          input$t1a8_4t,
          input$t1a9_4t,
          input$t1a10_4t
        ),
        away_from_team = rep(t1_name(), 10),
        to_team = c(
          input$t1a1_4t_to,
          input$t1a2_4t_to,
          input$t1a3_4t_to,
          input$t1a4_4t_to,
          input$t1a5_4t_to,
          input$t1a6_4t_to,
          input$t1a7_4t_to,
          input$t1a8_4t_to,
          input$t1a9_4t_to,
          input$t1a10_4t_to
        ),
        note = c(
          input$t1a1_4t_pro,
          input$t1a2_4t_pro,
          input$t1a3_4t_pro,
          input$t1a4_4t_pro,
          input$t1a5_4t_pro,
          input$t1a6_4t_pro,
          input$t1a7_4t_pro,
          input$t1a8_4t_pro,
          input$t1a9_4t_pro,
          input$t1a10_4t_pro
        )
      )
    }
    else if(length(input$teamsInvolved) == 1){
      data.frame(
        asset = NA,
        away_from_team = NA,
        to_team = NA,
        note = NA
      )
    }
  )

  # Team 2
  t2_players = reactive(
    if(length(input$teamsInvolved) == 2){
      data.frame(
        asset = c(
          input$t2p1_2t,
          input$t2p2_2t,
          input$t2p3_2t,
          input$t2p4_2t,
          input$t2p5_2t,
          input$t2p6_2t,
          input$t2p7_2t,
          input$t2p8_2t,
          input$t2p9_2t,
          input$t2p10_2t
        ),
        away_from_team = rep(t2_name(), 10),
        to_team = c(
          input$t2p1_2t_to,
          input$t2p2_2t_to,
          input$t2p3_2t_to,
          input$t2p4_2t_to,
          input$t2p5_2t_to,
          input$t2p6_2t_to,
          input$t2p7_2t_to,
          input$t2p8_2t_to,
          input$t2p9_2t_to,
          input$t2p10_2t_to
        ),
        note = rep("", 10)
      )
    }
    else if(length(input$teamsInvolved) == 3){
      data.frame(
        asset = c(
          input$t2p1_3t,
          input$t2p2_3t,
          input$t2p3_3t,
          input$t2p4_3t,
          input$t2p5_3t,
          input$t2p6_3t,
          input$t2p7_3t,
          input$t2p8_3t,
          input$t2p9_3t,
          input$t2p10_3t
        ),
        away_from_team = rep(t2_name(), 10),
        to_team = c(
          input$t2p1_3t_to,
          input$t2p2_3t_to,
          input$t2p3_3t_to,
          input$t2p4_3t_to,
          input$t2p5_3t_to,
          input$t2p6_3t_to,
          input$t2p7_3t_to,
          input$t2p8_3t_to,
          input$t2p9_3t_to,
          input$t2p10_3t_to
        ),
        note = rep("", 10)
      )
    }
    else if(length(input$teamsInvolved) == 4){
      data.frame(
        asset = c(
          input$t2p1_4t,
          input$t2p2_4t,
          input$t2p3_4t,
          input$t2p4_4t,
          input$t2p5_4t,
          input$t2p6_4t,
          input$t2p7_4t,
          input$t2p8_4t,
          input$t2p9_4t,
          input$t2p10_4t
        ),
        away_from_team = rep(t2_name(), 10),
        to_team = c(
          input$t2p1_4t_to,
          input$t2p2_4t_to,
          input$t2p3_4t_to,
          input$t2p4_4t_to,
          input$t2p5_4t_to,
          input$t2p6_4t_to,
          input$t2p7_4t_to,
          input$t2p8_4t_to,
          input$t2p9_4t_to,
          input$t2p10_4t_to
        ),
        note = rep("", 10)
      )
    }
  )

  t2_picks = reactive(
    if(length(input$teamsInvolved) == 2){
      data.frame(
        asset = c(
          input$t2a1_2t,
          input$t2a2_2t,
          input$t2a3_2t,
          input$t2a4_2t,
          input$t2a5_2t,
          input$t2a6_2t,
          input$t2a7_2t,
          input$t2a8_2t,
          input$t2a9_2t,
          input$t2a10_2t
        ),
        away_from_team = rep(t2_name(), 10),
        to_team = c(
          input$t2a1_2t_to,
          input$t2a2_2t_to,
          input$t2a3_2t_to,
          input$t2a4_2t_to,
          input$t2a5_2t_to,
          input$t2a6_2t_to,
          input$t2a7_2t_to,
          input$t2a8_2t_to,
          input$t2a9_2t_to,
          input$t2a10_2t_to
        ),
        note = c(
          input$t2a1_2t_pro,
          input$t2a2_2t_pro,
          input$t2a3_2t_pro,
          input$t2a4_2t_pro,
          input$t2a5_2t_pro,
          input$t2a6_2t_pro,
          input$t2a7_2t_pro,
          input$t2a8_2t_pro,
          input$t2a9_2t_pro,
          input$t2a10_2t_pro
        )
      )
    }
    else if(length(input$teamsInvolved) == 3){
      data.frame(
        asset = c(
          input$t2a1_3t,
          input$t2a2_3t,
          input$t2a3_3t,
          input$t2a4_3t,
          input$t2a5_3t,
          input$t2a6_3t,
          input$t2a7_3t,
          input$t2a8_3t,
          input$t2a9_3t,
          input$t2a10_3t
        ),
        away_from_team = rep(t2_name(), 10),
        to_team = c(
          input$t2a1_3t_to,
          input$t2a2_3t_to,
          input$t2a3_3t_to,
          input$t2a4_3t_to,
          input$t2a5_3t_to,
          input$t2a6_3t_to,
          input$t2a7_3t_to,
          input$t2a8_3t_to,
          input$t2a9_3t_to,
          input$t2a10_3t_to
        ),
        note = c(
          input$t2a1_3t_pro,
          input$t2a2_3t_pro,
          input$t2a3_3t_pro,
          input$t2a4_3t_pro,
          input$t2a5_3t_pro,
          input$t2a6_3t_pro,
          input$t2a7_3t_pro,
          input$t2a8_3t_pro,
          input$t2a9_3t_pro,
          input$t2a10_3t_pro
        )
      )
    }
    else if(length(input$teamsInvolved) == 4){
      data.frame(
        asset = c(
          input$t2a1_4t,
          input$t2a2_4t,
          input$t2a3_4t,
          input$t2a4_4t,
          input$t2a5_4t,
          input$t2a6_4t,
          input$t2a7_4t,
          input$t2a8_4t,
          input$t2a9_4t,
          input$t2a10_4t
        ),
        away_from_team = rep(t1_name(), 10),
        to_team = c(
          input$t2a1_4t_to,
          input$t2a2_4t_to,
          input$t2a3_4t_to,
          input$t2a4_4t_to,
          input$t2a5_4t_to,
          input$t2a6_4t_to,
          input$t2a7_4t_to,
          input$t2a8_4t_to,
          input$t2a9_4t_to,
          input$t2a10_4t_to
        ),
        note = c(
          input$t2a1_4t_pro,
          input$t2a2_4t_pro,
          input$t2a3_4t_pro,
          input$t2a4_4t_pro,
          input$t2a5_4t_pro,
          input$t2a6_4t_pro,
          input$t2a7_4t_pro,
          input$t2a8_4t_pro,
          input$t2a9_4t_pro,
          input$t2a10_4t_pro
        )
      )
    }
  )

  # Team 3
  t3_players = reactive(
    if(length(input$teamsInvolved) == 3){
      data.frame(
        asset = c(
          input$t3p1_3t,
          input$t3p2_3t,
          input$t3p3_3t,
          input$t3p4_3t,
          input$t3p5_3t,
          input$t3p6_3t,
          input$t3p7_3t,
          input$t3p8_3t,
          input$t3p9_3t,
          input$t3p10_3t
        ),
        away_from_team = rep(t3_name(), 10),
        to_team = c(
          input$t3p1_3t_to,
          input$t3p2_3t_to,
          input$t3p3_3t_to,
          input$t3p4_3t_to,
          input$t3p5_3t_to,
          input$t3p6_3t_to,
          input$t3p7_3t_to,
          input$t3p8_3t_to,
          input$t3p9_3t_to,
          input$t3p10_3t_to
        ),
        note = rep("", 10)
      )
    }
    else if(length(input$teamsInvolved) == 4){
      data.frame(
        asset = c(
          input$t3p1_4t,
          input$t3p2_4t,
          input$t3p3_4t,
          input$t3p4_4t,
          input$t3p5_4t,
          input$t3p6_4t,
          input$t3p7_4t,
          input$t3p8_4t,
          input$t3p9_4t,
          input$t3p10_4t
        ),
        away_from_team = rep(t3_name(), 10),
        to_team = c(
          input$t3p1_4t_to,
          input$t3p2_4t_to,
          input$t3p3_4t_to,
          input$t3p4_4t_to,
          input$t3p5_4t_to,
          input$t3p6_4t_to,
          input$t3p7_4t_to,
          input$t3p8_4t_to,
          input$t3p9_4t_to,
          input$t3p10_4t_to
        ),
        note = rep("", 10)
      )
    }
  )

  t3_picks = reactive(
    if(length(input$teamsInvolved) == 3){
      data.frame(
        asset = c(
          input$t3a1_3t,
          input$t3a2_3t,
          input$t3a3_3t,
          input$t3a4_3t,
          input$t3a5_3t,
          input$t3a6_3t,
          input$t3a7_3t,
          input$t3a8_3t,
          input$t3a9_3t,
          input$t3a10_3t
        ),
        away_from_team = rep(t3_name(), 10),
        to_team = c(
          input$t3a1_3t_to,
          input$t3a2_3t_to,
          input$t3a3_3t_to,
          input$t3a4_3t_to,
          input$t3a5_3t_to,
          input$t3a6_3t_to,
          input$t3a7_3t_to,
          input$t3a8_3t_to,
          input$t3a9_3t_to,
          input$t3a10_3t_to
        ),
        note = c(
          input$t3a1_3t_pro,
          input$t3a2_3t_pro,
          input$t3a3_3t_pro,
          input$t3a4_3t_pro,
          input$t3a5_3t_pro,
          input$t3a6_3t_pro,
          input$t3a7_3t_pro,
          input$t3a8_3t_pro,
          input$t3a9_3t_pro,
          input$t3a10_3t_pro
        )
      )
    }
    else if(length(input$teamsInvolved) == 4){
      data.frame(
        asset = c(
          input$t3a1_4t,
          input$t3a2_4t,
          input$t3a3_4t,
          input$t3a4_4t,
          input$t3a5_4t,
          input$t3a6_4t,
          input$t3a7_4t,
          input$t3a8_4t,
          input$t3a9_4t,
          input$t3a10_4t
        ),
        away_from_team = rep(t3_name(), 10),
        to_team = c(
          input$t3a1_4t_to,
          input$t3a2_4t_to,
          input$t3a3_4t_to,
          input$t3a4_4t_to,
          input$t3a5_4t_to,
          input$t3a6_4t_to,
          input$t3a7_4t_to,
          input$t3a8_4t_to,
          input$t3a9_4t_to,
          input$t3a10_4t_to
        ),
        note = c(
          input$t3a1_4t_pro,
          input$t3a2_4t_pro,
          input$t3a3_4t_pro,
          input$t3a4_4t_pro,
          input$t3a5_4t_pro,
          input$t3a6_4t_pro,
          input$t3a7_4t_pro,
          input$t3a8_4t_pro,
          input$t3a9_4t_pro,
          input$t3a10_4t_pro
        )
      )
    }
  )

  # Team 4
  t4_players = reactive(
    if(length(input$teamsInvolved) == 4){
      data.frame(
        asset = c(
          input$t4p1_4t,
          input$t4p2_4t,
          input$t4p3_4t,
          input$t4p4_4t,
          input$t4p5_4t,
          input$t4p6_4t,
          input$t4p7_4t,
          input$t4p8_4t,
          input$t4p9_4t,
          input$t4p10_4t
        ),
        away_from_team = rep(t4_name(), 10),
        to_team = c(
          input$t4p1_4t_to,
          input$t4p2_4t_to,
          input$t4p3_4t_to,
          input$t4p4_4t_to,
          input$t4p5_4t_to,
          input$t4p6_4t_to,
          input$t4p7_4t_to,
          input$t4p8_4t_to,
          input$t4p9_4t_to,
          input$t4p10_4t_to
        ),
        note = rep("", 10)
      )
    }
  )

  t4_picks = reactive(
    if(length(input$teamsInvolved) == 4){
      data.frame(
        asset = c(
          input$t4a1_4t,
          input$t4a2_4t,
          input$t4a3_4t,
          input$t4a4_4t,
          input$t4a5_4t,
          input$t4a6_4t,
          input$t4a7_4t,
          input$t4a8_4t,
          input$t4a9_4t,
          input$t4a10_4t
        ),
        away_from_team = rep(t4_name(), 10),
        to_team = c(
          input$t4a1_4t_to,
          input$t4a2_4t_to,
          input$t4a3_4t_to,
          input$t4a4_4t_to,
          input$t4a5_4t_to,
          input$t4a6_4t_to,
          input$t4a7_4t_to,
          input$t4a8_4t_to,
          input$t4a9_4t_to,
          input$t4a10_4t_to
        ),
        note = c(
          input$t4a1_4t_pro,
          input$t4a2_4t_pro,
          input$t4a3_4t_pro,
          input$t4a4_4t_pro,
          input$t4a5_4t_pro,
          input$t4a6_4t_pro,
          input$t4a7_4t_pro,
          input$t4a8_4t_pro,
          input$t4a9_4t_pro,
          input$t4a10_4t_pro
        )
      )
    }
  )

  proposedTrade = reactive(
    if(length(input$teamsInvolved) == 2){
      rbind(t1_players(), t2_players(), t1_picks(), t2_picks()) |>
        filter(asset != "")
    }
    else if(length(input$teamsInvolved) == 3){
      rbind(t1_players(), t2_players(), t3_players(),
            t1_picks(), t2_picks(), t3_picks()) |>
        filter(asset != "")
    }
    else if(length(input$teamsInvolved) == 4){
      rbind(t1_players(), t2_players(), t3_players(), t4_players(),
            t1_picks(), t2_picks(), t3_picks(), t4_picks()) |>
        filter(asset != "")
    }
    else if(length(input$teamsInvolved) == 1){
      rbind(t1_players()) |>
        filter(asset != "")
    }
  )
  
  proposedTrade_players = reactive(
    proposedTrade() |>
      group_by(to_team) |>
      arrange(asset) |>
      filter(
        substr(asset, 1, 2) != '20',
        substr(asset, 1, 19) != "Cash Considerations"
      ) |>
      mutate(
        players = paste(asset, collapse = ", ")
      ) |>
      select(to_team, players) |>
      unique()
  )
  
  proposedTrade_picks = reactive(
    proposedTrade() |>
      group_by(to_team) |>
      arrange(asset) |>
      filter(
        substr(asset, 1, 2) == '20' | 
          substr(asset, 1, 19) == "Cash Considerations"
      ) |>
      mutate(
        picks = paste(asset, collapse = ", "),
        notes = paste(note, collapse = ", ")
      ) |>
      select(to_team, picks, notes) |>
      unique()
  )
  
  proposedTrade_incoming_by_team = reactive(
    full_join(
        proposedTrade_players(), 
        proposedTrade_picks(), 
        by = c('to_team')
      ) |>
      mutate(
        players = ifelse(is.na(players), "", players),
        picks = ifelse(is.na(picks), "", picks),
        notes = ifelse(is.na(notes), "", notes)
      ) |>
      ungroup() |>
      arrange(to_team) %>%
      left_join(
        hoopR_espn_nba_teams |> select(display_name, logo), 
        by = c("to_team" = "display_name")
      ) |>
      select(logo, to_team, players, picks, notes)
  )
  
  # Launch confirmation
  observeEvent(
    input$submit, {
    showModal(
      modalDialog(
        easyClose = TRUE,
        size = "l",
        title = "Confirm this trade?",
        render_gt(
          width = "100%",
          proposedTrade_incoming_by_team() |>
            mutate(
              players = str_replace_all(players, ", ", "<br>"),
              picks = str_replace_all(picks, ", ", "<br>"),
              notes = str_replace_all(notes, ", ", "<br>"),
              notes = str_replace_all(notes, "NA", " ")
            ) |>
            gt() |>
            gt_img_rows(columns = logo, height = 25) |>
            gt_theme_guardian() |>
            #vertical align in players and picks cells
            tab_style(
              style = "vertical-align:top",
              locations = cells_body(columns = c("players", "picks", "notes"))
            ) |>
            tab_style(
              style = list(cell_borders(sides = "top", color = "white")),
              locations = cells_column_labels()
            ) |>
            cols_hide(columns = c(to_team)) |>
            fmt_markdown(columns = c("players", "picks", "notes")) |>
            cols_width(
              players ~ pct(25),
              logo ~ pct(8),
              picks ~ pct(30),
              notes ~ pct(37)
            ) |>
            cols_align(
              align = "left",
              columns = c("notes", "picks")
            ) |>
            cols_label(logo = "Team",
                       players = "Players",
                       picks = "Picks",
                       notes = "Notes")
        ),
        footer = tagList(
          actionButton("confirm", "Confirm Trade"),
          actionButton("notyet", "Not Yet")
        )
      )
    )
  })
  
  # Not yet button from trade submit
  observeEvent(input$notyet, {
    removeModal()
  })
  
  # Finish button from trade submit
  observeEvent(input$finish, {
    session$reload()
  })
  
  # Confirm trade
  observeEvent(
    input$confirm, {
      new_transID = read_sheet(ss, sheet = "All_TL") |>
        pull(trans_ID) |>
        substr(1,3) |>
        max() |>
        as.numeric()+1
      
      enteredTrade = proposedTrade() |>
        mutate(
          trans_ID = paste0(new_transID, runif(1, 0, 99) |> round(digits = 0) |> sprintf(fmt = "%02d")),
          status = "Pending"
        ) |>
        select(trans_ID, asset, away_from_team, to_team, note, status)
    
    sheet_append(ss, enteredTrade, sheet = "All_TL")
    removeModal()
    showModal(
      modalDialog(
        easyClose = FALSE,
        title = "Success!",
        h4("Your transaction ID for this trade is:"),
        h2(enteredTrade |> pull(trans_ID) |> unique()),
        h2(""),
        h6("This is the last time you will see this ID and you will not be able to recover it later."),
        footer = tagList(
          actionButton("finish", "Close this Popup")
        )
      )
    )
  })
  
  
  
  
}


shinyApp(ui, server)