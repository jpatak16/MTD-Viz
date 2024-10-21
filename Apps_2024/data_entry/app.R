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
            style = "text-align: left; width: 60%; padding-left: 1.2em; padding-right: .6em;",
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
            h5("Add Protection?", style = "font-size: 1.1rem; width: 110%;"),
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
            style = "text-align: left; width: 60%; padding-left: 1.2em; padding-right: .6em;",
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
            style = "text-align: left; width: 35%; padding-left: .6em; padding-right: 1.2em;",
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
            style = "text-align: left; width: 60%; padding-left: 1.2em; padding-right: .6em;",
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
            h5("Add Protection?", style = "font-size: 1.1rem; width: 110%;"),
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
        class = "col-lg-4 col-md-6",
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
        class = "col-lg-4 col-md-6",
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
        class = "col-lg-4 col-md-6",
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
        class = "col-lg-4 col-md-6",
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
  )
  # Five Team Panel
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
  
  output$team1_logo = output$team1_logo2 = output$team1_logo3 = output$team1_logo4 = renderText({
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
  
  output$team2_logo = output$team2_logo3 = output$team2_logo4 = renderText({
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
  
  output$team3_logo = output$team3_logo4 = renderText({
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
  
  output$team4_logo = renderText({
    paste0(
      '<img src ="', 
      logo4_url(), 
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
  
  team6_players <- reactive(
    if(length(input$teamsInvolved) < 6){}
    else{
      read_sheet(
        ss,
        sheet = hoopR_espn_nba_teams |>
          filter(
            display_name == input$teamsInvolved[[6]]
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
  output$t1p1_2t = renderUI({
    if(length(input$teamsInvolved) == 2){
      selectizeInput(
        "t1p1_2t", 
        "", 
        choices = team1_players(), 
        options = list(create = TRUE)
      )
    }
  })
  output$t1p2_2t = renderUI({
    if(length(input$teamsInvolved) == 2){
      selectizeInput(
        "t1p2_2t", 
        "", 
        choices = team1_players(), 
        options = list(create = TRUE)
      )
    }
  })
  output$t1p3_2t = renderUI({
    if(length(input$teamsInvolved) == 2){
      selectizeInput(
        "t1p3_2t", 
        "", 
        choices = team1_players(), 
        options = list(create = TRUE)
      )
    }
  })
  output$t1p4_2t = renderUI({
    if(length(input$teamsInvolved) == 2){
      selectizeInput(
        "t1p4_2t", 
        "", 
        choices = team1_players(), 
        options = list(create = TRUE)
      )
    }
  })
  output$t1p5_2t = renderUI({
    if(length(input$teamsInvolved) == 2){
      selectizeInput(
        "t1p5_2t", 
        "", 
        choices = team1_players(), 
        options = list(create = TRUE)
      )
    }
  })
  output$t1p6_2t = renderUI({
    if(length(input$teamsInvolved) == 2){
      selectizeInput(
        "t1p6_2t", 
        "", 
        choices = team1_players(), 
        options = list(create = TRUE)
      )
    }
  })
  output$t1p7_2t = renderUI({
    if(length(input$teamsInvolved) == 2){
      selectizeInput(
        "t1p7_2t", 
        "", 
        choices = team1_players(), 
        options = list(create = TRUE)
      )
    }
  })
  output$t1p8_2t = renderUI({
    if(length(input$teamsInvolved) == 2){
      selectizeInput(
        "t1p8_2t", 
        "", 
        choices = team1_players(), 
        options = list(create = TRUE)
      )
    }
  })
  output$t1p9_2t = renderUI({
    if(length(input$teamsInvolved) == 2){
      selectizeInput(
        "t1p9_2t", 
        "", 
        choices = team1_players(), 
        options = list(create = TRUE)
      )
    }
  })
  output$t1p10_2t = renderUI({
    if(length(input$teamsInvolved) == 2){
      selectizeInput(
        "t1p10_2t", 
        "", 
        choices = team1_players(), 
        options = list(create = TRUE)
      )
    }
  })
  
  output$t1p1_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      selectizeInput(
        "t1p1_3t", 
        "", 
        choices = team1_players(), 
        options = list(create = TRUE)
      )
    }
  })
  output$t1p2_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      selectizeInput(
        "t1p2_3t", 
        "", 
        choices = team1_players(), 
        options = list(create = TRUE)
      )
    }
  })
  output$t1p3_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      selectizeInput(
        "t1p3_3t", 
        "", 
        choices = team1_players(), 
        options = list(create = TRUE)
      )
    }
  })
  output$t1p4_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      selectizeInput(
        "t1p4_3t", 
        "", 
        choices = team1_players(), 
        options = list(create = TRUE)
      )
    }
  })
  output$t1p5_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      selectizeInput(
        "t1p5_3t", 
        "", 
        choices = team1_players(), 
        options = list(create = TRUE)
      )
    }
  })
  output$t1p6_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      selectizeInput(
        "t1p6_3t", 
        "", 
        choices = team1_players(), 
        options = list(create = TRUE)
      )
    }
  })
  output$t1p7_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      selectizeInput(
        "t1p7_3t", 
        "", 
        choices = team1_players(), 
        options = list(create = TRUE)
      )
    }
  })
  output$t1p8_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      selectizeInput(
        "t1p8_3t", 
        "", 
        choices = team1_players(), 
        options = list(create = TRUE)
      )
    }
  })
  output$t1p9_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      selectizeInput(
        "t1p9_3t", 
        "", 
        choices = team1_players(), 
        options = list(create = TRUE)
      )
    }
  })
  output$t1p10_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      selectizeInput(
        "t1p10_3t", 
        "", 
        choices = team1_players(), 
        options = list(create = TRUE)
      )
    }
  })
  
  output$t1p1_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      selectizeInput(
        "t1p1_4t", 
        "", 
        choices = team1_players(), 
        options = list(create = TRUE)
      )
    }
  })
  output$t1p2_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      selectizeInput(
        "t1p2_4t", 
        "", 
        choices = team1_players(), 
        options = list(create = TRUE)
      )
    }
  })
  output$t1p3_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      selectizeInput(
        "t1p3_4t", 
        "", 
        choices = team1_players(), 
        options = list(create = TRUE)
      )
    }
  })
  output$t1p4_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      selectizeInput(
        "t1p4_4t", 
        "", 
        choices = team1_players(), 
        options = list(create = TRUE)
      )
    }
  })
  output$t1p5_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      selectizeInput(
        "t1p5_4t", 
        "", 
        choices = team1_players(), 
        options = list(create = TRUE)
      )
    }
  })
  output$t1p6_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      selectizeInput(
        "t1p6_4t", 
        "", 
        choices = team1_players(), 
        options = list(create = TRUE)
      )
    }
  })
  output$t1p7_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      selectizeInput(
        "t1p7_4t", 
        "", 
        choices = team1_players(), 
        options = list(create = TRUE)
      )
    }
  })
  output$t1p8_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      selectizeInput(
        "t1p8_4t", 
        "", 
        choices = team1_players(), 
        options = list(create = TRUE)
      )
    }
  })
  output$t1p9_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      selectizeInput(
        "t1p9_4t", 
        "", 
        choices = team1_players(), 
        options = list(create = TRUE)
      )
    }
  })
  output$t1p10_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      selectizeInput(
        "t1p10_4t", 
        "", 
        choices = team1_players(), 
        options = list(create = TRUE)
      )
    }
  })
  
  # Team 2 
  output$t2p1_2t = renderUI({
    if(length(input$teamsInvolved) == 2){
      selectizeInput(
        "t2p1_2t", 
        "", 
        choices = team2_players(), 
        options = list(create = TRUE)
      )
    }
  })
  output$t2p2_2t = renderUI({
    if(length(input$teamsInvolved) == 2){
      selectizeInput(
        "t2p2_2t", 
        "", 
        choices = team2_players(), 
        options = list(create = TRUE)
      )
    }
  })
  output$t2p3_2t = renderUI({
    if(length(input$teamsInvolved) == 2){
      selectizeInput(
        "t2p3_2t", 
        "", 
        choices = team2_players(), 
        options = list(create = TRUE)
      )
    }
  })
  output$t2p4_2t = renderUI({
    if(length(input$teamsInvolved) == 2){
      selectizeInput(
        "t2p4_2t", 
        "", 
        choices = team2_players(), 
        options = list(create = TRUE)
      )
    }
  })
  output$t2p5_2t = renderUI({
    if(length(input$teamsInvolved) == 2){
      selectizeInput(
        "t2p5_2t", 
        "", 
        choices = team2_players(), 
        options = list(create = TRUE)
      )
    }
  })
  output$t2p6_2t = renderUI({
    if(length(input$teamsInvolved) == 2){
      selectizeInput(
        "t2p6_2t", 
        "", 
        choices = team2_players(), 
        options = list(create = TRUE)
      )
    }
  })
  output$t2p7_2t = renderUI({
    if(length(input$teamsInvolved) == 2){
      selectizeInput(
        "t2p7_2t", 
        "", 
        choices = team2_players(), 
        options = list(create = TRUE)
      )
    }
  })
  output$t2p8_2t = renderUI({
    if(length(input$teamsInvolved) == 2){
      selectizeInput(
        "t2p8_2t", 
        "", 
        choices = team2_players(), 
        options = list(create = TRUE)
      )
    }
  })
  output$t2p9_2t = renderUI({
    if(length(input$teamsInvolved) == 2){
      selectizeInput(
        "t2p9_2t", 
        "", 
        choices = team2_players(), 
        options = list(create = TRUE)
      )
    }
  })
  output$t2p10_2t = renderUI({
    if(length(input$teamsInvolved) == 2){
      selectizeInput(
        "t2p10_2t", 
        "", 
        choices = team2_players(), 
        options = list(create = TRUE)
      )
    }
  })
  
  output$t2p1_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      selectizeInput(
        "t2p1_3t", 
        "", 
        choices = team2_players(), 
        options = list(create = TRUE)
      )
    }
  })
  output$t2p2_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      selectizeInput(
        "t2p2_3t", 
        "", 
        choices = team2_players(), 
        options = list(create = TRUE)
      )
    }
  })
  output$t2p3_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      selectizeInput(
        "t2p3_3t", 
        "", 
        choices = team2_players(), 
        options = list(create = TRUE)
      )
    }
  })
  output$t2p4_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      selectizeInput(
        "t2p4_3t", 
        "", 
        choices = team2_players(), 
        options = list(create = TRUE)
      )
    }
  })
  output$t2p5_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      selectizeInput(
        "t2p5_3t", 
        "", 
        choices = team2_players(), 
        options = list(create = TRUE)
      )
    }
  })
  output$t2p6_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      selectizeInput(
        "t2p6_3t", 
        "", 
        choices = team2_players(), 
        options = list(create = TRUE)
      )
    }
  })
  output$t2p7_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      selectizeInput(
        "t2p7_3t", 
        "", 
        choices = team2_players(), 
        options = list(create = TRUE)
      )
    }
  })
  output$t2p8_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      selectizeInput(
        "t2p8_3t", 
        "", 
        choices = team2_players(), 
        options = list(create = TRUE)
      )
    }
  })
  output$t2p9_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      selectizeInput(
        "t2p9_3t", 
        "", 
        choices = team2_players(), 
        options = list(create = TRUE)
      )
    }
  })
  output$t2p10_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      selectizeInput(
        "t2p10_3t", 
        "", 
        choices = team2_players(), 
        options = list(create = TRUE)
      )
    }
  })
  
  output$t2p1_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      selectizeInput(
        "t2p1_4t", 
        "", 
        choices = team2_players(), 
        options = list(create = TRUE)
      )
    }
  })
  output$t2p2_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      selectizeInput(
        "t2p2_4t", 
        "", 
        choices = team2_players(), 
        options = list(create = TRUE)
      )
    }
  })
  output$t2p3_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      selectizeInput(
        "t2p3_4t", 
        "", 
        choices = team2_players(), 
        options = list(create = TRUE)
      )
    }
  })
  output$t2p4_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      selectizeInput(
        "t2p4_4t", 
        "", 
        choices = team2_players(), 
        options = list(create = TRUE)
      )
    }
  })
  output$t2p5_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      selectizeInput(
        "t2p5_4t", 
        "", 
        choices = team2_players(), 
        options = list(create = TRUE)
      )
    }
  })
  output$t2p6_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      selectizeInput(
        "t2p6_4t", 
        "", 
        choices = team2_players(), 
        options = list(create = TRUE)
      )
    }
  })
  output$t2p7_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      selectizeInput(
        "t2p7_4t", 
        "", 
        choices = team2_players(), 
        options = list(create = TRUE)
      )
    }
  })
  output$t2p8_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      selectizeInput(
        "t2p8_4t", 
        "", 
        choices = team2_players(), 
        options = list(create = TRUE)
      )
    }
  })
  output$t2p9_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      selectizeInput(
        "t2p9_4t", 
        "", 
        choices = team2_players(), 
        options = list(create = TRUE)
      )
    }
  })
  output$t2p10_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      selectizeInput(
        "t2p10_4t", 
        "", 
        choices = team2_players(), 
        options = list(create = TRUE)
      )
    }
  })
  
  # Team 3
  output$t3p1_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      selectizeInput(
        "t3p1_3t", 
        "", 
        choices = team3_players(), 
        options = list(create = TRUE)
      )
    }
  })
  output$t3p2_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      selectizeInput(
        "t3p2_3t", 
        "", 
        choices = team3_players(), 
        options = list(create = TRUE)
      )
    }
  })
  output$t3p3_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      selectizeInput(
        "t3p3_3t", 
        "", 
        choices = team3_players(), 
        options = list(create = TRUE)
      )
    }
  })
  output$t3p4_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      selectizeInput(
        "t3p4_3t", 
        "", 
        choices = team3_players(), 
        options = list(create = TRUE)
      )
    }
  })
  output$t3p5_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      selectizeInput(
        "t3p5_3t", 
        "", 
        choices = team3_players(), 
        options = list(create = TRUE)
      )
    }
  })
  output$t3p6_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      selectizeInput(
        "t3p6_3t", 
        "", 
        choices = team3_players(), 
        options = list(create = TRUE)
      )
    }
  })
  output$t3p7_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      selectizeInput(
        "t3p7_3t", 
        "", 
        choices = team3_players(), 
        options = list(create = TRUE)
      )
    }
  })
  output$t3p8_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      selectizeInput(
        "t3p8_3t", 
        "", 
        choices = team3_players(), 
        options = list(create = TRUE)
      )
    }
  })
  output$t3p9_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      selectizeInput(
        "t3p9_3t", 
        "", 
        choices = team3_players(), 
        options = list(create = TRUE)
      )
    }
  })
  output$t3p10_3t = renderUI({
    if(length(input$teamsInvolved) == 3){
      selectizeInput(
        "t3p10_3t", 
        "", 
        choices = team3_players(), 
        options = list(create = TRUE)
      )
    }
  })
  
  output$t3p1_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      selectizeInput(
        "t3p1_4t", 
        "", 
        choices = team3_players(), 
        options = list(create = TRUE)
      )
    }
  })
  output$t3p2_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      selectizeInput(
        "t3p2_4t", 
        "", 
        choices = team3_players(), 
        options = list(create = TRUE)
      )
    }
  })
  output$t3p3_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      selectizeInput(
        "t3p3_4t", 
        "", 
        choices = team3_players(), 
        options = list(create = TRUE)
      )
    }
  })
  output$t3p4_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      selectizeInput(
        "t3p4_4t", 
        "", 
        choices = team3_players(), 
        options = list(create = TRUE)
      )
    }
  })
  output$t3p5_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      selectizeInput(
        "t3p5_4t", 
        "", 
        choices = team3_players(), 
        options = list(create = TRUE)
      )
    }
  })
  output$t3p6_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      selectizeInput(
        "t3p6_4t", 
        "", 
        choices = team3_players(), 
        options = list(create = TRUE)
      )
    }
  })
  output$t3p7_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      selectizeInput(
        "t3p7_4t", 
        "", 
        choices = team3_players(), 
        options = list(create = TRUE)
      )
    }
  })
  output$t3p8_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      selectizeInput(
        "t3p8_4t", 
        "", 
        choices = team3_players(), 
        options = list(create = TRUE)
      )
    }
  })
  output$t3p9_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      selectizeInput(
        "t3p9_4t", 
        "", 
        choices = team3_players(), 
        options = list(create = TRUE)
      )
    }
  })
  output$t3p10_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      selectizeInput(
        "t3p10_4t", 
        "", 
        choices = team3_players(), 
        options = list(create = TRUE)
      )
    }
  })
  
  # Team 4
  output$t4p1_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      selectizeInput(
        "t4p1_4t", 
        "", 
        choices = team4_players(), 
        options = list(create = TRUE)
      )
    }
  })
  output$t4p2_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      selectizeInput(
        "t4p2_4t", 
        "", 
        choices = team4_players(), 
        options = list(create = TRUE)
      )
    }
  })
  output$t4p3_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      selectizeInput(
        "t4p3_4t", 
        "", 
        choices = team4_players(), 
        options = list(create = TRUE)
      )
    }
  })
  output$t4p4_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      selectizeInput(
        "t4p4_4t", 
        "", 
        choices = team4_players(), 
        options = list(create = TRUE)
      )
    }
  })
  output$t4p5_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      selectizeInput(
        "t4p5_4t", 
        "", 
        choices = team4_players(), 
        options = list(create = TRUE)
      )
    }
  })
  output$t4p6_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      selectizeInput(
        "t4p6_4t", 
        "", 
        choices = team4_players(), 
        options = list(create = TRUE)
      )
    }
  })
  output$t4p7_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      selectizeInput(
        "t4p7_4t", 
        "", 
        choices = team4_players(), 
        options = list(create = TRUE)
      )
    }
  })
  output$t4p8_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      selectizeInput(
        "t4p8_4t", 
        "", 
        choices = team4_players(), 
        options = list(create = TRUE)
      )
    }
  })
  output$t4p9_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      selectizeInput(
        "t4p9_4t", 
        "", 
        choices = team4_players(), 
        options = list(create = TRUE)
      )
    }
  })
  output$t4p10_4t = renderUI({
    if(length(input$teamsInvolved) == 4){
      selectizeInput(
        "t4p10_4t", 
        "", 
        choices = team4_players(), 
        options = list(create = TRUE)
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
  output$t1p1_2t_to = renderUI({
    if(length(input$teamsInvolved) == 2){
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
        options = list(`dropdown-align-center` = TRUE),
      )
    }
  })
  output$t1p2_2t_to = renderUI({
    if(length(input$teamsInvolved) == 2){
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
        options = list(`dropdown-align-center` = TRUE),
      )
    }
  })
  output$t1p3_2t_to = renderUI({
    if(length(input$teamsInvolved) == 2){
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
        options = list(`dropdown-align-center` = TRUE),
      )
    }
  })
  output$t1p4_2t_to = renderUI({
    if(length(input$teamsInvolved) == 2){
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
        options = list(`dropdown-align-center` = TRUE),
      )
    }
  })
  output$t1p5_2t_to = renderUI({
    if(length(input$teamsInvolved) == 2){
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
        options = list(`dropdown-align-center` = TRUE),
      )
    }
  })
  output$t1p6_2t_to = renderUI({
    if(length(input$teamsInvolved) == 2){
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
        options = list(`dropdown-align-center` = TRUE),
      )
    }
  })
  output$t1p7_2t_to = renderUI({
    if(length(input$teamsInvolved) == 2){
      pickerInput(
        "t1p7_2t_to", 
        "", 
        choices = input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]],
        choicesOpt = list(
          content = hoopR_espn_nba_teams |>
            filter(
              display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]
            ) |>
            pull(dropdown_logo)
        ),
        options = list(`dropdown-align-center` = TRUE),
      )
    }
  })
  output$t1p8_2t_to = renderUI({
    if(length(input$teamsInvolved) == 2){
      pickerInput(
        "t1p8_2t_to", 
        "", 
        choices = input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]],
        choicesOpt = list(
          content = hoopR_espn_nba_teams |>
            filter(
              display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]
            ) |>
            pull(dropdown_logo)
        ),
        options = list(`dropdown-align-center` = TRUE),
      )
    }
  })
  output$t1p9_2t_to = renderUI({
    if(length(input$teamsInvolved) == 2){
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
        options = list(`dropdown-align-center` = TRUE),
      )
    }
  })
  output$t1p10_2t_to = renderUI({
    if(length(input$teamsInvolved) == 2){
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
        options = list(`dropdown-align-center` = TRUE),
      )
    }
  })
  
  output$t1p1_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      pickerInput(
        "t1p1_3t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]),
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
        options = list(`dropdown-align-center` = TRUE),
        selected = ""
      )
    }
  })
  output$t1p2_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      pickerInput(
        "t1p2_3t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]),
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
        options = list(`dropdown-align-center` = TRUE),
        selected = ""
      )
    }
  })
  output$t1p3_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      pickerInput(
        "t1p3_3t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]),
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
        options = list(`dropdown-align-center` = TRUE),
        selected = ""
      )
    }
  })
  output$t1p4_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      pickerInput(
        "t1p4_3t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]),
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
        options = list(`dropdown-align-center` = TRUE),
        selected = ""
      )
    }
  })
  output$t1p5_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      pickerInput(
        "t1p5_3t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]),
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
        options = list(`dropdown-align-center` = TRUE),
        selected = ""
      )
    }
  })
  output$t1p6_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      pickerInput(
        "t1p6_3t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]),
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
        options = list(`dropdown-align-center` = TRUE),
        selected = ""
      )
    }
  })
  output$t1p7_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      pickerInput(
        "t1p7_3t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]),
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
        options = list(`dropdown-align-center` = TRUE),
        selected = ""
      )
    }
  })
  output$t1p8_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      pickerInput(
        "t1p8_3t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]),
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
        options = list(`dropdown-align-center` = TRUE),
        selected = ""
      )
    }
  })
  output$t1p9_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      pickerInput(
        "t1p9_3t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]),
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
        options = list(`dropdown-align-center` = TRUE),
        selected = ""
      )
    }
  })
  output$t1p10_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      pickerInput(
        "t1p10_3t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]),
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
        options = list(`dropdown-align-center` = TRUE),
        selected = ""
      )
    }
  })
  
  output$t1p1_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      pickerInput(
        "t1p1_4t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]),
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
        options = list(`dropdown-align-center` = TRUE),
        selected = ""
      )
    }
  })
  output$t1p2_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      pickerInput(
        "t1p2_4t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]),
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
        options = list(`dropdown-align-center` = TRUE),
        selected = ""
      )
    }
  })
  output$t1p3_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      pickerInput(
        "t1p3_4t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]),
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
        options = list(`dropdown-align-center` = TRUE),
        selected = ""
      )
    }
  })
  output$t1p4_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      pickerInput(
        "t1p4_4t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]),
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
        options = list(`dropdown-align-center` = TRUE),
        selected = ""
      )
    }
  })
  output$t1p5_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      pickerInput(
        "t1p5_4t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]),
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
        options = list(`dropdown-align-center` = TRUE),
        selected = ""
      )
    }
  })
  output$t1p6_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      pickerInput(
        "t1p6_4t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]),
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
        options = list(`dropdown-align-center` = TRUE),
        selected = ""
      )
    }
  })
  output$t1p7_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      pickerInput(
        "t1p7_4t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]),
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
        options = list(`dropdown-align-center` = TRUE),
        selected = ""
      )
    }
  })
  output$t1p8_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      pickerInput(
        "t1p8_4t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]),
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
        options = list(`dropdown-align-center` = TRUE),
        selected = ""
      )
    }
  })
  output$t1p9_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      pickerInput(
        "t1p9_4t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]),
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
        options = list(`dropdown-align-center` = TRUE),
        selected = ""
      )
    }
  })
  output$t1p10_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      pickerInput(
        "t1p10_4t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]),
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
        options = list(`dropdown-align-center` = TRUE),
        selected = ""
      )
    }
  })
  
  # Team 2
  output$t2p1_2t_to = renderUI({
    if(length(input$teamsInvolved) == 2){
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
        options = list(`dropdown-align-center` = TRUE),
      )
    }
  })
  output$t2p2_2t_to = renderUI({
    if(length(input$teamsInvolved) == 2){
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
        options = list(`dropdown-align-center` = TRUE),
      )
    }
  })
  output$t2p3_2t_to = renderUI({
    if(length(input$teamsInvolved) == 2){
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
        options = list(`dropdown-align-center` = TRUE),
      )
    }
  })
  output$t2p4_2t_to = renderUI({
    if(length(input$teamsInvolved) == 2){
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
        options = list(`dropdown-align-center` = TRUE),
      )
    }
  })
  output$t2p5_2t_to = renderUI({
    if(length(input$teamsInvolved) == 2){
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
        options = list(`dropdown-align-center` = TRUE),
      )
    }
  })
  output$t2p6_2t_to = renderUI({
    if(length(input$teamsInvolved) == 2){
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
        options = list(`dropdown-align-center` = TRUE),
      )
    }
  })
  output$t2p7_2t_to = renderUI({
    if(length(input$teamsInvolved) == 2){
      pickerInput(
        "t2p7_2t_to", 
        "", 
        choices = input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]],
        choicesOpt = list(
          content = hoopR_espn_nba_teams |>
            filter(
              display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]
            ) |>
            pull(dropdown_logo)
        ),
        options = list(`dropdown-align-center` = TRUE),
      )
    }
  })
  output$t2p8_2t_to = renderUI({
    if(length(input$teamsInvolved) == 2){
      pickerInput(
        "t2p8_2t_to", 
        "", 
        choices = input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]],
        choicesOpt = list(
          content = hoopR_espn_nba_teams |>
            filter(
              display_name %in% input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]
            ) |>
            pull(dropdown_logo)
        ),
        options = list(`dropdown-align-center` = TRUE),
      )
    }
  })
  output$t2p9_2t_to = renderUI({
    if(length(input$teamsInvolved) == 2){
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
        options = list(`dropdown-align-center` = TRUE),
      )
    }
  })
  output$t2p10_2t_to = renderUI({
    if(length(input$teamsInvolved) == 2){
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
        options = list(`dropdown-align-center` = TRUE),
      )
    }
  })
  
  output$t2p1_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      pickerInput(
        "t2p1_3t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]),
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
        options = list(`dropdown-align-center` = TRUE),
        selected = ""
      )
    }
  })
  output$t2p2_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      pickerInput(
        "t2p2_3t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]),
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
        options = list(`dropdown-align-center` = TRUE),
        selected = ""
      )
    }
  })
  output$t2p3_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      pickerInput(
        "t2p3_3t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]),
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
        options = list(`dropdown-align-center` = TRUE),
        selected = ""
      )
    }
  })
  output$t2p4_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      pickerInput(
        "t2p4_3t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]),
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
        options = list(`dropdown-align-center` = TRUE),
        selected = ""
      )
    }
  })
  output$t2p5_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      pickerInput(
        "t2p5_3t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]),
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
        options = list(`dropdown-align-center` = TRUE),
        selected = ""
      )
    }
  })
  output$t2p6_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      pickerInput(
        "t2p6_3t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]),
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
        options = list(`dropdown-align-center` = TRUE),
        selected = ""
      )
    }
  })
  output$t2p7_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      pickerInput(
        "t2p7_3t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]),
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
        options = list(`dropdown-align-center` = TRUE),
        selected = ""
      )
    }
  })
  output$t2p8_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      pickerInput(
        "t2p8_3t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]),
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
        options = list(`dropdown-align-center` = TRUE),
        selected = ""
      )
    }
  })
  output$t2p9_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      pickerInput(
        "t2p9_3t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]),
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
        options = list(`dropdown-align-center` = TRUE),
        selected = ""
      )
    }
  })
  output$t2p10_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      pickerInput(
        "t2p10_3t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]),
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
        options = list(`dropdown-align-center` = TRUE),
        selected = ""
      )
    }
  })
  
  output$t2p1_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      pickerInput(
        "t2p1_4t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]),
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
        options = list(`dropdown-align-center` = TRUE),
        selected = ""
      )
    }
  })
  output$t2p2_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      pickerInput(
        "t2p2_4t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]),
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
        options = list(`dropdown-align-center` = TRUE),
        selected = ""
      )
    }
  })
  output$t2p3_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      pickerInput(
        "t2p3_4t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]),
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
        options = list(`dropdown-align-center` = TRUE),
        selected = ""
      )
    }
  })
  output$t2p4_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      pickerInput(
        "t2p4_4t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]),
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
        options = list(`dropdown-align-center` = TRUE),
        selected = ""
      )
    }
  })
  output$t2p5_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      pickerInput(
        "t2p5_4t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]),
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
        options = list(`dropdown-align-center` = TRUE),
        selected = ""
      )
    }
  })
  output$t2p6_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      pickerInput(
        "t2p6_4t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]),
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
        options = list(`dropdown-align-center` = TRUE),
        selected = ""
      )
    }
  })
  output$t2p7_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      pickerInput(
        "t2p7_4t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]),
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
        options = list(`dropdown-align-center` = TRUE),
        selected = ""
      )
    }
  })
  output$t2p8_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      pickerInput(
        "t2p8_4t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]),
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
        options = list(`dropdown-align-center` = TRUE),
        selected = ""
      )
    }
  })
  output$t2p9_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      pickerInput(
        "t2p9_4t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]),
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
        options = list(`dropdown-align-center` = TRUE),
        selected = ""
      )
    }
  })
  output$t2p10_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      pickerInput(
        "t2p10_4t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]),
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
        options = list(`dropdown-align-center` = TRUE),
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
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]),
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
        options = list(`dropdown-align-center` = TRUE),
        selected = ""
      )
    }
  })
  output$t3p2_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      pickerInput(
        "t3p2_3t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]),
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
        options = list(`dropdown-align-center` = TRUE),
        selected = ""
      )
    }
  })
  output$t3p3_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      pickerInput(
        "t3p3_3t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]),
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
        options = list(`dropdown-align-center` = TRUE),
        selected = ""
      )
    }
  })
  output$t3p4_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      pickerInput(
        "t3p4_3t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]),
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
        options = list(`dropdown-align-center` = TRUE),
        selected = ""
      )
    }
  })
  output$t3p5_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      pickerInput(
        "t3p5_3t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]),
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
        options = list(`dropdown-align-center` = TRUE),
        selected = ""
      )
    }
  })
  output$t3p6_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      pickerInput(
        "t3p6_3t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]),
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
        options = list(`dropdown-align-center` = TRUE),
        selected = ""
      )
    }
  })
  output$t3p7_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      pickerInput(
        "t3p7_3t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]),
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
        options = list(`dropdown-align-center` = TRUE),
        selected = ""
      )
    }
  })
  output$t3p8_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      pickerInput(
        "t3p8_3t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]),
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
        options = list(`dropdown-align-center` = TRUE),
        selected = ""
      )
    }
  })
  output$t3p9_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      pickerInput(
        "t3p9_3t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]),
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
        options = list(`dropdown-align-center` = TRUE),
        selected = ""
      )
    }
  })
  output$t3p10_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      pickerInput(
        "t3p10_3t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]),
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
        options = list(`dropdown-align-center` = TRUE),
        selected = ""
      )
    }
  })
  
  output$t3p1_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      pickerInput(
        "t3p1_4t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]),
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
        options = list(`dropdown-align-center` = TRUE),
        selected = ""
      )
    }
  })
  output$t3p2_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      pickerInput(
        "t3p2_4t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]),
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
        options = list(`dropdown-align-center` = TRUE),
        selected = ""
      )
    }
  })
  output$t3p3_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      pickerInput(
        "t3p3_4t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]),
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
        options = list(`dropdown-align-center` = TRUE),
        selected = ""
      )
    }
  })
  output$t3p4_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      pickerInput(
        "t3p4_4t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]),
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
        options = list(`dropdown-align-center` = TRUE),
        selected = ""
      )
    }
  })
  output$t3p5_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      pickerInput(
        "t3p5_4t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]),
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
        options = list(`dropdown-align-center` = TRUE),
        selected = ""
      )
    }
  })
  output$t3p6_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      pickerInput(
        "t3p6_4t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]),
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
        options = list(`dropdown-align-center` = TRUE),
        selected = ""
      )
    }
  })
  output$t3p7_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      pickerInput(
        "t3p7_4t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]),
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
        options = list(`dropdown-align-center` = TRUE),
        selected = ""
      )
    }
  })
  output$t3p8_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      pickerInput(
        "t3p8_4t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]),
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
        options = list(`dropdown-align-center` = TRUE),
        selected = ""
      )
    }
  })
  output$t3p9_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      pickerInput(
        "t3p9_4t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]),
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
        options = list(`dropdown-align-center` = TRUE),
        selected = ""
      )
    }
  })
  output$t3p10_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      pickerInput(
        "t3p10_4t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]),
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
        options = list(`dropdown-align-center` = TRUE),
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
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[4]]]),
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
        options = list(`dropdown-align-center` = TRUE),
        selected = ""
      )
    }
  })
  output$t4p2_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      pickerInput(
        "t4p2_4t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[4]]]),
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
        options = list(`dropdown-align-center` = TRUE),
        selected = ""
      )
    }
  })
  output$t4p3_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      pickerInput(
        "t4p3_4t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[4]]]),
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
        options = list(`dropdown-align-center` = TRUE),
        selected = ""
      )
    }
  })
  output$t4p4_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      pickerInput(
        "t4p4_4t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[4]]]),
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
        options = list(`dropdown-align-center` = TRUE),
        selected = ""
      )
    }
  })
  output$t4p5_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      pickerInput(
        "t4p5_4t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[4]]]),
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
        options = list(`dropdown-align-center` = TRUE),
        selected = ""
      )
    }
  })
  output$t4p6_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      pickerInput(
        "t4p6_4t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[4]]]),
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
        options = list(`dropdown-align-center` = TRUE),
        selected = ""
      )
    }
  })
  output$t4p7_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      pickerInput(
        "t4p7_4t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[4]]]),
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
        options = list(`dropdown-align-center` = TRUE),
        selected = ""
      )
    }
  })
  output$t4p8_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      pickerInput(
        "t4p8_4t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[4]]]),
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
        options = list(`dropdown-align-center` = TRUE),
        selected = ""
      )
    }
  })
  output$t4p9_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      pickerInput(
        "t4p9_4t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[4]]]),
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
        options = list(`dropdown-align-center` = TRUE),
        selected = ""
      )
    }
  })
  output$t4p10_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      pickerInput(
        "t4p10_4t_to", 
        "", 
        choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[4]]]),
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
        options = list(`dropdown-align-center` = TRUE),
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
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t1a1_3t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]),
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
          options = list(`dropdown-align-center` = TRUE),
          selected = ""
        )
      )
    }
  })
  output$t1a2_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t1a2_3t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]),
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
          options = list(`dropdown-align-center` = TRUE),
          selected = ""
        )
      )
    }
  })
  output$t1a3_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t1a3_3t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]),
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
          options = list(`dropdown-align-center` = TRUE),
          selected = ""
        )
      )
    }
  })
  output$t1a4_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t1a4_3t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]),
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
          options = list(`dropdown-align-center` = TRUE),
          selected = ""
        )
      )
    }
  })
  output$t1a5_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t1a5_3t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]),
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
          options = list(`dropdown-align-center` = TRUE),
          selected = ""
        )
      )
    }
  })
  output$t1a6_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t1a6_3t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]),
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
          options = list(`dropdown-align-center` = TRUE),
          selected = ""
        )
      )
    }
  })
  output$t1a7_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t1a7_3t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]),
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
          options = list(`dropdown-align-center` = TRUE),
          selected = ""
        )
      )
    }
  })
  output$t1a8_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t1a8_3t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]),
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
          options = list(`dropdown-align-center` = TRUE),
          selected = ""
        )
      )
    }
  })
  output$t1a9_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t1a9_3t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]),
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
          options = list(`dropdown-align-center` = TRUE),
          selected = ""
        )
      )
    }
  })
  output$t1a10_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t1a10_3t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]),
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
          options = list(`dropdown-align-center` = TRUE),
          selected = ""
        )
      )
    }
  })
  
  output$t1a1_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t1a1_4t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]),
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
          options = list(`dropdown-align-center` = TRUE),
          selected = ""
        )
      )
    }
  })
  output$t1a2_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t1a2_4t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]),
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
          options = list(`dropdown-align-center` = TRUE),
          selected = ""
        )
      )
    }
  })
  output$t1a3_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t1a3_4t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]),
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
          options = list(`dropdown-align-center` = TRUE),
          selected = ""
        )
      )
    }
  })
  output$t1a4_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t1a4_4t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]),
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
          options = list(`dropdown-align-center` = TRUE),
          selected = ""
        )
      )
    }
  })
  output$t1a5_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t1a5_4t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]),
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
          options = list(`dropdown-align-center` = TRUE),
          selected = ""
        )
      )
    }
  })
  output$t1a6_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t1a6_4t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]),
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
          options = list(`dropdown-align-center` = TRUE),
          selected = ""
        )
      )
    }
  })
  output$t1a7_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t1a7_4t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]),
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
          options = list(`dropdown-align-center` = TRUE),
          selected = ""
        )
      )
    }
  })
  output$t1a8_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t1a8_4t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]),
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
          options = list(`dropdown-align-center` = TRUE),
          selected = ""
        )
      )
    }
  })
  output$t1a9_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t1a9_4t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]),
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
          options = list(`dropdown-align-center` = TRUE),
          selected = ""
        )
      )
    }
  })
  output$t1a10_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t1a10_4t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[1]]]),
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
          options = list(`dropdown-align-center` = TRUE),
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
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t2a1_3t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]),
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
          options = list(`dropdown-align-center` = TRUE),
          selected = ""
        )
      )
    }
  })
  output$t2a2_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t2a2_3t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]),
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
          options = list(`dropdown-align-center` = TRUE),
          selected = ""
        )
      )
    }
  })
  output$t2a3_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t2a3_3t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]),
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
          options = list(`dropdown-align-center` = TRUE),
          selected = ""
        )
      )
    }
  })
  output$t2a4_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t2a4_3t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]),
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
          options = list(`dropdown-align-center` = TRUE),
          selected = ""
        )
      )
    }
  })
  output$t2a5_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t2a5_3t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]),
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
          options = list(`dropdown-align-center` = TRUE),
          selected = ""
        )
      )
    }
  })
  output$t2a6_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t2a6_3t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]),
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
          options = list(`dropdown-align-center` = TRUE),
          selected = ""
        )
      )
    }
  })
  output$t2a7_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t2a7_3t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]),
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
          options = list(`dropdown-align-center` = TRUE),
          selected = ""
        )
      )
    }
  })
  output$t2a8_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t2a8_3t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]),
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
          options = list(`dropdown-align-center` = TRUE),
          selected = ""
        )
      )
    }
  })
  output$t2a9_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t2a9_3t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]),
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
          options = list(`dropdown-align-center` = TRUE),
          selected = ""
        )
      )
    }
  })
  output$t2a10_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t2a10_3t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]),
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
          options = list(`dropdown-align-center` = TRUE),
          selected = ""
        )
      )
    }
  })
  
  output$t2a1_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t2a1_4t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]),
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
          options = list(`dropdown-align-center` = TRUE),
          selected = ""
        )
      )
    }
  })
  output$t2a2_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t2a2_4t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]),
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
          options = list(`dropdown-align-center` = TRUE),
          selected = ""
        )
      )
    }
  })
  output$t2a3_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t2a3_4t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]),
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
          options = list(`dropdown-align-center` = TRUE),
          selected = ""
        )
      )
    }
  })
  output$t2a4_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t2a4_4t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]),
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
          options = list(`dropdown-align-center` = TRUE),
          selected = ""
        )
      )
    }
  })
  output$t2a5_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t2a5_4t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]),
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
          options = list(`dropdown-align-center` = TRUE),
          selected = ""
        )
      )
    }
  })
  output$t2a6_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t2a6_4t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]),
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
          options = list(`dropdown-align-center` = TRUE),
          selected = ""
        )
      )
    }
  })
  output$t2a7_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t2a7_4t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]),
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
          options = list(`dropdown-align-center` = TRUE),
          selected = ""
        )
      )
    }
  })
  output$t2a8_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t2a8_4t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]),
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
          options = list(`dropdown-align-center` = TRUE),
          selected = ""
        )
      )
    }
  })
  output$t2a9_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t2a9_4t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]),
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
          options = list(`dropdown-align-center` = TRUE),
          selected = ""
        )
      )
    }
  })
  output$t2a10_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t2a10_4t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[2]]]),
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
          options = list(`dropdown-align-center` = TRUE),
          selected = ""
        )
      )
    }
  })
  
  # Team 3
  output$t3a1_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t3a1_3t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]),
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
          options = list(`dropdown-align-center` = TRUE),
          selected = ""
        )
      )
    }
  })
  output$t3a2_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t3a2_3t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]),
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
          options = list(`dropdown-align-center` = TRUE),
          selected = ""
        )
      )
    }
  })
  output$t3a3_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t3a3_3t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]),
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
          options = list(`dropdown-align-center` = TRUE),
          selected = ""
        )
      )
    }
  })
  output$t3a4_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t3a4_3t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]),
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
          options = list(`dropdown-align-center` = TRUE),
          selected = ""
        )
      )
    }
  })
  output$t3a5_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t3a5_3t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]),
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
          options = list(`dropdown-align-center` = TRUE),
          selected = ""
        )
      )
    }
  })
  output$t3a6_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t3a6_3t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]),
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
          options = list(`dropdown-align-center` = TRUE),
          selected = ""
        )
      )
    }
  })
  output$t3a7_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t3a7_3t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]),
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
          options = list(`dropdown-align-center` = TRUE),
          selected = ""
        )
      )
    }
  })
  output$t3a8_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t3a8_3t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]),
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
          options = list(`dropdown-align-center` = TRUE),
          selected = ""
        )
      )
    }
  })
  output$t3a9_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t3a9_3t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]),
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
          options = list(`dropdown-align-center` = TRUE),
          selected = ""
        )
      )
    }
  })
  output$t3a10_3t_to = renderUI({
    if(length(input$teamsInvolved) == 3){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t3a10_3t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]),
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
          options = list(`dropdown-align-center` = TRUE),
          selected = ""
        )
      )
    }
  })
  
  output$t3a1_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t3a1_4t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]),
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
          options = list(`dropdown-align-center` = TRUE),
          selected = ""
        )
      )
    }
  })
  output$t3a2_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t3a2_4t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]),
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
          options = list(`dropdown-align-center` = TRUE),
          selected = ""
        )
      )
    }
  })
  output$t3a3_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t3a3_4t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]),
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
          options = list(`dropdown-align-center` = TRUE),
          selected = ""
        )
      )
    }
  })
  output$t3a4_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t3a4_4t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]),
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
          options = list(`dropdown-align-center` = TRUE),
          selected = ""
        )
      )
    }
  })
  output$t3a5_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t3a5_4t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]),
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
          options = list(`dropdown-align-center` = TRUE),
          selected = ""
        )
      )
    }
  })
  output$t3a6_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t3a6_4t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]),
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
          options = list(`dropdown-align-center` = TRUE),
          selected = ""
        )
      )
    }
  })
  output$t3a7_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t3a7_4t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]),
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
          options = list(`dropdown-align-center` = TRUE),
          selected = ""
        )
      )
    }
  })
  output$t3a8_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t3a8_4t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]),
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
          options = list(`dropdown-align-center` = TRUE),
          selected = ""
        )
      )
    }
  })
  output$t3a9_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t3a9_4t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]),
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
          options = list(`dropdown-align-center` = TRUE),
          selected = ""
        )
      )
    }
  })
  output$t3a10_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t3a10_4t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[3]]]),
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
          options = list(`dropdown-align-center` = TRUE),
          selected = ""
        )
      )
    }
  })
  
  # Team 4
  output$t4a1_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t4a1_4t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[4]]]),
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
          options = list(`dropdown-align-center` = TRUE),
          selected = ""
        )
      )
    }
  })
  output$t4a2_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t4a2_4t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[4]]]),
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
          options = list(`dropdown-align-center` = TRUE),
          selected = ""
        )
      )
    }
  })
  output$t4a3_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t4a3_4t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[4]]]),
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
          options = list(`dropdown-align-center` = TRUE),
          selected = ""
        )
      )
    }
  })
  output$t4a4_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t4a4_4t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[4]]]),
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
          options = list(`dropdown-align-center` = TRUE),
          selected = ""
        )
      )
    }
  })
  output$t4a5_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t4a5_4t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[4]]]),
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
          options = list(`dropdown-align-center` = TRUE),
          selected = ""
        )
      )
    }
  })
  output$t4a6_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t4a6_4t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[4]]]),
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
          options = list(`dropdown-align-center` = TRUE),
          selected = ""
        )
      )
    }
  })
  output$t4a7_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t4a7_4t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[4]]]),
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
          options = list(`dropdown-align-center` = TRUE),
          selected = ""
        )
      )
    }
  })
  output$t4a8_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t4a8_4t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[4]]]),
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
          options = list(`dropdown-align-center` = TRUE),
          selected = ""
        )
      )
    }
  })
  output$t4a9_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t4a9_4t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[4]]]),
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
          options = list(`dropdown-align-center` = TRUE),
          selected = ""
        )
      )
    }
  })
  output$t4a10_4t_to = renderUI({
    if(length(input$teamsInvolved) == 4){
      div(
        style = "margin-bottom: -0.05rem;",
        pickerInput(
          "t4a10_4t_to", 
          "", 
          choices = c("", input$teamsInvolved[input$teamsInvolved != input$teamsInvolved[[4]]]),
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
          options = list(`dropdown-align-center` = TRUE),
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
  
}


shinyApp(ui, server)