library(shiny)
library(magrittr)
library(dplyr)
library(stringr)
library(gt)
library(gtExtras)
library(googlesheets4)
library(bslib)

gs4_auth(path = ".secrets/asu-mtd-c191882d31e3.json")

hoopR_espn_nba_teams <- read.csv("espn_nba_teams.csv")

nba_teams <- hoopR_espn_nba_teams |> 
  pull(display_name)

ui = bootstrapPage(
  theme = bs_theme(version = 5, bootswatch = "journal"),
  div(
    class = "title",
    img(
      src="ASU-NTDC-Logo.png", 
      width = "100vw",
      style = "display: block; margin-left: auto; margin-right: auto;"
    ),
    div("2024 NBA Trade Deadline Competition", 
        style = 'color:rgba(169,20,20,128); text-align: center'), 
    div("Judge's Trade Review", 
        style = 'font-weight: bold; text-align: center')
  ),
  tags$hr(style="border-width: 2px; border-color:rgba(169,20,20,128); margin-bottom:0px;"),
  # Input/Button Box
  div(
    class = "row",
    style = "background-color: #dddddd; padding-top: 10px; padding-bottom: 10px; align-items: center; padding-left: 10px",
    div(
      class = "col-4",
      textInput(
        "tradeID", 
        "Trade ID"
      )
    ),
    div(
      class = "col-6",
      actionButton(
        "submitID",
        "Search for Trade"
      )
    )
  ),
  # Trade DNE
  conditionalPanel(
    condition = "input.submitID > 0 && !output.exist",
    div(
      class = "col-md-12",
      h3("This trade ID does not exist!", style = "text-align: center; border-top: 2px solid black;")
    )
  ),
  # Trade exist
  conditionalPanel(
    condition = "input.submitID > 0 && output.exist",
    div(
      class = "row",
      div(
        class = "col-md-10",
        style = "border-top: 2px solid black;",
        tabsetPanel(
          type = "pills",
          tabPanel(
            "Trade Overview",
            gt_output("pending_trade")
          ),
          tabPanel(
            "By Team"
          )
        )
      ),
      div(
        class = "col-md-2",
        style = "border-top: 2px solid black; background-color: #dddddd"
      )
    )
  ),
  # Trade not searched yet
  conditionalPanel(
    condition = "input.submitID == 0",
    div(
      class = "col-xs-12",
      style = "text-align: center; border-top: 2px solid black; padding-left: 1.2em; padding-right: 1.2em;",
      h2("Thank you for trade checking during our 2024 Mock Trade Deadline Competition!", style = "margin-bottom: 1em;"),
      p("During competition hours, teams will enter trades into the system via another portal. They will recieve a transaction ID for any trade entered, but it does not become a completed trade until checked by you.", style = "margin-bottom: 1em;"),
      p("Please enter a transaction ID into the blank above, and you will be given options to Approve, Conditionally Approve, or Reject the trade.", style = "margin-bottom: 1em;"),
      h3("Thank You!")
    )
  )
  
)


server <- function(input, output, session) {
  
  transaction_log <- reactive({
    read_sheet("https://docs.google.com/spreadsheets/d/13TtpNrsgFBv9UR4s-mw15uDd17UruiV9OZM6fWs03kE/edit?usp=sharing", sheet = "All_TL")
  })
  
  submittedID <- reactiveVal("")
  
  observeEvent(
    input$submitID, {
      submittedID(input$tradeID)
    })
  
  trade <- reactive(
    transaction_log() |>
      filter(
        trans_ID == submittedID()
      )
  )
  
  output$exist <- reactive(
    nrow(trade()) > 0
  )
  outputOptions(output, "exist", suspendWhenHidden = FALSE)
  
  players <- reactive(
    trade() |> 
      group_by(to_team) |>
      arrange(asset) |> 
      filter(
        substr(asset, 1, 2) != '20',
        substr(asset, 1, 19) != "Cash Considerations"
      ) |>
      mutate(
        players = paste(asset, collapse = ", ")
      ) |>
      select(
        trans_ID, to_team, players, status
      ) |>
      unique()
  )
  
  picks <- reactive(
    trade() |> 
      group_by(to_team) |>
      arrange(asset) |>
      filter(
        substr(asset, 1, 2) == '20' | 
          substr(asset, 1, 19) == "Cash Considerations"
      ) |>
      mutate(
        picks = paste(asset, collapse = ", "),
        notes = paste(note, collapse = ",, ")
      ) |>
      select(
        trans_ID, to_team, picks, notes, status
      ) |>
      unique()
  )
  
  incoming_by_team <- reactive(
      full_join(
        players(), 
        picks(), 
        by = c('to_team', 'status', "trans_ID")
      ) |>
      mutate(
        players = ifelse(is.na(players), "", players),
        picks = ifelse(is.na(picks), "", picks),
        notes = ifelse(is.na(notes), "", notes)
      ) |>
      select(
        trans_ID, to_team, players, picks, notes, status
      ) |>
      ungroup() |>
      arrange(to_team) |>
      left_join(
        hoopR_espn_nba_teams, 
        by = c("to_team" = "display_name")
      ) |>
      select(
        trans_ID, status, to_team, logo, players, picks, notes
      )
  )
  
  output$pending_trade <- render_gt({
    incoming_by_team() |>
      mutate(
        players = str_replace_all(players, ", ", "<br>"),
        picks = str_replace_all(picks, ", ", "<br>"), 
        notes = str_replace_all(notes, ",, ", "<br>"),
        notes = str_replace_all(notes, "NA", "")
      ) |>
      mutate(
        status = ifelse(row_number() == 1, status, "")
      ) |>
      ungroup() |>
      gt() |>
      gt_img_rows(
        columns = logo, 
        height = 75
      ) |>
      gt_theme_guardian() |>
      tab_style(
        style = "white-space: nowrap;",
        locations = cells_body()
      ) |>
      tab_style(
        style = list(cell_borders(sides = "top", color = "white")),
        locations = cells_column_labels()
      ) |>
      cols_hide(
        columns = c(trans_ID, to_team, status)
      ) |>
      fmt_markdown(
        columns = c("players", "picks", "notes")
      ) |>
      cols_width(
        everything() ~ "auto"
      ) |>
      cols_align(
        align = "left",
        columns = "notes"
      ) |>
      cols_align(
        align = "left",
        columns = "picks"
      ) |>
      cols_label(
        logo = "Team",
        players = "Incoming Players",
        picks = "Incoming Picks",
        notes = "Notes"
      ) |>
      #vertical align in players and picks cells
      tab_style(
        style = "vertical-align:top",
        locations = cells_body(
          columns = c("players", "picks", "notes")
        )
      )
  })
  

}


shinyApp(ui, server)
