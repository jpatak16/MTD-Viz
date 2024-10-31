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

# Define the deadline date and time
deadline <- as.POSIXct("2024-11-08 16:00:00")

ui = bootstrapPage(
  theme = bs_theme(version = 5, bootswatch = "journal"),
  div(
    class = "row",
    # Title Header
    div(
      class = "col-3",
      img(
        src="ASU-NTDC-Logo.png", 
        width = "100vw",
        style = "display: block; margin-left: auto; margin-right: auto;"
      ),
      div("2024 NBA Trade Deadline Competition", 
          style = 'color:rgba(169,20,20,128); text-align: center'), 
      div("Transaction Log", 
          style = 'font-weight: bold; text-align: center')
    ),
    # Countdown Clock
    div(
      class = "col-9",
      style = "display: flex; align-items: center; justify-content: center;",
      h3(uiOutput("countdown_output"), style = "text-align: center; margin-right: 7em;"),
      h3(uiOutput("trades_output"), style = "text-align: center;")
    )
  ),
  tags$hr(style="border-width: 2px; border-color:rgba(169,20,20,128); margin-bottom:0px;"),
  # Transaction Log
  div(
    class = "row",
    div(
      class = "col-6",
      style = "text-align: center; padding-left: 1.2em; padding-right: 0.2em;",
      gt_output("TransactionLog1")
    ),
    div(
      class = "col-6",
      style = "text-align: center; padding-left: 0.2em; padding-right: 1.2em;",
      gt_output("TransactionLog2")
    )
  )
  
)

server <- function(input, output, session) {
  
  # Create a reactive timer that updates every second
  countdownTimer <- reactiveTimer(1000)
  
  # Calculate the time remaining
  output$countdown_output <- renderText({
    countdownTimer()  # Trigger every second
    
    # Calculate the time left
    time_left <- difftime(deadline, Sys.time(), units = "secs") |> as.numeric()
    
    # Check if countdown has finished
    if (time_left <= 0) {
      HTML("Time Is Up!")
    } else {
      # Calculate days, hours, minutes, and seconds left
      hours <- as.integer(time_left %/% (60 * 60))
      minutes <- as.integer((time_left %% (60 * 60)) %/% 60)
      seconds <- as.integer(time_left %% 60)
      
      # Format the output
      HTML(paste0("Time Remaining:", "<br>", hours, " Hours, ", minutes, " Minutes, ", seconds, " Seconds"))
    }
  })
  
  output$trades_output <- renderText({
    
    # Calculate the total completed trades
    total_trades <- transaction_log() |>
      filter(
        !str_detect(
          asset,
          "Waived"
        )
      ) |>
      pull(trans_ID) |>
      unique() |>
      length()
    
    HTML(paste0("Trades Completed:", "<br>", total_trades))
    
  })
  
  transaction_log <- reactive({
    refresh_timer()
    
    read_sheet("https://docs.google.com/spreadsheets/d/13TtpNrsgFBv9UR4s-mw15uDd17UruiV9OZM6fWs03kE/edit?usp=sharing", sheet = "All_TL") |>
      filter(
        status == "Complete"
      )
  })
  
  players <- reactive(
      transaction_log() |> 
      group_by(trans_ID, to_team) |>
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
      transaction_log() |> 
      group_by(trans_ID, to_team) |>
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
      by = c('trans_ID', 'to_team', 'status')
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
    arrange(
      trans_ID, to_team
    ) |>
    mutate(
      first = ifelse(row_number() == 1, 1, 0),
      .by = trans_ID
    ) |>
    left_join(
      hoopR_espn_nba_teams, 
      by = c("to_team" = "display_name")
    ) |>
    select(
      trans_ID, status, to_team, logo, players, picks, first, notes)
    )
  
  
  transaction_log_fixed <- reactive(
    incoming_by_team() |>
      mutate(
        players = str_replace_all(players, ", ", "<br>"),
        picks = str_replace_all(picks, ", ", "<br>"), 
        notes = str_replace_all(notes, ",, ", "<br>"),
        notes = str_replace_all(notes, "NA", "")
      ) |> 
      arrange(desc(trans_ID)) |>
      group_by(trans_ID) |>
      mutate(
        status = ifelse(row_number() == 1, status, "")
      ) |>
      ungroup()
    )
  
  ids_1 <- reactive(
      transaction_log_fixed() |> 
      arrange(desc(trans_ID)) |>
      pull(trans_ID) |> 
      unique()
    )
  
  ids_1_filtered <- reactive(
      ids_1()[c(TRUE, FALSE)]
    )
  
  tl_1 <- reactive(
      transaction_log_fixed() |>
      filter(
        trans_ID %in% ids_1_filtered()
      ) |>
      gt() |>
      gt_img_rows(
        columns = logo, 
        height = 75
      ) |>
      gt_theme_guardian() |>
      #dark borders between trades
      tab_style(
        style = list(
          cell_borders(sides = "top", weight = px(5))),
        locations = cells_body(rows = status!="")
      ) |>
      #vertical align in players and picks cells
      tab_style(
        style = list(cell_text(weight = "bold", size = "18px")),
        locations = cells_body(columns = c("players", "picks"))
      ) |>
      tab_style(
        style = list(cell_borders(sides = "top", color = "white")),
        locations = cells_column_labels()
      ) |>
      cols_hide(
        columns = c(trans_ID, to_team, first, status, notes)
      ) |>
      fmt_markdown(
        columns = c("players", "picks", "notes")
      ) |>
      cols_width(
        logo ~ pct(5),
        players ~ pct(8),
        picks ~ pct(12),
        notes ~ pct(12)
      ) |>
      cols_align(
        align = "left",
        columns = "picks"
      ) |>
      cols_label(
        logo = "Team",
        players = "Players",
        picks = "Picks"
      )
    )
  
  ids_2_filtered <- reactive(
      ids_1()[c(FALSE, TRUE)]
    )
  
  tl_2 <- reactive(
      transaction_log_fixed() |>
      filter(
        trans_ID %in% ids_2_filtered()
      ) |>
      gt() |>
      gt_img_rows(
        columns = logo, 
        height = 75
      ) |>
      gt_theme_guardian() |>
      #dark borders between trades
      tab_style(
        style = list(
          cell_borders(sides = "top", weight = px(5))),
        locations = cells_body(rows = status!="")
      ) |>
      #vertical align in players and picks cells
      tab_style(
        style = list(cell_text(weight = "bold", size = "18px")),
        locations = cells_body(columns = c("players", "picks"))
      ) |>
      tab_style(
        style = list(cell_borders(sides = "top", color = "white")),
        locations = cells_column_labels()
      ) |>
      cols_hide(
        columns = c(trans_ID, to_team, first, status, notes)
      ) |>
      fmt_markdown(
        columns = c("players", "picks", "notes")
      ) |>
      cols_width(
        logo ~ pct(5),
        players ~ pct(8),
        picks ~ pct(12),
        notes ~ pct(12)
      ) |>
      cols_align(
        align = "left",
        columns = "picks"
      ) |>
      cols_label(
        logo = "Team",
        players = "Players",
        picks = "Picks"
      )
    )
    
  
  refresh_timer <- reactiveTimer(30000)
  
  output$TransactionLog1 <- render_gt({
    tl_1()
  })
  
  output$TransactionLog2 <- render_gt({
    tl_2()
  })
  
}



shinyApp(ui, server)
