library(shiny)
library(magrittr)
library(dplyr)
library(stringr)
library(gt)
library(gtExtras)
library(googlesheets4)

gs4_auth(path = ".secrets/asu-mtd-c191882d31e3.json")

hoopR_espn_nba_teams = read.csv("espn_nba_teams.csv")

nba_teams = hoopR_espn_nba_teams |>
  pull(display_name)

# Define UI for application that draws a histogram
ui = navbarPage("2024 NBA Trade Deadline Competition", 
                fluid = TRUE,
                tabPanel(
                  "Transaction Log",
                  tags$style(
                    type="text/css",
                    ".shiny-output-error { visibility: hidden; }",
                    ".shiny-output-error:before { visibility: hidden; }",
                    ".modal-dialog { width: fit-content !important; }"
                  ),
                  fluidRow(
                    column(
                      12, 
                      h1(span(textOutput("countdown"), 
                              style = 'font-size: 40px; font-weight: bold;'))
                    ), 
                    align = 'center'
                  ),
                  fluidRow(
                    column(
                      6, 
                      gt_output("TransactionLog1")
                    ),
                    column(
                      6, 
                      gt_output("TransactionLog2")
                    )
                  )
                )
              )
                

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  sec_rem <- reactiveVal(
      1699059600 - as.numeric(Sys.time())
    )
  
  hours_rem <- reactive(
      ifelse(
        floor(sec_rem() / 3600) |> as.numeric() > 0, 
        floor(sec_rem() / 3600) |> as.numeric(), 
        0
      )
    )
  
  mins_rem <- reactive(
      ifelse(
        floor((sec_rem() - (hours_rem()*3600)) / 60) |> as.numeric() >= 0, 
        floor((sec_rem() - (hours_rem()*3600)) / 60) |> as.numeric(), 
        0
      )
    )
  
  time_to_sub <- reactive(
      hours_rem()*3600 + mins_rem()*60
    )
  
  sec_rem_fix <- reactive(
      ifelse(
        floor(sec_rem() - time_to_sub()) >= 0, 
        floor(sec_rem() - time_to_sub()), 
        0
      )
    )
  
  output$countdown <- renderText(
      paste0(
        "Time Remaining: ", 
        hours_rem(), 
        " Hours, ", 
        mins_rem(), 
        " Minutes, ", 
        sec_rem_fix(), 
        " Seconds!"
      )
    )
  
  transaction_log <- reactiveVal(
      value = read_sheet("https://docs.google.com/spreadsheets/d/13TtpNrsgFBv9UR4s-mw15uDd17UruiV9OZM6fWs03kE/edit?usp=sharing")
    )
  
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
      first = c(1, diff(trans_ID)) == 1
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
      gt_theme_pff() |>
      #dark borders between trades
      tab_style(
        style = list(
          cell_borders(sides = "top", weight = px(5))),
        locations = cells_body(rows = status!="")
      ) |>
      #vertical align in players and picks cells
      tab_style(
        style = list(cell_text(weight = "bold", size = "16px")),
        locations = cells_body(columns = c("players", "picks"))
      ) |>
      cols_hide(
        columns = c(trans_ID, to_team, first, status)
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
        logo = "Team"
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
      gt_theme_pff() |>
      #dark borders between trades
      tab_style(
        style = list(
          cell_borders(sides = "top", weight = px(5))),
        locations = cells_body(rows = status!="")
      ) |>
      #vertical align in players and picks cells
      tab_style(
        style = list(cell_text(weight = "bold", size = "16px")),
        locations = cells_body(columns = c("players", "picks"))
      ) |>
      cols_hide(
        columns = c(trans_ID, to_team, first, status)
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
        logo = "Team"
      )
    )
    
  
  output$TransactionLog1 <- render_gt(tl_1())
  output$TransactionLog2 <- render_gt(tl_2())
  
  
  counter <- reactiveVal(0)
  
  observe({
    invalidateLater(1000*35, session)
    if (isolate(counter()) > 0) {
      session$reload()
    } else{
      counter(isolate(counter()) + 1)
    }
  })
  
}



shinyApp(ui, server)
