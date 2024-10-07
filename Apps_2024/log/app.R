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


ui = navbarPage("2024 NBA Trade Deadline Competition", fluid = TRUE,
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
                      9, 
                      h1(span("2024 NBA Trade Deadline Competition", 
                              style = 'color:rgba(169,20,20,128);')), 
                      h1(span("Transaction Log", 
                              style = 'font-size: 60px; font-weight: bold;'))
                    ),
                    column(
                      3, 
                      img(src="ASU-NTDC-Logo.png", 
                          height = 180, width = 240)
                    )
                  ),
                  tags$hr(style="border-color:rgba(169,20,20,128);"),
                  fluidRow(
                    column(
                      3, 
                      radioButtons(
                        "showNotes", 
                        "Show Notes?", 
                        choices = c("Yes", "No"), 
                        selected = "No"
                      )
                    ),
                    column(
                      3, 
                      actionButton(
                        "refreshPage", 
                        "Refresh the Page"
                      )
                    )
                  ),
                  fluidRow(
                    column(
                      12, 
                      gt_output("TransactionLog")
                    )
                  )
                ),
                tabPanel(
                  "Transactions by Team",
                  tags$style(
                    type="text/css",
                    ".shiny-output-error { visibility: hidden; }",
                    ".shiny-output-error:before { visibility: hidden; }",
                    ".modal-dialog { width: fit-content !important; }"
                  ),
                  fluidRow(
                    column(
                      9, 
                      h1(span("2024 NBA Trade Deadline Competition", 
                              style = 'color:rgba(169,20,20,128);')), 
                      h1(span("Transactions By Team", 
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
                      3, 
                      selectInput(
                        "teamSelect", 
                        "Select Team", 
                        choices = c("All", nba_teams)
                      )
                    ),
                    column(
                      3, 
                      radioButtons(
                        "showNotes_bt", 
                        "Show Notes?", 
                        choices = c("Yes", "No"), 
                        selected = "No"
                      )
                    ),
                    column(
                      3, 
                      actionButton(
                        "refreshPage_bt", 
                        "Refresh the Page"
                      )
                    )
                  ),
                  fluidRow(
                    column(
                      12, 
                      gt_output("bt_tl")
                    )
                  )
                )
              )
                

server <- function(input, output, session) {
  
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
      arrange(trans_ID, to_team) |>
      mutate(
        first = c(1, diff(trans_ID)) == 1
      ) |>
      left_join(
        hoopR_espn_nba_teams, 
        by = c("to_team" = "display_name")
      ) |>
      select(
        trans_ID, status, to_team, logo, players, picks, first, notes
      )
    )
  
  gt_transaction_log <- reactive(
      if(input$showNotes == "Yes"){
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
          ungroup() |>
          gt() |>
          gt_img_rows(
            columns = logo, 
            height = 75
          ) |>
          gt_theme_pff() %>%
          #dark borders between trades
          tab_style(
            style = list(
              cell_borders(
                sides = "top", 
                weight = px(5)
              )
            ),
            locations = cells_body(rows = first==TRUE)
          ) |>
          #vertical align in players and picks cells
          tab_style(
            style = "vertical-align:top",
            locations = cells_body(
              columns = c("players", "picks", "notes")
            )
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
            notes ~ pct(23),
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
          picks = "Incoming Picks"
        )
        
  }else{
    incoming_by_team() |>
      select(-c(notes)) |>
      mutate(
        players = str_replace_all(players, ", ", "<br>"),
        picks = str_replace_all(picks, ", ", "<br>")
      ) |> 
      arrange(desc(trans_ID)) |>
      group_by(trans_ID) |>
      mutate(
        status = ifelse(row_number() == 1, status, "")
      ) |>
      ungroup() |>
      gt() |>
      gt_img_rows(
        columns = logo, 
        height = 75
      ) |>
      gt_theme_pff() |>
      #dark borders between trades
      tab_style(
        style = list(
          cell_borders(sides = "top", weight = px(5))
        ),
        locations = cells_body(rows = first==TRUE)
      ) |>
      #vertical align in players and picks cells
      tab_style(
        style = "vertical-align:top",
        locations = cells_body(columns = c("players", "picks"))
      ) |>
      cols_hide(
        columns = c(trans_ID, to_team, first, status)
      ) |>
      fmt_markdown(
        columns = c("players", "picks")
      ) |>
      cols_width(
        logo ~ pct(5),
        players ~ pct(10),
        picks ~ pct(15)
      ) |>
      cols_align(
        align = "left",
        columns = "picks"
      ) |>
      cols_label(
        logo = "Team"
      )
  }
  )
  
  output$TransactionLog <- render_gt(gt_transaction_log())
  
  observeEvent(input$refreshPage, {
    transaction_log()
    session$reload()
  })
  
  observeEvent(input$refreshPage_bt, {
    transaction_log()
    session$reload()
  })
  
  bt_ia_player <- reactive(
      transaction_log() |> 
      group_by(trans_ID, to_team) |>
      arrange(asset) |> 
      filter(
        substr(asset, 1, 2) != '20',
        substr(asset, 1, 19) != "Cash Considerations"
      ) |>
      mutate(
        players = paste(asset, collapse = ", "),
        note1 = paste(rep("NA", length(asset)), 
                      collapse = ", ")
      ) |>
      select(
        trans_ID, to_team, players, note1, status
      ) |>
      unique()
    )
  
  bt_ia_picks <- reactive(
      transaction_log() |> 
      group_by(trans_ID, to_team) |>
      arrange(asset) |> 
      filter(
        substr(asset, 1, 2) == '20' | 
          substr(asset, 1, 19) == "Cash Considerations"
      ) |>
      mutate(
        picks = paste(asset, collapse = ", "),
        note2 = paste(note, collapse = ", ")
      ) |>
      select(
        trans_ID, to_team, picks, note2, status
      ) |>
      unique()
    )
  
  bt_ia <- reactive(
      full_join(
        bt_ia_player(), 
        bt_ia_picks(), 
        by = c("trans_ID", "to_team", "status")
      ) |>
      mutate(
        incoming_asset = paste0(players, "<br>", picks),
        notes = paste0(note1, "<br>", note2)
      ) |>
      mutate(
        notes = str_replace_all(notes, ", ", "<br>"),
        incoming_asset = str_replace_all(incoming_asset, ", ", "<br>")
      ) |>
      mutate(
        notes = str_replace_all(notes, "NA", ""),
        incoming_asset = str_replace_all(incoming_asset, "NA", "")
      ) |>
      select(
        trans_ID, team=to_team, incoming_asset, notes_i=notes, status
      )
    )
  
  bt_oa_player <- reactive(
      transaction_log() |> 
      group_by(trans_ID, away_from_team) |>
      arrange(asset) |> 
      filter(
        substr(asset, 1, 2) != '20',
        substr(asset, 1, 19) != "Cash Considerations"
      ) |>
      mutate(
        players = paste(asset, collapse = ", "),
        note1 = paste(rep("NA", length(asset)), collapse = ", ")
      ) |>
      select(
        trans_ID, away_from_team, players, note1, status
      ) |>
      unique()
    )
  
  bt_oa_picks <- reactive(
      transaction_log() |> 
      group_by(trans_ID, away_from_team) |>
      arrange(asset) |> 
      filter(
        substr(asset, 1, 2) == '20' | 
          substr(asset, 1, 19) == "Cash Considerations"
      ) |>
      mutate(
        picks = paste(asset, collapse = ", "),
        note2 = paste(note, collapse = ", ")
      ) |>
      select(
        trans_ID, away_from_team, picks, note2, status
      ) |>
      unique()
    )
  
  bt_oa <- reactive(
      full_join(
        bt_oa_player(), 
        bt_oa_picks(), 
        by = c("trans_ID", "away_from_team", "status")
      ) |>
      mutate(
        outgoing_asset = paste0(players, "<br>", picks),
        notes = paste0(note1, "<br>", note2)
      ) |>
      mutate(
        notes = str_replace_all(notes, ", ", "<br>"),
        outgoing_asset = str_replace_all(outgoing_asset, ", ", "<br>")
      ) |>
      mutate(
        notes = str_replace_all(notes, "NA", ""),
        outgoing_asset = str_replace_all(outgoing_asset, "NA", "")
      ) |>
      select(
        trans_ID, team=away_from_team, outgoing_asset, notes_o=notes, status
      )
    )
  
  bt_tl <- reactive(
      full_join(
        bt_ia(), 
        bt_oa(), 
        by = c("trans_ID", "team", "status")
      )
    )
  
  bt_tl_filtered1 <- reactive(
      if(input$teamSelect == "All"){
        bt_tl()
      } else {
      bt_tl() |> 
        filter(
          team == input$teamSelect
        )
      }
    )
  
  output$bt_tl <- render_gt(
      if(input$showNotes_bt == "Yes"){
        bt_tl_filtered1() |>
        ungroup() |>
        left_join(
          hoopR_espn_nba_teams, 
          by = c("team" = "display_name")
        ) |>
        select(
          trans_ID, logo, team, incoming_asset, notes_i, outgoing_asset, notes_o) |>
        arrange(desc(trans_ID)) |>
        gt() |>
        gt_img_rows(
          columns = logo, 
          height = 50
        ) |>
        gt_theme_pff() |>
        #dark borders between trades
        tab_style(
          style = list(
            cell_borders(sides = "top", weight = px(5))),
          locations = cells_body(rows = trans_ID>0)
        ) |>
        #vertical borders
        tab_style(
          style = list(
            cell_borders(sides = "left", weight = px(1))),
          locations = cells_body(columns = c(incoming_asset, outgoing_asset))
        ) |>
        #vertical align in players and picks cells
        tab_style(
          style = "vertical-align:top",
          locations = cells_body(columns = c("incoming_asset", "outgoing_asset", "notes_i", "notes_o"))
        ) |>
        cols_hide(
          columns = c(trans_ID, team)
        ) |>
        fmt_markdown(
          columns = c("incoming_asset", "outgoing_asset", "notes_i", "notes_o")
        ) |>
        cols_width(
          logo ~ pct(5),
          incoming_asset ~ pct(10),
          outgoing_asset ~ pct(15),
          notes_i ~ pct(18),
          notes_o ~ pct(18)
        ) |>
        cols_align(
          align = "left",
          columns = "notes_i"
        ) |>
        cols_align(
          align = "left",
          columns = "notes_o"
        ) |>
        cols_label(
          logo = "Team",
          notes_i = "Notes",
          notes_o = "Notes",
          incoming_asset = "Incoming Assets",
          outgoing_asset = "Outgoing Assets"
        )
        
    }else{
      bt_tl_filtered1() |>
        ungroup() |>
        left_join(
          hoopR_espn_nba_teams, 
          by = c("team" = "display_name")
        ) |>
        select(
          trans_ID, logo, team, incoming_asset, outgoing_asset
        ) |>
        arrange(desc(trans_ID)) |>
        gt() |>
        gt_img_rows(
          columns = logo, 
          height = 75
        ) |>
        gt_theme_pff() %>%
        #dark borders between trades
        tab_style(
          style = list(
            cell_borders(sides = "top", weight = px(5))),
          locations = cells_body(rows = trans_ID>0)
        ) |>
        #vertical borders
        tab_style(
          style = list(
            cell_borders(sides = "left", weight = px(1))),
          locations = cells_body(columns = c(incoming_asset, outgoing_asset))
        ) |>
        #vertical align in players and picks cells
        tab_style(
          style = "vertical-align:top",
          locations = cells_body(columns = c("incoming_asset", "outgoing_asset"))
        ) |>
        cols_hide(
          columns = c(trans_ID, team)
        ) |>
        fmt_markdown(
          columns = c("incoming_asset", "outgoing_asset")
        ) |>
        cols_width(
          logo ~ pct(5),
          incoming_asset ~ pct(10),
          outgoing_asset ~ pct(15)
        ) |>
        cols_label(
          logo = "Team",
          incoming_asset = "Incoming Assets",
          outgoing_asset = "Outgoing Assets"
        )
    }
  )
                          
                        
  
  
  
  
}


shinyApp(ui, server)
