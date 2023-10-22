library(shiny)
library(magrittr)
library(dplyr)
library(stringr)
library(gt)
library(gtExtras)
library(googlesheets4)

gs4_auth(cache = ".secrets", email = "jeremypatak16@gmail.com")

nba_teams = hoopR::espn_nba_teams() %>% pull(display_name)

# Define UI for application that draws a histogram
ui = navbarPage("2023 Mock Trade Deadline", fluid = TRUE,
                tabPanel("Transaction Log",
                         tags$style(type="text/css",
                                    ".shiny-output-error { visibility: hidden; }",
                                    ".shiny-output-error:before { visibility: hidden; }",
                                    ".modal-dialog { width: fit-content !important; }"),
                         fluidRow(column(9, h1(span("2023 Mock Trade Deadline", style = 'color:rgba(169,20,20,128);')), 
                                         h1(span("Transaction Log", style = 'font-size: 60px; font-weight: bold;'))),
                                  column(3, img(src="ASU-NTDC-Logo.png", height = 180, width = 240))),
                         tags$hr(style="border-color:rgba(169,20,20,128);"),
                         fluidRow(column(3, selectInput("statusFilter", "Status Filter", choices = c("All", "Pending", "Approved", "Voided"))),
                                  column(3, radioButtons("showNotes", "Show Notes/Judge Notes?", choices = c("Yes", "No"))),
                                  column(3, actionButton("refreshPage", "Refresh the Page"), h5("Please use sparingly"))),
                         fluidRow(column(12, gt_output("TransactionLog")))),
                tabPanel("Transactions by Team",
                         tags$style(type="text/css",
                                    ".shiny-output-error { visibility: hidden; }",
                                    ".shiny-output-error:before { visibility: hidden; }",
                                    ".modal-dialog { width: fit-content !important; }"),
                         fluidRow(column(9, h1(span("2023 Mock Trade Deadline", style = 'color:rgba(169,20,20,128);')), 
                                         h1(span("Transactions By Team", style = 'font-size: 60px; font-weight: bold;'))),
                                  column(3, img(src="ASU-NTDC-Logo.png", height = 180, width = 240))),
                         tags$hr(style="border-color:rgba(169,20,20,128);"),
                         fluidRow(column(3, selectInput("teamSelect", "Select Team", choices = c("All", nba_teams))),
                                  column(3, selectInput("statusFilter_bt", "Status Filter", choices = c("All", "Pending", "Approved", "Voided"))),
                                  column(3, radioButtons("showNotes_bt", "Show Notes/Judge Notes?", choices = c("Yes", "No"))),
                                  column(3, actionButton("refreshPage_bt", "Refresh the Page"), h5("Please use sparingly"))),
                         fluidRow(column(12, gt_output("bt_tl")))))
                

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  transaction_log = reactiveVal(value = read_sheet("https://docs.google.com/spreadsheets/d/1Ti24DVdNRNHGZ4IKGJ342dCDSvycJtdfC8OFh0jlLEI/edit?usp=sharing"))
  
  players = reactive(transaction_log() %>% 
    group_by(trans_ID, to_team) %>%
    arrange(asset) %>% 
    filter(substr(asset, 1, 2) != '20',
           substr(asset, 1, 19) != "Cash Considerations") %>%
    mutate(players = paste(asset, collapse = ", ")) %>%
    select(trans_ID, to_team, players, status) %>%
    unique())
  
  picks = reactive(transaction_log() %>% 
    group_by(trans_ID, to_team) %>%
    arrange(asset) %>% 
    filter(substr(asset, 1, 2) == '20' | substr(asset, 1, 19) == "Cash Considerations") %>%
    mutate(picks = paste(asset, collapse = ", "),
           notes = paste(note, collapse = ", ")) %>%
    select(trans_ID, to_team, picks, notes, judge_note, status) %>%
    unique())
  
  incoming_by_team = reactive(full_join(players(), picks(), by = c('trans_ID', 'to_team', 'status')) %>%
    mutate(players = ifelse(is.na(players), "", players)) %>%
    mutate(picks = ifelse(is.na(picks), "", picks)) %>%
    mutate(notes = ifelse(is.na(notes), "", notes)) %>%
    select(trans_ID, to_team, players, picks, notes, judge_note, status) %>%
    ungroup() %>%
    arrange(trans_ID, to_team) %>%
    mutate(first = c(1, diff(trans_ID)) == 1) %>%
    left_join(hoopR::espn_nba_teams(), by = c("to_team" = "display_name")) %>%
    select(trans_ID, status, to_team, logo_dark, players, picks, first, notes, judge_note))
  
  incoming_by_team_filtered = reactive(if(input$statusFilter == "All"){
    incoming_by_team()
    }else if(input$statusFilter == "Pending"){
      incoming_by_team() %>% filter(status == "Pending")
    }else if(input$statusFilter == "Approved"){
      incoming_by_team() %>% filter(status == "Approved")
    }else if(input$statusFilter == "Voided"){
      incoming_by_team() %>% filter(status == "Voided")
    })
  
  gt_transaction_log = reactive(if(input$showNotes == "Yes"){
    incoming_by_team_filtered() %>%
      mutate(players = str_replace_all(players, ", ", "<br>"),
             picks = str_replace_all(picks, ", ", "<br>"), 
             notes = str_replace_all(notes, ", ", "<br>"),
             notes = str_replace_all(notes, "NA", ""),
             judge_note = str_replace_na(judge_note, "")) %>% 
      group_by(trans_ID) %>%
      mutate(status = ifelse(row_number() == 1, status, "")) %>%
      ungroup() %>%
      gt() %>%
      gt_img_rows(columns = logo_dark, height = 75) %>%
      gt_theme_pff() %>%
      #dark borders between trades
      tab_style(
        style = list(
          cell_borders(sides = "top", weight = px(5))),
        locations = cells_body(rows = first==TRUE)) %>%
      #vertical align in players and picks cells
      tab_style(
        style = "vertical-align:top",
        locations = cells_body(columns = c("players", "picks", "notes", "judge_note"))) %>%
      cols_hide(columns = c(trans_ID, to_team, first)) %>%
      fmt_markdown(columns = c("players", "picks", "notes", "judge_note")) %>%
      cols_width(status ~ pct(5),
                 logo_dark ~ pct(5),
                 players ~ pct(10),
                 picks ~ pct(15),
                 notes ~ pct(18),
                 judge_note ~ pct(15)) %>%
      cols_align(align = "left",
                 columns = "notes") %>%
      cols_align(align = "left",
                 columns = "picks") %>%
      cols_align(align = "left",
                 columns = "judge_note") %>%
      cols_label(logo_dark = "Team",
                 judge_note = "Judge's Note")
  }else{
    incoming_by_team_filtered() %>%
      select(-c(notes, judge_note)) %>%
      mutate(players = str_replace_all(players, ", ", "<br>"),
             picks = str_replace_all(picks, ", ", "<br>")) %>% 
      group_by(trans_ID) %>%
      mutate(status = ifelse(row_number() == 1, status, "")) %>%
      ungroup() %>%
      gt() %>%
      gt_img_rows(columns = logo_dark, height = 75) %>%
      gt_theme_pff() %>%
      #dark borders between trades
      tab_style(
        style = list(
          cell_borders(sides = "top", weight = px(5))),
        locations = cells_body(rows = first==TRUE)) %>%
      #vertical align in players and picks cells
      tab_style(
        style = "vertical-align:top",
        locations = cells_body(columns = c("players", "picks"))) %>%
      cols_hide(columns = c(trans_ID, to_team, first)) %>%
      fmt_markdown(columns = c("players", "picks")) %>%
      cols_width(status ~ pct(5),
                 logo_dark ~ pct(5),
                 players ~ pct(10),
                 picks ~ pct(15)) %>%
      cols_align(align = "left",
                 columns = "picks") %>%
      cols_label(logo_dark = "Team")
  })
  
  output$TransactionLog <- render_gt(gt_transaction_log())
  
  observeEvent(input$refreshPage, {
    transaction_log()
    session$reload()
  })
  
  observeEvent(input$refreshPage_bt, {
    transaction_log()
    session$reload()
  })
  
  bt_ia_player = reactive(transaction_log() %>% 
                            group_by(trans_ID, to_team) %>%
                            arrange(asset) %>% 
                            filter(substr(asset, 1, 2) != '20',
                                   substr(asset, 1, 19) != "Cash Considerations") %>%
                            mutate(players = paste(asset, collapse = ", "),
                                   note1 = paste(rep("NA", length(asset)), collapse = ", ")) %>%
                            select(trans_ID, to_team, players, note1, status) %>%
                            unique())
  
  bt_ia_picks = reactive(transaction_log() %>% 
                     group_by(trans_ID, to_team) %>%
                     arrange(asset) %>% 
                     filter(substr(asset, 1, 2) == '20' | substr(asset, 1, 19) == "Cash Considerations") %>%
                     mutate(picks = paste(asset, collapse = ", "),
                            note2 = paste(note, collapse = ", ")) %>%
                     select(trans_ID, to_team, picks, note2, judge_note, status) %>%
                     unique())
  
  bt_ia = reactive(full_join(bt_ia_player(), bt_ia_picks(), by = c("trans_ID", "to_team", "status")) %>%
                     mutate(incoming_asset = paste0(players, "<br>", picks),
                            notes = paste0(note1, "<br>", note2)) %>%
                     mutate(notes = str_replace_all(notes, ", ", "<br>"),
                            incoming_asset = str_replace_all(incoming_asset, ", ", "<br>")) %>%
                     mutate(notes = str_replace_all(notes, "NA", ""),
                            incoming_asset = str_replace_all(incoming_asset, "NA", "")) %>%
                     select(trans_ID, team=to_team, incoming_asset, notes_i=notes, judge_note, status))
  
  bt_oa_player = reactive(transaction_log() %>% 
                            group_by(trans_ID, away_from_team) %>%
                            arrange(asset) %>% 
                            filter(substr(asset, 1, 2) != '20',
                                   substr(asset, 1, 19) != "Cash Considerations") %>%
                            mutate(players = paste(asset, collapse = ", "),
                                   note1 = paste(rep("NA", length(asset)), collapse = ", ")) %>%
                            select(trans_ID, away_from_team, players, note1, status) %>%
                            unique())
  
  bt_oa_picks = reactive(transaction_log() %>% 
                           group_by(trans_ID, away_from_team) %>%
                           arrange(asset) %>% 
                           filter(substr(asset, 1, 2) == '20' | substr(asset, 1, 19) == "Cash Considerations") %>%
                           mutate(picks = paste(asset, collapse = ", "),
                                  note2 = paste(note, collapse = ", ")) %>%
                           select(trans_ID, away_from_team, picks, note2, judge_note, status) %>%
                           unique())
  
  bt_oa = reactive(full_join(bt_oa_player(), bt_oa_picks(), by = c("trans_ID", "away_from_team", "status")) %>%
                     mutate(outgoing_asset = paste0(players, "<br>", picks),
                            notes = paste0(note1, "<br>", note2)) %>%
                     mutate(notes = str_replace_all(notes, ", ", "<br>"),
                            outgoing_asset = str_replace_all(outgoing_asset, ", ", "<br>")) %>%
                     mutate(notes = str_replace_all(notes, "NA", ""),
                            outgoing_asset = str_replace_all(outgoing_asset, "NA", "")) %>%
                     select(trans_ID, team=away_from_team, outgoing_asset, notes_o=notes, judge_note, status))
  
  bt_tl = reactive(full_join(bt_ia(), bt_oa(), by = c("trans_ID", "team", "judge_note", "status")) %>%
                     mutate(judge_note = str_replace_na(judge_note, "")))
  
  bt_tl_filtered1 = reactive(if(input$teamSelect == "All"){
    bt_tl()
    } else {
      bt_tl() %>% filter(team == input$teamSelect)
      })
  
  bt_tl_filtered2 = reactive(if(input$statusFilter_bt == "All"){
    bt_tl_filtered1()
  } else {
    bt_tl_filtered1() %>% filter(status == input$statusFilter_bt)
  })
  
  output$bt_tl = render_gt(if(input$showNotes_bt == "Yes"){
    bt_tl_filtered2() %>%
      ungroup() %>%
      left_join(hoopR::espn_nba_teams(), by = c("team" = "display_name")) %>%
      select(trans_ID, status, logo_dark, team, incoming_asset, notes_i, outgoing_asset, notes_o, judge_note) %>%
      arrange(desc(trans_ID)) %>%
      gt() %>%
      gt_img_rows(columns = logo_dark, height = 75) %>%
      gt_theme_pff() %>%
      #dark borders between trades
      tab_style(
        style = list(
          cell_borders(sides = "top", weight = px(5))),
        locations = cells_body(rows = trans_ID>0)) %>%
      #vertical borders
      tab_style(
        style = list(
          cell_borders(sides = "left", weight = px(1))),
        locations = cells_body(columns = c(incoming_asset, outgoing_asset, judge_note))) %>%
      #vertical align in players and picks cells
      tab_style(
        style = "vertical-align:top",
        locations = cells_body(columns = c("incoming_asset", "outgoing_asset", "notes_i", "notes_o", "judge_note"))) %>%
      cols_hide(columns = c(trans_ID, team)) %>%
      fmt_markdown(columns = c("incoming_asset", "outgoing_asset", "notes_i", "notes_o")) %>%
      cols_width(status ~ pct(5),
                 logo_dark ~ pct(5),
                 incoming_asset ~ pct(10),
                 outgoing_asset ~ pct(15),
                 notes_i ~ pct(18),
                 notes_o ~ pct(18),
                 judge_note ~ pct(15)) %>%
      cols_align(align = "left",
                 columns = "notes_i") %>%
      cols_align(align = "left",
                 columns = "notes_o") %>%
      cols_align(align = "left",
                 columns = "judge_note") %>%
      cols_label(logo_dark = "Team",
                 judge_note = "Judge's Note",
                 notes_i = "Notes",
                 notes_o = "Notes",
                 incoming_asset = "Incoming Assets",
                 outgoing_asset = "Outgoing Assets")
    }else{
      bt_tl_filtered2() %>%
        ungroup() %>%
        left_join(hoopR::espn_nba_teams(), by = c("team" = "display_name")) %>%
        select(trans_ID, status, logo_dark, team, incoming_asset, outgoing_asset) %>%
        arrange(desc(trans_ID)) %>%
        gt() %>%
        gt_img_rows(columns = logo_dark, height = 75) %>%
        gt_theme_pff() %>%
        #dark borders between trades
        tab_style(
          style = list(
            cell_borders(sides = "top", weight = px(5))),
          locations = cells_body(rows = trans_ID>0)) %>%
        #vertical borders
        tab_style(
          style = list(
            cell_borders(sides = "left", weight = px(1))),
          locations = cells_body(columns = c(incoming_asset, outgoing_asset))) %>%
        #vertical align in players and picks cells
        tab_style(
          style = "vertical-align:top",
          locations = cells_body(columns = c("incoming_asset", "outgoing_asset"))) %>%
        cols_hide(columns = c(trans_ID, team)) %>%
        fmt_markdown(columns = c("incoming_asset", "outgoing_asset")) %>%
        cols_width(status ~ pct(5),
                   logo_dark ~ pct(5),
                   incoming_asset ~ pct(10),
                   outgoing_asset ~ pct(15)) %>%
        cols_label(logo_dark = "Team",
                   incoming_asset = "Incoming Assets",
                   outgoing_asset = "Outgoing Assets")
    })
                          
                        
  
  
  
  
}


shinyApp(ui, server)
