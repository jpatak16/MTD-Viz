library(shiny)
library(magrittr)
library(dplyr)
library(stringr)
library(gt)
library(gtExtras)
library(googlesheets4)
library(bslib)

gs4_auth(path = ".secrets/asu-mtd-c191882d31e3.json")

ss <- gs4_get("https://docs.google.com/spreadsheets/d/13TtpNrsgFBv9UR4s-mw15uDd17UruiV9OZM6fWs03kE/edit?usp=sharing")

hoopR_espn_nba_teams <- read.csv("espn_nba_teams.csv")

nba_teams <- hoopR_espn_nba_teams |> 
  pull(display_name)

ui = bootstrapPage(
  theme = bs_theme(version = 5, bootswatch = "journal"),
  tags$head(
    tags$title("NTDC Judge Review")
  ),
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
      class = "col-3",
      textInput(
        "tradeID", 
        "Trade ID"
      )
    ),
    div(
      class = "col-3",
      actionButton(
        "submitID",
        "Search for Trade"
      )
    ),
    div(
      class = "col-2",
      conditionalPanel(
        condition = "input.submitID > 0 && output.exist",
        h5("Trade Status:"),
        p(textOutput("status"))
      )
    ),
    div(
      class = "col-3",
      conditionalPanel(
        condition = "input.submitID > 0 && output.exist && output.notes",
        h5("Reason:"),
        p(textOutput("reason"))
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
      class = "col-md-12",
      style = "border-top: 2px solid black;",
      tabsetPanel(
        type = "pills",
        tabPanel(
          "Trade Overview",
          gt_output("pending_trade")
        ),
        tabPanel(
          "By Team",
          gt_output("pt_bt")
        )
      )
    ),
    conditionalPanel(
      condition = "output.status == 'Pending'",
      div(
        class = "row",
        div(
          class = "col-2",
          h3("")
        ),
        div(
          class = "col-md-5",
          style = "background-color: #dddddd; padding-top: 5px; padding-bottom: 5px; display: flex; align-items: center; justify-content: center;",
          actionButton(
            "approve",
            "Approve Trade",
            style = "margin-right: 2em; --bs-btn-hover-bg: green;"
          ),
          actionButton(
            "cond_approve",
            "Conditionally Approve",
            style = "margin-right: 2em; --bs-btn-hover-bg: #FFCC00;"
          ),
          actionButton(
            "reject",
            "Reject Trade",
            style = "--bs-btn-hover-bg: #a91b0d;"
          )
        ),
        div(
          class = "col-md-3",
          style = "background-color: #dddddd; padding-top: 5px; padding-bottom: 5px; display: flex; flex-direction: column; align-items: center;",
          textAreaInput(
            "reason",
            "Reason"
          )
        )
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
    input$submitID
    
    read_sheet("https://docs.google.com/spreadsheets/d/13TtpNrsgFBv9UR4s-mw15uDd17UruiV9OZM6fWs03kE/edit?usp=sharing", sheet = "All_TL")
    
    })
  
  submittedID <- reactiveVal("")
  
  observeEvent(
    input$submitID, {
      showPageSpinner()
      submittedID(input$tradeID)
      Sys.sleep(1)
      hidePageSpinner()
    })
  
  # Approve Trade Button
  observeEvent(
    input$approve, {
      showModal(
        modalDialog(
          easyClose = TRUE,
          size = "m",
          title = "Would you like to approve this trade?",
          p("If approved, this trade will immediatley become official and be added to the transaction log, as is."),
          footer = tagList(
            actionButton("confirm_approval", "Confirm Approval"),
            actionButton("notyet_approval", "Not Yet")
          )
        )
      )
    })
  
  observeEvent(input$notyet_approval, {
    removeModal()
  })
  
  observeEvent(input$finish_approval, {
    session$reload()
  })
  
  observeEvent(
    input$confirm_approval, {
      transaction_log()
      
      start_row <- which(
          transaction_log()$trans_ID == submittedID()
        ) |>
        min()
      
      new_confID = read_sheet(ss, sheet = "All_TL") |>
        pull(confirmID) |>
        substr(1,3) |>
        max(na.rm = T) |>
        as.numeric()+1
      
      range_write(
        ss,
        data = trade() |>
          mutate(
            status = "Complete",
            reason = input$reason,
            confirmID = paste0(new_confID, runif(1, 0, 99) |> round(digits = 0) |> sprintf(fmt = "%02d")),
            time_reviewed = format(Sys.time(), "%H:%M:%S")
          ),
        range = paste0("A", start_row+1),
        col_names = FALSE,
        reformat = FALSE
      )
      
      removeModal()
      
      showModal(
        modalDialog(
          easyClose = FALSE,
          title = "Success!",
          h4("This trade has been approved and completed!"),
          footer = tagList(
            actionButton("finish_approval", "Close this Popup")
          )
        )
      )
    })
  
  # CA Trade Button
  observeEvent(
    input$cond_approve, {
      showModal(
        modalDialog(
          easyClose = TRUE,
          size = "m",
          title = "Would you like to conditionally approve this trade?",
          p("If conditionally approved, this trade will be altered based on your comments, then officially approved and added to the transaction log. No penalty will be given to the teams in scoring. You must provide a reason so that the trade may be altered, then approved."),
          footer = tagList(
            actionButton("confirm_ca", "Confirm Conditional Approval"),
            actionButton("notyet_ca", "Not Yet")
          )
        )
      )
    })
  
  observeEvent(input$notyet_ca, {
    removeModal()
  })
  
  observeEvent(input$finish_ca, {
    session$reload()
  })
  
  observeEvent(
    input$confirm_ca, {
      transaction_log()
      
      start_row <- which(
        transaction_log()$trans_ID == submittedID()
      ) |>
        min()
      
      new_confID = read_sheet(ss, sheet = "All_TL") |>
        pull(confirmID) |>
        substr(1,3) |>
        max(na.rm = T) |>
        as.numeric()+1
      
      range_write(
        ss,
        data = trade() |>
          mutate(
            status = "Conditionally Approved",
            reason = input$reason,
            confirmID = paste0(new_confID, runif(1, 0, 99) |> round(digits = 0) |> sprintf(fmt = "%02d")),
            time_reviewed = format(Sys.time(), "%H:%M:%S")
          ),
        range = paste0("A", start_row+1),
        col_names = FALSE,
        reformat = FALSE
      )
      
      sheet_append(
        ss,
        data = data.frame(
          trans_ID = submittedID(),
          reason = input$reason
        ),
        sheet = "CA"
      )
      
      removeModal()
      
      showModal(
        modalDialog(
          easyClose = FALSE,
          title = "Success!",
          h4("This trade has been conditionally approved!"),
          footer = tagList(
            actionButton("finish_ca", "Close this Popup")
          )
        )
      )
    })
  
  # Reject Trade Button
  observeEvent(
    input$reject, {
      showModal(
        modalDialog(
          easyClose = TRUE,
          size = "m",
          title = "Would you like to reject this trade?",
          p("If rejected, this trade will immediatley be deemed an illegal trade and will count against the teams involved in judges scoring."),
          footer = tagList(
            actionButton("confirm_reject", "Confirm Rejection"),
            actionButton("notyet_reject", "Not Yet")
          )
        )
      )
    })
  
  observeEvent(input$notyet_reject, {
    removeModal()
  })
  
  observeEvent(input$finish_reject, {
    session$reload()
  })
  
  observeEvent(
    input$confirm_reject, {
      transaction_log()
      
      start_row <- which(
        transaction_log()$trans_ID == submittedID()
      ) |>
        min()
      
      new_confID = read_sheet(ss, sheet = "All_TL") |>
        pull(confirmID) |>
        substr(1,3) |>
        max(na.rm = T) |>
        as.numeric()+1
      
      range_write(
        ss,
        data = trade() |>
          mutate(
            status = "Rejected",
            reason = input$reason,
            confirmID = paste0(new_confID, runif(1, 0, 99) |> round(digits = 0) |> sprintf(fmt = "%02d")),
            time_reviewed = format(Sys.time(), "%H:%M:%S")
          ),
        range = paste0("A", start_row+1),
        col_names = FALSE,
        reformat = FALSE
      )
      
      removeModal()
      
      showModal(
        modalDialog(
          easyClose = FALSE,
          title = "Success!",
          h4("This trade has been rejected!"),
          footer = tagList(
            actionButton("finish_reject", "Close this Popup")
          )
        )
      )
    })
  
  trade <- reactive(
    transaction_log() |>
      filter(
        trans_ID == submittedID()
      )
  )
  
  # Status output in top bar
  output$status <- reactive(
    trade() |>
      pull(status) |>
      unique()
  )
  
  output$exist <- reactive(
    nrow(trade()) > 0
  )
  outputOptions(output, "exist", suspendWhenHidden = FALSE)
  
  # Reason output in top bar
  output$reason <- reactive(
    trade() |>
      pull(reason) |>
      unique()
  )
  
  output$notes <- reactive(
    trade() |>
      pull(reason) |>
      unique()
  )
  outputOptions(output, "notes", suspendWhenHidden = FALSE)
  
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
  
  bt_ia_player <- reactive(
    trade() |> 
      group_by(to_team) |>
      arrange(asset) |> 
      filter(
        substr(asset, 1, 2) != '20',
        substr(asset, 1, 19) != "Cash Considerations"
      ) |>
      mutate(
        players = paste(asset, collapse = ", "),
        note1 = paste(rep("NA", length(asset)), 
                      collapse = ",, ")
      ) |>
      select(
        trans_ID, to_team, players, note1, status
      ) |>
      unique()
  )
  
  bt_ia_picks <- reactive(
    trade() |> 
      group_by(to_team) |>
      arrange(asset) |> 
      filter(
        substr(asset, 1, 2) == '20' | 
          substr(asset, 1, 19) == "Cash Considerations"
      ) |>
      mutate(
        picks = paste(asset, collapse = ", "),
        note2 = paste(note, collapse = ",, ")
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
        notes = str_replace_all(notes, ",, ", "<br>"),
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
    trade() |> 
      group_by(away_from_team) |>
      arrange(asset) |> 
      filter(
        substr(asset, 1, 2) != '20',
        substr(asset, 1, 19) != "Cash Considerations"
      ) |>
      mutate(
        players = paste(asset, collapse = ", "),
        note1 = paste(rep("NA", length(asset)), collapse = ",, ")
      ) |>
      select(
        trans_ID, away_from_team, players, note1, status
      ) |>
      unique()
  )
  
  bt_oa_picks <- reactive(
    trade() |> 
      group_by(away_from_team) |>
      arrange(asset) |> 
      filter(
        substr(asset, 1, 2) == '20' | 
          substr(asset, 1, 19) == "Cash Considerations"
      ) |>
      mutate(
        picks = paste(asset, collapse = ", "),
        note2 = paste(note, collapse = ",, ")
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
        notes = str_replace_all(notes, ",, ", "<br>"),
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
  
  output$pt_bt = render_gt(
    bt_tl() |>
      ungroup() |>
      left_join(
        hoopR_espn_nba_teams, 
        by = c("team" = "display_name")
      ) |>
      select(
        trans_ID, logo, team, incoming_asset, notes_i, outgoing_asset, notes_o) |>
      gt() |>
      gt_img_rows(
        columns = logo, 
        height = 50
      ) |>
      gt_theme_guardian() |>
      #dark borders between trades
      tab_style(
        style = list(
          cell_borders(sides = "top", weight = px(2))),
        locations = cells_body(rows = trans_ID>0)
      ) |>
      #vertical borders
      tab_style(
        style = list(
          cell_borders(sides = "left", weight = px(1))),
        locations = cells_body(columns = c(incoming_asset, outgoing_asset))
      ) |>
      tab_style(
        style = "white-space: nowrap;",
        locations = cells_body()
      ) |>
      tab_style(
        style = list(cell_borders(sides = "top", color = "white")),
        locations = cells_column_labels()
      ) |>
      cols_hide(
        columns = c(trans_ID, team)
      ) |>
      fmt_markdown(
        columns = c("incoming_asset", "outgoing_asset", "notes_i", "notes_o")
      ) |>
      cols_width(
        everything() ~ "auto"
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
      ) |> 
      #vertical align in players and picks cells
      tab_style(
        style = "vertical-align:top",
        locations = cells_body(columns = c("incoming_asset", "outgoing_asset", "notes_i", "notes_o"))
      )
  )
  

}


shinyApp(ui, server)
