library(pacman)
library(shiny)
library(magrittr)
library(dplyr)
library(stringr)
library(gt)
library(gtExtras)
library(googlesheets4)

gs4_auth(cache = ".secrets", email = "jeremypatak16@gmail.com")

ss = gs4_get("https://docs.google.com/spreadsheets/d/1Ti24DVdNRNHGZ4IKGJ342dCDSvycJtdfC8OFh0jlLEI/edit?usp=sharing")

nba_teams = hoopR::espn_nba_teams() %>% pull(display_name)

# Define UI for application that draws a histogram
ui = navbarPage("2023 Mock Trade Deadline", fluid = TRUE,
                tabPanel("Trade Entry",
                         tags$style(type="text/css",
                                    ".shiny-output-error { visibility: hidden; }",
                                    ".shiny-output-error:before { visibility: hidden; }",
                                    ".modal-dialog { width: fit-content !important; }"),
                         fluidRow(column(9, h1(span("2023 Mock Trade Deadline", style = 'color:rgba(169,20,20,128);')), 
                                         h1(span("Trade Entry", style = 'font-size: 60px; font-weight: bold;'))),
                                  column(3, img(src="ASU-NTDC-Logo.png", height = 180, width = 240))),
                         tags$hr(style="border-color:rgba(169,20,20,128);"),
                         fluidRow(column(6, selectizeInput("teamsInvolved", label = "Teams Involved", choices = nba_teams, multiple = T)),
                                  column(2, actionButton("submit", "Submit For Approval")),
                                  column(2, actionButton("clearpg", "Clear All Input"))),
                         fluidRow(column(3, textInput("tradedItem1", "Asset")),
                                  column(3, uiOutput("selectTradeFrom1")),
                                  column(3, uiOutput("selectTradeTo1")),
                                  column(3, textInput("notes1", "Notes"))),
                         fluidRow(column(3, textInput("tradedItem2", "")),
                                  column(3, uiOutput("selectTradeFrom2")),
                                  column(3, uiOutput("selectTradeTo2")),
                                  column(3, textInput("notes2", ""))),
                         fluidRow(column(3, textInput("tradedItem3", "")),
                                  column(3, uiOutput("selectTradeFrom3")),
                                  column(3, uiOutput("selectTradeTo3")),
                                  column(3, textInput("notes3", ""))),
                         fluidRow(column(3, uiOutput("tradedItem4")),
                                  column(3, uiOutput("selectTradeFrom4")),
                                  column(3, uiOutput("selectTradeTo4")),
                                  column(3, uiOutput("notes4"))),
                         fluidRow(column(3, uiOutput("tradedItem5")),
                                  column(3, uiOutput("selectTradeFrom5")),
                                  column(3, uiOutput("selectTradeTo5")),
                                  column(3, uiOutput("notes5"))),
                         fluidRow(column(3, uiOutput("tradedItem6")),
                                  column(3, uiOutput("selectTradeFrom6")),
                                  column(3, uiOutput("selectTradeTo6")),
                                  column(3, uiOutput("notes6"))),
                         fluidRow(column(3, uiOutput("tradedItem7")),
                                  column(3, uiOutput("selectTradeFrom7")),
                                  column(3, uiOutput("selectTradeTo7")),
                                  column(3, uiOutput("notes7"))),
                         fluidRow(column(3, uiOutput("tradedItem8")),
                                  column(3, uiOutput("selectTradeFrom8")),
                                  column(3, uiOutput("selectTradeTo8")),
                                  column(3, uiOutput("notes8"))),
                         fluidRow(column(3, uiOutput("tradedItem9")),
                                  column(3, uiOutput("selectTradeFrom9")),
                                  column(3, uiOutput("selectTradeTo9")),
                                  column(3, uiOutput("notes9"))),
                         fluidRow(column(3, uiOutput("tradedItem10")),
                                  column(3, uiOutput("selectTradeFrom10")),
                                  column(3, uiOutput("selectTradeTo10")),
                                  column(3, uiOutput("notes10"))),
                         fluidRow(column(3, uiOutput("tradedItem11")),
                                  column(3, uiOutput("selectTradeFrom11")),
                                  column(3, uiOutput("selectTradeTo11")),
                                  column(3, uiOutput("notes11"))),
                         fluidRow(column(3, uiOutput("tradedItem12")),
                                  column(3, uiOutput("selectTradeFrom12")),
                                  column(3, uiOutput("selectTradeTo12")),
                                  column(3, uiOutput("notes12"))),
                         fluidRow(column(3, uiOutput("tradedItem13")),
                                  column(3, uiOutput("selectTradeFrom13")),
                                  column(3, uiOutput("selectTradeTo13")),
                                  column(3, uiOutput("notes13"))),
                         fluidRow(column(3, uiOutput("tradedItem14")),
                                  column(3, uiOutput("selectTradeFrom14")),
                                  column(3, uiOutput("selectTradeTo14")),
                                  column(3, uiOutput("notes14"))),
                         fluidRow(column(3, uiOutput("tradedItem15")),
                                  column(3, uiOutput("selectTradeFrom15")),
                                  column(3, uiOutput("selectTradeTo15")),
                                  column(3, uiOutput("notes15"))),
                         fluidRow(column(3, uiOutput("tradedItem16")),
                                  column(3, uiOutput("selectTradeFrom16")),
                                  column(3, uiOutput("selectTradeTo16")),
                                  column(3, uiOutput("notes16"))),
                         fluidRow(column(3, uiOutput("tradedItem17")),
                                  column(3, uiOutput("selectTradeFrom17")),
                                  column(3, uiOutput("selectTradeTo17")),
                                  column(3, uiOutput("notes17"))),
                         fluidRow(column(3, uiOutput("tradedItem18")),
                                  column(3, uiOutput("selectTradeFrom18")),
                                  column(3, uiOutput("selectTradeTo18")),
                                  column(3, uiOutput("notes18"))),
                         fluidRow(column(3, uiOutput("tradedItem19")),
                                  column(3, uiOutput("selectTradeFrom19")),
                                  column(3, uiOutput("selectTradeTo19")),
                                  column(3, uiOutput("notes19"))),
                         fluidRow(column(3, uiOutput("tradedItem20")),
                                  column(3, uiOutput("selectTradeFrom20")),
                                  column(3, uiOutput("selectTradeTo20")),
                                  column(3, uiOutput("notes20"))),
                         fluidRow(column(3, uiOutput("tradedItem21")),
                                  column(3, uiOutput("selectTradeFrom21")),
                                  column(3, uiOutput("selectTradeTo21")),
                                  column(3, uiOutput("notes21")))),
                tabPanel("Trade Review",
                         fluidRow(column(9, h1(span("2023 Mock Trade Deadline", style = 'color:rgba(169,20,20,128);')), 
                                         h1(span("Trade Review", style = 'font-size: 60px; font-weight: bold;'))),
                                  column(3, img(src="ASU-NTDC-Logo.png", height = 180, width = 240))),
                         uiOutput("hline0"),
                         fluidRow(column(7, gt_output("tradeReview1")),
                                  column(2, uiOutput("judgeReview1"), uiOutput("judgeSubmit1")),
                                  column(3, uiOutput("judgeNotes1"))),
                         uiOutput("hline1"),
                         fluidRow(column(7, gt_output("tradeReview2")),
                                  column(2, uiOutput("judgeReview2"), uiOutput("judgeSubmit2")),
                                  column(3, uiOutput("judgeNotes2"))),
                         uiOutput("hline2"),
                         fluidRow(column(7, gt_output("tradeReview3")),
                                  column(2, uiOutput("judgeReview3"), uiOutput("judgeSubmit3")),
                                  column(3, uiOutput("judgeNotes3"))),
                         uiOutput("hline3"),
                         fluidRow(column(7, gt_output("tradeReview4")),
                                  column(2, uiOutput("judgeReview4"), uiOutput("judgeSubmit4")),
                                  column(3, uiOutput("judgeNotes4"))),
                         uiOutput("hline4"),
                         fluidRow(column(7, gt_output("tradeReview5")),
                                  column(2, uiOutput("judgeReview5"), uiOutput("judgeSubmit5")),
                                  column(3, uiOutput("judgeNotes5"))),
                         uiOutput("hline5"),
                         fluidRow(column(7, gt_output("tradeReview6")),
                                  column(2, uiOutput("judgeReview6"), uiOutput("judgeSubmit6")),
                                  column(3, uiOutput("judgeNotes6"))),
                         uiOutput("hline6"),
                         fluidRow(column(7, gt_output("tradeReview7")),
                                  column(2, uiOutput("judgeReview7"), uiOutput("judgeSubmit7")),
                                  column(3, uiOutput("judgeNotes7"))),
                         uiOutput("hline7"),
                         fluidRow(column(7, gt_output("tradeReview8")),
                                  column(2, uiOutput("judgeReview8"), uiOutput("judgeSubmit8")),
                                  column(3, uiOutput("judgeNotes8"))),
                         uiOutput("hline8"),
                         fluidRow(column(7, gt_output("tradeReview9")),
                                  column(2, uiOutput("judgeReview9"), uiOutput("judgeSubmit9")),
                                  column(3, uiOutput("judgeNotes9"))),
                         uiOutput("hline9"),
                         fluidRow(column(7, gt_output("tradeReview10")),
                                  column(2, uiOutput("judgeReview10"), uiOutput("judgeSubmit10")),
                                  column(3, uiOutput("judgeNotes10")))))

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  transaction_log = reactiveVal(value = read_sheet("https://docs.google.com/spreadsheets/d/1Ti24DVdNRNHGZ4IKGJ342dCDSvycJtdfC8OFh0jlLEI/edit?usp=sharing"))

  #trade from
  output$selectTradeFrom1 <- renderUI({ 
    selectInput("tradedFrom1", "Traded From", c("", input$teamsInvolved), selected = NULL)
  })
  output$selectTradeFrom2 <- renderUI({ 
    selectInput("tradedFrom2", "", c("", input$teamsInvolved), selected = NULL)
  })
  output$selectTradeFrom3 <- renderUI({ 
    selectInput("tradedFrom3", "", c("", input$teamsInvolved), selected = NULL)
  })
  output$selectTradeFrom4 <- renderUI({ 
    if(input$tradedItem3 == ""){}
    else{selectInput("tradedFrom4", "", c("", input$teamsInvolved), selected = NULL)}
  })
  output$selectTradeFrom5 <- renderUI({ 
    if(input$tradedItem4 == ""){}
    else{selectInput("tradedFrom5", "", c("", input$teamsInvolved), selected = NULL)}
  })
  output$selectTradeFrom6 <- renderUI({ 
    if(input$tradedItem5 == ""){}
    else{selectInput("tradedFrom6", "", c("", input$teamsInvolved), selected = NULL)}
  })
  output$selectTradeFrom7 <- renderUI({ 
    if(input$tradedItem6 == ""){}
    else{selectInput("tradedFrom7", "", c("", input$teamsInvolved), selected = NULL)}
  })
  output$selectTradeFrom8 <- renderUI({ 
    if(input$tradedItem7 == ""){}
    else{selectInput("tradedFrom8", "", c("", input$teamsInvolved), selected = NULL)}
  })
  output$selectTradeFrom9 <- renderUI({ 
    if(input$tradedItem8 == ""){}
    else{selectInput("tradedFrom9", "", c("", input$teamsInvolved), selected = NULL)}
  })
  output$selectTradeFrom10 <- renderUI({ 
    if(input$tradedItem9 == ""){}
    else{selectInput("tradedFrom10", "", c("", input$teamsInvolved), selected = NULL)}
  })
  output$selectTradeFrom11 <- renderUI({ 
    if(input$tradedItem10 == ""){}
    else{selectInput("tradedFrom11", "", c("", input$teamsInvolved), selected = NULL)}
  })
  output$selectTradeFrom12 <- renderUI({ 
    if(input$tradedItem11 == ""){}
    else{selectInput("tradedFrom12", "", c("", input$teamsInvolved), selected = NULL)}
  })
  output$selectTradeFrom13 <- renderUI({ 
    if(input$tradedItem12 == ""){}
    else{selectInput("tradedFrom13", "", c("", input$teamsInvolved), selected = NULL)}
  })
  output$selectTradeFrom14 <- renderUI({ 
    if(input$tradedItem13 == ""){}
    else{selectInput("tradedFrom14", "", c("", input$teamsInvolved), selected = NULL)}
  })
  output$selectTradeFrom15 <- renderUI({ 
    if(input$tradedItem14 == ""){}
    else{selectInput("tradedFrom15", "", c("", input$teamsInvolved), selected = NULL)}
  })
  output$selectTradeFrom16 <- renderUI({ 
    if(input$tradedItem15 == ""){}
    else{selectInput("tradedFrom16", "", c("", input$teamsInvolved), selected = NULL)}
  })
  output$selectTradeFrom17 <- renderUI({ 
    if(input$tradedItem16 == ""){}
    else{selectInput("tradedFrom17", "", c("", input$teamsInvolved), selected = NULL)}
  })
  output$selectTradeFrom18 <- renderUI({ 
    if(input$tradedItem17 == ""){}
    else{selectInput("tradedFrom18", "", c("", input$teamsInvolved), selected = NULL)}
  })
  output$selectTradeFrom19 <- renderUI({ 
    if(input$tradedItem18 == ""){}
    else{selectInput("tradedFrom19", "", c("", input$teamsInvolved), selected = NULL)}
  })
  output$selectTradeFrom20 <- renderUI({ 
    if(input$tradedItem19 == ""){}
    else{selectInput("tradedFrom20", "", c("", input$teamsInvolved), selected = NULL)}
  })
  output$selectTradeFrom21 <- renderUI({ 
    if(input$tradedItem20 == ""){}
    else{selectInput("tradedFrom21", "", c("", input$teamsInvolved), selected = NULL)}
  })
  
  
  #trade to
  output$selectTradeTo1 <- renderUI({ 
    selectInput("tradedTo1", "Traded To", c("", input$teamsInvolved), selected = NULL)
  })
  output$selectTradeTo2 <- renderUI({ 
    selectInput("tradedTo2", "", c("", input$teamsInvolved), selected = NULL)
  })
  output$selectTradeTo3 <- renderUI({ 
    selectInput("tradedTo3", "", c("", input$teamsInvolved), selected = NULL)
  })
  output$selectTradeTo4 <- renderUI({ 
    if(input$tradedItem3 == ""){}
    else{selectInput("tradedTo4", "", c("", input$teamsInvolved), selected = NULL)}
  })
  output$selectTradeTo5 <- renderUI({ 
    if(input$tradedItem4 == ""){}
    else{selectInput("tradedTo5", "", c("", input$teamsInvolved), selected = NULL)}
  })
  output$selectTradeTo6 <- renderUI({ 
    if(input$tradedItem5 == ""){}
    else{selectInput("tradedTo6", "", c("", input$teamsInvolved), selected = NULL)}
  })
  output$selectTradeTo7 <- renderUI({ 
    if(input$tradedItem6 == ""){}
    else{selectInput("tradedTo7", "", c("", input$teamsInvolved), selected = NULL)}
  })
  output$selectTradeTo8 <- renderUI({ 
    if(input$tradedItem7 == ""){}
    else{selectInput("tradedTo8", "", c("", input$teamsInvolved), selected = NULL)}
  })
  output$selectTradeTo9 <- renderUI({ 
    if(input$tradedItem8 == ""){}
    else{selectInput("tradedTo9", "", c("", input$teamsInvolved), selected = NULL)}
  })
  output$selectTradeTo10 <- renderUI({ 
    if(input$tradedItem9 == ""){}
    else{selectInput("tradedTo10", "", c("", input$teamsInvolved), selected = NULL)}
  })
  output$selectTradeTo11 <- renderUI({ 
    if(input$tradedItem10 == ""){}
    else{selectInput("tradedTo11", "", c("", input$teamsInvolved), selected = NULL)}
  })
  output$selectTradeTo12 <- renderUI({ 
    if(input$tradedItem11 == ""){}
    else{selectInput("tradedTo12", "", c("", input$teamsInvolved), selected = NULL)}
  })
  output$selectTradeTo13 <- renderUI({ 
    if(input$tradedItem12 == ""){}
    else{selectInput("tradedTo13", "", c("", input$teamsInvolved), selected = NULL)}
  })
  output$selectTradeTo14 <- renderUI({ 
    if(input$tradedItem13 == ""){}
    else{selectInput("tradedTo14", "", c("", input$teamsInvolved), selected = NULL)}
  })
  output$selectTradeTo15 <- renderUI({ 
    if(input$tradedItem14 == ""){}
    else{selectInput("tradedTo15", "", c("", input$teamsInvolved), selected = NULL)}
  })
  output$selectTradeTo16 <- renderUI({ 
    if(input$tradedItem15 == ""){}
    else{selectInput("tradedTo16", "", c("", input$teamsInvolved), selected = NULL)}
  })
  output$selectTradeTo17 <- renderUI({ 
    if(input$tradedItem16 == ""){}
    else{selectInput("tradedTo17", "", c("", input$teamsInvolved), selected = NULL)}
  })
  output$selectTradeTo18 <- renderUI({ 
    if(input$tradedItem17 == ""){}
    else{selectInput("tradedTo18", "", c("", input$teamsInvolved), selected = NULL)}
  })
  output$selectTradeTo19 <- renderUI({ 
    if(input$tradedItem18 == ""){}
    else{selectInput("tradedTo19", "", c("", input$teamsInvolved), selected = NULL)}
  })
  output$selectTradeTo20 <- renderUI({ 
    if(input$tradedItem19 == ""){}
    else{selectInput("tradedTo20", "", c("", input$teamsInvolved), selected = NULL)}
  })
  output$selectTradeTo21 <- renderUI({ 
    if(input$tradedItem20 == ""){}
    else{selectInput("tradedTo21", "", c("", input$teamsInvolved), selected = NULL)}
  })
  
  
  
  #traded item
  output$tradedItem4 <- renderUI({ 
    if(input$tradedItem3 == ""){}
    else{textInput("tradedItem4", "")}
  })
  output$tradedItem5 <- renderUI({ 
    if(input$tradedItem4 == ""){}
    else{textInput("tradedItem5", "")}
  })
  output$tradedItem6 <- renderUI({ 
    if(input$tradedItem5 == ""){}
    else{textInput("tradedItem6", "")}
  })
  output$tradedItem7 <- renderUI({ 
    if(input$tradedItem6 == ""){}
    else{textInput("tradedItem7", "")}
  })
  output$tradedItem8 <- renderUI({ 
    if(input$tradedItem7 == ""){}
    else{textInput("tradedItem8", "")}
  })
  output$tradedItem9 <- renderUI({ 
    if(input$tradedItem8 == ""){}
    else{textInput("tradedItem9", "")}
  })
  output$tradedItem10 <- renderUI({ 
    if(input$tradedItem9 == ""){}
    else{textInput("tradedItem10", "")}
  })
  output$tradedItem11 <- renderUI({ 
    if(input$tradedItem10 == ""){}
    else{textInput("tradedItem11", "")}
  })
  output$tradedItem12 <- renderUI({ 
    if(input$tradedItem11 == ""){}
    else{textInput("tradedItem12", "")}
  })
  output$tradedItem13 <- renderUI({ 
    if(input$tradedItem12 == ""){}
    else{textInput("tradedItem13", "")}
  })
  output$tradedItem14 <- renderUI({ 
    if(input$tradedItem13 == ""){}
    else{textInput("tradedItem14", "")}
  })
  output$tradedItem15 <- renderUI({ 
    if(input$tradedItem14 == ""){}
    else{textInput("tradedItem15", "")}
  })
  output$tradedItem16 <- renderUI({ 
    if(input$tradedItem15 == ""){}
    else{textInput("tradedItem16", "")}
  })
  output$tradedItem17 <- renderUI({ 
    if(input$tradedItem16 == ""){}
    else{textInput("tradedItem17", "")}
  })
  output$tradedItem18 <- renderUI({ 
    if(input$tradedItem17 == ""){}
    else{textInput("tradedItem18", "")}
  })
  output$tradedItem19 <- renderUI({ 
    if(input$tradedItem18 == ""){}
    else{textInput("tradedItem19", "")}
  })
  output$tradedItem20 <- renderUI({ 
    if(input$tradedItem19 == ""){}
    else{textInput("tradedItem20", "")}
  })
  output$tradedItem21 <- renderUI({ 
    if(input$tradedItem20 == ""){}
    else{textInput("tradedItem21", "")}
  })
  
  
  
  #notes
  output$notes4 <- renderUI({ 
    if(input$tradedItem3 == ""){}
    else{textInput("notes4", "")}
  })
  output$notes5 <- renderUI({ 
    if(input$tradedItem4 == ""){}
    else{textInput("notes5", "")}
  })
  output$notes6 <- renderUI({ 
    if(input$tradedItem5 == ""){}
    else{textInput("notes6", "")}
  })
  output$notes7 <- renderUI({ 
    if(input$tradedItem6 == ""){}
    else{textInput("notes7", "")}
  })
  output$notes8 <- renderUI({ 
    if(input$tradedItem7 == ""){}
    else{textInput("notes8", "")}
  })
  output$notes9 <- renderUI({ 
    if(input$tradedItem8 == ""){}
    else{textInput("notes9", "")}
  })
  output$notes10 <- renderUI({ 
    if(input$tradedItem9 == ""){}
    else{textInput("notes10", "")}
  })
  output$notes11 <- renderUI({ 
    if(input$tradedItem10 == ""){}
    else{textInput("notes11", "")}
  })
  output$notes12 <- renderUI({ 
    if(input$tradedItem11 == ""){}
    else{textInput("notes12", "")}
  })
  output$notes13 <- renderUI({ 
    if(input$tradedItem12 == ""){}
    else{textInput("notes13", "")}
  })
  output$notes14 <- renderUI({ 
    if(input$tradedItem13 == ""){}
    else{textInput("notes14", "")}
  })
  output$notes15 <- renderUI({ 
    if(input$tradedItem14 == ""){}
    else{textInput("notes15", "")}
  })
  output$notes16 <- renderUI({ 
    if(input$tradedItem15 == ""){}
    else{textInput("notes16", "")}
  })
  output$notes17 <- renderUI({ 
    if(input$tradedItem16 == ""){}
    else{textInput("notes17", "")}
  })
  output$notes18 <- renderUI({ 
    if(input$tradedItem17 == ""){}
    else{textInput("notes18", "")}
  })
  output$notes19 <- renderUI({ 
    if(input$tradedItem18 == ""){}
    else{textInput("notes19", "")}
  })
  output$notes20 <- renderUI({ 
    if(input$tradedItem19 == ""){}
    else{textInput("notes20", "")}
  })
  output$notes21 <- renderUI({ 
    if(input$tradedItem20 == ""){}
    else{textInput("notes21", "")}
  })
  
  
  #fill dataframe for new trade
  newtrade_asset = reactive(c(input$tradedItem1, input$tradedItem2, input$tradedItem3, input$tradedItem4,
                              input$tradedItem5, input$tradedItem6, input$tradedItem7, input$tradedItem8,
                              input$tradedItem9, input$tradedItem10, input$tradedItem11, input$tradedItem12,
                              input$tradedItem13, input$tradedItem14, input$tradedItem15, input$tradedItem16,
                              input$tradedItem17, input$tradedItem18, input$tradedItem19, input$tradedItem20, input$tradedItem21))
  newtrade_away_from_team = reactive(c(input$tradedFrom1, input$tradedFrom2, input$tradedFrom3, input$tradedFrom4,
                                       input$tradedFrom5, input$tradedFrom6, input$tradedFrom7, input$tradedFrom8,
                                       input$tradedFrom9, input$tradedFrom10, input$tradedFrom11, input$tradedFrom12,
                                       input$tradedFrom13, input$tradedFrom14, input$tradedFrom15, input$tradedFrom16,
                                       input$tradedFrom17, input$tradedFrom18, input$tradedFrom19, input$tradedFrom20, input$tradedFrom21))
  newtrade_to_team = reactive(c(input$tradedTo1, input$tradedTo2, input$tradedTo3, input$tradedTo4,
                                input$tradedTo5, input$tradedTo6, input$tradedTo7, input$tradedTo8,
                                input$tradedTo9, input$tradedTo10, input$tradedTo11, input$tradedTo12,
                                input$tradedTo13, input$tradedTo14, input$tradedTo15, input$tradedTo16,
                                input$tradedTo17, input$tradedTo18, input$tradedTo19, input$tradedTo20, input$tradedTo21))
  newtrade_note = reactive(c(input$notes1, input$notes2, input$notes3, input$notes4,
                             input$notes5, input$notes6, input$notes7, input$notes8,
                             input$notes9, input$notes10, input$notes11, input$notes12,
                             input$notes13, input$notes14, input$notes15, input$notes16,
                             input$notes17, input$notes18, input$notes19, input$notes20, input$notes21))
  
  newtrade = reactive(data.frame(asset = newtrade_asset(),
                                 away_from_team = newtrade_away_from_team(),
                                 to_team = newtrade_to_team(),
                                 note = newtrade_note()))
  
  newtrade2 = reactive(newtrade() %>%
                         mutate(trans_ID = ifelse(transaction_log()$trans_ID %>% length() == 0, 
                                                  1001,
                                                  transaction_log()$trans_ID %>% max() + 1),
                                status = "Pending",
                                judge_note = "") %>%
                         select(trans_ID, asset, away_from_team, to_team, note, status, judge_note) %>%
                         filter(asset != ""))
  
  #make new trade look good for a gt table
  #make trades look good for tables
  newtrade_players = reactive(newtrade2() %>% 
                       group_by(trans_ID, to_team) %>%
                       arrange(asset) %>% 
                       filter(substr(asset, 1, 2) != '20',
                              substr(asset, 1, 19) != "Cash Considerations") %>%
                       mutate(players = paste(asset, collapse = ", ")) %>%
                       select(trans_ID, to_team, players) %>%
                       unique())
  
  newtrade_picks = reactive(newtrade2() %>% 
                     group_by(trans_ID, to_team) %>%
                     arrange(asset) %>% 
                     filter(substr(asset, 1, 2) == '20' | substr(asset, 1, 19) == "Cash Considerations") %>%
                     mutate(picks = paste(asset, collapse = ", "),
                            notes = paste(note, collapse = ", ")) %>%
                     select(trans_ID, to_team, picks, notes) %>%
                     unique())
  
  newtrade_incoming_by_team = reactive(full_join(newtrade_players(), newtrade_picks(), by = c('trans_ID', 'to_team')) %>%
                                mutate(players = ifelse(is.na(players), "", players)) %>%
                                mutate(picks = ifelse(is.na(picks), "", picks)) %>%
                                mutate(notes = ifelse(is.na(notes), "", notes)) %>%
                                select(trans_ID, to_team, players, picks, notes) %>%
                                ungroup() %>%
                                arrange(trans_ID, to_team) %>%
                                mutate(first = c(1, diff(trans_ID)) == 1) %>%
                                left_join(hoopR::espn_nba_teams(), by = c("to_team" = "display_name")) %>%
                                select(trans_ID, to_team, logo_dark, players, picks, first, notes))
  
  
  # Launch confirmation
  observeEvent(input$submit, {
    showModal(
      modalDialog(
        easyClose = TRUE,
        title = "Confirm this trade?",
        render_gt(width = "600px",
                  newtrade_incoming_by_team() %>%
                    mutate(players = str_replace_all(players, ", ", "<br>"),
                           picks = str_replace_all(picks, ", ", "<br>"), 
                           notes = str_replace_all(notes, ", ", "<br>"),
                           notes = str_replace_all(notes, "NA", " ")) %>% 
                    gt() %>%
                    gt_img_rows(columns = logo_dark, height = 50) %>%
                    gt_theme_pff() %>%
                    #dark borders between trades
                    tab_style(
                      style = list(
                        cell_borders(sides = "top", weight = px(5))),
                      locations = cells_body(rows = first==TRUE)) %>%
                    #vertical align in players and picks cells
                    tab_style(
                      style = "vertical-align:top",
                      locations = cells_body(columns = c("players", "picks", "notes"))) %>%
                    cols_hide(columns = c(trans_ID, to_team, first)) %>%
                    fmt_markdown(columns = c("players", "picks", "notes")) %>%
                    cols_width(players ~ pct(20),
                               logo_dark ~ pct(10),
                               picks ~ pct(30),
                               notes ~ pct(40)) %>%
                    cols_align(align = "left",
                               columns = "notes") %>%
                    cols_align(align = "left",
                               columns = "picks") %>%
                    cols_label(logo_dark = "Team")),
        footer = tagList(actionButton("confirm", "Confirm Trade"),
                         actionButton("notyet", "Not Yet"))
        )
      )
  })
  
  #clear page on button click
  observeEvent(input$clearpg, {
    session$reload()
  })
  
  #not yet button from modal popup
  observeEvent(input$notyet, {
    removeModal()
  })
  
  #confirm trade
  observeEvent(input$confirm, {
    sheet_append(ss$spreadsheet_id, newtrade2())
    transaction_log()
    session$reload()
  })
  
  
  
  
  
  #find trades that need reviewed
  needs_review = reactive(transaction_log() %>% filter(status == "Pending") %>% pull(trans_ID) %>% unique())
  
  #make trades look good for tables
  players = reactive(transaction_log() %>% 
                       group_by(trans_ID, to_team) %>%
                       arrange(asset) %>% 
                       filter(substr(asset, 1, 2) != '20',
                              substr(asset, 1, 19) != "Cash Considerations") %>%
                       mutate(players = paste(asset, collapse = ", ")) %>%
                       select(trans_ID, to_team, players) %>%
                       unique())
  
  picks = reactive(transaction_log() %>% 
                     group_by(trans_ID, to_team) %>%
                     arrange(asset) %>% 
                     filter(substr(asset, 1, 2) == '20' | substr(asset, 1, 19) == "Cash Considerations") %>%
                     mutate(picks = paste(asset, collapse = ", "),
                            notes = paste(note, collapse = ", ")) %>%
                     select(trans_ID, to_team, picks, notes) %>%
                     unique())
  
  incoming_by_team = reactive(full_join(players(), picks(), by = c('trans_ID', 'to_team')) %>%
                                mutate(players = ifelse(is.na(players), "", players)) %>%
                                mutate(picks = ifelse(is.na(picks), "", picks)) %>%
                                mutate(notes = ifelse(is.na(notes), "", notes)) %>%
                                select(trans_ID, to_team, players, picks, notes) %>%
                                ungroup() %>%
                                arrange(trans_ID, to_team) %>%
                                mutate(first = c(1, diff(trans_ID)) == 1) %>%
                                left_join(hoopR::espn_nba_teams(), by = c("to_team" = "display_name")) %>%
                                select(trans_ID, to_team, logo_dark, players, picks, first, notes))
  
  #make inputs for trade reviews if there are that many trades that need reviewed
  
  #radio buttons
  output$judgeReview1 <- renderUI({ 
    if(needs_review() %>% length() >= 1){radioButtons("judgeReview1", "", c("Approve", "Void"), selected = NULL)}
    else{}
  })
  output$judgeReview2 <- renderUI({ 
    if(needs_review() %>% length() >= 2){radioButtons("judgeReview2", "", c("Approve", "Void"), selected = NULL)}
    else{}
  })
  output$judgeReview3 <- renderUI({ 
    if(needs_review() %>% length() >= 3){radioButtons("judgeReview3", "", c("Approve", "Void"), selected = NULL)}
    else{}
  })
  output$judgeReview4 <- renderUI({ 
    if(needs_review() %>% length() >= 4){radioButtons("judgeReview4", "", c("Approve", "Void"), selected = NULL)}
    else{}
  })
  output$judgeReview5 <- renderUI({ 
    if(needs_review() %>% length() >= 5){radioButtons("judgeReview5", "", c("Approve", "Void"), selected = NULL)}
    else{}
  })
  output$judgeReview6 <- renderUI({ 
    if(needs_review() %>% length() >= 6){radioButtons("judgeReview6", "", c("Approve", "Void"), selected = NULL)}
    else{}
  })
  output$judgeReview7 <- renderUI({ 
    if(needs_review() %>% length() >= 7){radioButtons("judgeReview7", "", c("Approve", "Void"), selected = NULL)}
    else{}
  })
  output$judgeReview8 <- renderUI({ 
    if(needs_review() %>% length() >= 8){radioButtons("judgeReview8", "", c("Approve", "Void"), selected = NULL)}
    else{}
  })
  output$judgeReview9 <- renderUI({ 
    if(needs_review() %>% length() >= 9){radioButtons("judgeReview9", "", c("Approve", "Void"), selected = NULL)}
    else{}
  })
  output$judgeReview10 <- renderUI({ 
    if(needs_review() %>% length() >= 10){radioButtons("judgeReview10", "", c("Approve", "Void"), selected = NULL)}
    else{}
  })
  
  
  
  #submit button
  output$judgeSubmit1 <- renderUI({ 
    if(needs_review() %>% length() >= 1){actionButton("judgeSubmit1", "Submit")}
    else{}
  })
  output$judgeSubmit2 <- renderUI({ 
    if(needs_review() %>% length() >= 2){actionButton("judgeSubmit2", "Submit")}
    else{}
  })
  output$judgeSubmit3 <- renderUI({ 
    if(needs_review() %>% length() >= 3){actionButton("judgeSubmit3", "Submit")}
    else{}
  })
  output$judgeSubmit4 <- renderUI({ 
    if(needs_review() %>% length() >= 4){actionButton("judgeSubmit4", "Submit")}
    else{}
  })
  output$judgeSubmit5 <- renderUI({ 
    if(needs_review() %>% length() >= 5){actionButton("judgeSubmit5", "Submit")}
    else{}
  })
  output$judgeSubmit6 <- renderUI({ 
    if(needs_review() %>% length() >= 6){actionButton("judgeSubmit6", "Submit")}
    else{}
  })
  output$judgeSubmit7 <- renderUI({ 
    if(needs_review() %>% length() >= 7){actionButton("judgeSubmit7", "Submit")}
    else{}
  })
  output$judgeSubmit8 <- renderUI({ 
    if(needs_review() %>% length() >= 8){actionButton("judgeSubmit8", "Submit")}
    else{}
  })
  output$judgeSubmit9 <- renderUI({ 
    if(needs_review() %>% length() >= 9){actionButton("judgeSubmit9", "Submit")}
    else{}
  })
  output$judgeSubmit10 <- renderUI({ 
    if(needs_review() %>% length() >= 10){actionButton("judgeSubmit10", "Submit")}
    else{}
  })
  
  #judge notes text box
  output$judgeNotes1 <- renderUI({ 
    if(needs_review() %>% length() >= 1){textAreaInput("judgeNotes1", "Judge Notes", width = "100%", height = "100px")}
    else{}
  })
  output$judgeNotes2 <- renderUI({ 
    if(needs_review() %>% length() >= 2){textAreaInput("judgeNotes2", "Judge Notes", width = "100%", height = "100px")}
    else{}
  })
  output$judgeNotes3 <- renderUI({ 
    if(needs_review() %>% length() >= 3){textAreaInput("judgeNotes3", "Judge Notes", width = "100%", height = "100px")}
    else{}
  })
  output$judgeNotes4 <- renderUI({ 
    if(needs_review() %>% length() >= 4){textAreaInput("judgeNotes4", "Judge Notes", width = "100%", height = "100px")}
    else{}
  })
  output$judgeNotes5 <- renderUI({ 
    if(needs_review() %>% length() >= 5){textAreaInput("judgeNotes5", "Judge Notes", width = "100%", height = "100px")}
    else{}
  })
  output$judgeNotes6 <- renderUI({ 
    if(needs_review() %>% length() >= 6){textAreaInput("judgeNotes6", "Judge Notes", width = "100%", height = "100px")}
    else{}
  })
  output$judgeNotes7 <- renderUI({ 
    if(needs_review() %>% length() >= 7){textAreaInput("judgeNotes7", "Judge Notes", width = "100%", height = "100px")}
    else{}
  })
  output$judgeNotes8 <- renderUI({ 
    if(needs_review() %>% length() >= 8){textAreaInput("judgeNotes8", "Judge Notes", width = "100%", height = "100px")}
    else{}
  })
  output$judgeNotes9 <- renderUI({ 
    if(needs_review() %>% length() >= 9){textAreaInput("judgeNotes9", "Judge Notes", width = "100%", height = "100px")}
    else{}
  })
  output$judgeNotes10 <- renderUI({ 
    if(needs_review() %>% length() >= 10){textAreaInput("judgeNotes10", "Judge Notes", width = "100%", height = "100px")}
    else{}
  })
  
  #hlines to seperate each trade
  output$hline0 <- renderUI({ 
    if(needs_review() %>% length() >= 0){tags$hr(style="border-color:rgba(169,20,20,128);")}
    else{}
  })
  output$hline1 <- renderUI({ 
    if(needs_review() %>% length() > 1){tags$hr(style="border-color:rgba(169,20,20,128);")}
    else{}
  })
  output$hline2 <- renderUI({ 
    if(needs_review() %>% length() > 2){tags$hr(style="border-color:rgba(169,20,20,128);")}
    else{}
  })
  output$hline3 <- renderUI({ 
    if(needs_review() %>% length() > 3){tags$hr(style="border-color:rgba(169,20,20,128);")}
    else{}
  })
  output$hline4 <- renderUI({ 
    if(needs_review() %>% length() > 4){tags$hr(style="border-color:rgba(169,20,20,128);")}
    else{}
  })
  output$hline5 <- renderUI({ 
    if(needs_review() %>% length() > 5){tags$hr(style="border-color:rgba(169,20,20,128);")}
    else{}
  })
  output$hline6 <- renderUI({ 
    if(needs_review() %>% length() > 6){tags$hr(style="border-color:rgba(169,20,20,128);")}
    else{}
  })
  output$hline7 <- renderUI({ 
    if(needs_review() %>% length() > 7){tags$hr(style="border-color:rgba(169,20,20,128);")}
    else{}
  })
  output$hline8 <- renderUI({ 
    if(needs_review() %>% length() > 8){tags$hr(style="border-color:rgba(169,20,20,128);")}
    else{}
  })
  output$hline9 <- renderUI({ 
    if(needs_review() %>% length() > 9){tags$hr(style="border-color:rgba(169,20,20,128);")}
    else{}
  })
  
  #make gt tables for trades that need reviewed
  output$tradeReview1 = render_gt(if(needs_review() %>% length() >= 1){
    incoming_by_team() %>%
      filter(trans_ID == needs_review()[1]) %>%
      mutate(players = str_replace_all(players, ", ", "<br>"),
             picks = str_replace_all(picks, ", ", "<br>"), 
             notes = str_replace_all(notes, ", ", "<br>"),
             notes = str_replace_all(notes, "NA", " ")) %>% 
      gt() %>%
      gt_img_rows(columns = logo_dark, height = 50) %>%
      gt_theme_pff() %>%
      #dark borders between trades
      tab_style(
        style = list(
          cell_borders(sides = "top", weight = px(5))),
        locations = cells_body(rows = first==TRUE)) %>%
      #vertical align in players and picks cells
      tab_style(
        style = "vertical-align:top",
        locations = cells_body(columns = c("players", "picks", "notes"))) %>%
      cols_hide(columns = c(trans_ID, to_team, first)) %>%
      fmt_markdown(columns = c("players", "picks", "notes")) %>%
      cols_width(players ~ pct(20),
                 logo_dark ~ pct(10),
                 picks ~ pct(20),
                 notes ~ pct(50)) %>%
      cols_label(logo_dark = "Team")
    } else{})
  
  output$tradeReview2 = render_gt(if(needs_review() %>% length() >= 2){
    incoming_by_team() %>%
      filter(trans_ID == needs_review()[2]) %>%
      mutate(players = str_replace_all(players, ", ", "<br>"),
             picks = str_replace_all(picks, ", ", "<br>"), 
             notes = str_replace_all(notes, ", ", "<br>"),
             notes = str_replace_all(notes, "NA", " ")) %>% 
      gt() %>%
      gt_img_rows(columns = logo_dark, height = 50) %>%
      gt_theme_pff() %>%
      #dark borders between trades
      tab_style(
        style = list(
          cell_borders(sides = "top", weight = px(5))),
        locations = cells_body(rows = first==TRUE)) %>%
      #vertical align in players and picks cells
      tab_style(
        style = "vertical-align:top",
        locations = cells_body(columns = c("players", "picks", "notes"))) %>%
      cols_hide(columns = c(trans_ID, to_team, first)) %>%
      fmt_markdown(columns = c("players", "picks", "notes")) %>%
      cols_width(players ~ pct(20),
                 logo_dark ~ pct(10),
                 picks ~ pct(25),
                 notes ~ pct(45)) %>%
      cols_align(align = "left",
                 columns = "notes") %>%
      cols_label(logo_dark = "Team")
  } else{})
  
  output$tradeReview3 = render_gt(if(needs_review() %>% length() >= 3){
    incoming_by_team() %>%
      filter(trans_ID == needs_review()[3]) %>%
      mutate(players = str_replace_all(players, ", ", "<br>"),
             picks = str_replace_all(picks, ", ", "<br>"), 
             notes = str_replace_all(notes, ", ", "<br>"),
             notes = str_replace_all(notes, "NA", " ")) %>% 
      gt() %>%
      gt_img_rows(columns = logo_dark, height = 50) %>%
      gt_theme_pff() %>%
      #dark borders between trades
      tab_style(
        style = list(
          cell_borders(sides = "top", weight = px(5))),
        locations = cells_body(rows = first==TRUE)) %>%
      #vertical align in players and picks cells
      tab_style(
        style = "vertical-align:top",
        locations = cells_body(columns = c("players", "picks", "notes"))) %>%
      cols_hide(columns = c(trans_ID, to_team, first)) %>%
      fmt_markdown(columns = c("players", "picks", "notes")) %>%
      cols_width(players ~ pct(20),
                 logo_dark ~ pct(10),
                 picks ~ pct(25),
                 notes ~ pct(45)) %>%
      cols_align(align = "left",
                 columns = "notes") %>%
      cols_label(logo_dark = "Team")
  } else{})
  
  output$tradeReview4 = render_gt(if(needs_review() %>% length() >= 4){
    incoming_by_team() %>%
      filter(trans_ID == needs_review()[4]) %>%
      mutate(players = str_replace_all(players, ", ", "<br>"),
             picks = str_replace_all(picks, ", ", "<br>"), 
             notes = str_replace_all(notes, ", ", "<br>"),
             notes = str_replace_all(notes, "NA", " ")) %>% 
      gt() %>%
      gt_img_rows(columns = logo_dark, height = 50) %>%
      gt_theme_pff() %>%
      #dark borders between trades
      tab_style(
        style = list(
          cell_borders(sides = "top", weight = px(5))),
        locations = cells_body(rows = first==TRUE)) %>%
      #vertical align in players and picks cells
      tab_style(
        style = "vertical-align:top",
        locations = cells_body(columns = c("players", "picks", "notes"))) %>%
      cols_hide(columns = c(trans_ID, to_team, first)) %>%
      fmt_markdown(columns = c("players", "picks", "notes")) %>%
      cols_width(players ~ pct(20),
                 logo_dark ~ pct(10),
                 picks ~ pct(25),
                 notes ~ pct(45)) %>%
      cols_align(align = "left",
                 columns = "notes") %>%
      cols_label(logo_dark = "Team")
  } else{})
  
  output$tradeReview5 = render_gt(if(needs_review() %>% length() >= 5){
    incoming_by_team() %>%
      filter(trans_ID == needs_review()[5]) %>%
      mutate(players = str_replace_all(players, ", ", "<br>"),
             picks = str_replace_all(picks, ", ", "<br>"), 
             notes = str_replace_all(notes, ", ", "<br>"),
             notes = str_replace_all(notes, "NA", " ")) %>% 
      gt() %>%
      gt_img_rows(columns = logo_dark, height = 50) %>%
      gt_theme_pff() %>%
      #dark borders between trades
      tab_style(
        style = list(
          cell_borders(sides = "top", weight = px(5))),
        locations = cells_body(rows = first==TRUE)) %>%
      #vertical align in players and picks cells
      tab_style(
        style = "vertical-align:top",
        locations = cells_body(columns = c("players", "picks", "notes"))) %>%
      cols_hide(columns = c(trans_ID, to_team, first)) %>%
      fmt_markdown(columns = c("players", "picks", "notes")) %>%
      cols_width(players ~ pct(20),
                 logo_dark ~ pct(10),
                 picks ~ pct(25),
                 notes ~ pct(45)) %>%
      cols_align(align = "left",
                 columns = "notes") %>%
      cols_label(logo_dark = "Team")
  } else{})
  
  output$tradeReview6 = render_gt(if(needs_review() %>% length() >= 6){
    incoming_by_team() %>%
      filter(trans_ID == needs_review()[6]) %>%
      mutate(players = str_replace_all(players, ", ", "<br>"),
             picks = str_replace_all(picks, ", ", "<br>"), 
             notes = str_replace_all(notes, ", ", "<br>"),
             notes = str_replace_all(notes, "NA", " ")) %>% 
      gt() %>%
      gt_img_rows(columns = logo_dark, height = 50) %>%
      gt_theme_pff() %>%
      #dark borders between trades
      tab_style(
        style = list(
          cell_borders(sides = "top", weight = px(5))),
        locations = cells_body(rows = first==TRUE)) %>%
      #vertical align in players and picks cells
      tab_style(
        style = "vertical-align:top",
        locations = cells_body(columns = c("players", "picks", "notes"))) %>%
      cols_hide(columns = c(trans_ID, to_team, first)) %>%
      fmt_markdown(columns = c("players", "picks", "notes")) %>%
      cols_width(players ~ pct(20),
                 logo_dark ~ pct(10),
                 picks ~ pct(25),
                 notes ~ pct(45)) %>%
      cols_align(align = "left",
                 columns = "notes") %>%
      cols_label(logo_dark = "Team")
  } else{})
  
  output$tradeReview7 = render_gt(if(needs_review() %>% length() >= 7){
    incoming_by_team() %>%
      filter(trans_ID == needs_review()[7]) %>%
      mutate(players = str_replace_all(players, ", ", "<br>"),
             picks = str_replace_all(picks, ", ", "<br>"), 
             notes = str_replace_all(notes, ", ", "<br>"),
             notes = str_replace_all(notes, "NA", " ")) %>% 
      gt() %>%
      gt_img_rows(columns = logo_dark, height = 50) %>%
      gt_theme_pff() %>%
      #dark borders between trades
      tab_style(
        style = list(
          cell_borders(sides = "top", weight = px(5))),
        locations = cells_body(rows = first==TRUE)) %>%
      #vertical align in players and picks cells
      tab_style(
        style = "vertical-align:top",
        locations = cells_body(columns = c("players", "picks", "notes"))) %>%
      cols_hide(columns = c(trans_ID, to_team, first)) %>%
      fmt_markdown(columns = c("players", "picks", "notes")) %>%
      cols_width(players ~ pct(20),
                 logo_dark ~ pct(10),
                 picks ~ pct(25),
                 notes ~ pct(45)) %>%
      cols_align(align = "left",
                 columns = "notes") %>%
      cols_label(logo_dark = "Team")
  } else{})
  
  output$tradeReview8 = render_gt(if(needs_review() %>% length() >= 8){
    incoming_by_team() %>%
      filter(trans_ID == needs_review()[8]) %>%
      mutate(players = str_replace_all(players, ", ", "<br>"),
             picks = str_replace_all(picks, ", ", "<br>"), 
             notes = str_replace_all(notes, ", ", "<br>"),
             notes = str_replace_all(notes, "NA", " ")) %>% 
      gt() %>%
      gt_img_rows(columns = logo_dark, height = 50) %>%
      gt_theme_pff() %>%
      #dark borders between trades
      tab_style(
        style = list(
          cell_borders(sides = "top", weight = px(5))),
        locations = cells_body(rows = first==TRUE)) %>%
      #vertical align in players and picks cells
      tab_style(
        style = "vertical-align:top",
        locations = cells_body(columns = c("players", "picks", "notes"))) %>%
      cols_hide(columns = c(trans_ID, to_team, first)) %>%
      fmt_markdown(columns = c("players", "picks", "notes")) %>%
      cols_width(players ~ pct(20),
                 logo_dark ~ pct(10),
                 picks ~ pct(25),
                 notes ~ pct(45)) %>%
      cols_align(align = "left",
                 columns = "notes") %>%
      cols_label(logo_dark = "Team")
  } else{})
  
  output$tradeReview9 = render_gt(if(needs_review() %>% length() >= 9){
    incoming_by_team() %>%
      filter(trans_ID == needs_review()[9]) %>%
      mutate(players = str_replace_all(players, ", ", "<br>"),
             picks = str_replace_all(picks, ", ", "<br>"), 
             notes = str_replace_all(notes, ", ", "<br>"),
             notes = str_replace_all(notes, "NA", " ")) %>% 
      gt() %>%
      gt_img_rows(columns = logo_dark, height = 50) %>%
      gt_theme_pff() %>%
      #dark borders between trades
      tab_style(
        style = list(
          cell_borders(sides = "top", weight = px(5))),
        locations = cells_body(rows = first==TRUE)) %>%
      #vertical align in players and picks cells
      tab_style(
        style = "vertical-align:top",
        locations = cells_body(columns = c("players", "picks", "notes"))) %>%
      cols_hide(columns = c(trans_ID, to_team, first)) %>%
      fmt_markdown(columns = c("players", "picks", "notes")) %>%
      cols_width(players ~ pct(20),
                 logo_dark ~ pct(10),
                 picks ~ pct(25),
                 notes ~ pct(45)) %>%
      cols_align(align = "left",
                 columns = "notes") %>%
      cols_label(logo_dark = "Team")
  } else{})
  
  output$tradeReview10 = render_gt(if(needs_review() %>% length() >= 10){
    incoming_by_team() %>%
      filter(trans_ID == needs_review()[10]) %>%
      mutate(players = str_replace_all(players, ", ", "<br>"),
             picks = str_replace_all(picks, ", ", "<br>"), 
             notes = str_replace_all(notes, ", ", "<br>"),
             notes = str_replace_all(notes, "NA", " ")) %>% 
      gt() %>%
      gt_img_rows(columns = logo_dark, height = 50) %>%
      gt_theme_pff() %>%
      #dark borders between trades
      tab_style(
        style = list(
          cell_borders(sides = "top", weight = px(5))),
        locations = cells_body(rows = first==TRUE)) %>%
      #vertical align in players and picks cells
      tab_style(
        style = "vertical-align:top",
        locations = cells_body(columns = c("players", "picks", "notes"))) %>%
      cols_hide(columns = c(trans_ID, to_team, first)) %>%
      fmt_markdown(columns = c("players", "picks", "notes")) %>%
      cols_width(players ~ pct(20),
                 logo_dark ~ pct(10),
                 picks ~ pct(25),
                 notes ~ pct(45)) %>%
      cols_align(align = "left",
                 columns = "notes") %>%
      cols_label(logo_dark = "Team")
  } else{})
  
  
  #for each trade that needs reviewed, have a df to replace the transaction log when the review is submitted
  tl_tr1 = reactive(if(needs_review() %>% length() >= 1){
    transaction_log() %>%
      mutate(status = case_when(trans_ID == needs_review()[1] & input$judgeReview1 == "Approve" ~ "Approved",
                                trans_ID == needs_review()[1] & input$judgeReview1 == "Void" ~ "Voided",
                                .default = status)) %>%
      mutate(judge_note = ifelse(trans_ID == needs_review()[1],
                                 c(input$judgeNotes1, rep("", transaction_log() %>% filter(trans_ID == needs_review()[1]) %>% nrow() - 1)),
                                 judge_note))
    } else{})
  
  tl_tr2 = reactive(if(needs_review() %>% length() >= 2){
    transaction_log() %>%
      mutate(status = case_when(trans_ID == needs_review()[2] & input$judgeReview2 == "Approve" ~ "Approved",
                                trans_ID == needs_review()[2] & input$judgeReview2 == "Void" ~ "Voided",
                                .default = status)) %>%
      mutate(judge_note = ifelse(trans_ID == needs_review()[2],
                                 c(input$judgeNotes2, rep("", transaction_log() %>% filter(trans_ID == needs_review()[2]) %>% nrow() - 1)),
                                 judge_note))
  } else{})
  
  tl_tr3 = reactive(if(needs_review() %>% length() >= 3){
    transaction_log() %>%
      mutate(status = case_when(trans_ID == needs_review()[3] & input$judgeReview3 == "Approve" ~ "Approved",
                                trans_ID == needs_review()[3] & input$judgeReview3 == "Void" ~ "Voided",
                                .default = status)) %>%
      mutate(judge_note = ifelse(trans_ID == needs_review()[3],
                                 c(input$judgeNotes3, rep("", transaction_log() %>% filter(trans_ID == needs_review()[3]) %>% nrow() - 1)),
                                 judge_note))
  } else{})
  
  tl_tr4 = reactive(if(needs_review() %>% length() >= 4){
    transaction_log() %>%
      mutate(status = case_when(trans_ID == needs_review()[4] & input$judgeReview4 == "Approve" ~ "Approved",
                                trans_ID == needs_review()[4] & input$judgeReview4 == "Void" ~ "Voided",
                                .default = status)) %>%
      mutate(judge_note = ifelse(trans_ID == needs_review()[4],
                                 c(input$judgeNotes4, rep("", transaction_log() %>% filter(trans_ID == needs_review()[4]) %>% nrow() - 1)),
                                 judge_note))
  } else{})
  
  tl_tr5 = reactive(if(needs_review() %>% length() >= 5){
    transaction_log() %>%
      mutate(status = case_when(trans_ID == needs_review()[5] & input$judgeReview5 == "Approve" ~ "Approved",
                                trans_ID == needs_review()[5] & input$judgeReview5 == "Void" ~ "Voided",
                                .default = status)) %>%
      mutate(judge_note = ifelse(trans_ID == needs_review()[5],
                                 c(input$judgeNotes5, rep("", transaction_log() %>% filter(trans_ID == needs_review()[5]) %>% nrow() - 1)),
                                 judge_note))
  } else{})
  
  tl_tr6 = reactive(if(needs_review() %>% length() >= 6){
    transaction_log() %>%
      mutate(status = case_when(trans_ID == needs_review()[6] & input$judgeReview6 == "Approve" ~ "Approved",
                                trans_ID == needs_review()[6] & input$judgeReview6 == "Void" ~ "Voided",
                                .default = status)) %>%
      mutate(judge_note = ifelse(trans_ID == needs_review()[6],
                                 c(input$judgeNotes6, rep("", transaction_log() %>% filter(trans_ID == needs_review()[6]) %>% nrow() - 1)),
                                 judge_note))
  } else{})
  
  tl_tr7 = reactive(if(needs_review() %>% length() >= 7){
    transaction_log() %>%
      mutate(status = case_when(trans_ID == needs_review()[7] & input$judgeReview7 == "Approve" ~ "Approved",
                                trans_ID == needs_review()[7] & input$judgeReview7 == "Void" ~ "Voided",
                                .default = status)) %>%
      mutate(judge_note = ifelse(trans_ID == needs_review()[7],
                                 c(input$judgeNotes7, rep("", transaction_log() %>% filter(trans_ID == needs_review()[7]) %>% nrow() - 1)),
                                 judge_note))
  } else{})
  
  tl_tr8 = reactive(if(needs_review() %>% length() >= 8){
    transaction_log() %>%
      mutate(status = case_when(trans_ID == needs_review()[8] & input$judgeReview8 == "Approve" ~ "Approved",
                                trans_ID == needs_review()[8] & input$judgeReview8 == "Void" ~ "Voided",
                                .default = status)) %>%
      mutate(judge_note = ifelse(trans_ID == needs_review()[8],
                                 c(input$judgeNotes8, rep("", transaction_log() %>% filter(trans_ID == needs_review()[8]) %>% nrow() - 1)),
                                 judge_note))
  } else{})
  
  tl_tr9 = reactive(if(needs_review() %>% length() >= 9){
    transaction_log() %>%
      mutate(status = case_when(trans_ID == needs_review()[9] & input$judgeReview9 == "Approve" ~ "Approved",
                                trans_ID == needs_review()[9] & input$judgeReview9 == "Void" ~ "Voided",
                                .default = status)) %>%
      mutate(judge_note = ifelse(trans_ID == needs_review()[9],
                                 c(input$judgeNotes9, rep("", transaction_log() %>% filter(trans_ID == needs_review()[9]) %>% nrow() - 1)),
                                 judge_note))
  } else{})
  
  tl_tr10 = reactive(if(needs_review() %>% length() >= 10){
    transaction_log() %>%
      mutate(status = case_when(trans_ID == needs_review()[10] & input$judgeReview10 == "Approve" ~ "Approved",
                                trans_ID == needs_review()[10] & input$judgeReview10 == "Void" ~ "Voided",
                                .default = status)) %>%
      mutate(judge_note = ifelse(trans_ID == needs_review()[10],
                                 c(input$judgeNotes10, rep("", transaction_log() %>% filter(trans_ID == needs_review()[10]) %>% nrow() - 1)),
                                 judge_note))
  } else{})
  
  
  
  #judge review submission
  observeEvent(input$judgeSubmit1, {
    showModal(
      modalDialog(
        easyClose = TRUE,
        title = "Confirm decsion for this pending trade?",
        renderText(paste("You are currently choosing to", input$judgeReview1, "this trade.")),
        footer = tagList(actionButton("confirmJudgeChoice1", "Confirm"),
                         actionButton("notyetJudgeChoice1", "Not Yet"))
        )
      )
    })
  observeEvent(input$judgeSubmit2, {
    showModal(
      modalDialog(
        easyClose = TRUE,
        title = "Confirm decsion for this pending trade?",
        renderText(paste("You are currently choosing to", input$judgeReview2, "this trade.")),
        footer = tagList(actionButton("confirmJudgeChoice2", "Confirm"),
                         actionButton("notyetJudgeChoice2", "Not Yet"))
      )
    )
  })
  observeEvent(input$judgeSubmit3, {
    showModal(
      modalDialog(
        easyClose = TRUE,
        title = "Confirm decsion for this pending trade?",
        renderText(paste("You are currently choosing to", input$judgeReview3, "this trade.")),
        footer = tagList(actionButton("confirmJudgeChoice3", "Confirm"),
                         actionButton("notyetJudgeChoice3", "Not Yet"))
      )
    )
  })
  observeEvent(input$judgeSubmit4, {
    showModal(
      modalDialog(
        easyClose = TRUE,
        title = "Confirm decsion for this pending trade?",
        renderText(paste("You are currently choosing to", input$judgeReview4, "this trade.")),
        footer = tagList(actionButton("confirmJudgeChoice4", "Confirm"),
                         actionButton("notyetJudgeChoice4", "Not Yet"))
      )
    )
  })
  observeEvent(input$judgeSubmit5, {
    showModal(
      modalDialog(
        easyClose = TRUE,
        title = "Confirm decsion for this pending trade?",
        renderText(paste("You are currently choosing to", input$judgeReview5, "this trade.")),
        footer = tagList(actionButton("confirmJudgeChoice5", "Confirm"),
                         actionButton("notyetJudgeChoice5", "Not Yet"))
      )
    )
  })
  observeEvent(input$judgeSubmit6, {
    showModal(
      modalDialog(
        easyClose = TRUE,
        title = "Confirm decsion for this pending trade?",
        renderText(paste("You are currently choosing to", input$judgeReview6, "this trade.")),
        footer = tagList(actionButton("confirmJudgeChoice6", "Confirm"),
                         actionButton("notyetJudgeChoice6", "Not Yet"))
      )
    )
  })
  observeEvent(input$judgeSubmit7, {
    showModal(
      modalDialog(
        easyClose = TRUE,
        title = "Confirm decsion for this pending trade?",
        renderText(paste("You are currently choosing to", input$judgeReview7, "this trade.")),
        footer = tagList(actionButton("confirmJudgeChoice7", "Confirm"),
                         actionButton("notyetJudgeChoice7", "Not Yet"))
      )
    )
  })
  observeEvent(input$judgeSubmit8, {
    showModal(
      modalDialog(
        easyClose = TRUE,
        title = "Confirm decsion for this pending trade?",
        renderText(paste("You are currently choosing to", input$judgeReview8, "this trade.")),
        footer = tagList(actionButton("confirmJudgeChoice8", "Confirm"),
                         actionButton("notyetJudgeChoice8", "Not Yet"))
      )
    )
  })
  observeEvent(input$judgeSubmit9, {
    showModal(
      modalDialog(
        easyClose = TRUE,
        title = "Confirm decsion for this pending trade?",
        renderText(paste("You are currently choosing to", input$judgeReview9, "this trade.")),
        footer = tagList(actionButton("confirmJudgeChoice9", "Confirm"),
                         actionButton("notyetJudgeChoice9", "Not Yet"))
      )
    )
  })
  observeEvent(input$judgeSubmit10, {
    showModal(
      modalDialog(
        easyClose = TRUE,
        title = "Confirm decsion for this pending trade?",
        renderText(paste("You are currently choosing to", input$judgeReview10, "this trade.")),
        footer = tagList(actionButton("confirmJudgeChoice10", "Confirm"),
                         actionButton("notyetJudgeChoice10", "Not Yet"))
      )
    )
  })
  
  #not yet button from modal popup
  observeEvent(input$notyetJudgeChoice1, {
    removeModal()
  })
  observeEvent(input$notyetJudgeChoice2, {
    removeModal()
  })
  observeEvent(input$notyetJudgeChoice3, {
    removeModal()
  })
  observeEvent(input$notyetJudgeChoice4, {
    removeModal()
  })
  observeEvent(input$notyetJudgeChoice5, {
    removeModal()
  })
  observeEvent(input$notyetJudgeChoice6, {
    removeModal()
  })
  observeEvent(input$notyetJudgeChoice7, {
    removeModal()
  })
  observeEvent(input$notyetJudgeChoice8, {
    removeModal()
  })
  observeEvent(input$notyetJudgeChoice9, {
    removeModal()
  })
  observeEvent(input$notyetJudgeChoice10, {
    removeModal()
  })
  
  #confirm trade
  observeEvent(input$confirmJudgeChoice1, {
    sheet_write(tl_tr1(), ss, sheet = 1)
    transaction_log()
    session$reload()
  })
  observeEvent(input$confirmJudgeChoice2, {
    sheet_write(tl_tr2(), ss, sheet = 1)
    transaction_log()
    session$reload()
  })
  observeEvent(input$confirmJudgeChoice3, {
    sheet_write(tl_tr3(), ss, sheet = 1)
    transaction_log()
    session$reload()
  })
  observeEvent(input$confirmJudgeChoice4, {
    sheet_write(tl_tr4(), ss, sheet = 1)
    transaction_log()
    session$reload()
  })
  observeEvent(input$confirmJudgeChoice5, {
    sheet_write(tl_tr5(), ss, sheet = 1)
    transaction_log()
    session$reload()
  })
  observeEvent(input$confirmJudgeChoice6, {
    sheet_write(tl_tr6(), ss, sheet = 1)
    transaction_log()
    session$reload()
  })
  observeEvent(input$confirmJudgeChoice7, {
    sheet_write(tl_tr7(), ss, sheet = 1)
    transaction_log()
    session$reload()
  })
  observeEvent(input$confirmJudgeChoice8, {
    sheet_write(tl_tr8(), ss, sheet = 1)
    transaction_log()
    session$reload()
  })
  observeEvent(input$confirmJudgeChoice9, {
    sheet_write(tl_tr9(), ss, sheet = 1)
    transaction_log()
    session$reload()
  })
  observeEvent(input$confirmJudgeChoice10, {
    sheet_write(tl_tr10(), ss, sheet = 1)
    transaction_log()
    session$reload()
  })
  
  
}


shinyApp(ui, server)
