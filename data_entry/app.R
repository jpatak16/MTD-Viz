library(pacman)
p_load(shiny, magrittr, dplyr, stringr, gt, gtExtras, googlesheets4)

transaction_log = read_sheet("https://docs.google.com/spreadsheets/d/1Ti24DVdNRNHGZ4IKGJ342dCDSvycJtdfC8OFh0jlLEI/edit?usp=sharing")

nba_teams = hoopR::espn_nba_teams() %>% pull(display_name)

# Define UI for application that draws a histogram
ui = navbarPage("2023 Mock Trade Deadline", fluid = TRUE,
                tabPanel("Trade Entry",
                         tags$style(type="text/css",
                                    ".shiny-output-error { visibility: hidden; }",
                                    ".shiny-output-error:before { visibility: hidden; }"),
                         fluidRow(column(9, h1(span("2023 Mock Trade Deadline", style = 'color:rgba(169,20,20,128);')), 
                                         h1(span("Trade Entry", style = 'font-size: 60px; font-weight: bold;'))),
                                  column(3, img(src="ASU-NTDC-Logo.png", height = 180, width = 240))),
                         fluidRow(column(6, selectizeInput("teamsInvolved", label = "Teams Involved", choices = nba_teams, multiple = T)),
                                  column(6, actionButton("submit", "Submit For Approval"))),
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
                         fluidRow(column(8, gt_output("tradeReview1")),
                                  column(4, radioButtons("judgeDecsion", "", c("Approve", "Void"), selected = NULL), actionButton("submitDecsion", "Submit Decsion")))))

# Define server logic required to draw a histogram
server <- function(input, output, session) {

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
  
  
  
  
  
  
  
  
  
  
  
  output$tradeReview1 = render_gt(transaction_log %>% gt() %>%
                                    gt_theme_pff() %>%
                                    cols_hide(c(trans_ID, note, status)) %>%
                                    cols_width(asset ~ px(200),
                                               away_from_team ~ px(200),
                                               to_team ~ px(200)))
  
  
}


shinyApp(ui, server)
