library(pacman)
p_load(shiny)

# Define UI for application that draws a histogram
ui = navbarPage("2023 Mock Trade Deadline", fluid = TRUE,
                tabPanel("Transaction Log",
                         fluidRow(column(9, h1(span("2023 Mock Trade Deadline", style = 'color:rgba(169,20,20,128);')), 
                                         h1(span("Transaction Log", style = 'font-size: 60px; font-weight: bold;'))),
                                  column(3, img(src="ASU-NTDC-Logo.png", height = 180, width = 240)))))

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
}


shinyApp(ui, server)
