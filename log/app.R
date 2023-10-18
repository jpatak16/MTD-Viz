library(pacman)
p_load(shiny, magrittr, dplyr, stringr, gt, gtExtras)

transaction_log = read_sheet("https://docs.google.com/spreadsheets/d/1Ti24DVdNRNHGZ4IKGJ342dCDSvycJtdfC8OFh0jlLEI/edit?usp=sharing")


players = transaction_log %>% 
  group_by(trans_ID, to_team) %>%
  arrange(asset) %>% 
  filter(substr(asset, 1, 2) != '20',
         substr(asset, 1, 19) != "Cash Considerations") %>%
  mutate(players = paste(asset, collapse = ", ")) %>%
  select(trans_ID, to_team, players) %>%
  unique()

picks = transaction_log %>% 
  group_by(trans_ID, to_team) %>%
  arrange(asset) %>% 
  filter(substr(asset, 1, 2) == '20' | substr(asset, 1, 19) == "Cash Considerations") %>%
  mutate(picks = paste(asset, collapse = ", "),
         notes = paste(note, collapse = ", ")) %>%
  select(trans_ID, to_team, picks, notes) %>%
  unique()

incoming_by_team = full_join(players, picks, by = c('trans_ID', 'to_team')) %>%
  mutate(players = ifelse(is.na(players), "", players)) %>%
  mutate(picks = ifelse(is.na(picks), "", picks)) %>%
  mutate(notes = ifelse(is.na(notes), "", notes)) %>%
  select(trans_ID, to_team, players, picks, notes) %>%
  ungroup() %>%
  arrange(trans_ID, to_team) %>%
  mutate(first = c(1, diff(trans_ID)) == 1) %>%
  left_join(hoopR::espn_nba_teams(), by = c("to_team" = "display_name")) %>%
  select(trans_ID, to_team, logo_dark, players, picks, first, notes)

rm(players, picks)

gt_transaction_log = incoming_by_team %>%
  mutate(players = str_replace_all(players, ", ", "<br>"),
         picks = str_replace_all(picks, ", ", "<br>"), 
         notes = str_replace_all(notes, ", ", "<br>"),
         notes = str_replace_all(notes, "NA", " ")) %>% 
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
    locations = cells_body(columns = c("players", "picks", "notes"))) %>%
  cols_hide(columns = c(trans_ID, to_team, first)) %>%
  fmt_markdown(columns = c("players", "picks", "notes")) %>%
  cols_width(players ~ px(200),
             logo_dark ~ px(200),
             picks ~ px(200)) %>%
  cols_label(logo_dark = "Team")
  

# Define UI for application that draws a histogram
ui = navbarPage("2023 Mock Trade Deadline", fluid = TRUE,
                tabPanel("Transaction Log",
                         fluidRow(column(9, h1(span("2023 Mock Trade Deadline", style = 'color:rgba(169,20,20,128);')), 
                                         h1(span("Transaction Log", style = 'font-size: 60px; font-weight: bold;'))),
                                  column(3, img(src="ASU-NTDC-Logo.png", height = 180, width = 240))),
                         fluidRow(column(12, gt_output("TransactionLog")))))

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  output$TransactionLog <- render_gt(gt_transaction_log)
  
}


shinyApp(ui, server)
