pacman::p_load(dplyr, gt, gtExtras, gtUtils)

gt_color_pills_1r <- function(gt_object, columns, palette = c("#C84630", "#5DA271"),
                           fill_type = "continuous", rank_order = "desc",
                           digits = NULL, domain = NULL, format_type = "number",
                           scale_percent = TRUE, suffix = "", reverse = FALSE,
                           outline_color = NULL, outline_width = 0.25,
                           pal_type = "discrete", pill_height = 25, ...) {
  
  stopifnot(`Table must be of class 'gt_tbl'` = "gt_tbl" %in% class(gt_object))
  
  data <- gt_object[["_data"]]
  
  column_data <- data[[rlang::as_string(rlang::ensym(columns))]]
  
  if (fill_type == "rank") {
    ranked_column <- rank(column_data, na.last = "keep", ties.method = "average")
    
    if (rank_order == "desc") {
      ranked_column <- max(ranked_column, na.rm = TRUE) - ranked_column + 1
    }
  } else {
    ranked_column <- column_data
  }
  
  if (is.null(domain)) {
    domain <- range(ranked_column, na.rm = TRUE)
    warning("Domain not specified, defaulting to observed range within the specified column.", call. = FALSE)
  }
  
  pal <- if (grepl(x = palette[1], pattern = "::")) {
    paletteer::paletteer_d(palette = palette, direction = if (reverse) -1 else 1, type = pal_type) %>% as.character()
  } else {
    if (reverse) rev(palette) else palette
  }
  
  format_value <- function(value, digits, format_type) {
    if (format_type == "percent" && scale_percent) {
      value <- value * 100
    }
    
    if (!is.null(digits)) {
      value <- round(value, digits)
    }
    
    formatted_value <- switch(format_type,
                              "number" = formatC(value, format = "f", digits = digits),
                              "comma" = formatC(value, format = "f", big.mark = ",", digits = digits),
                              "currency" = paste0("$", formatC(value, format = "f", big.mark = ",", digits = digits)),
                              "percent" = paste0(formatC(value, format = "f", digits = digits), "%"),
                              as.character(value))
    
    return(paste0(formatted_value, suffix))
  }
  
  formatted_values <- sapply(column_data, function(x) format_value(x, digits, format_type))
  max_width <- max(nchar(formatted_values))
  
  generate_pill_html <- function(value, rank_value) {
    color <- scales::col_numeric(palette = pal, domain = domain)(rank_value)
    text_color <- gt:::ideal_fgnd_color(color)
    formatted_value <- format_value(as.numeric(value), digits, format_type)
    
    outline_style <- if (!is.null(outline_color)) glue::glue("border: {outline_width}px solid {outline_color};") else ""
    
    glue::glue("<span style='display: inline-block; width: {max_width}ch; padding-left: 10px; padding-right: 10px; height: {pill_height}px; line-height: {pill_height}px; background-color: {color}; color: {text_color}; border-radius: 10px; text-align: center; {outline_style}'>{formatted_value}</span>")
  }
  
  gt_object %>%
    text_transform(
      locations = cells_body(columns = {{ columns }}, rows = 1),
      fn = function(x) {
        mapply(generate_pill_html, x, ranked_column)
      }
    )
}

df = read.csv("Apps_2024/teamresults.csv") |>
  mutate(
    title = "Score"
  )
notes_df = read.csv("Apps_2024/results.csv") |>
  rename(
    notes = 9,
    team = 3
  ) |>
  filter(
    !is.na(notes),
    notes != ""
  )

cols_to_rank = colnames(df)[3:7]
ranks <- df |> 
  mutate(
    across(all_of(cols_to_rank), ~ rank(-.)),
    title = "Rank"
  ) |>
  select(-total) |>
  rename(
    total = X
  )

final <- rbind(df |> select(-X), ranks)

for(t in df$team){
  
  logo <- final |>
    filter(team == t) |>
    left_join(
      hoopR::espn_nba_teams() |> 
        select(abbreviation, logo) |>
        mutate(
          abbreviation = case_when(
            abbreviation == "GS" ~ "GSW",
            abbreviation == "NO" ~ "NOP",
            abbreviation == "SA" ~ "SAS",
            abbreviation == "UTAH" ~ "UTA",
            abbreviation == "WSH" ~ "WAS",
            .default = abbreviation
          )
        ),
      by = c("team" = "abbreviation")
    ) |>
    pull(logo) |>
    unique()
  
  tab <- final |>
    filter(
      team == t
    ) |>
    select(
      title, battle_of_equity, cap_strategy, justification_of_moves, team_evaluation, overall_presentation, total 
    ) |>
    gt() |>
    gt_theme_espn() |>
    fmt_number(
      columns = c(cols_to_rank, total),
      decimals = 0
    ) |>
    gt_border_bars_bottom(
      bar_height = 7,
      colors = c("grey", "#f88b30", "black")
    ) |>
    gt_border_bars_top(
      bar_height = 4,
      colors = c("black", "#f88b30", "grey")
    ) |>
    tab_header(
      title = md(paste0("<div style='display: flex; justify-content: center; flex-direction: row; align-items: center; text-align: center;'><img src='https://events.asucollegeoflaw.com/nba-trade-deadline/wp-content/uploads/sites/39/2023/05/ASU-NTDC-Logo-300x240.png' style='height:150px;'>", 
                        "<img src=", logo, " style='height:130px;'></div>"))
    ) |>
    tab_style(
      style = list(cell_text(weight = "bold")),
      locations = cells_body(
        columns = title,
      )
    ) |>
    cols_label(
      title = "",
      battle_of_equity = "Battle of Equity",
      cap_strategy = "Cap Strategy",
      justification_of_moves = "Justification of Moves",
      team_evaluation = "Team Evaluation",
      overall_presentation = "Overall Presentation"
    ) |>
    gt_color_pills_1r(
      columns = battle_of_equity,
      digits = 0,
      domain = c(0, 42)
    ) |>
    gt_color_pills_1r(
      columns = cap_strategy,
      digits = 0,
      domain = c(0, 42)
    ) |>
    gt_color_pills_1r(
      columns = justification_of_moves,
      digits = 0,
      domain = c(0, 42)
    ) |>
    gt_color_pills_1r(
      columns = team_evaluation,
      digits = 0,
      domain = c(0, 42)
    ) |>
    gt_color_pills_1r(
      columns = overall_presentation,
      digits = 0,
      domain = c(0, 42)
    ) |>
    gt_color_pills_1r(
      columns = total,
      digits = 0,
      domain = c(0, 210)
    ) |>
    cols_width(
      battle_of_equity ~ px(100),
      cap_strategy ~ px(100),
      justification_of_moves ~ px(100),
      team_evaluation ~ px(100),
      overall_presentation ~ px(100),
      total ~ px(100)
    ) |>
    cols_align(
      align = c("center"),
      columns = c(battle_of_equity, cap_strategy, justification_of_moves, team_evaluation, overall_presentation, total)
    ) |>
    tab_style(
      style = list(cell_fill(color = "#f5f5f5")),
      locations = cells_body(
        columns = c(total)
      )
    ) |>
    tab_footnote(
      "Each category is out of 42 possible points. Total is out of 210 possible points."
    )
  
  gtsave(tab, paste0("Apps_2024/result_outputs/scores/", t, "_scores.png"))
  
  tab2 <- notes_df |>
    filter(
      team == t
    ) |>
    select(notes) |>
    mutate(row_type = ifelse(row_number() %% 2 == 1, "odd", "even")) |>
    gt() |>
    gt_theme_espn() |>
    gt_border_bars_bottom(
      bar_height = 7,
      colors = c("grey", "#f88b30", "black")
    ) |>
    gt_border_bars_top(
      bar_height = 4,
      colors = c("black", "#f88b30", "grey")
    ) |>
    tab_header(
      title = md(paste0("<div style='display: flex; justify-content: center; flex-direction: row; align-items: center; text-align: center;'><img src='https://events.asucollegeoflaw.com/nba-trade-deadline/wp-content/uploads/sites/39/2023/05/ASU-NTDC-Logo-300x240.png' style='height:150px;'>", 
                        "<img src=", logo, " style='height:130px;'></div>"))
    ) |>
    tab_style(
      style = list(cell_text(weight = "bold")),
      locations = cells_body(
        columns = notes,
      )
    ) |>
    cols_label(
      notes ~ "Notes From Our Judges"
    ) |>
    cols_width(
      notes ~ px(650)
    ) |>
    cols_align(
      align = c("center"),
      columns = c(notes)
    ) |>
    tab_style(
      style = list(cell_fill(color = "#f5f5f5")),
      locations = cells_body(
        columns = c(notes),
        rows = row_type == "even"
      )
    ) |>
    cols_hide(row_type)
  
  gtsave(tab2, paste0("Apps_2024/result_outputs/judge_notes/", t, "_notes.png"))
    
    
}
