library(dplyr)
library(scales)
library(stringr)

raw = read.csv('results.csv') |>
  mutate(
    Username = str_extract(Username, "^[^@]+")
  ) |>
  rename(
    username = 2,
    team = 3,
    battle_of_equity = 4,
    cap_strategy = 5,
    justification_of_moves = 6,
    team_evaluation = 7,
    overall_presentation = 8,
    notes = 9
  )

judges = raw |>
  summarize(
    n = n(),
    battle_of_equity = mean(battle_of_equity)*7,
    cap_strategy = mean(cap_strategy)*7,
    justification_of_moves = mean(justification_of_moves)*7,
    team_evaluation = mean(team_evaluation)*7,
    overall_presentation = mean(overall_presentation)*7,
    .by = username
  ) |>
  mutate(
    total = battle_of_equity + cap_strategy + justification_of_moves + 
      team_evaluation + overall_presentation
  )
  

results = raw |>
  summarize(
    battle_of_equity = mean(battle_of_equity)*7,
    cap_strategy = mean(cap_strategy)*7,
    justification_of_moves = mean(justification_of_moves)*7,
    team_evaluation = mean(team_evaluation)*7,
    overall_presentation = mean(overall_presentation)*7,
    .by = team
  ) |>
  mutate(
    total = battle_of_equity + cap_strategy + justification_of_moves + 
      team_evaluation + overall_presentation
  )

# Standardize each judge's scores
results_standardized <- raw |>
  mutate(
    total = battle_of_equity + cap_strategy + justification_of_moves + 
      team_evaluation + overall_presentation
  ) |>
  mutate(
    st_total = (total - mean(total)) / sd(total),
    .by = username
  ) |>
  summarize(
    battle_of_equity = mean(battle_of_equity)*7,
    cap_strategy = mean(cap_strategy)*7,
    justification_of_moves = mean(justification_of_moves)*7,
    team_evaluation = mean(team_evaluation)*7,
    overall_presentation = mean(overall_presentation)*7,
    total = mean(total)*7,
    st_total = mean(st_total),
    .by = team
  ) |>
  mutate(
    st_total = rescale(st_total, to = c(min(results$total), max(results$total)))
  ) |>
  mutate(
    battle_of_equity = (battle_of_equity/total)*st_total,
    cap_strategy = (cap_strategy/total)*st_total,
    justification_of_moves = (justification_of_moves/total)*st_total,
    team_evaluation = (team_evaluation/total)*st_total,
    overall_presentation = (overall_presentation/total)*st_total,
  ) |>
  select(-total)

comb_results <- results |>
  select(team, total) |>
  left_join(
    results_standardized |> select(team, st_total)
  )

