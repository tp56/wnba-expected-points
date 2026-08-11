library(tidyverse)
library(dplyr)
library(ggrepel)
library(wehoop)


xp_residuals <- function(frame) {
  residuals <- frame
  residuals$residual_made <- frame$scored - frame$pred_make
  residuals$residual_score <- frame$score_value - frame$xP
  residuals$residual_oreb <- frame$offensive_rebound - frame$pred_oreb
  residuals$residual_dreb <- frame$defensive_rebound - frame$pred_dreb
  residuals$residual_foul <- frame$fouled - frame$pred_foul
  
  residuals
}

resids <- xp_residuals(xp25)

players <- purrr::map_dfr(
  unique(resids$team_id),
  ~ espn_wnba_team_roster(team_id = .x)
) |>
  mutate(athlete_id = as.integer(athlete_id))


resids <- resids |>
  left_join(
    players |>
      select(
        athlete_id,
        position_abbrev,
        height,
        age
      ),
    by = c("player_id" = "athlete_id")
  )

shot_efficiency <- function(resids) {
  shooter_tbl <- resids |>
    group_by(player = shooter) |>
    summarise(
      n_shots = n(),
      team_name = first(team_name),
      position_abbrev = first(position_abbrev),
      points_scored = sum(score_value, na.rm = TRUE),
      fg_percentage = mean(scored, na.rm = TRUE),
      xP_total_shots = sum(xP, na.rm = TRUE),
      xP_mean_shots  = mean(xP, na.rm = TRUE),
      pred_mean_made_shots = mean(pred_make, na.rm = TRUE),
      resid_score_mean_shots = mean(residual_score, na.rm = TRUE),
      resid_score_sum_shots  = sum(residual_score, na.rm = TRUE),
      resid_made_mean_shots  = mean(residual_made, na.rm = TRUE),
      resid_made_sum_shots  = sum(residual_made, na.rm = TRUE),
      relative_edge_shots = resid_score_sum_shots/xP_total_shots,

      n_foul_events = sum(fouled, na.rm = TRUE), 
      foul_rate = mean(fouled, na.rm = TRUE),
      pred_foul_mean = mean(pred_foul, na.rm = TRUE),
      resid_foul_mean = mean(residual_foul, na.rm = TRUE),
      resid_foul_sum  = sum(residual_foul, na.rm = TRUE),
      relative_foul_edge = resid_foul_sum / n_foul_events, 

      .groups = "drop"
    )
  
  #issue where only tags assists for shots made, so inflated by those who assist the most threes
  passer_tbl <- resids |>
    group_by(player = passer) |>
    summarise(
      n_assists = n(),
      xP_total_assists = sum(xP, na.rm = TRUE),
      xP_mean_assists  = mean(xP, na.rm = TRUE),
      pred_mean_assists = mean(pred_make, na.rm = TRUE),
      resid_score_mean_assists = mean(residual_score, na.rm = TRUE),
      resid_score_sum_assists  = sum(residual_score, na.rm = TRUE),
      resid_made_mean_assists  = mean(residual_made, na.rm = TRUE),
      relative_edge_assists = resid_score_sum_assists/xP_total_assists,
      .groups = "drop"
    )

  combined_tbl <- resids |>
    select(shooter, passer, xP, residual_score, residual_made) |>
    pivot_longer(
      cols = c(shooter, passer),
      names_to = "role",
      values_to = "player"
    ) |>
    group_by(player) |>
    summarise(
      n_events = n(),
      xP_total_combined = sum(xP, na.rm = TRUE),
      xP_mean_combined  = mean(xP, na.rm = TRUE),
      resid_score_mean_combined = mean(residual_score, na.rm = TRUE),
      resid_score_sum_combined  = sum(residual_score, na.rm = TRUE),
      resid_made_mean_combined  = mean(residual_made, na.rm = TRUE),
      relative_edge_combined = resid_score_sum_combined/xP_total_combined,
      .groups = "drop"
    )

  combined_tbl |>
    full_join(shooter_tbl, by = "player") |>
    full_join(passer_tbl, by = "player")
}

efficiency <- shot_efficiency(resids) |>
  filter(!is.na(player))

efficiency_ranked <- efficiency |>
  mutate(
    rank_xP_total_shots   = min_rank(desc(xP_total_shots)),
    rank_xP_mean_shots    = min_rank(desc(xP_mean_shots)),
    rank_pred_mean_made_shots = min_rank(desc(pred_mean_made_shots)),
    rank_resid_score_mean_shots = min_rank(desc(resid_score_mean_shots)),
  )

reb_rank <- resids |>
  mutate(
    residual_reb = coalesce(residual_oreb, residual_dreb),
    pred_reb = coalesce(pred_oreb, pred_dreb),
    player_id = rebounder_player_id
  ) |>
  group_by(rebounder) |>
  summarise(
    # team_id = first(team_id),
    # team_name = first(rebounder_team_name),
    player_id = first(rebounder_player_id),
    position_abbrev = first(position_abbrev),
    n_reb_events = n(),
    n_orebs = sum(offensive_rebound, na.rm = TRUE),
    n_drebs = sum(defensive_rebound, na.rm = TRUE),
    xP_oreb_sum = sum(xP_oreb, na.rm = TRUE),
    xP_dreb_sum = sum(xP_dreb, na.rm = TRUE),
    xP_oreb_mean = mean(xP_oreb, na.rm = TRUE),
    xP_dreb_mean = mean(xP_dreb, na.rm = TRUE),

    pred_reb_sum = sum(pred_reb, na.rm = TRUE ),
    pred_oreb_sum = sum(pred_oreb, na.rm = TRUE),
    pred_oreb_mean = mean(pred_oreb, na.rm = TRUE),
    pred_dreb_sum = sum(pred_dreb, na.rm = TRUE),
    pred_dreb_mean = mean(pred_dreb, na.rm = TRUE),
    reb_resid_mean = mean(residual_reb, na.rm = TRUE),
    reb_resid_sum  = sum(residual_reb, na.rm = TRUE),

    oreb_resid_mean = mean(residual_oreb, na.rm = TRUE),
    dreb_resid_mean = mean(residual_dreb, na.rm = TRUE),

    oreb_resid_mean_suprise = mean(residual_oreb[offensive_rebound], na.rm = TRUE),
    dreb_resid_mean_suprise = mean(residual_dreb[defensive_rebound], na.rm = TRUE),

    .groups = "drop"
  ) |>
  filter(
    !is.na(rebounder),
    !str_detect(rebounder, regex("team", ignore_case = TRUE))
  )

efficiency_with_reb <- efficiency |>
  left_join(
    reb_rank,
    by = c("player" = "rebounder")
  )


summary_by_type <- resids |>
  group_by(type_text) |>
  summarise(
    across(
      c(points_attempted,
        pred_make, pred_oreb, pred_dreb, pred_foul,
        xP_shot, xP_oreb, xP_dreb, xP_foul, xP,
        residual_made, residual_score,
        residual_oreb, residual_dreb
      ),
      ~ mean(.x, na.rm = TRUE)
    ),
    n = n(),
    .groups = "drop"
  )

team_performance <- resids |>
  group_by(team_name) |>
  summarise(
    across(
      where(is.numeric),
      ~ sum(.x, na.rm = TRUE)
    ),
    n = n(),
    .groups = "drop"
  ) |>
  select(-c(game_id, team_id, season, player_id, rebounder_player_id, x, y, dis, dis_log,
            angle, angle_sq, drive, run, pullup, stepback, fade, cut, time_left, rebounder_team_id))



