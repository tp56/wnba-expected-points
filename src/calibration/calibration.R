library(dplyr)
library(tidyr)
library(purrr)
library(yardstick)


# build frames -----------------------------------------------------------
# Train on full 2024 data
xp_frame_2024 <- make_xP_frame(data24)
xp_frame_2025 <- make_xP_frame(data)

# Build models on 2024
xp_model <- build_make_model(xp_frame_2024)
xoreb_model <- build_oreb_model(xp_frame_2024 |> filter(scored == 0))
xdreb_model <- build_dreb_model(xp_frame_2024 |> filter(scored == 0))
xfoul_model <- build_foul_model(xp_frame_2024)


# generate predictions ---------------------------------------------------

xp_frame_2025 <- xp_frame_2025 |>
  mutate(
    p_scored = predict(xp_model, newdata = xp_frame_2025, type = "response"),
    p_miss = 1 - p_scored,
    p_oreb = 0,
    p_dreb = 0,
    p_foul = predict(xfoul_model, newdata = xp_frame_2025, type = "response")
  )

# Fill rebound predictions only for missed shots
missed_idx <- which(xp_frame_2025$scored == 0)
xp_frame_2025$p_oreb[missed_idx] <- predict(xoreb_model, newdata = xp_frame_2025[missed_idx, ], type = "response")
xp_frame_2025$p_dreb[missed_idx] <- predict(xdreb_model, newdata = xp_frame_2025[missed_idx, ], type = "response")

# Calculate total xP
xp_frame_2025 <- xp_frame_2025 |> 
  mutate(
    xp = (p_scored * points_attempted) + 
         (p_miss * ((p_oreb * EOREB) - (p_dreb * EDREB)))
  )


# split data -------------------------------------------------------------

early_2025 <- xp_frame_2025 |> filter(game_date <= "2025-07-31")
late_2025 <- xp_frame_2025 |> filter(game_date > "2025-07-31")

# plot calibration curves ------------------------------------------------
plot_calibration <- function(df, pred_col, outcome_col, n_bins = 10, title = "Calibration Plot") {
  
  calibration_data <- df |>
    mutate(
      bin = ntile({{ pred_col }}, n_bins),
      outcome = {{ outcome_col }}
    ) |>
    group_by(bin) |>
    summarise(
      pred_mean = mean({{ pred_col }}, na.rm = TRUE),
      obs_mean = mean(outcome, na.rm = TRUE),
      n = n(),
      se = sqrt(obs_mean * (1 - obs_mean) / n),
      .groups = "drop"
    )
  
  plot <- ggplot(calibration_data, aes(x = pred_mean, y = obs_mean)) +
    geom_point(aes(size = n), alpha = 0.6, color = "#1f77b4") +
    geom_errorbar(
      aes(ymin = obs_mean - 1.96 * se, ymax = obs_mean + 1.96 * se), 
      width = 0.02, alpha = 0.5
    ) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red", size = 1) +
    xlim(0, 1) +
    ylim(0, 1) +
    labs(
      title = title,
      x = "Predicted Probability",
      y = "Observed Frequency",
      size = "Number of Shots"
    ) +
    theme_minimal() +
    theme(
      aspect.ratio = 1,
      plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
      panel.grid.major = element_line(color = "lightgray", size = 0.3),
      panel.grid.minor = element_blank()
    )
  
  return(plot)
}

prob_metrics <- function(data, truth, estimate, metric_name) {
  truth_col <- enquo(truth)
  estimate_col <- enquo(estimate)
  
  data %>%
    mutate(
      truth = !!truth_col,
      estimate = !!estimate_col
    ) %>%
    filter(is.finite(truth) & is.finite(estimate) & !is.na(truth) & !is.na(estimate)) %>%
    summarise(
      brier = mean((truth - estimate)^2, na.rm = TRUE),
      roc_auc = roc_auc_vec(factor(truth), estimate, event_level = "second"),
      .groups = "drop"
    ) %>%
    pivot_longer(cols = everything(), names_to = "metric", values_to = "value") %>%
    mutate(metric_type = metric_name)
}

xP_metrics <- function(data, outcome, prediction, metric_name) {
  outcome_col <- enquo(outcome)
  prediction_col <- enquo(prediction)
  
  data %>%
    mutate(
      outcome = !!outcome_col,
      prediction = !!prediction_col
    ) %>%
    filter(is.finite(outcome) & is.finite(prediction) & !is.na(outcome) & !is.na(prediction)) %>%
    summarise(
      rmse = sqrt(mean((outcome - prediction)^2, na.rm = TRUE)),
      mae = mean(abs(outcome - prediction), na.rm = TRUE),
      me = mean(outcome - prediction, na.rm = TRUE),
      median_ae = median(abs(outcome - prediction)),
      calib_intercept = coef(lm(outcome ~ prediction))[1],
      calib_slope = coef(lm(outcome ~ prediction))[2],
      .groups = "drop"
    ) %>%
    pivot_longer(cols = everything(), names_to = "metric", values_to = "value") %>%
    mutate(metric_type = metric_name)
}

evaluate_split <- function(data) {
  make_tbl <- prob_metrics(data, scored, pred_make, "make")
  oreb_tbl <- data %>%
    filter(scored == 0) %>%
    prob_metrics(offensive_rebound, pred_oreb, "oreb")
  foul_tbl <- prob_metrics(data, fouled, pred_foul, "foul")
  xp_tbl <- xP_metrics(data, score_value, xP, "xp")
  
  bind_rows(make_tbl, oreb_tbl, foul_tbl, xp_tbl)
}

results <- evaluate_split(early_2025_raw)
results_late <- evaluate_split(late_2025_raw)

cal_xp_early <- plot_calibration(
  early_2025, pred_col = xp, outcome_col = score_value, 
  n_bins = 10, title = "Total xP Calibration — Early 2025 (Calibration Set)"
)
print(cal_xp_early)

cal_xp_late <- plot_calibration(
  late_2025, pred_col = xp, outcome_col = score_value, 
  n_bins = 10, title = "Total xP Calibration — Late 2025 (Test Set)"
)
print(cal_xp_late)