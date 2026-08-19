library(tidyverse)
library(yardstick)

# data prep --------------------------------------------------------------
xP_2024 <- make_xP_frame(data24)
xP_2025 <- make_xP_frame(data)

early_2025 <- xP_2025 |>
  filter(game_date <= as.Date("2025-07-31"))
late_2025 <- xP_2025 |>
  filter(game_date > as.Date("2025-07-31"))

# train models -----------------------------------------------------------

# Main logistic regression models
make_model <- build_make_model(xP_2024)

oreb_model <- build_oreb_model(
  xP_2024 |>
    filter(scored == 0)
)

foul_model <- build_foul_model(xP_2024)



# train benchmarks ------------------------------------------------------------------

# The benchmark predicts the historical 2024 event rate.

# Make: all 2024 shots
# OREB: all missed 2024 shots
# Foul: all 2024 shots

baseline_make <- glm(
  scored ~ 1,
  data = xP_2024,
  family = binomial()
)

baseline_oreb <- glm(
  offensive_rebound ~ 1,
  data = xP_2024 |> filter(scored == 0),
  family = binomial()
)

baseline_foul <- glm(
  fouled ~ 1,
  data = xP_2024,
  family = binomial()
)



# generate predictions ---------------------------------------------------
predict_period <- function(data) {
  data |>
    mutate(
      pred_make = predict(
        make_model,
        newdata = data,
        type = "response"
      ),

      pred_foul = predict(
        foul_model,
        newdata = data,
        type = "response"
      ),

      pred_make_baseline = predict(
        baseline_make,
        newdata = data,
        type = "response"
      ),

      pred_foul_baseline = predict(
        baseline_foul,
        newdata = data,
        type = "response"
      )
    )
}


# Make/Foul predictions
early_pred <- predict_period(early_2025)
late_pred  <- predict_period(late_2025)

# OREB predictions only on missed shots
early_pred <- early_pred |>
  mutate(
    pred_oreb = NA_real_,
    pred_oreb_baseline = NA_real_
  )

early_pred$pred_oreb[early_pred$scored == 0] <-
  predict(
    oreb_model,
    newdata = early_pred |> filter(scored == 0),
    type = "response"
  )

early_pred$pred_oreb_baseline[early_pred$scored == 0] <-
  predict(
    baseline_oreb,
    newdata = early_pred |> filter(scored == 0),
    type = "response"
  )


late_pred <- late_pred |>
  mutate(
    pred_oreb = NA_real_,
    pred_oreb_baseline = NA_real_
  )

late_pred$pred_oreb[late_pred$scored == 0] <-
  predict(
    oreb_model,
    newdata = late_pred |> filter(scored == 0),
    type = "response"
  )

late_pred$pred_oreb_baseline[late_pred$scored == 0] <-
  predict(
    baseline_oreb,
    newdata = late_pred |> filter(scored == 0),
    type = "response"
  )

# evaluation -------------------------------------------------------------

evaluate_binary <- function(data, outcome, prediction, benchmark, model_name) {
  outcome <- rlang::ensym(outcome)
  prediction <- rlang::ensym(prediction)
  benchmark <- rlang::ensym(benchmark)
  truth <- data |>
    pull(!!outcome)
  model_pred <- data |>
    pull(!!prediction)
  benchmark_pred <- data |>
    pull(!!benchmark)
  tibble(
    # model
    model = model_name,
    model_brier = mean(
      (truth - model_pred)^2,
      na.rm = TRUE
    ),
    model_roc_auc = roc_auc_vec(
      truth = factor(truth, levels = c(0, 1)),
      estimate = model_pred,
      event_level = "second"
    ),

    # benchmark
    benchmark_brier = mean(
      (truth - benchmark_pred)^2,
      na.rm = TRUE
    ),
    benchmark_roc_auc = roc_auc_vec(
      truth = factor(truth, levels = c(0, 1)),
      estimate = benchmark_pred,
      event_level = "second"
    )
  ) |>
    mutate(
      brier_improvement = benchmark_brier - model_brier,
      auc_improvement = model_roc_auc - benchmark_roc_auc
    )
}

evaluate_period <- function(data, period_name) {

  make_results <- evaluate_binary(
    data = data,
    outcome = scored,
    prediction = pred_make,
    benchmark = pred_make_baseline,
    model_name = "Make"
  )

  oreb_data <- data |>
    filter(scored == 0)

  oreb_results <- evaluate_binary(
    data = oreb_data,
    outcome = offensive_rebound,
    prediction = pred_oreb,
    benchmark = pred_oreb_baseline,
    model_name = "OREB"
  )

  foul_results <- evaluate_binary(
    data = data,
    outcome = fouled,
    prediction = pred_foul,
    benchmark = pred_foul_baseline,
    model_name = "Foul"
  )

  bind_rows(
    make_results,
    oreb_results,
    foul_results
  ) |>
    mutate(period = period_name) |>
    select(
      period,
      model,
      model_brier,
      benchmark_brier,
      brier_improvement,
      model_roc_auc,
      benchmark_roc_auc,
      auc_improvement
    )
}

# out of sample evaluation -----------------------------------------------

results <- bind_rows(
  evaluate_period(early_pred, "Early 2025"),
  evaluate_period(late_pred, "Late 2025")
)

results


# probability calibration plots ------------------------------------------

library(dplyr)
library(ggplot2)

calibration_plot <- function(data, truth, prediction,
                             title = NULL,
                             bin_width = 0.05) {

  truth <- rlang::ensym(truth)
  prediction <- rlang::ensym(prediction)

  calibration_data <- data |>
    transmute(
      actual = as.numeric(!!truth),
      predicted = !!prediction
    ) |>
    filter(
      !is.na(actual),
      !is.na(predicted),
      is.finite(actual),
      is.finite(predicted)
    ) |>
    mutate(
      bin_pred_prob = round(predicted / bin_width) * bin_width
    ) |>
    group_by(bin_pred_prob) |>
    summarise(
      n_shots = n(),
      bin_actual_prob = mean(actual),

      bin_se = sqrt(
        bin_actual_prob *
          (1 - bin_actual_prob) /
          n_shots
      ),

      .groups = "drop"
    ) |>
    mutate(
      bin_upper = pmin(
        bin_actual_prob + 2 * bin_se,
        1
      ),

      bin_lower = pmax(
        bin_actual_prob - 2 * bin_se,
        0
      )
    )

  ggplot(
    calibration_data,
    aes(
      x = bin_pred_prob,
      y = bin_actual_prob
    )
  ) +

    geom_abline(
      slope = 1,
      intercept = 0,
      color = "black",
      linetype = "dashed"
    ) +

    geom_errorbar(
      aes(
        ymin = bin_lower,
        ymax = bin_upper
      ),
      width = 0,
      linewidth = 0.5
    ) +

    geom_point(
      alpha = 0.5
    ) +

    coord_equal() +

    scale_x_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, 0.2)
    ) +

    scale_y_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, 0.2)
    ) +

    labs(
      title = title,
      x = "Predicted probability",
      y = "Observed frequency"
    ) +

    theme_bw() +

    theme(
      plot.title = element_text(
        hjust = 0.5,
        face = "bold"
      )
    )
}

make_early <- calibration_plot(
  early_pred,
  scored,
  pred_make,
  "Make — Early 2025"
)

make_late <- calibration_plot(
  late_pred,
  scored,
  pred_make,
  "Make — Late 2025"
)

oreb_early <- calibration_plot(
  early_pred |> filter(scored == 0),
  offensive_rebound,
  pred_oreb,
  "OREB — Early 2025"
)

oreb_late <- calibration_plot(
  late_pred |> filter(scored == 0),
  offensive_rebound,
  pred_oreb,
  "OREB — Late 2025"
)

foul_early <- calibration_plot(
  early_pred,
  fouled,
  pred_foul,
  "Foul — Early 2025"
)

foul_late <- calibration_plot(
  late_pred,
  fouled,
  pred_foul,
  "Foul — Late 2025"
)

library(patchwork)

(make_early / make_late) |
(oreb_early / oreb_late) |
(foul_early / foul_late) 
