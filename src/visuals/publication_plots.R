library(dplyr)
library(forcats)
library(patchwork)
library(nbaplotR)
library(sportyR)
library(reshape2)
library(dotwhisker)

txP <- team_performance |>
  filter(!is.na(team_name)) |>
  mutate(team_name = fct_reorder(team_name, xP, .fun = mean, .desc = FALSE)) |>
  ggplot(aes(y = team_name)) +
  geom_bar(aes(x = xP), stat = "identity", fill = '#FFFFFF', color = '#003DA5', linewidth = 0.25 ) +
  geom_wnba_logos(aes(x = xP, team_abbr = team_name), width = 0.06) +
  geom_text(aes(x = xP, label = scales::number(xP, accuracy = 1)),
            hjust = 1.5, size = 3) +
  theme_bw() +
  labs(x = "Total xP", y = "Team Name")

txF <- team_performance |>
  filter(!is.na(team_name)) |>
  mutate(team_name = fct_reorder(team_name, pred_foul, .fun = mean, .desc = FALSE)) |>
  ggplot(aes(y = team_name)) +
  geom_bar(aes(x = pred_foul), stat = "identity", fill = '#FFFFFF', color = '#D50032', linewidth = 0.25 ) +
  geom_wnba_logos(aes(x = pred_foul, team_abbr = team_name), width = 0.06) +
  geom_text(aes(x = pred_foul, label = scales::number(pred_foul, accuracy = 0.1)),
            hjust = 1.5, size = 3) +
  theme_bw() +
  labs(x = "Total Expected Fouls", y = NULL)

txP | txF

EtxP <- team_performance |>
  filter(!is.na(team_name)) |>
  mutate(team_name = fct_reorder(team_name, residual_made, .fun = mean, .desc = FALSE)) |>
  ggplot(aes(y = team_name)) +
  geom_bar(aes(x = residual_made), stat = "identity", fill = '#FFFFFF', color = '#003DA5', linewidth = 0.25 ) +
  geom_wnba_logos(aes(x = residual_made, team_abbr = team_name), width = 0.06) +
  theme_bw() +
  labs(x = "Shots Scored Over Expected", y = "Team Name")

EtxF <- team_performance |>
  filter(!is.na(team_name)) |>
  mutate(team_name = fct_reorder(team_name, residual_foul, .fun = mean, .desc = FALSE)) |>
  ggplot(aes(y = team_name)) +
  geom_bar(aes(x = residual_foul), stat = "identity", fill = '#FFFFFF', color = '#D50032', linewidth = 0.25 ) +
  geom_wnba_logos(aes(x = residual_foul, team_abbr = team_name), width = 0.06) +
  theme_bw() +
  labs(x = "Fouls Drawn Over Expected", y = NULL)

EtxP | EtxF

shot_selection_team |>
  filter(!is.na(team_name)) |>
  ggplot(aes(x = residual_made, y = residual_foul)) +
  geom_wnba_logos(aes(team_abbr = team_name), width = 0.1) +
  geom_hline(yintercept = 0, linetype='dashed', color = 'red')+
  geom_vline(xintercept = 0, linetype='dashed', color = 'blue')+
  facet_wrap(~shot_type) +
  theme_bw() +
  labs(x = 'Shots Scored Over Expected', y = "Fouls Over Expected")

makep <- geom_basketball(
  league = "WNBA",
  display_range = "defensive_half_court",
  rotation = 90,
  x_trans = 45,
  y_trans = -25,
  court_units = "ft",
  ylims = 50
) +
  stat_summary_hex(
    data = resids,
    aes(x = x, y = y, z = xP),
    fun = mean,
    bins = 30
  ) +
  scale_fill_viridis_c(
    option = "F",
    name = NULL
  ) +
  coord_fixed(ylim = c(-10, 50)) +
  labs(title = "Mean xP") +
   theme(
    legend.position = "bottom",
    legend.text = element_text(angle = 45, hjust = 1)
  )

orebp <- geom_basketball(
  league = "WNBA",
  display_range = "defense",
  rotation = 90,
  x_trans = 45,
  y_trans = -25,
  court_units = "ft"
) +
  stat_summary_hex(
    data = resids,
    aes(x = x, y = y, z = pred_oreb),
    fun = mean,
    bins = 30
  ) +
  scale_fill_viridis_c(
    option = "A",
    name = NULL
  ) +
  coord_fixed(ylim = c(-10, 50)) +
  labs(title = "Mean p(oreb)") +
   theme(
    legend.position = "bottom",
    legend.text = element_text(angle = 50, hjust = 1)
  )

foulp <- geom_basketball(
  league = "WNBA",
  display_range = "defense",
  rotation = 90,
  x_trans = 45,
  y_trans = -25,
  court_units = "ft"
) +
  stat_summary_hex(
    data = resids,
    aes(x = x, y = y, z = pred_foul),
    fun = mean,
    bins = 30
  ) +
  scale_fill_viridis_c(
    option = "G",
    name = NULL
  ) +
  coord_fixed(ylim = c(-10, 50)) +
  labs(title = "Mean p(foul)") +
   theme(
    legend.position = "bottom",
    legend.text = element_text(angle = 45, hjust = 1) #angle = 90, hjust = 1
  )

makep|orebp|foulp

extract_glm_coefs <- function(fit, model_name) {
  s <- summary(fit)

  ct <- as.data.frame(s$coefficients) |>
    rownames_to_column("term")

  # Typical column names from glm summary: Estimate, Std. Error, z value, Pr(>|z|)
  # Normalize to std.error
  names(ct) <- sub("Std\\. Error", "std.error", names(ct))

  ct |>
    filter(term != "(Intercept)") |>
    transmute(
      predictor = term,
      estimate = Estimate,
      std.error = std.error,
      model = model_name
    )
}

ci <- 0.68
z <- qnorm(1 - (1 - ci)/2)

feature_order <- c(
  "dis_log",
  "angle",
  "angle_sq",
  "drive",
  "run",
  "pullup",
  "stepback",
  "fade",
  "cut",
  "turnaround",
  "time_left",
  "assisted"
)

df_all <- bind_rows(
  extract_glm_coefs(xp_model,   "Make"),
  extract_glm_coefs(xoreb_model, "OREB"),
  extract_glm_coefs(xfoul_model, "Foul")
) |>
  filter(!is.na(estimate), !is.na(std.error)) |>
  mutate(
    lower = estimate - z * std.error,
    upper = estimate + z * std.error,
    predictor = factor(predictor, levels = rev(feature_order))
  )

ggplot(df_all, aes(x = estimate, y = predictor, color = model, shape = model)) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_point(position = position_dodge(width = 0.7), size = 2.8, alpha = 0.5) +
  geom_errorbarh(
    aes(xmin = lower, xmax = upper),
    height = 0.25,
    position = position_dodge(width = 0.7)
  ) +
  theme_bw() +
  labs(x = NULL, y = NULL, color = NULL, shape = NULL) +
  scale_color_manual(
    values = c(
      "Foul" = "#B2182B",
      "OREB" = "#F57B20",
      "Make" = "#003DA5"
    )
  ) +
  scale_shape_manual(
    values = c(
      "Foul" = 17, 
      "Make" = 16, 
      "OREB" = 15  
    )
  ) +
    theme(
    legend.position = "bottom"
  )

matrix_data <- summary_by_type |>
  filter(n >= 30) |>
  select(type_text, xP, pred_oreb, pred_foul) |>
  arrange(desc(xP)) |>
  rename(
    "Shot Type" = type_text,
    "P(OREB)" = pred_oreb,
    "P(Foul)" = pred_foul
  )

matrix_melted <- matrix_data |>
  pivot_longer(cols = -"Shot Type", names_to = "Metric", values_to = "Value") |>
  group_by(Metric) |>
  mutate(Value_scaled = (Value - min(Value)) / (max(Value) - min(Value))) |>
  ungroup()

ggplot(matrix_melted, aes(x = Metric, y = factor(`Shot Type`, levels = unique(matrix_data$`Shot Type`)), fill = Value_scaled)) +
  geom_tile(color = "white", size = 0.3) +
  geom_text(aes(label = round(Value, 4)), color = "black", size = 2.5, fontface = "bold") +
  scale_fill_gradient(low = "white", high = "#228b22", name = "Relative Rank") +
  theme_bw() +
  theme(
    panel.background = element_blank(),
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(size = 11),
    axis.title = element_blank(),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    legend.position = "right"
  ) +
  labs(title = NULL)


# player plots -----------------------------------------------------------

smoe <- efficiency_with_reb |>
  filter(!is.na(team_name)) |>
  slice_max(order_by = resid_made_sum_shots, n = 10, with_ties = FALSE) |>
  mutate(player = fct_reorder(player, resid_made_sum_shots, .fun = mean, .desc = FALSE)) |>
  ggplot(aes(y = player)) +
  geom_bar(aes(x = resid_made_sum_shots), stat = "identity",
           fill = '#FFFFFF', color = '#003DA5', linewidth = 0.25 ) +
  geom_wnba_logos(aes(x = resid_made_sum_shots, team_abbr = team_name), width = 0.1) +
  geom_text(aes(x = resid_made_sum_shots, label = scales::number(resid_made_sum_shots, accuracy = 0.1)),
            hjust = 1.5, size = 3) +
  theme_bw() +
  labs(x = "SMOE", y = NULL)

roe <- efficiency_with_reb |>
  filter(!is.na(team_name)) |>
  slice_max(order_by = reb_resid_sum, n = 10, with_ties = FALSE) |>
  mutate(player = fct_reorder(player, reb_resid_sum, .fun = mean, .desc = FALSE)) |>
  ggplot(aes(y = player)) +
  geom_bar(aes(x = reb_resid_sum), stat = "identity",
           fill = '#FFFFFF', color = '#F57B20', linewidth = 0.25 ) +
  geom_wnba_logos(aes(x = reb_resid_sum, team_abbr = team_name), width = 0.1) +
  geom_text(aes(x = reb_resid_sum, label = scales::number(reb_resid_sum, accuracy = 0.1)),
            hjust = 1.5, size = 3) +
  theme_bw() +
  labs(x = "ROE", y = NULL)

fdoe <- efficiency_with_reb |>
  filter(!is.na(team_name)) |>
  slice_max(order_by = resid_foul_sum, n = 10, with_ties = FALSE) |>
  mutate(player = fct_reorder(player, resid_foul_sum, .fun = mean, .desc = FALSE)) |>
  ggplot(aes(y = player)) +
  geom_bar(aes(x = resid_foul_sum), stat = "identity",
           fill = '#FFFFFF', color = '#D50032', linewidth = 0.25 ) +
  geom_wnba_logos(aes(x = resid_foul_sum, team_abbr = team_name), width = 0.1) +
  geom_text(aes(x = resid_foul_sum, label = scales::number(resid_foul_sum, accuracy = 0.1)),
            hjust = 1.5, size = 3) +
  theme_bw() +
  labs(x = "FDOE", y = NULL)

smoe | roe | fdoe

shot_selection|>
  ggplot(aes(x = residual_made, y = residual_foul, label = shooter)) +
  geom_text_repel() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "blue") +
  geom_point(alpha = 0.5) +
  theme_bw() +
  labs(x = "SMOE", y = "FDOE") +
  facet_wrap(~shot_type)

plot_data <- shot_selection |>
  mutate(
    player_short = str_replace(
      shooter,
      "^(.).*\\s(\\S+)$",
      "\\1. \\2"
    )
  ) |>
  group_by(shot_type) |>
  mutate(
    rank = min_rank(desc(residual_made)),
    group = case_when(
      rank <= 5 ~ "Top 5",
      rank > n() - 5 ~ "Bottom 5",
      TRUE ~ "Other"
    )
  ) |>
  ungroup()

ggplot(plot_data, aes(x = residual_made, y = residual_foul)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "blue") +

  # All players
  geom_point(
    data = filter(plot_data, group == "Other"),
    color = "grey70",
    alpha = 0.35
  ) +

  # Top/bottom players
  geom_point(
    data = filter(plot_data, group != "Other"),
    aes(color = group),
    size = 2.5
  ) +

  # Labels only for highlighted players
  geom_text_repel(
    data = filter(plot_data, group != "Other"),
    aes(label = player_short, color = group),
    size = 3,
    max.overlaps = Inf,
    box.padding = 0.4,
    point.padding = 0.2
  ) +

  scale_color_manual(
    values = c(
      "Top 5" = "#2166AC",
      "Bottom 5" = "#B2182B"
    )
  ) +

  facet_wrap(~shot_type) +
  theme_bw() +
  labs(
    x = "SMOE",
    y = "FDOE",
    color = NULL
  ) +
  theme(
    legend.position = "bottom",
  )


msoe <- efficiency |>
  filter(n_events >= 40) |>
  mutate(
    top10_mean = rank(desc(resid_made_mean_shots)) <= 10,
    top10_sum = rank(desc(resid_made_sum_shots)) <= 10,
    color_group = case_when(
      top10_mean & top10_sum ~ "Top 10 Both",
      top10_mean ~ "Top 10 Mean",
      top10_sum ~ "Top 10 Sum",
      TRUE ~ "Other"
    )
  ) |>
  ggplot(aes(x = resid_made_sum_shots, y = resid_made_mean_shots, label = player, color = color_group)) +
  geom_text_repel(size = 3, max.overlaps = 5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "blue") +
  geom_point(alpha = 0.75, size = 2.5) +
  scale_color_manual(values = c("Top 10 Both" = "#F57B20", "Top 10 Mean" = "#2166AC", "Top 10 Sum" = "#B2182B", "Other" = "grey70")) +
  coord_cartesian(ylim = c(-0.15, NA)) +  
  theme_bw() +
  labs(x = "SMOE", y = "Mean SMOE", color = "")

# Plot 2: Rebounds
mroe <- efficiency_with_reb |>
  filter(n_events >= 40) |>
  mutate(
    top10_mean = rank(desc(reb_resid_mean)) <= 10,
    top10_sum = rank(desc(reb_resid_sum)) <= 10,
    color_group = case_when(
      top10_mean & top10_sum ~ "Top 10 Both",
      top10_mean ~ "Top 10 Mean",
      top10_sum ~ "Top 10 Sum",
      TRUE ~ "Other"
    )
  ) |>
  ggplot(aes(x = reb_resid_sum, y = reb_resid_mean, label = player, color = color_group)) +
  geom_text_repel(size = 3, max.overlaps = 5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "blue") +
  geom_point(alpha = 0.75, size = 2.5) +
  scale_color_manual(values = c("Top 10 Both" = "#F57B20", "Top 10 Mean" = "#2166AC", "Top 10 Sum" = "#B2182B", "Other" = "grey70")) +
  theme_bw() +
  labs(x = "ROE", y = "Mean ROE", color = "")

# Plot 3: Fouls
mfdoe <- efficiency_with_reb |>
  filter(n_events >= 40) |>
  mutate(
    top10_mean = rank(desc(resid_foul_mean)) <= 10,
    top10_sum = rank(desc(resid_foul_sum)) <= 10,
    color_group = case_when(
      top10_mean & top10_sum ~ "Top 10 Both",
      top10_mean ~ "Top 10 Mean",
      top10_sum ~ "Top 10 Sum",
      TRUE ~ "Other"
    )
  ) |>
  ggplot(aes(x = resid_foul_sum, y = resid_foul_mean, label = player, color = color_group)) +
  geom_text_repel(size = 3, max.overlaps = 5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "blue") +
  geom_point(alpha = 0.75, size = 2.5) +
  scale_color_manual(values = c("Top 10 Both" = "#F57B20", "Top 10 Mean" = "#2166AC", "Top 10 Sum" = "#B2182B", "Other" = "grey70")) +
  theme_bw() +
  labs(x = "FDOE", y = "Mean FDOE", color = "")

(msoe + theme(legend.position = "none"))|
(mroe + theme(legend.position = "bottom") + guides(color = guide_legend(nrow = 2))) | 
(mfdoe + theme(legend.position = "none")) +
  plot_layout(guides = "collect") +
  plot_annotation(theme = theme(legend.position = "bottom"))

