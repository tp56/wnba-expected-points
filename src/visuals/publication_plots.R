library(dplyr)
library(forcats)
library(patchwork)
library(nbaplotR)
library(sportyR)

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

geom_basketball(
  league = "WNBA",
  display_range = "defense",
  rotation = 90,
  x_trans = 45,
  y_trans = -25,
  court_units = "ft"
) +
  stat_summary_hex(
    data = resids,
    aes(x = x, y = y, z = xP),
    fun = mean,
    bins = 30
  ) +
  scale_fill_viridis_c(
    option = "A",
    name = "Mean xP"
  ) +
  coord_fixed()