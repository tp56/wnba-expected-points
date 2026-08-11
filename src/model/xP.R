# imports ---- 
library(here)
library(dplyr)
library(stringr)
library(purrr)
library(lubridate)
library(wehoop)

data <- readRDS(here("wbb/data/rds/espn_pbp_2025.rds"))
data24 <- readRDS(here("wbb/data/rds/espn_pbp_2024.rds"))

# clean data ----
extract_shooter_from_block <- function(text) {
  match <- str_match(text, "blocks\\s+([^']+)'s\\s+")
  return(trimws(match[, 2]))
}

classify_shots <- function(df) {
  shots <- df |>
    filter(shooting_play == TRUE) |>
    filter(points_attempted !=1 ) |>
    mutate(
       shooter_name = case_when(
        str_detect(text, "blocks") ~ extract_shooter_from_block(text),
        TRUE ~ trimws(sub("\\s+(misses|makes|shoots|attempts).*", "", text))
      ),
      passer = str_match(text, "\\(([^)]+) assists\\)")[, 2],
    )

  next_plays <- df |>
    arrange(game_id, sequence_number) |>
    group_by(game_id) |>
    mutate(
      sequence_number = as.numeric(sequence_number) - 1
    ) |>
    ungroup() |>
    select(game_id, sequence_number, text, type_text, team_id, athlete_id_1) |>
    rename(
      next_play_text = text,
      next_play_type = type_text,
      next_play_team_id = team_id,
      next_play_player_id = athlete_id_1
    )
  
  classified_shots <- shots |>
    mutate(sequence_number = as.numeric(sequence_number)) |>
    left_join(
      next_plays,
      by = c("game_id", "sequence_number")
    ) |>
    mutate(
      rebounder = if_else(
        next_play_type %in% c("Offensive Rebound", "Defensive Rebound"),
        trimws(sub("\\s+(offensive|defensive)\\s+rebound.*", "", next_play_text, ignore.case = TRUE)),
        NA_character_
      ),
      rebounder_team_id = if_else(
        next_play_type %in% c("Offensive Rebound", "Defensive Rebound"),
        next_play_team_id,
        NA_integer_
      ),
      rebounder_player_id = if_else(
        next_play_type %in% c("Offensive Rebound", "Defensive Rebound"),
        next_play_player_id,
        NA_integer_
      ),
      shot_family = case_when(
        str_detect(type_text, "^Free Throw") ~ "Free Throw",
        str_detect(type_text, "Dunk") ~ "Dunk",
        str_detect(type_text, "Tip Shot") ~ "Tip-In",
        str_detect(type_text, "Floating") ~ "Floater",
        str_detect(type_text, "Hook") ~ "Hook",
        str_detect(type_text, "Layup|Finger Roll") ~ "Layup",
        str_detect(type_text, "Jump Shot") ~ "Jumper",
        TRUE ~ NA_character_
      ),
      
      is_driving = str_detect(type_text, "Driving"),
      is_running = str_detect(type_text, "Running"),
      is_pullup = str_detect(type_text, "Pullup"),
      is_stepback = str_detect(type_text, "Step Back"),
      is_fadeaway = str_detect(type_text, "Fade Away|Fadeaway"),
      is_turnaround = str_detect(type_text, "Turnaround"),
      is_bank = str_detect(type_text, "Bank"),
      is_reverse = str_detect(type_text, "Reverse"),
      is_finger_roll = str_detect(type_text, "Finger Roll"),
      is_cutting = str_detect(type_text, "Cutting"),
      is_alley_oop = str_detect(type_text, "Alley Oop")
    )
  
  return(classified_shots)
}
# calculation classification functions ----
distance <- function(x, y) {
  basket_x <- 25
  basket_y <- 0

  abs_x = abs(x)
  abs_y = abs(y)
  return (sqrt(((abs_x) - basket_x)^2 + (abs_y - basket_y)^2))
}

angle <- function(x, y) {
  basket_x <- 25
  basket_y <- 0

  abs_x = abs(x)
  abs_y = abs(y)
  angle_rad <- atan2(abs_y - basket_y, abs_x - basket_x)
  angle_deg <- angle_rad * (180 / pi)

  return (angle_deg)
}

driving <- function(drive_check) {
  as.integer(coalesce(drive_check, FALSE))
}

running <- function(running_check) {
  as.integer(coalesce(running_check, FALSE))
}

pullup <- function(pullup_check) {
  as.integer(coalesce(pullup_check, FALSE))
}

stepback <- function(is_stepback) {
  as.integer(coalesce(is_stepback, FALSE))
}

fadeaway <- function(is_fadeaway) {
  as.integer(coalesce(is_fadeaway, FALSE))
}

cutting <- function(is_cutting) {
  as.integer(coalesce(is_cutting, FALSE))
}

turnaround <- function(is_turnaround) {
  as.integer(coalesce(is_turnaround, FALSE))
}

oreb <- function(next_play_type) {
  as.integer(coalesce(next_play_type == "Offensive Rebound", FALSE))
}

dreb <- function(next_play_type) {
  as.integer(coalesce(next_play_type == "Defensive Rebound", FALSE))
}

foul <- function(next_play_type) {
  as.integer(coalesce(next_play_type == "Shooting Foul", FALSE))
}

assisted <- function(passer) {
  as.integer(!is.na(passer))
}

time_left <- function(period_display_value, clock_display_value) {
  time_in_period <- period_to_seconds(ms(clock_display_value))

  offset <- dplyr::case_when(
    period_display_value == "1st Quarter" ~ 1800L,
    period_display_value == "2nd Quarter" ~ 1200L,
    period_display_value == "3rd Quarter" ~ 600L,
    TRUE ~ 0L
  )
  offset + time_in_period
}

# xP make functions ----
make_xP_frame <- function(df) {
  classifed_df <- classify_shots(df)
  dis <- distance(classifed_df$coordinate_x_raw, classifed_df$coordinate_y_raw)
  angle_val <- angle(classifed_df$coordinate_x_raw, classifed_df$coordinate_y_raw)
  drive_val <- driving(classifed_df$is_driving)
  run_val <- running(classifed_df$is_running)
  pullup_val <- pullup(classifed_df$is_pullup)
  stepback_val <- stepback(classifed_df$is_stepback)
  fade_val <- fadeaway(classifed_df$is_fadeaway)
  cut_val <- cutting(classifed_df$is_cutting)
  turnaround_val <- turnaround(classifed_df$is_turnaround)
  scored <- as.integer(coalesce(classifed_df$scoring_play, FALSE))
  orebd <- oreb(classifed_df$next_play_type)
  drebd <- dreb(classifed_df$next_play_type)
  foul_val <- foul(classifed_df$next_play_type)
  assisted_val <- assisted((classifed_df$passer))
  time_left_val <- time_left(classifed_df$period_display_value, classifed_df$clock_display_value)

  xP_frame <- data.frame(
    game_id = classifed_df$game_id,
    team_id = classifed_df$team_id,
    season = classifed_df$season,
    game_date = classifed_df$game_date,
    shooter = classifed_df$shooter_name,
    passer = classifed_df$passer,
    rebounder = classifed_df$rebounder,
    rebounder_team_id = classifed_df$rebounder_team_id,
    rebounder_player_id = classifed_df$rebounder_player_id,
    text = classifed_df$text,
    type_text = classifed_df$type_text,
    player_id = classifed_df$athlete_id_1,
    short_description = classifed_df$short_description,
    shot_family = classifed_df$shot_family,
    scored = scored,
    score_value = classifed_df$score_value,
    points_attempted = classifed_df$points_attempted,
    x = classifed_df$coordinate_x_raw,
    y = classifed_df$coordinate_y_raw,
    dis = dis,
    dis_log = log1p(dis),
    angle = angle_val,
    angle_sq = angle_val ** 2,
    drive = drive_val,
    run = run_val,
    pullup = pullup_val,
    stepback = stepback_val,
    fade = fade_val,
    cut = cut_val,
    turnaround = turnaround_val,
    offensive_rebound = orebd,
    defensive_rebound = drebd,
    fouled = foul_val, 
    assisted = assisted_val,
    time_left = time_left_val
  )
  teams <- espn_wnba_teams()

  xP_frame <- xP_frame |>
    left_join(
      teams |>
        select(team_id, abbreviation),
      by = "team_id"
    ) |>
    rename(team_name = abbreviation) |>
    left_join(
      teams |>
        select(team_id, abbreviation),
      by = c("rebounder_team_id" = "team_id")
    ) |>
    rename(rebounder_team_name = abbreviation)
  return (xP_frame)
}

build_make_model <- function(df) {
  model <- glm(
    scored ~ dis_log + angle + angle_sq + drive + run + turnaround + fade + pullup + 
      stepback + cut + time_left, 
    data = df,
    family = binomial(link = "logit")
  )
  return (model)
}

build_oreb_model <- function(df) {
  model <- glm(
    offensive_rebound ~ dis_log + angle + angle_sq + drive + pullup + stepback + 
              turnaround + time_left,
    data = df,
    family = binomial(link = "logit")
  )
  return (model)
}

build_dreb_model <- function(df) {
  model <- glm(
    defensive_rebound ~ dis_log + angle + angle_sq + drive + run + pullup + stepback + 
                        turnaround + fade + cut + assisted + time_left,
    data = df,
    family = binomial(link = "logit")
  )
  return (model)
}

build_foul_model <- function(df) {
  model <- glm(
    fouled ~ dis_log + angle + angle_sq + drive + run + pullup + 
              turnaround + stepback + fade + cut + assisted + time_left,
    data = df,
    family = binomial(link = "logit")
  )
  return (model)
}

# expected oreb and dreb ----
classify_rebounds <- function(df) {
  rebounds <- df |>
    filter(type_text == "Offensive Rebound" | type_text == "Defensive Rebound")

  next_plays <- df |>
    arrange(game_id, sequence_number) |>
    group_by(game_id) |>
    mutate(sequence_number = as.numeric(sequence_number) - 1) |>
    ungroup() |>
    select(game_id, sequence_number, text, type_text, score_value, points_attempted) |>
    rename(
      next_play_text = text,
      next_play_type = type_text,
      next_play_score_value = score_value,
      next_play_points_attempted = points_attempted,
    )
  
  classified_rebounds <- rebounds |>
    mutate(sequence_number = as.numeric(sequence_number)) |>
    left_join(next_plays, by = c("game_id", "sequence_number"))
  
  return(classified_rebounds)
}

rebds <- classify_rebounds(data24)

EOREB <- rebds |>
  filter(type_text == "Offensive Rebound")|>
  summarise(
    ev_oreb = mean(next_play_score_value, na.rm = TRUE),
    .groups = "drop"
  ) |>
  pull(ev_oreb)


EDREB <- rebds |>
  filter(type_text == "Defensive Rebound")|>
  summarise(
    ev_dreb = mean(next_play_score_value, na.rm = TRUE),
    .groups = "drop"
  ) |>
  pull(ev_dreb)

# expected value of fouls ----
EFOUL <- data24 |>
    filter(type_id %in% c(97,98,99,100,102,103)) |>
    summarise(val = mean(as.integer(scoring_play), na.rm = TRUE)) |>
    pull(val)

calcuate_xp <- function(train_df, test_df, eoreb, edreb, efoul) {
  xP_train <- make_xP_frame(train_df)
  model_make <- build_make_model(xP_train)
  model_oreb <- build_oreb_model(xP_train |> filter(scored == 0))
  model_dreb <- build_dreb_model(xP_train |> filter(scored == 0))
  model_foul <- build_foul_model(xP_train)

  xP_test <- make_xP_frame(test_df)
  xP_test <- xP_test |>
  mutate(
    pred_make = predict(model_make, newdata = xP_test, type = "response"),
    pred_oreb = predict(model_oreb, newdata = xP_test, type = "response"),
    pred_dreb = 1 - pred_oreb, 
    pred_foul = predict(model_foul, newdata = xP_test, type = "response")
  )

  xP_test <- xP_test |>
  mutate(
    xP_shot = pred_make * points_attempted,
    xP_oreb = pred_oreb * eoreb,
    xP_dreb = pred_dreb * edreb,  
    xP_foul = pred_foul * efoul * if_else(scored == 0, points_attempted, 1), 
    
    xP = xP_shot + (1- pred_make) * (xP_oreb - xP_dreb) + xP_foul
  )
  
  xP_test
}

xp25 <- calcuate_xp(data24, data, EOREB, EDREB, EFOUL)


