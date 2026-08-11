library(wehoop)
library(dplyr)
library(furrr)

# Function to fetch and save WNBA PBP data for a given year
fetch_wnba_pbp_data <- function(year, num_workers = 4) {
  
  # Validate year input
  if (!is.numeric(year) || year < 1996 || year > as.integer(format(Sys.Date(), "%Y")) + 1) {
    stop("Year must be a valid WNBA season (1996 or later)")
  }
  
  # Plan parallel execution
  plan(multisession, workers = num_workers)
  
  cat("Loading WNBA play-by-play data for", year, "...\n")
  
  # Load PBP data for the specified year
  pbp_data <- load_wnba_pbp(seasons = year)
  
  if (nrow(pbp_data) == 0) {
    stop("No data available for year", year)
  }
  
  # Get unique game IDs
  game_ids <- unique(pbp_data$game_id)
  n_games <- length(game_ids)
  
  cat("Fetching ESPN PBP for", n_games, "games in parallel...\n")
  
  # Parallel fetch with timeout handling and retry logic
  espn_pbp <- future_map_dfr(
    game_ids,
    ~ {
      library(wehoop)
      
      # Retry logic for failed requests
      for (attempt in 1:3) {
        result <- tryCatch(
          espn_wnba_pbp(.x),
          error = function(e) NULL,
          warning = function(w) NULL
        )
        
        if (!is.null(result) && nrow(result) > 0) {
          return(result)
        }
        
        if (attempt < 3) Sys.sleep(0.5)  # Brief pause before retry
      }
      
      return(tibble())  # Return empty if all retries fail
    },
    .progress = TRUE,
    .options = furrr_options(seed = NULL, scheduling = 1)
  )
  
  cat("\n✓ Fetch complete:", nrow(espn_pbp), "plays\n")
  
  # Create filenames with year
  rds_filename <- paste0("wbb/data/rds/espn_pbp_", year, ".rds")
  csv_filename <- paste0("wbb/data/csvs/wnba_", year, "_pbp.csv")
  
  # Save data
  saveRDS(espn_pbp, rds_filename)
  cat("✓ Saved to", rds_filename, "\n")
  write.csv(espn_pbp, csv_filename, row.names = FALSE)
  cat("✓ Saved to", csv_filename, "\n")
  
  # Quick validation
  cat("\nData summary for", year, ":\n")
  cat("  Games:", n_distinct(espn_pbp$game_id), "\n")
  cat("  Player columns:", any(grepl("player|name|athlete", tolower(names(espn_pbp)))), "\n")
  cat("  Total rows:", nrow(espn_pbp), "\n")
  
  return(espn_pbp)
}

# Usage examples:
# Fetch 2025 data with default 4 workers
data_2026 <- fetch_wnba_pbp_data(2026)

# Fetch 2024 data with 6 workers
data_2024 <- fetch_wnba_pbp_data(2024, num_workers = 6)


library(wehoop)
library(dplyr)
library(furrr)

# Function to fetch and save college women's basketball PBP data for a given year
fetch_wbb_pbp_data <- function(year, num_workers = 4) {
  
  # Validate year input
  if (!is.numeric(year) || year < 2004 || year > as.integer(format(Sys.Date(), "%Y")) + 1) {
    stop("Year must be a valid college basketball season (2004 or later)")
  }
  
  # Plan parallel execution
  plan(multisession, workers = num_workers)
  
  cat("Loading college women's basketball play-by-play data for", year, "...\n")
  
  # Load PBP data for the specified year
  pbp_data <- load_wbb_pbp(seasons = year)
  
  if (nrow(pbp_data) == 0) {
    stop("No data available for year", year)
  }
  
  # Get unique game IDs
  game_ids <- unique(pbp_data$game_id)
  n_games <- length(game_ids)
  
  cat("Fetching ESPN PBP for", n_games, "games in parallel...\n")
  
  # Parallel fetch with timeout handling and retry logic
  espn_pbp <- future_map_dfr(
    game_ids,
    ~ {
      library(wehoop)
      
      # Retry logic for failed requests
      for (attempt in 1:3) {
        result <- tryCatch(
          espn_wbb_pbp(.x),
          error = function(e) NULL,
          warning = function(w) NULL
        )
        
        if (!is.null(result) && nrow(result) > 0) {
          return(result)
        }
        
        if (attempt < 3) Sys.sleep(0.5)  # Brief pause before retry
      }
      
      return(tibble())  # Return empty if all retries fail
    },
    .progress = TRUE,
    .options = furrr_options(seed = NULL, scheduling = 1)
  )
  
  cat("\n✓ Fetch complete:", nrow(espn_pbp), "plays\n")
  
  # Create filenames with year
  rds_filename <- paste0("wbb/data/rds/espn_wbb_pbp_", year, ".rds")
  csv_filename <- paste0("wbb/data/csvs/espn_wbb_", year, "_pbp.csv")
  
  # Save data
  saveRDS(espn_pbp, rds_filename)
  cat("✓ Saved to", rds_filename, "\n")
  write.csv(espn_pbp, csv_filename, row.names = FALSE)
  cat("✓ Saved to", csv_filename, "\n")
  
  # Quick validation
  cat("\nData summary for", year, ":\n")
  cat("  Games:", n_distinct(espn_pbp$game_id), "\n")
  cat("  Player columns:", any(grepl("player|name|athlete", tolower(names(espn_pbp)))), "\n")
  cat("  Total rows:", nrow(espn_pbp), "\n")
  
  return(espn_pbp)
}

# Usage examples:
# Fetch 2024 college data with default 4 workers
data_2024 <- fetch_wbb_pbp_data(2024)

# Fetch 2024 data with 6 workers if you want more parallel processing
data_2024_fast <- fetch_wbb_pbp_data(2025, num_workers = 6)
