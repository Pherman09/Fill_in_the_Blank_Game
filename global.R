# global.R

# This file is sourced once when the Shiny application starts.
# It's used for global settings, data loading, and function definitions
# that are needed by both ui.R and server.R.

library(shiny)
library(dplyr)
library(digest) # For creating unique hashes for answers

# Define the file paths for storing data
answers_file <- "answers.rds"
votes_file <- "votes.rds"

# Define the questions as a named character vector
# The names are the internal IDs, and the values are the full question text
QUESTIONS <- c(
  "q1" = "Due to unforseen circumstances, we will be adding a new Spring training module in 2026: ________.",
  "q2" = "We were all excited for the summer social activity, until we found out it was ________.",
  "q3" = "Instead of AAPOR or JSM, we will all be attending a new conference more relevant to our work next year: ________.",
  "q4" = "The best advice I ever got about my career was: ________."
)


# Define canonical empty data frames to ensure consistent structure
# This is crucial for read_rds_robust to handle missing or corrupt files
canonical_empty_answers_df <- data.frame(
  user_email = character(0),
  question_id = character(0),
  answer_text = character(0),
  timestamp = character(0),
  stringsAsFactors = FALSE
)

canonical_empty_votes_df <- data.frame(
  user_email = character(0),
  question_id = character(0),
  voted_answer_id = character(0),
  timestamp = character(0),
  stringsAsFactors = FALSE
)

# Function to robustly read RDS files, providing an empty canonical dataframe if file is missing or corrupt
read_rds_robust <- function(filepath, canonical_df) {
  if (file.exists(filepath)) {
    tryCatch({
      # Attempt to read the RDS file
      data <- readRDS(filepath)
      # Ensure the read data has the expected columns
      if (all(names(canonical_df) %in% names(data))) {
        return(data %>% dplyr::select(all_of(names(canonical_df)))) # Select and reorder columns
      } else {
        warning(paste("File", filepath, "is corrupt or has unexpected columns. Initializing empty dataframe."))
        return(canonical_df)
      }
    }, error = function(e) {
      warning(paste("Error reading", filepath, ":", e$message, ". Initializing empty dataframe."))
      return(canonical_df)
    })
  } else {
    # If file does not exist, return the canonical empty dataframe
    return(canonical_df)
  }
}

# Initialize RDS files if they don't exist
if (!file.exists(answers_file)) {
  saveRDS(canonical_empty_answers_df, answers_file)
}
if (!file.exists(votes_file)) {
  saveRDS(canonical_empty_votes_df, votes_file)
}
