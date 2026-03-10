# app.R
library(shiny)
library(shinydashboard)
library(readxl)
library(dplyr)

# ---- 1. READ EXCEL DATA ----
# Make sure your Excel file is in the working directory or provide full path

excel_file <- read_excel("C:/Users/cex/Desktop/Shiny/sample_datasets.xlsx")

denom <- read_excel("C:/Users/cex/Desktop/Shiny/sample_datasets.xlsx", sheet = "denominator")
lab    <- read_excel("C:/Users/cex/Desktop/Shiny/sample_datasets.xlsx", sheet = "lab_data")

denominator <- as.data.frame(denom)
lab_data <- as.data.frame(lab_data)


# ---- 4. MERGE WITH DENOMINATOR ----
merged_data <- lab_data %>%
  left_join(denominator, by = "subject")


merged <- as.data.frame(merged_data)

# ---- 5. SUMMARY FUNCTION (BY PARAMETER ONLY) ----
get_summary <- function(df, flag_type) {
  df %>%
    group_by(!!sym(flag_type), lab_param, visit) %>%
    summarise(
      N = n(),
      MEAN = mean(lab_value, na.rm = TRUE),
      MEDIAN = median(lab_value, na.rm = TRUE),
      SD = sd(lab_value, na.rm = TRUE),
      MIN = min(lab_value, na.rm = TRUE),
      MAX = max(lab_value, na.rm = TRUE),
      .groups = "drop"
    )
}

