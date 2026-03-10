library(readxl)
library(dplyr)
library(lubridate)
library(stringr)

# Read file
data <- read_excel("C:/Users/cex/OneDrive/Desktop/Shiny/sample_partial_dates.xlsx", sheet = "SampleData")

# Function to impute START DATE
impute_start <- function(x){
  
  if(is.na(x) || x == "") return(NA)
  
  parts <- str_split(x, "-", simplify = TRUE)
  
  if(str_count(x, "-") == 2){
    # Full date
    return(ymd(x))
  }
  
  if(str_count(x, "-") == 1){
    # Missing day → first day of month
    return(ymd(paste0(x, "-01")))
  }
  
  if(str_count(x, "-") == 0){
    # Missing month & day → January 1
    return(ymd(paste0(x, "-01-01")))
  }
}

# Function to impute END DATE
impute_end <- function(x){
  
  if(is.na(x) || x == "") return(NA)
  
  parts <- str_split(x, "-", simplify = TRUE)
  
  if(str_count(x, "-") == 2){
    # Full date
    return(ymd(x))
  }
  
  if(str_count(x, "-") == 1){
    # Missing day → last day of month
    temp <- ymd(paste0(x, "-01"))
    return(ceiling_date(temp, "month") - days(1))
  }
  
  if(str_count(x, "-") == 0){
    # Missing month & day → December 31
    return(ymd(paste0(x, "-12-31")))
  }
}


# Apply imputation
# Apply imputation safely (preserve Date class)
data_clean <- data %>%
  mutate(
    START_DATE_IMP = as.Date(
      vapply(START_DATE, impute_start, FUN.VALUE = as.Date(NA))
    ),
    END_DATE_IMP = as.Date(
      vapply(END_DATE, impute_end, FUN.VALUE = as.Date(NA))
    ),
    
    # Format to ddmmmyyyy
    START_DATE_FINAL = if_else(
      is.na(START_DATE_IMP),
      NA_character_,
      toupper(format(START_DATE_IMP, "%d%b%Y"))
    ),
    
    END_DATE_FINAL = if_else(
      is.na(END_DATE_IMP),
      NA_character_,
      toupper(format(END_DATE_IMP, "%d%b%Y"))
    ),
    
    MISSING = ifelse(
      (is.na(START_DATE) | START_DATE == "") &
        (is.na(END_DATE) | END_DATE == ""),
      "Y", "N"
    )
  )

print(data_clean)