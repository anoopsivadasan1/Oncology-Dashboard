library(dplyr)
library(lubridate)
library(stringr)

# ==========================================
# 1️⃣ Create Sample Data (10 observations)
# ==========================================

data <- tibble(
  EVENT = paste("Visit", 1:10),
  
  START_DATE = c(
    "2024-01-15",  # full
    "2024-03",     # missing day
    "2024",        # missing month & day
    NA,            # fully missing
    "2023-07-10",  # full
    "2023-08",     # missing day
    "2022",        # missing month & day
    NA,            # fully missing
    "2021-11",     # missing day
    "2020-05-05"   # full
  ),
  
  END_DATE = c(
    "2024-02-20",  # full
    "2024-04",     # missing day
    "2024",        # missing month & day
    NA,            # fully missing
    "2023-07",     # missing day
    "2023-08-25",  # full
    NA,            # fully missing
    "2022-12",     # missing day
    "2021",        # missing month & day
    "2020-05-30"   # full
  )
)

# ==========================================
# 2️⃣ Vectorised ISO8601-Compliant Imputation
# ==========================================

data_final <- data %>%
  mutate(
    
    # ---- Pattern Detection ----
    START_YEAR_ONLY = str_detect(START_DATE, "^\\d{4}$"),
    START_YM_ONLY   = str_detect(START_DATE, "^\\d{4}-\\d{2}$"),
    START_FULL      = str_detect(START_DATE, "^\\d{4}-\\d{2}-\\d{2}$"),
    
    END_YEAR_ONLY   = str_detect(END_DATE, "^\\d{4}$"),
    END_YM_ONLY     = str_detect(END_DATE, "^\\d{4}-\\d{2}$"),
    END_FULL        = str_detect(END_DATE, "^\\d{4}-\\d{2}-\\d{2}$"),
    
    # ---- START DATE RULES ----
    START_DATE_IMP = case_when(
      is.na(START_DATE) ~ NA_Date_,
      START_FULL        ~ ymd(START_DATE),
      START_YM_ONLY     ~ ymd(paste0(START_DATE, "-01")),
      START_YEAR_ONLY   ~ ymd(paste0(START_DATE, "-01-01"))
    ),
    
    # ---- END DATE RULES ----
    END_DATE_IMP = case_when(
      is.na(END_DATE) ~ NA_Date_,
      END_FULL        ~ ymd(END_DATE),
      END_YM_ONLY     ~ ceiling_date(
        ymd(paste0(END_DATE, "-01")),
        "month"
      ) - days(1),
      END_YEAR_ONLY   ~ ymd(paste0(END_DATE, "-12-31"))
    ),
    
    # ---- ISO8601 Character Output ----
    START_DATE_ISO = format(START_DATE_IMP, "%Y-%m-%d"),
    END_DATE_ISO   = format(END_DATE_IMP, "%Y-%m-%d"),
    
    # ---- Missing Flag ----
    MISSING = if_else(
      is.na(START_DATE) & is.na(END_DATE),
      "Y", "N"
    ),
    
    # ---- Partial Flags ----
    START_PARTIAL = if_else(
      START_YM_ONLY | START_YEAR_ONLY,
      "Y", "N"
    ),
    
    END_PARTIAL = if_else(
      END_YM_ONLY | END_YEAR_ONLY,
      "Y", "N"
    )
  ) %>%
  select(EVENT,
         START_DATE, START_DATE_ISO,
         END_DATE, END_DATE_ISO,
         START_PARTIAL, END_PARTIAL,
         MISSING)

print(data_final)