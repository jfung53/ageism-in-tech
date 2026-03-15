# ------ Data cleaning: Bureau of Labor Statistics ----------------------------
# ------ Table 27: Unemployed persons by reason for unemployment, sex, and age
#
# no. of unemployed people in thousands
# % dist. of total unemployed
# unemployed as % of civilian labour force
# 
# - by reason for unemployment
#     temporary layoff
#     permanent job losers
#     completed
#     temporary jobs
#     job leavers
#     reentrants
#     new entrants
# 
# sex: men, women
# age: [ Total 16+ ] [ Men 20+ ] [ Women 20+ ] [ Both 16-19 ]
#
# use filter(measure_type == "count")

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)


# ------ config ---------------------------------------------------------------

input_dir  <- "/Users/jocelyn/Documents/Pratt/Projects/ageism-in-tech/bls-data/27-unemployed-reason"
output_dir <- "/Users/jocelyn/Documents/Pratt/Projects/ageism-in-tech/bls-cleaned"

years <- c(2019, 2020, 2021, 2022, 2023, 2024, 2025)

# column names from headers in excel file
col_names_9 <- c(
  "reason",
  "total_16plus_prev",  "total_16plus",
  "men_20plus_prev",    "men_20plus",
  "women_20plus_prev",  "women_20plus",
  "youth_1619_prev",    "youth_1619"
)

# labels from NUMBER OF UNEMPLOYED section
# PERCENT DISTRIBUTION section uses the same labels (minus "Total unemployed").
# PERCENT OF LF section has only 4 labels (no Total or Permanent/Temp sub-rows).

reason_levels <- c(
  "Total unemployed",
  "Job losers and persons who completed temporary jobs",
  "On temporary layoff",
  "Not on temporary layoff",
  "Permanent job losers",
  "Persons who completed temporary jobs",
  "Job leavers",
  "Reentrants",
  "New entrants"
)

# map 2025 alternate wording back to previous wording for grouping
reason_recode <- c(
  "Job losers and people who completed temporary jobs" =
    "Job losers and persons who completed temporary jobs",
  "People who completed temporary jobs" =
    "Persons who completed temporary jobs"
)

# row numbers where each section begins
section_starts <- list(
  count   = 7L,   # "NUMBER OF UNEMPLOYED" header
  pct_dist = 18L, # "PERCENT DISTRIBUTION" header
  pct_lf   = 27L  # "UNEMPLOYED AS A PERCENT OF..." header
)

# data rows within each section (relative to section header row, 0-indexed offset).
# count:    9 data rows (rows 8–16, offsets 1–9)
# pct_dist: 6 data rows (rows 19–25, offsets 1–6, no "Total unemployed" line)
# pct_lf:   4 data rows (rows 28–31, offsets 1–4)
section_data_offsets <- list(
  count    = 1:9,
  pct_dist = 1:6,
  pct_lf   = 1:4
)


# ------ helper: read one section from the raw (skip=6) data frame ------------
#
# raw_df: data frame read with skip=6 header rows
# section_start: 1-indexed row number of the section header in the original file
# offsets: which rows after the header contain data

extract_section <- function(raw_df, section_start, offsets, year, measure_type) {
  
  # adjust for the 6-row skip: original row N → raw_df row (N - 6)
  data_rows <- (section_start - 6) + offsets
  
  df <- raw_df[data_rows, ] %>%
    # drop prev-year columns (even-numbered positions 2, 4, 6, 8)
    select(reason, total_16plus, men_20plus, women_20plus, youth_1619) %>%
    # standardize reason wording across all years ("people" → "persons")
    mutate(
      reason = str_replace_all(str_trim(reason), reason_recode)
    ) %>%
    # cast numeric columns
    mutate(across(-reason, ~suppressWarnings(as.numeric(as.character(.))))) %>%
    # tag measure type and year
    mutate(
      measure_type = measure_type,
      year         = year,
      .before      = reason
    )
  
  return(df)
}


# ------ cleaning function ----------------------------------------------------

clean_table27 <- function(filepath, year) {
  
  # step 1: read raw file, skipping the 6-row metadata header.
  #   row 7 in the original (the "NUMBER OF UNEMPLOYED" header) becomes row 1.
  #   2021 has 11 columns; pad col_names with dummy names and drop extras.
  raw <- read_excel(filepath, skip = 6, col_names = FALSE)
  
  n_cols <- ncol(raw)
  if (n_cols > length(col_names_9)) {
    # pad with dummy names for extra trailing cols
    padded_names <- c(col_names_9, paste0("drop_", seq_len(n_cols - length(col_names_9))))
    names(raw) <- padded_names
    raw <- raw %>% select(all_of(col_names_9))
  } else {
    names(raw) <- col_names_9
  }
  
  # step 2: extract each of the three sections
  df_count    <- extract_section(raw, section_starts$count,    section_data_offsets$count,    year, "count")
  df_pct_dist <- extract_section(raw, section_starts$pct_dist, section_data_offsets$pct_dist, year, "pct_dist")
  df_pct_lf   <- extract_section(raw, section_starts$pct_lf,   section_data_offsets$pct_lf,   year, "pct_lf")
  
  # step 3: combine all three sections
  df <- bind_rows(df_count, df_pct_dist, df_pct_lf)
  
  # step 4: pivot demographic columns to long format
  #   Each row will be: year × measure_type × reason × demographic × value
  df <- df %>%
    pivot_longer(
      cols      = c(total_16plus, men_20plus, women_20plus, youth_1619),
      names_to  = "demographic",
      values_to = "value"
    ) %>%
    # clean up demographic labels
    mutate(
      demographic = case_match(demographic,
                               "total_16plus" ~ "Total 16+",
                               "men_20plus"   ~ "Men 20+",
                               "women_20plus" ~ "Women 20+",
                               "youth_1619"   ~ "Both sexes 16-19"
      )
    )
  
  return(df)
}


# ------ process all years ----------------------------------------------------

df_all <- lapply(years, function(year) {
  
  filename <- sprintf("cpsaat27-%d.xlsx", year)
  filepath <- file.path(input_dir, filename)
  
  cat(sprintf("Processing %d ... ", year))
  df <- clean_table27(filepath, year)
  cat(sprintf("%d rows\n", nrow(df)))
  
  return(df)
  
}) %>% bind_rows()


# ------ save output ----------------------------------------------------------

output_path <- file.path(output_dir, "table27_all_years.csv")
write.csv(df_all, output_path, row.names = FALSE)
