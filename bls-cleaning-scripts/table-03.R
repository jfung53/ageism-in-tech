
# ------ Data cleaning: Bureau of Labor Statistics ----------------------------
# ------ Table 3: Employment status of the civilian noninstitutional ----------
# ------ population by age, sex, and race
#
# no. of people in thousands:
# - population
# - in labour force (employed, unemployed)
# - not in labour force
#
# age: very granular
# sex by age: men, women
# race by age and sex: white, black, asian
#
# cleaned output: each row represents one race x sex x age_group combination
# per year. aggregate/summary age bands (e.g. "25 to 54 years") are flagged
# with is_aggregate = TRUE so they can be filtered out easily.

library(readxl)
library(dplyr)
library(stringr)
library(tidyr)


# ------ config ---------------------------------------------------------------

input_dir  <- "/Users/jocelyn/Documents/Pratt/Projects/ageism-in-tech/bls-data/3-employment-status"
output_dir <- "/Users/jocelyn/Documents/Pratt/Projects/ageism-in-tech/bls-cleaned"

years <- c(2019, 2020, 2021, 2022, 2023, 2024, 2025)

# column names based on excel headers
col_names <- c(
  "category",              # col A: age/sex/race label
  "pop_total",             # civilian noninstitutional population
  "lf_total",              # civilian labor force: total
  "lf_pct",                # civilian labor force: % of population
  "employed_total",        # employed: total
  "employed_pct",          # employed: % of population
  "unemployed_total",      # unemployed: number
  "unemployed_pct",        # unemployed: % of labor force
  "not_in_lf"              # not in labor force
)

# age rows that are summary/aggregate bands
# flagged as is_aggregate = TRUE so they can be filtered out if needed
aggregate_age_bands <- c(
  "25 to 54 years",
  "25 to 34 years",
  "35 to 44 years",
  "45 to 54 years",
  "55 to 64 years",
  "65 years and over"
)

# race section header labels from column A
race_headers <- c("TOTAL", "WHITE", "BLACK OR AFRICAN AMERICAN", "ASIAN")


# ------ cleaning function ----------------------------------------------------

clean_table03 <- function(filepath, year) {
  
  # step 1: read in excel file, skip metadata rows
  df <- read_excel(filepath, skip = 8, col_names = FALSE) %>%
    setNames(col_names)
  
  # step 2: drop completely blank rows and the NOTE footer
  df <- df %>%
    filter(
      !if_all(everything(), is.na),
      !str_starts(coalesce(category, ""), "NOTE")
    )
  
  # step 3: assign race by forward-filling from section header rows
  #   section headers are all-caps rows like "TOTAL", "WHITE", etc.
  #   header used for fill
  df <- df %>%
    mutate(
      race_data = if_else(category %in% race_headers, category, NA_character_)
    ) %>%
    fill(race_data, .direction = "down") %>%
    # recode to clean labels
    mutate(
      race = case_when(
        race_data == "TOTAL"                    ~ "Total",
        race_data == "WHITE"                    ~ "White",
        race_data == "BLACK OR AFRICAN AMERICAN" ~ "Black or African American",
        race_data == "ASIAN"                    ~ "Asian",
        TRUE                                   ~ NA_character_
      )
    ) %>%
    select(-race_data)
  
  # step 4: assign sex by forward-filling from "Men" / "Women" rows,
  #   scoped within each race block so "Women" from one block does not
  #   bleed into the pre-Men age rows of the next block.
  df <- df %>%
    mutate(
      sex_data = case_when(
        category == "Men"   ~ "Men",
        category == "Women" ~ "Women",
        TRUE                ~ NA_character_
      )
    ) %>%
    group_by(race) %>%
    fill(sex_data, .direction = "down") %>%
    ungroup() %>%
    mutate(
      sex = coalesce(sex_data, "Total")
    ) %>%
    select(-sex_data)
  
  # step 5: drop section header rows — they have no numeric data
  #   these are: race section headers ("TOTAL", "WHITE", ...) and sex headers ("Men", "Women")
  df <- df %>%
    filter(
      !category %in% race_headers,
      !category %in% c("Men", "Women")
    )
  
  # step 6: parse age_group from the category column
  df <- df %>%
    mutate(
      age_group = category %>%
        str_replace("(\\d+) years and over", "\\1+") %>%   # must come first
        str_replace("(\\d+) years$", "\\1")              %>%
        str_replace("(\\d+) to (\\d+)", "\\1-\\2")       %>%
        str_trim()
    )
  
  # step 7: flag aggregate age bands (not mutually exclusive with 5-year bands)
  df <- df %>%
    mutate(
      is_aggregate = category %in% aggregate_age_bands
    )
  
  # step 8: cast numeric columns
  numeric_cols <- col_names[col_names != "category"]
  df <- df %>%
    mutate(across(all_of(numeric_cols), ~suppressWarnings(as.numeric(as.character(.)))))
  
  # step 9: add year, clean up column order
  df <- df %>%
    mutate(year = year) %>%
    select(year, race, sex, age_group, is_aggregate,
           all_of(numeric_cols),
           -category)
  
  return(df)
}


# ------ process all years ----------------------------------------------------

df_all <- lapply(years, function(year) {
  
  filename <- sprintf("cpsaat03-%d.xlsx", year)
  filepath <- file.path(input_dir, filename)
  
  cat(sprintf("Processing %d ... ", year))
  df <- clean_table03(filepath, year)
  cat(sprintf("%d rows\n", nrow(df)))
  
  return(df)
  
}) %>% bind_rows()



# ------ save output ----------------------------------------------------------

output_path <- file.path(output_dir, "table03_all_years.csv")
write.csv(df_all, output_path, row.names = FALSE)
