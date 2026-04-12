# ------ Data cleaning: Bureau of Labor Statistics ----------------------------
# ------ Table 31: Unemployed persons by age, sex, race, Hispanic or Latino
# ------ ethnicity, marital status, and duration of unemployment
#
# no. of unemployed people in thousands
# - by age and sex
#   - duration
#       mean, median
#       less than 5
#       5-14 weeks
#       15+ weeks
#       15-26 weeks
#       27+ weeks
#   - age
#       [ Total 16+ ] [ 16-19 ] [ 20-24 ] [ 25-34 ] [ 35-44 ] [ 45-54 ] [ 55-64 ] [ 65+ ]
# 
# - by race and sex
#     sex: men, women
#     race: white, black, asian, hispanic/latino ethnicity
# 
# - by marital status and sex
#     sex: men, women
#     marital status:
#       - married
#       - widowed/divorced/separated
#       - never married
#
# note: before 2020, "married" referred only to hetero couples, same sex couples included after starting 2020
#   
# cleaned output: each row represents one demographic group × year combination
# 3 sections, each tagged with 'section' column:
#   "age_sex"   – age × sex breakdown (rows 10–35 in source file)
#   "race"      – race/ethnicity with Men/Women sub-rows (rows 38–49)
#   "marital"   – marital status by sex (rows 53–59)



library(readxl)
library(dplyr)
library(stringr)


# ------ config ---------------------------------------------------------------

if (file.exists("../.Renviron")) readRenviron("../.Renviron")
root <- Sys.getenv("PROJECT_ROOT")
if (root == "") stop("Set PROJECT_ROOT in repo-root .Renviron (see .Renviron.example).", call. = FALSE)
input_dir  <- file.path(root, "bls-data/31-unemployed-by-duration")
output_dir <- file.path(root, "bls-cleaned")

years <- c(2019, 2020, 2021, 2022, 2023, 2024, 2025)

# column names based on excel file headers
col_names <- c(
  "category",
  "total_unemployed",   # total unemployed (thousands)
  "less_5_weeks",       # unemployed less than 5 weeks
  "5_to_14_weeks",      # unemployed 5 to 14 weeks
  "15_plus_weeks",      # unemployed 15 weeks and over (= 15_to_26 + 27_plus)
  "15_to_26_weeks",     # unemployed 15 to 26 weeks
  "27_plus_weeks",      # unemployed 27 weeks and over
  "avg_duration",       # average (mean) weeks unemployed
  "median_duration"     # median weeks unemployed
)

# section/sub-section header rows with no numeric data — drop after using them for forward-fill
section_headers <- c(
  "AGE AND SEX",
  "RACE AND HISPANIC OR LATINO ETHNICITY",
  "MARITAL STATUS"
)

# "aggregate" age group rows that also hold numeric data, so we keep the rows
# but use the label to assign the sex column before normalizing category text
age_sex_group_labels <- c(
  "Total, 16 years and over",
  "Men, 16 years and over",
  "Women, 16 years and over"
)

# row labels used as sub-headers in the marital status section of excel file (rows 52, 56).
# these rows have no numeric data
marital_sex_headers <- c(
  "Men, 16 years and over",
  "Women, 16 years and over"
)

# race group aggregate rows (rows 38, 41, 44, 47): these hold totals for each
# race and are kept as data rows. label encodes both race and age
race_group_labels <- c(
  "White, 16 years and over",
  "Black or African American, 16 years and over",
  "Asian, 16 years and over",
  "Hispanic or Latino ethnicity, 16 years and over"
)


# ------ cleaning function ----------------------------------------------------

clean_table31 <- function(filepath, year) {
  
  # step 1: read. in excel file and skip header rows
  df <- read_excel(filepath, skip = 8, col_names = FALSE)
  
  # step 2: 2021 has 2 trailing empty columns; pad names and drop the extras
  n_cols <- ncol(df)
  if (n_cols > length(col_names)) {
    padded <- c(col_names, paste0("drop_", seq_len(n_cols - length(col_names))))
    names(df) <- padded
    df <- df %>% select(all_of(col_names))
  } else {
    names(df) <- col_names
  }
  
  # step 3: drop fully blank rows and footer rows (Footnotes, NOTE, blank).
  df <- df %>%
    filter(
      !if_all(everything(), is.na),
      !str_starts(coalesce(as.character(category), ""), "NOTE"),
      !str_starts(coalesce(as.character(category), ""), "Footnotes"),
      !str_starts(coalesce(as.character(category), ""), "\\("),
      str_trim(coalesce(as.character(category), "")) != ""
    )
  
  # step 4: assign 'section' by forward-filling from section header rows.
  #   section headers are all-caps strings (AGE AND SEX, etc.).
  df <- df %>%
    mutate(
      section_raw = case_when(
        category == "AGE AND SEX"                          ~ "age_sex",
        category == "RACE AND HISPANIC OR LATINO ETHNICITY" ~ "race",
        category == "MARITAL STATUS"                       ~ "marital",
        TRUE                                               ~ NA_character_
      )
    ) %>%
    fill(section_raw, .direction = "down") %>%
    rename(section = section_raw)
  
  # step 5: drop section header rows — they hold no numeric data.
  df <- df %>%
    filter(!category %in% section_headers)
  
  # step 6: assign 'sex' column.
  df <- df %>%
    mutate(
      sex_raw = case_when(
        # age_sex group labels encode sex in the row label
        category == "Total, 16 years and over"   ~ "Total",
        category == "Men, 16 years and over"      ~ "Men",
        category == "Women, 16 years and over"    ~ "Women",
        # race section: race aggregate rows are totals; Men/Women rows explicit
        category %in% race_group_labels           ~ "Total",
        category == "Men"                         ~ "Men",
        category == "Women"                       ~ "Women",
        TRUE                                      ~ NA_character_
      )
    ) %>%
    fill(sex_raw, .direction = "down") %>%
    rename(sex = sex_raw)
  
  # step 7: drop marital sex sub-headers (no data, just context)
  df <- df %>%
    filter(!(category %in% marital_sex_headers & section == "marital"))
  
  # step 8: assign 'race' column (relevant only for the race section;
  #   NA elsewhere is intentional).
  df <- df %>%
    mutate(
      race_raw = case_when(
        str_starts(category, "White")                       ~ "White",
        str_starts(category, "Black or African American")   ~ "Black or African American",
        str_starts(category, "Asian")                       ~ "Asian",
        str_starts(category, "Hispanic or Latino")          ~ "Hispanic or Latino",
        TRUE                                                ~ NA_character_
      )
    ) %>%
    # fill within section only: fill down but reset at section boundaries
    # (since race fill only makes sense within the race section, we fill
    # conditionally — only propagate if we're still in the race section)
    fill(race_raw, .direction = "down") %>%
    mutate(
      race = if_else(section == "race", race_raw, NA_character_)
    ) %>%
    select(-race_raw)
  
  # step 9: assign `age_group` column.
  #   aggregate age rows are all totals 16 years and over
  #   age sub-rows have labels like "16 to 19 years"
  df <- df %>%
    mutate(
      age_group = case_when(
        category %in% age_sex_group_labels ~
          str_extract(category, "\\d+") %>% paste0("+"),  # "16+"
        str_detect(category, "to \\d+ years")    ~
          str_replace(category, "(\\d+) to (\\d+) years", "\\1-\\2"),
        str_detect(category, "\\d+ years and over") ~
          str_replace(category, "(\\d+) years and over", "\\1+"),
        TRUE ~ NA_character_
      )
    )
  
  # step 10: assign `marital_status` column (marital section only).
  #   strip footnote markers (1), (2)
  df <- df %>%
    mutate(
      marital_status = if_else(
        section == "marital",
        str_remove(category, "\\(\\d+\\)") %>% str_trim(),
        NA_character_
      )
    )
  
  # step 11: cast numeric columns.
  numeric_cols <- col_names[col_names != "category"]
  df <- df %>%
    mutate(across(all_of(numeric_cols), ~suppressWarnings(as.numeric(as.character(.)))))
  
  # step 12: add year, finalize column order, drop raw category.
  df <- df %>%
    mutate(year = year) %>%
    select(
      year, section, sex, age_group, race, marital_status,
      all_of(numeric_cols),
      -category
    )
  
  return(df)
}


# ------ process all years ----------------------------------------------------

df_all <- lapply(years, function(year) {
  
  filename <- sprintf("cpsaat31-%d.xlsx", year)
  filepath <- file.path(input_dir, filename)
  
  cat(sprintf("Processing %d ... ", year))
  df <- clean_table31(filepath, year)
  cat(sprintf("%d rows\n", nrow(df)))
  
  return(df)
  
}) %>% bind_rows()



# ------ save output ----------------------------------------------------------

output_path <- file.path(output_dir, "table31_all_years.csv")
write.csv(df_all, output_path, row.names = FALSE)
