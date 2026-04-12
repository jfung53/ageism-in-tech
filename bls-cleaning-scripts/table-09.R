
# ------ Data cleaning: Bureau of Labor Statistics ----------------------------
# ------ Table 9: Employed people by occupation, sex, and age -----------------
# 
# no. of employed people in thousands
# occupation (NOT detailed)
# sex: men, women
# age: [ 16+ ] [  20+ ]

library(readxl)
library(dplyr)
library(stringr)
library(tibble)


# ------ config ---------------------------------------------------------------

if (file.exists("../.Renviron")) readRenviron("../.Renviron")
root <- Sys.getenv("PROJECT_ROOT")
if (root == "") stop("Set PROJECT_ROOT in repo-root .Renviron (see .Renviron.example).", call. = FALSE)
input_dir  <- file.path(root, "bls-data/9-detailed-occupation")
output_dir <- file.path(root, "bls-cleaned")

years <- c(2019, 2020, 2021, 2022, 2023, 2024, 2025)

# column names based on header rows
col_names <- c(
  "occupation",
  "total_16plus_prev",  "total_16plus",
  "men_16plus_prev",    "men_16plus",
  "men_20plus_prev",    "men_20plus",
  "women_16plus_prev",  "women_16plus",
  "women_20plus_prev",  "women_20plus"
)

# columns to drop: previous year's values
prev_cols <- c(
  "total_16plus_prev",
  "men_16plus_prev",
  "men_20plus_prev",
  "women_16plus_prev",
  "women_20plus_prev"
)

# ------ occupation hierarchy lookup ------------------------------------------
# this deals with the indent formatting of the original spreadsheets

occ_hierarchy <- tribble(~occupation, ~occ_level, ~occ_group, ~occ_subgroup,
  
  # ---- Management, professional, and related (indent=0) ----------------
  "Management, professional, and related occupations",                0L, "management_professional",        NA,
  
  # ---- subgroup: Management, business, and financial (indent=1) --------
  "Management, business, and financial operations occupations",       1L, "management_professional",        "management_business_financial",
  "Management occupations",                                           2L, "management_professional",        "management_business_financial",
  "Business and financial operations occupations",                    2L, "management_professional",        "management_business_financial",
  
  # ---- subgroup: Professional and related (indent=1) -------------------
  "Professional and related occupations",                             1L, "management_professional",        "professional_related",
  "Computer and mathematical occupations",                            2L, "management_professional",        "professional_related",
  "Architecture and engineering occupations",                         2L, "management_professional",        "professional_related",
  "Life, physical, and social science occupations",                   2L, "management_professional",        "professional_related",
  "Community and social service occupations",                         2L, "management_professional",        "professional_related",
  "Legal occupations",                                                2L, "management_professional",        "professional_related",
  "Education, training, and library occupations",                     2L, "management_professional",        "professional_related",
  "Arts, design, entertainment, sports, and media occupations",       2L, "management_professional",        "professional_related",
  "Healthcare practitioners and technical occupations",               2L, "management_professional",        "professional_related",
  
  # ---- Service occupations (indent=0) ----------------------------------
  "Service occupations",                                              0L, "service",                        NA,
  "Healthcare support occupations",                                   1L, "service",                        NA,
  "Protective service occupations",                                   1L, "service",                        NA,
  "Food preparation and serving related occupations",                 1L, "service",                        NA,
  "Building and grounds cleaning and maintenance occupations",        1L, "service",                        NA,
  "Personal care and service occupations",                            1L, "service",                        NA,
  
  # ---- Sales and office occupations (indent=0) -------------------------
  "Sales and office occupations",                                     0L, "sales_office",                   NA,
  "Sales and related occupations",                                    1L, "sales_office",                   NA,
  "Office and administrative support occupations",                    1L, "sales_office",                   NA,
  
  # ---- Natural resources, construction, and maintenance (indent=0) -----
  "Natural resources, construction, and maintenance occupations",     0L, "natural_resources_construction", NA,
  "Farming, fishing, and forestry occupations",                       1L, "natural_resources_construction", NA,
  "Construction and extraction occupations",                          1L, "natural_resources_construction", NA,
  "Installation, maintenance, and repair occupations",                1L, "natural_resources_construction", NA,
  
  # ---- Production, transportation, and material moving (indent=0) ------
  "Production, transportation, and material moving occupations",      0L, "production_transportation",      NA,
  "Production occupations",                                           1L, "production_transportation",      NA,
  "Transportation and material moving occupations",                   1L, "production_transportation",      NA
)


# ------ cleaning function ----------------------------------------------------

clean_table09 <- function(filepath, year) {
  
  # step 1: read in excel file, skipping the 7-row metadata header
  df <- read_excel(filepath, skip = 7, col_names = FALSE)
  
  # step 2: assign column names
  names(df) <- col_names
  
  # step 3: drop unnecessary rows
  #   - blank spacer rows (occupation is NA or empty string)
  #   - NOTE footer row
  #   - "Total" aggregate row (row 8 in the original file)
  df <- df %>%
    filter(
      !is.na(occupation),
      str_trim(occupation) != "",
      !str_starts(occupation, "NOTE"),
      occupation != "Total"
    )
  
  # step 4: drop _prev year columns
  df <- df %>%
    select(-all_of(prev_cols))
  
  # step 5: cast numeric columns
  df <- df %>%
    mutate(across(-occupation, as.numeric))
  
  # step 6: add year column (based on filename)
  df <- df %>%
    mutate(year = year, .before = occupation)
  
  # step 7: join occupation hierarchy flags
  df <- df %>%
    left_join(occ_hierarchy, by = "occupation") %>%
    relocate(occ_level, occ_group, occ_subgroup, .after = occupation)
  
  return(df)
}


# ------ process all years ----------------------------------------------------

df_all <- lapply(years, function(year) {
  
  filename <- sprintf("cpsaat09-%d.xlsx", year)
  filepath <- file.path(input_dir, filename)
  
  cat(sprintf("Processing %d ... ", year))
  df <- clean_table09(filepath, year)
  cat(sprintf("%d rows\n", nrow(df)))
  
  return(df)
  
}) %>% bind_rows()


# ------ save output ----------------------------------------------------------

output_path <- file.path(output_dir, "table09_all_years.csv")
write.csv(df_all, output_path, row.names = FALSE)
