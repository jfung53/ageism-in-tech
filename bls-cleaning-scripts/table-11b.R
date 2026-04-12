# ------ Data cleaning: Bureau of Labor Statistics ----------------------------
# ------ Table 11b: Employed persons by detailed occupation and age -----------
#
# no. of employed people in thousands
# DETAILED occupation
# age: pretty granular, plus median age
#
# 2019 uses older SOC classifications, 2020+ uses newer. 
# difference of 30 occupation titles flagged by soc_system column


library(readxl)
library(dplyr)
library(stringr)
library(tibble)


# ------ config ---------------------------------------------------------------

if (file.exists("../.Renviron")) readRenviron("../.Renviron")
root <- Sys.getenv("PROJECT_ROOT")
if (root == "") stop("Set PROJECT_ROOT in repo-root .Renviron (see .Renviron.example).", call. = FALSE)
input_dir  <- file.path(root, "bls-data/11b-more-detailed-occupation")
output_dir <- file.path(root, "bls-cleaned")

years <- c(2019, 2020, 2021, 2022, 2023, 2024, 2025)

# column names based on excel file
col_names <- c(
  "occupation",
  "total_16plus",
  "age_16to19",
  "age_20to24",
  "age_25to34",
  "age_35to44",
  "age_45to54",
  "age_55to64",
  "age_65plus",
  "median_age"
)


# ------ occupation lookup: IT, engineering, life sciences, healthcare ---------
#
# Four comparison groups for analysis:
#
#   IT occupations  (occ_category = "it")
#     Core information technology: software, systems, networking, security,
#     support, data science/ML, IT management, and technical writing.
#
#   Engineering occupations  (occ_category = "engineering")
#     Full architecture & engineering occupations section from excel files.
#
#   Life & physical science occupations  (occ_category = "life_science")
#     Life, physical, and associated science technician occupations. 
#
#   Healthcare occupations  (occ_category = "healthcare")
#     Healthcare practitioners and technicians. 
    
    
occ_lookup <- tribble(
  ~occupation,                                                                ~occ_category,  ~occ_group,           ~occ_level,    ~soc_note,
  
  # ==========================================================================
  # IT OCCUPATIONS
  # ==========================================================================
  
  # ---- Computer & mathematical: group header --------------------------------
  # Note: includes Actuaries, Statisticians, etc. which are not in IT scope.
  # Use leaf-level rows only (occ_level == "occupation") for precise IT totals.
  "Computer and mathematical occupations",                                    "it",           "computer",           "group",        NA,
  
  # ---- Core computer occupations: 2020+ titles (2018 SOC) ------------------
  "Computer and information research scientists",                             "it",           "computer",           "occupation",   NA,
  "Computer systems analysts",                                                "it",           "computer",           "occupation",   NA,
  "Information security analysts",                                            "it",           "computer",           "occupation",   NA,
  "Computer programmers",                                                     "it",           "computer",           "occupation",   NA,
  "Software developers",                                                      "it",           "computer",           "occupation",   "2020+ only; see 2019 note",
  "Software quality assurance analysts and testers",                          "it",           "computer",           "occupation",   "2020+ only; see 2019 note",
  "Web developers",                                                           "it",           "computer",           "occupation",   NA,
  "Web and digital interface designers",                                      "it",           "computer",           "occupation",   "2020+ only",
  "Computer support specialists",                                             "it",           "computer",           "occupation",   NA,
  "Database administrators and architects",                                   "it",           "computer",           "occupation",   "2020+ only; see 2019 note",
  "Network and computer systems administrators",                              "it",           "computer",           "occupation",   NA,
  "Computer network architects",                                              "it",           "computer",           "occupation",   NA,
  "Computer occupations, all other",                                          "it",           "computer",           "occupation",   NA,
  
  # ---- Core computer occupations: 2019-only titles (2010 SOC) ---------------
  "Software developers, applications and systems software",                   "it",           "computer",           "occupation",   "2019 only; split into two titles in 2020+",
  "Database administrators",                                                  "it",           "computer",           "occupation",   "2019 only; renamed in 2020+",
  
  # ---- Data science & quantitative analytics --------------------------------
  # These BLS titles absorb data scientists, ML engineers, and quantitative
  # analysts not captured under computer occupations. Actuaries and
  # Mathematicians are excluded (insurance math and academic research).
  "Operations research analysts",                                             "it",           "data_science",       "occupation",   NA,
  "Statisticians",                                                            "it",           "data_science",       "occupation",   NA,
  
  # ---- IT management --------------------------------------------------------
  "Computer and information systems managers",                                "it",           "it_management",      "occupation",   NA,
  
  # ---- Technical writers ----------------------------------------------------
  "Technical writers",                                                        "it",           "technical_writer",   "occupation",   NA,
  
  
  # ==========================================================================
  # ENGINEERING OCCUPATIONS
  # ==========================================================================
  
  # ---- Architecture & engineering: group header ----------------------------
  "Architecture and engineering occupations",                                 "engineering",  "arch_engineering",   "group",        NA,
  
  # ---- Architects & surveyors -----------------------------------------------
  "Architects, except landscape and naval",                                   "engineering",  "arch_engineering",   "occupation",   "2020+ only; see 2019 note",
  "Architects, except naval",                                                 "engineering",  "arch_engineering",   "occupation",   "2019 only; renamed in 2020+",
  "Landscape architects",                                                     "engineering",  "arch_engineering",   "occupation",   NA,
  "Surveyors, cartographers, and photogrammetrists",                          "engineering",  "arch_engineering",   "occupation",   NA,
  
  # ---- Engineers ------------------------------------------------------------
  "Aerospace engineers",                                                      "engineering",  "arch_engineering",   "occupation",   NA,
  "Agricultural engineers",                                                   "engineering",  "arch_engineering",   "occupation",   NA,
  "Bioengineers and biomedical engineers",                                    "engineering",  "arch_engineering",   "occupation",   "2020+ only; see 2019 note",
  "Biomedical engineers",                                                     "engineering",  "arch_engineering",   "occupation",   "2019 only; renamed in 2020+",
  "Chemical engineers",                                                       "engineering",  "arch_engineering",   "occupation",   NA,
  "Civil engineers",                                                          "engineering",  "arch_engineering",   "occupation",   NA,
  "Environmental engineers",                                                  "engineering",  "arch_engineering",   "occupation",   NA,
  "Computer hardware engineers",                                              "engineering",  "arch_engineering",   "occupation",   NA,
  "Electrical and electronics engineers",                                     "engineering",  "arch_engineering",   "occupation",   NA,
  "Industrial engineers, including health and safety",                        "engineering",  "arch_engineering",   "occupation",   NA,
  "Marine engineers and naval architects",                                    "engineering",  "arch_engineering",   "occupation",   NA,
  "Materials engineers",                                                      "engineering",  "arch_engineering",   "occupation",   NA,
  "Mechanical engineers",                                                     "engineering",  "arch_engineering",   "occupation",   NA,
  "Mining and geological engineers, including mining safety engineers",       "engineering",  "arch_engineering",   "occupation",   NA,
  "Nuclear engineers",                                                        "engineering",  "arch_engineering",   "occupation",   NA,
  "Petroleum engineers",                                                      "engineering",  "arch_engineering",   "occupation",   NA,
  "Engineers, all other",                                                     "engineering",  "arch_engineering",   "occupation",   NA,
  
  # ---- Drafters -------------------------------------------------------------
  "Architectural and civil drafters",                                         "engineering",  "arch_engineering",   "occupation",   "2020+ only; see 2019 note",
  "Other drafters",                                                           "engineering",  "arch_engineering",   "occupation",   "2020+ only; see 2019 note",
  "Drafters",                                                                 "engineering",  "arch_engineering",   "occupation",   "2019 only; split into two titles in 2020+",
  
  # ---- Engineering technicians ----------------------------------------------
  "Electrical and electronic engineering technologists and technicians",      "engineering",  "eng_technician",     "occupation",   "2020+ only; see 2019 note",
  "Other engineering technologists and technicians, except drafters",         "engineering",  "eng_technician",     "occupation",   "2020+ only; see 2019 note",
  "Engineering technicians, except drafters",                                 "engineering",  "eng_technician",     "occupation",   "2019 only; split into two titles in 2020+",
  "Surveying and mapping technicians",                                        "engineering",  "eng_technician",     "occupation",   NA,
  
  # ---- Engineering management -----------------------------------------------
  "Architectural and engineering managers",                                   "engineering",  "arch_engineering",   "occupation",   NA,
  
  
  # ==========================================================================
  # LIFE & PHYSICAL SCIENCE OCCUPATIONS
  # ==========================================================================
  
  # ---- Life, physical, and social science: group header --------------------
  "Life, physical, and social science occupations",                           "life_science", "life_science",       "group",        NA,
  
  # ---- Life scientists ------------------------------------------------------
  "Agricultural and food scientists",                                         "life_science", "life_science",       "occupation",   NA,
  "Biological scientists",                                                    "life_science", "life_science",       "occupation",   NA,
  "Conservation scientists and foresters",                                    "life_science", "life_science",       "occupation",   NA,
  "Medical scientists",                                                       "life_science", "life_science",       "occupation",   NA,
  "Life scientists, all other",                                               "life_science", "life_science",       "occupation",   NA,
  
  # ---- Physical scientists --------------------------------------------------
  "Astronomers and physicists",                                               "life_science", "physical_science",   "occupation",   NA,
  "Atmospheric and space scientists",                                         "life_science", "physical_science",   "occupation",   NA,
  "Chemists and materials scientists",                                        "life_science", "physical_science",   "occupation",   NA,
  "Environmental scientists and specialists, including health",               "life_science", "physical_science",   "occupation",   "2020+ only; see 2019 note",
  "Geoscientists and hydrologists, except geographers",                       "life_science", "physical_science",   "occupation",   "2020+ only; see 2019 note",
  "Environmental scientists and geoscientists",                               "life_science", "physical_science",   "occupation",   "2019 only; split into two titles in 2020+",
  "Physical scientists, all other",                                           "life_science", "physical_science",   "occupation",   NA,
  
  # ---- Science technicians --------------------------------------------------
  "Agricultural and food science technicians",                                "life_science", "science_technician", "occupation",   NA,
  "Biological technicians",                                                   "life_science", "science_technician", "occupation",   NA,
  "Chemical technicians",                                                     "life_science", "science_technician", "occupation",   NA,
  "Environmental science and geoscience technicians",                         "life_science", "science_technician", "occupation",   "2020+ only; see 2019 note",
  "Geological and petroleum technicians",                                     "life_science", "science_technician", "occupation",   "2019 only; absorbed into env. science technicians in 2020+",
  "Nuclear technicians",                                                      "life_science", "science_technician", "occupation",   NA,
  "Other life, physical, and social science technicians",                     "life_science", "science_technician", "occupation",   "2020+ only; see 2019 note",
  "Miscellaneous life, physical, and social science technicians",             "life_science", "science_technician", "occupation",   "2019 only; renamed in 2020+",
  
  # ---- Science management ---------------------------------------------------
  "Natural sciences managers",                                                "life_science", "life_science",       "occupation",   NA,
  
  
  # ==========================================================================
  # HEALTHCARE OCCUPATIONS
  # (practitioners and technicians only; support workers excluded)
  # ==========================================================================
  
  # ---- Healthcare practitioners & technical: group header ------------------
  "Healthcare practitioners and technical occupations",                       "healthcare",   "health_practitioner","group",        NA,
  
  # ---- Practitioners --------------------------------------------------------
  "Chiropractors",                                                            "healthcare",   "health_practitioner","occupation",   NA,
  "Dentists",                                                                 "healthcare",   "health_practitioner","occupation",   NA,
  "Dietitians and nutritionists",                                             "healthcare",   "health_practitioner","occupation",   NA,
  "Optometrists",                                                             "healthcare",   "health_practitioner","occupation",   NA,
  "Pharmacists",                                                              "healthcare",   "health_practitioner","occupation",   NA,
  "Emergency medicine physicians",                                            "healthcare",   "health_practitioner","occupation",   "2020+ only",
  "Radiologists",                                                             "healthcare",   "health_practitioner","occupation",   "2020+ only",
  "Other physicians",                                                         "healthcare",   "health_practitioner","occupation",   "2020+ only",
  "Surgeons",                                                                 "healthcare",   "health_practitioner","occupation",   "2020+ only",
  "Physicians and surgeons",                                                  "healthcare",   "health_practitioner","occupation",   "2019 only; split into separate titles in 2020+",
  "Physician assistants",                                                     "healthcare",   "health_practitioner","occupation",   NA,
  "Podiatrists",                                                              "healthcare",   "health_practitioner","occupation",   NA,
  "Audiologists",                                                             "healthcare",   "health_practitioner","occupation",   NA,
  "Occupational therapists",                                                  "healthcare",   "health_practitioner","occupation",   NA,
  "Physical therapists",                                                      "healthcare",   "health_practitioner","occupation",   NA,
  "Radiation therapists",                                                     "healthcare",   "health_practitioner","occupation",   NA,
  "Recreational therapists",                                                  "healthcare",   "health_practitioner","occupation",   NA,
  "Respiratory therapists",                                                   "healthcare",   "health_practitioner","occupation",   NA,
  "Speech-language pathologists",                                             "healthcare",   "health_practitioner","occupation",   NA,
  "Exercise physiologists",                                                   "healthcare",   "health_practitioner","occupation",   "2020+ only",
  "Therapists, all other",                                                    "healthcare",   "health_practitioner","occupation",   NA,
  "Veterinarians",                                                            "healthcare",   "health_practitioner","occupation",   NA,
  "Registered nurses",                                                        "healthcare",   "health_practitioner","occupation",   NA,
  "Nurse anesthetists",                                                       "healthcare",   "health_practitioner","occupation",   NA,
  "Nurse midwives",                                                           "healthcare",   "health_practitioner","occupation",   NA,
  "Nurse practitioners",                                                      "healthcare",   "health_practitioner","occupation",   NA,
  "Acupuncturists",                                                           "healthcare",   "health_practitioner","occupation",   "2020+ only",
  "Healthcare diagnosing or treating practitioners, all other",               "healthcare",   "health_practitioner","occupation",   "2020+ only; see 2019 note",
  "Health diagnosing and treating practitioners, all other",                  "healthcare",   "health_practitioner","occupation",   "2019 only; renamed in 2020+",
  
  # ---- Health technicians & technologists -----------------------------------
  "Clinical laboratory technologists and technicians",                        "healthcare",   "health_technician",  "occupation",   NA,
  "Dental hygienists",                                                        "healthcare",   "health_technician",  "occupation",   NA,
  "Cardiovascular technologists and technicians",                             "healthcare",   "health_technician",  "occupation",   "2020+ only",
  "Diagnostic medical sonographers",                                          "healthcare",   "health_technician",  "occupation",   "2020+ only",
  "Radiologic technologists and technicians",                                 "healthcare",   "health_technician",  "occupation",   "2020+ only",
  "Magnetic resonance imaging technologists",                                 "healthcare",   "health_technician",  "occupation",   "2020+ only",
  "Nuclear medicine technologists and medical dosimetrists",                  "healthcare",   "health_technician",  "occupation",   "2020+ only",
  "Emergency medical technicians",                                            "healthcare",   "health_technician",  "occupation",   "2020+ only",
  "Paramedics",                                                               "healthcare",   "health_technician",  "occupation",   "2020+ only",
  "Diagnostic related technologists and technicians",                         "healthcare",   "health_technician",  "occupation",   "2019 only; split into separate titles in 2020+",
  "Emergency medical technicians and paramedics",                             "healthcare",   "health_technician",  "occupation",   "2019 only; split into separate titles in 2020+",
  "Pharmacy technicians",                                                     "healthcare",   "health_technician",  "occupation",   NA,
  "Psychiatric technicians",                                                  "healthcare",   "health_technician",  "occupation",   NA,
  "Surgical technologists",                                                   "healthcare",   "health_technician",  "occupation",   NA,
  "Veterinary technologists and technicians",                                 "healthcare",   "health_technician",  "occupation",   NA,
  "Dietetic technicians and ophthalmic medical technicians",                  "healthcare",   "health_technician",  "occupation",   "2020+ only",
  "Licensed practical and licensed vocational nurses",                        "healthcare",   "health_technician",  "occupation",   NA,
  "Medical records specialists",                                              "healthcare",   "health_technician",  "occupation",   "2020+ only",
  "Medical records and health information technicians",                       "healthcare",   "health_technician",  "occupation",   "2019 only; renamed in 2020+",
  "Opticians, dispensing",                                                    "healthcare",   "health_technician",  "occupation",   NA,
  "Miscellaneous health technologists and technicians",                       "healthcare",   "health_technician",  "occupation",   NA,
  "Health practitioner support technologists and technicians",                "healthcare",   "health_technician",  "occupation",   "2019 only; absorbed into misc. health technicians in 2020+",
  "Other healthcare practitioners and technical occupations",                 "healthcare",   "health_technician",  "occupation",   NA
)


# ------ cleaning function ----------------------------------------------------

clean_table11b <- function(filepath, year) {
  
  # step 1: read in excel file, skip metadata rows
  df <- read_excel(filepath, skip = 6, col_names = FALSE)
  
  # step 2: drop phantom 11th column (present only in 2021; entirely blank)
  if (ncol(df) > length(col_names)) {
    df <- df[, seq_len(length(col_names))]
  }
  
  # step 3: assign column names
  names(df) <- col_names
  
  # step 4: drop unnecessary rows
  #   - blank spacer rows (occupation is NA or empty string)
  #   - NOTE footer row
  #   - "Total employed" aggregate row
  df <- df %>%
    filter(
      !is.na(occupation),
      str_trim(occupation) != "",
      !str_starts(occupation, "NOTE"),
      occupation != "Total employed"
    )
  
  # step 5: cast numeric columns
  df <- df %>%
    mutate(
      across(
        c(total_16plus, age_16to19, age_20to24, age_25to34,
          age_35to44, age_45to54, age_55to64, age_65plus),
        as.numeric
      ),
      median_age = suppressWarnings(as.numeric(as.character(median_age)))
    )
  
  # step 6: add year and SOC system flag
  df <- df %>%
    mutate(
      year       = year,
      soc_system = if_else(year <= 2019, "2010_SOC", "2018_SOC"),
      .before    = occupation
    )
  
  # step 7: join occupation category flags
  #   Adds: occ_category, occ_group, occ_level, soc_note
  #   occ_category is NA for occupations outside the four comparison groups.
  #   Use filter(!is.na(occ_category)) to restrict to defined groups,
  #   or filter(occ_category == "it") etc. for a specific group.
  df <- df %>%
    left_join(occ_lookup, by = "occupation") %>%
    relocate(occ_category, occ_group, occ_level, soc_note, .after = occupation)
  
  return(df)
}


# ------ process all years ----------------------------------------------------

df_all <- lapply(years, function(year) {
  
  filename <- sprintf("cpsaat11b-%d.xlsx", year)
  filepath <- file.path(input_dir, filename)
  
  cat(sprintf("Processing %d ... ", year))
  df <- clean_table11b(filepath, year)
  
  n_by_cat <- df %>%
    filter(!is.na(occ_category)) %>%
    count(occ_category) %>%
    mutate(label = paste0(occ_category, "=", n)) %>%
    pull(label) %>%
    paste(collapse = ", ")
  
  cat(sprintf("%d rows (%s)\n", nrow(df), n_by_cat))
  return(df)
  
}) %>% bind_rows()

df_all %>%
  filter(!is.na(occ_category)) %>%
  count(occ_category) %>%
  print()


# ------ save output ----------------------------------------------------------

# full table (all occupations)
output_path_all <- file.path(output_dir, "table11b_all_years.csv")
write.csv(df_all, output_path_all, row.names = FALSE)

# IT & stem comparison group subset (it, engineering, life_science, healthcare only)
output_path_groups <- file.path(output_dir, "table11b_comparison_groups.csv")
df_all %>%
  filter(!is.na(occ_category)) %>%
  write.csv(output_path_groups, row.names = FALSE)
