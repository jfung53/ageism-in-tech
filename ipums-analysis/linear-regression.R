# Lab 5: Linear regression ------------------------------------------------

library(ipumsr)   # for IPUMS API
library(dplyr)    # for data management
library(car)      # for evaluating model assumptions
library(olsrr)    # for stepwise modeling

# set working directory
if (file.exists("../.Renviron")) readRenviron("../.Renviron")
root <- Sys.getenv("IPUMS_DATA_ROOT")
if (root == "") stop("Set IPUMS_DATA_ROOT in repo-root .Renviron (see .Renviron.example).", call. = FALSE)
setwd(normalizePath(root, winslash = "/", mustWork = TRUE))

# IPUMS API key
ipums_key <- Sys.getenv("IPUMS_API_KEY")
set_ipums_api_key(ipums_key)

# Get data from IPUMS -----------------------------------------------------


## see list of data - i'm using CPS, not ACS
sample_list <- get_sample_info("cps")

## define an extract - 2025 annual samples
extract <- define_extract_micro(
  collection = "cps",                        # defines general data collection
  description = "IPUMS-CPS, ASEC ",          # defines specific data source
  samples = c("cps2025_03s", "cps2024_03s", 
              "cps2023_03s", "cps2022_03s", 
              "cps2021_03s", "cps2020_03s", 
              "cps2019_03s"),               # annual samples from 2019-2025 
  variables = c(
    "STATEFIP",   # state fips code
    "YEAR",      # year
    "AGE",       # age (in years)
    "SEX",       # sex (male=1 or female=2)
    "RACE",      # race
    "HISPAN",    # hispanic origin
    "EDUC",      # educational attainment
    "OCC",       # occupation code
    "IND",       # industry
    "EMPSTAT",   # employment status
    "LABFORCE",  # labour force participation (no=1, yes=2)
    "WHYUNEMP",  # reason for unemployment
    "DURUNEMP",  # duration of unemployment (in weeks. 000 = less than 1, 999 = missing)
    "WKSTAT",    # full/part-time status
    "CLASSWKR"   # class of worker
  ), 
  data_quality_flags = TRUE                  # will add indicator of potential data problems (e.g. missing values)
)

## submit the API and download results
extract <- submit_extract(extract)    # submits the request to ipums.org, sends email when ready
# wait_for_extract(cps_extract)         # not strictly necessary. Just cycles until download is ready. 
filepath <- download_extract(extract) # downloads to your working directory

## read data
ddi <- read_ipums_ddi(filepath)       # unzips and reads the code book (ddi file)
data <- read_ipums_micro(ddi)         # unzips and reads the data based on ddi, ads variable labels, etc. 
ls(data)

# having a look at the data
table(data$DURUNEMP)
table(data$AGE)
table(data$RACE)
table(data$HISPAN)
table(data$EDUC)
table(data$DURUNEMP) # 1,054,516 records are NIU, ~22k known
table(data$WHYUNEMP) # 1,064,497 records are NIU, ~22k known

# Q1: Define your model ---------------------------------------------------

## This part of lab is mostly conceptual. But check for correlations and eventually test assumptions. 

# potential predictor variables: sex, age, race, educational attainment, occupation, industry
# outcome variable: duration of unemployment

## variables for modeling interest (chose duration of unemployment, ultimately)
variables_model <- c("AGE", "SEX", "RACE", "HISPAN", 
                     "EDUC", "OCC", "IND", "DURUNEMP", "WHYUNEMP")

# OCC code → occ_category crosswalk, created with help from Claude
# covers both 2010 SOC (IPUMS 2011-2019) and 2018 SOC (IPUMS 2020+)
# matches occ_category definitions from BLS analysis (table-11b.R)
# creates buckets for science and tech occupations: IT, engineering, life sciences, and healthcare

occ_category_lookup <- tribble(
  ~OCC,  ~occ_category,
  
  # ---- IT: computer occupations ----
  1000L, "it",   # Computer scientists and systems analysts (2010 SOC aggregate; 2011-2019 only)
  1005L, "it",   # Computer and information research scientists
  1006L, "it",   # Computer systems analysts
  1007L, "it",   # Information security analysts
  1010L, "it",   # Computer programmers
  1020L, "it",   # Software developers, applications and systems software (2011-2019)
  1021L, "it",   # Software developers (2020+)
  1022L, "it",   # Software quality assurance analysts and testers (2020+)
  1030L, "it",   # Web developers (2011-2019)
  1031L, "it",   # Web developers (2020+)
  1032L, "it",   # Web and digital interface designers (2020+)
  1050L, "it",   # Computer support specialists
  1060L, "it",   # Database administrators (2011-2019)
  1065L, "it",   # Database administrators and architects (2020+)
  1105L, "it",   # Network and computer systems administrators
  1106L, "it",   # Computer network architects
  1107L, "it",   # Computer occupations, all other (2011-2019)
  1108L, "it",   # Computer occupations, all other (2020+)
  
  # ---- IT: data science ----
  1220L, "it",   # Operations research analysts
  1230L, "it",   # Statisticians (2011-2019 only; removed in 2020 SOC)
  
  # ---- IT: IT management ----
  0110L, "it",   # Computer and information systems managers
  
  # ---- IT: technical writers ----
  2840L, "it",   # Technical writers
  
  # ---- Engineering ----
  0300L, "engineering",  # Engineering managers (2011-2019: "Engineering managers"; 2020+: "Architectural and engineering managers")
  1300L, "engineering",  # Architects, except naval (2011-2019)
  1305L, "engineering",  # Architects, except landscape and naval (2020+)
  1306L, "engineering",  # Landscape architects (2020+)
  1310L, "engineering",  # Surveyors, cartographers, and photogrammetrists
  1320L, "engineering",  # Aerospace engineers
  1330L, "engineering",  # Agricultural engineers (2011-2019 only)
  1340L, "engineering",  # Biomedical engineers (2011-2019) / Bioengineers and biomedical engineers (2020+)
  1350L, "engineering",  # Chemical engineers
  1360L, "engineering",  # Civil engineers
  1400L, "engineering",  # Computer hardware engineers
  1410L, "engineering",  # Electrical and electronics engineers
  1420L, "engineering",  # Environmental engineers
  1430L, "engineering",  # Industrial engineers, including health and safety
  1440L, "engineering",  # Marine engineers and naval architects
  1450L, "engineering",  # Materials engineers
  1460L, "engineering",  # Mechanical engineers
  1500L, "engineering",  # Mining and geological engineers (2011-2019 only)
  1510L, "engineering",  # Nuclear engineers (2011-2019 only)
  1520L, "engineering",  # Petroleum engineers
  1530L, "engineering",  # Engineers, all other
  1540L, "engineering",  # Drafters (2011-2019)
  1541L, "engineering",  # Architectural and civil drafters (2020+)
  1545L, "engineering",  # Other drafters (2020+)
  1550L, "engineering",  # Engineering technicians, except drafters (2011-2019)
  1551L, "engineering",  # Electrical and electronic engineering technologists and technicians (2020+)
  1555L, "engineering",  # Other engineering technologists and technicians, except drafters (2020+)
  1560L, "engineering",  # Surveying and mapping technicians
  
  # ---- Life science ----
  1600L, "life_science", # Agricultural and food scientists
  1610L, "life_science", # Biological scientists
  1640L, "life_science", # Conservation scientists and foresters
  1650L, "life_science", # Medical scientists
  1660L, "life_science", # Life scientists, all other (2011-2019 only)
  1700L, "life_science", # Astronomers and physicists
  1710L, "life_science", # Atmospheric and space scientists
  1720L, "life_science", # Chemists and materials scientists
  1740L, "life_science", # Environmental scientists and geoscientists (2011-2019)
  1745L, "life_science", # Environmental scientists and specialists, including health (2020+)
  1750L, "life_science", # Geoscientists and hydrologists, except geographers (2020+)
  1760L, "life_science", # Physical scientists, all other
  0360L, "life_science", # Natural sciences managers
  1900L, "life_science", # Agricultural and food science technicians
  1910L, "life_science", # Biological technicians
  1920L, "life_science", # Chemical technicians
  1930L, "life_science", # Geological and petroleum technicians (2011-2019)
  1935L, "life_science", # Environmental science and geoscience technicians (2020+)
  1940L, "life_science", # Nuclear technicians
  1965L, "life_science", # Miscellaneous life, physical, and social science technicians (2011-2019)
  1970L, "life_science", # Other life, physical, and social science technicians (2020+)
  
  # ---- Healthcare practitioners ----
  3000L, "healthcare",   # Chiropractors
  3010L, "healthcare",   # Dentists
  3030L, "healthcare",   # Dietitians and nutritionists
  3040L, "healthcare",   # Optometrists
  3050L, "healthcare",   # Pharmacists
  3060L, "healthcare",   # Physicians and surgeons (2011-2019)
  3090L, "healthcare",   # Other physicians (2020+)
  3100L, "healthcare",   # Surgeons (2020+)
  3110L, "healthcare",   # Physician assistants
  3120L, "healthcare",   # Podiatrists
  3140L, "healthcare",   # Audiologists
  3150L, "healthcare",   # Occupational therapists
  3160L, "healthcare",   # Physical therapists
  3200L, "healthcare",   # Radiation therapists
  3210L, "healthcare",   # Recreational therapists
  3220L, "healthcare",   # Respiratory therapists
  3230L, "healthcare",   # Speech-language pathologists
  3235L, "healthcare",   # Exercise physiologists (2011-2019)
  3245L, "healthcare",   # Therapists, all other
  3250L, "healthcare",   # Veterinarians
  3255L, "healthcare",   # Registered nurses
  3256L, "healthcare",   # Nurse anesthetists
  3257L, "healthcare",   # Nurse midwives (2011-2019 only)
  3258L, "healthcare",   # Nurse practitioners
  3261L, "healthcare",   # Acupuncturists (2020+)
  3260L, "healthcare",   # Health diagnosing and treating practitioners, all other (2011-2019)
  3270L, "healthcare",   # Healthcare diagnosing or treating practitioners, all other (2020+)
  3300L, "healthcare",   # Clinical laboratory technologists and technicians
  3310L, "healthcare",   # Dental hygienists
  3320L, "healthcare",   # Diagnostic related technologists and technicians (2011-2019)
  3321L, "healthcare",   # Cardiovascular technologists and technicians (2020+)
  3322L, "healthcare",   # Diagnostic medical sonographers (2020+)
  3323L, "healthcare",   # Radiologic technologists and technicians (2020+)
  3324L, "healthcare",   # Magnetic resonance imaging technologists (2020+)
  3330L, "healthcare",   # Nuclear medicine technologists and medical dosimetrists (2020+)
  3400L, "healthcare",   # Emergency medical technicians and paramedics (2011-2019)
  3401L, "healthcare",   # Emergency medical technicians (2020+)
  3402L, "healthcare",   # Paramedics (2020+)
  3420L, "healthcare",   # Health practitioner support technologists and technicians (2011-2019)
  3421L, "healthcare",   # Pharmacy technicians (2020+)
  3422L, "healthcare",   # Psychiatric technicians (2020+)
  3423L, "healthcare",   # Surgical technologists (2020+)
  3424L, "healthcare",   # Veterinary technologists and technicians (2020+)
  3430L, "healthcare",   # Dietetic technicians and ophthalmic medical technicians (2020+)
  3500L, "healthcare",   # Licensed practical and licensed vocational nurses
  3510L, "healthcare",   # Medical records and health information technicians (2011-2019)
  3515L, "healthcare",   # Medical records specialists (2020+)
  3520L, "healthcare",   # Opticians, dispensing
  3535L, "healthcare",   # Miscellaneous health technologists and technicians (2011-2019)
  3545L, "healthcare",   # Miscellaneous health technologists and technicians (2020+)
  3540L, "healthcare",   # Other healthcare practitioners and technical occupations (2011-2019)
  3550L, "healthcare"    # Other healthcare practitioners and technical occupations (2020+)
)

## simplify to a matrix
# only including adults age 25-62 (average age of retirement for women in the US)
matrix <- data %>%
  select(all_of(variables_model)) %>%
  filter(between(AGE, 25, 62)) %>%
  filter(DURUNEMP != 999) %>%    # drop missing duration
  filter(WHYUNEMP != 0) %>%      # drop NIUs
  mutate(across(
    all_of(variables_model),
    ~ na_if(.x, 9999999)
  )) %>%
  # add science/tech occupation categories
  left_join(occ_category_lookup, by = "OCC") %>%
  # flag all other occupations as "other"
  mutate(
    occ_category = ifelse(is.na(occ_category), "other", occ_category),
    occ_category = as.factor(occ_category)
  )

ls(matrix)

# Q2: Interpret regression results ----------------------------------------

# came back and log transformed DURUNEMP to give it a normal dist
matrix <- matrix %>%
  mutate(log_DURUNEMP = log(DURUNEMP))
ls(matrix)

## define model
model <- lm(log_DURUNEMP ~ AGE + SEX + RACE + HISPAN + EDUC + occ_category, data = matrix)
summary(model)


##  normality of errors
hist(resid(model))

# homoscedasticity of errors
plot(fitted(model), resid(model))

# absence of outliers
plot(hatvalues(model))
plot(cooks.distance(model))

# and check variance inflation factor
car::vif(model)


# Q3: stepwise modeling ---------------------------------------------------

## a forward selection adds predictor variables one at a time 
## this version evaluates each iteration based on how the new variables change R2
forward <- ols_step_forward_r2(model) ## generate and save the modeling
forward  ## print the report
plot(forward)  ## visualize the change in R2 from one step to the next

## a backward selection starts with all variables, then removes them one by one
backward <- ols_step_backward_r2(model)
backward
plot(backward)

## a both selection (stepwise) adds and removes in all possible combinations
both <- ols_step_both_r2(model)
both
plot(both)

## or this function will run the possible indicators, in addition to R2
ols_step_all_possible(model)



# Save your data. We'll use it later in the semester for the Bayesian lab. 
write.csv(x = matrix, file = "lab_05_data.csv")

