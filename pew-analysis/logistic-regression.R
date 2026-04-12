
# Lab 6: generalized linear models ----------------------------------------

# set working directory
if (file.exists("../.Renviron")) readRenviron("../.Renviron")
root <- Sys.getenv("PEW_DATA_ROOT")
if (root == "") stop("Set PEW_DATA_ROOT in repo-root .Renviron (see .Renviron.example).", call. = FALSE)
setwd(normalizePath(root, winslash = "/", mustWork = TRUE))

# install libraries
library(devtools)
install_github(repo = "https://github.com/pewresearch/pewmethods")

library(haven)      # for reading SPSS data
library(pewmethods) # for pew data
library(dplyr)      # for data management
library(survey)     # for weighting survey data
library(broom)      # for incident rate ratios
library(ggplot2)    # for data visualization
library(tidyverse)  # for tidying


# need to create an account at pew research and download the data
# i'm using the American Trends Panel Wave 157 2024 Survey of Workers
# https://www.pewresearch.org/dataset/american-trends-panel-wave-157/

## load data
data_raw <- read_spss("W157_Oct24/ATP W157.sav")

## define variables of interest
variables <- c(
  "F_EDUCCAT2",         # education level
  "F_AGECAT",           # age category
  "F_GENDER",           # gender
  "F_RACETHNMOD",       # race/ethnicity
  "INDUSTRYCOMBO_W157", # industry (9 = info/tech)
  "FINDJOB_W157",       # perceived job search difficulty (1-5)
  "WEIGHT_W157"         # survey weight
)

## common codes used to designate null values
NA_codes <- c("99", "999", "9999", "900000")

## filter data 
data <- data_raw %>%
  select(all_of(variables)) %>%
  mutate(across(
    all_of(variables),
    ~if_else(as.character(.x) %in% NA_codes, NA, .x)
  ))

## and clean up to include variable labels
### define labels
variable_labels <- sapply(X = data_raw, 
                          FUN = function(x) attr(x, "label"))

### function to label data frames
apply_labels <- function(data, labels) {
  for (column in intersect(names(data), names(labels))) {
    attr(data[[column]], "label") <- labels[[column]]
  }
  data
}

### apply the labeling function
data <- apply_labels(data, variable_labels)


## test just to see
ls(data)

### weighted frequency tables

# age categories
get_totals(var = "F_AGECAT", df = data, wt = "WEIGHT_W157")
# 23% age 18-29
# 43% age 30-49
# 26% age 50-64
# 7% age 65+

# educational attainment
get_totals(var = "F_EDUCCAT2", df = data, wt = "WEIGHT_W157")
# 20% some college, no degree
# 24% college graduate/some post grad
# 17% post-graduate

# employment situation
get_totals(var = "EMPLSIT_W157", df = data_raw, wt = "WEIGHT_W157")
# turns out only employed (ft/pt) respondents are included in this dataset

# gender
get_totals(var = "F_GENDER", df = data, wt = "WEIGHT_W157")


# Q1: define and justify model --------------------------------------------

# I'll test potential predictors of perceived job search difficulty (FINDJOB_W157)
# using logistic regression.

# pew survey question: if you were to look for a new job today, how easy/difficult do you think
# it would be for you to get the kind of job you want?


# Q2: Logistic regression ----------------------------------------

## transform FINDJOB_W157 into a binary variable
# group 1-3 and 4-5
# 1 = difficult, 0 = easy
data$FINDJOB_binary <- ifelse(data$FINDJOB_W157 >= 4, 1, 0)
data <- data %>%
  mutate(FINDJOB_binary = haven ::as_factor(FINDJOB_binary))
table(data$FINDJOB_binary)

# transform INDUSTRYCOMBO_W157 into a binary
# info/tech (9) vs everything else
# 1 = info/tech, 0 = everything else
data <- data %>%
  mutate(IT_binary = if_else(INDUSTRYCOMBO_W157 == 9, 1, 0))
table(data$IT_binary)

# convert gender from continuous to factors
data <- data %>%
  mutate(F_GENDER = as.factor(F_GENDER) %>%
           fct_recode(
             "Man"   = "1",
             "Woman" = "2",
             "Other" = "3"
           ))

# making sure it worked
get_totals(var = "F_GENDER", df = data, wt = "WEIGHT_W157")

## and identify predictor variables by exclusion, and add IT_binary
predictors <- c(setdiff(variables, c("FINDJOB_W157", "WEIGHT_W157", "INDUSTRYCOMBO_W157")), "IT_binary")
predictors

## define a formula
logistic_formula <- as.formula(paste("as.factor(FINDJOB_binary) ~", paste(predictors, collapse = " + ")))
logistic_formula

## fit a basic model with raw data
logistic_model <- glm(formula = logistic_formula, data = data, family = "binomial")
summary(logistic_model)

## and compare with weighted survey data
design <- svydesign(ids = ~1, weights = ~WEIGHT_W157, data = data)
logistic_model_weighted <- svyglm(
  formula = logistic_formula,
  design = design,
  family = quasibinomial())
summary(logistic_model_weighted)

## convert coefficients and confidence intervals to odds-ratio terms
exp(coefficients(logistic_model))
exp(confint.default(logistic_model))
exp(coefficients(logistic_model_weighted))
exp(confint.default(logistic_model_weighted))


## Interpret analysis of deviance table
anova(logistic_model, "Chisquare")



