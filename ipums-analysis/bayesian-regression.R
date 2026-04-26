
# Lab 7: bayesian methods -------------------------------------------------

library(dplyr)      # for data management
library(ggplot2)    # for data visualization
library(rstanarm)   # for Bayesian regression
library(BayesFactor) # for Bayesian factors
library(olsrr)      # for stepwise modeling

# load data used in linear regression lab
setwd("/Users/jocelyn/Documents/Pratt/640-data-analysis")
data <- read.csv("lab_05_data.csv")

# mutate occupation category into a factor
data <- data %>%
  mutate(occ_category = factor(occ_category),
         SEX = factor(SEX),
         RACE = factor(RACE),
         HISPAN = factor(HISPAN),
         EDUC = factor(EDUC))

# plot some just to check
ggplot(data = data, aes(x = DURUNEMP)) +
  geom_histogram(binwidth = 5, na.rm = TRUE) +
  labs(title = "Duration of unemployment",
       subtitle = "PUMS 2019-2025 Data",
       x = "duration in weeks",
       y = "individuals") + 
  theme_minimal()




# Q1: correlation with Bayes factors --------------------------------------

## AGE
cor.test(x = data$DURUNEMP, y = data$AGE)
# Bayes factor
correlationBF(x = data$DURUNEMP, y = data$AGE)

## SEX
t.test(data$DURUNEMP ~ data$SEX)
# Bayes factor
ttestBF(formula = DURUNEMP ~ SEX, data = data)

## categorical variables
summary(aov(DURUNEMP ~ RACE, data = data))
summary(aov(DURUNEMP ~ HISPAN, data = data))
summary(aov(DURUNEMP ~ EDUC, data = data))
summary(aov(DURUNEMP ~ occ_category, data = data))
# Bayes factor
anovaBF(DURUNEMP ~ RACE, data = data)
anovaBF(DURUNEMP ~ HISPAN, data = data)
anovaBF(DURUNEMP ~ EDUC, data = data)
anovaBF(DURUNEMP ~ occ_category, data = data)




# Q2: bayesian linear regression ------------------------------------------

## conventional model for comparison
ols_model <- lm(DURUNEMP ~ AGE + SEX + RACE + HISPAN + EDUC + occ_category, data = data)
summary(ols_model)


## define bayseian model
bayes_model <- stan_glm(DURUNEMP ~ AGE + SEX + RACE + HISPAN + EDUC + occ_category, # formula
                        data = data,  # data frame
                        chains = 3,   # how many Markov chains? more = better diagnostics
                        iter = 2000,  # how many samples per chain?
                        seed = 123)   # sets the random seed for reproducibility
bayes_model
summary(bayes_model)

mean(data$DURUNEMP)

# plot coefficient estimates
parameters <- names(coef(bayes_model))
plot(bayes_model, 
     plotfun = "areas",  
     pars = parameters,  
     include = TRUE, 
     prob = 0.95,  border = "black") + 
  theme_minimal(base_size = 15) + 
  labs(title = "Posterior Distributions with 95% Credible Interval",
       x = "Parameter Value",
       y = "Density")



# Q3: baysian model selection ---------------------------------------------

## forward selection
bic_forward <- ols_step_forward_sbic(ols_model)
bic_forward
plot(bic_forward)

## both directions
bic_both <- ols_step_both_sbic(ols_model)
bic_both
plot(bic_both)
