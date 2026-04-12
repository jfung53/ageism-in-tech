#
# ------ exploratory data analysis #1 -----------------------------------------
#
# research question: 
#
# do age and sex predict employment status in tech jobs after involuntary job loss?
# in particular, were older women disproportionately affected by the mass tech layoffs in 2022-2023?
#
# ------ initial analysis of bureau of labor statistics data

if (file.exists("../.Renviron")) readRenviron("../.Renviron")
root <- Sys.getenv("PROJECT_ROOT")
if (root == "") stop("Set PROJECT_ROOT in repo-root .Renviron (see .Renviron.example).", call. = FALSE)
setwd(file.path(root, "bls-analysis"))

library(dplyr)
library(tidyr)
library(ggplot2)


# -----------------------------------------------------------------------------
# ------ table 27 - reason for unemployment
# -----------------------------------------------------------------------------

# bring in csv
table27 <- read.csv(file.path(root, "bls-cleaned/table27_all_years.csv"))
glimpse(table27)

# filter variables for permanent job loss (probably the closest thing to layoffs)
permanent <- table27 |>
  filter(
    reason == "Permanent job losers",
    demographic %in% c("Men 20+", "Women 20+")
  )

# look at it
permanent

# ------ calculate percentage (of unemployed due to permanent job loss)
# original BLS file only included percentage of its aggregated parent category

# isolate count measure type
table27_counts <- table27 |>
  filter(
    measure_type == "count",
    demographic %in% c("Men 20+", "Women 20+")
  )

# count of permanent job losers
perm_counts <- table27_counts |>
  filter(reason == "Permanent job losers") |>
  select(year, demographic, perm_losers = value)

# count of total unemployed
total_unemployed <- table27_counts |>
  filter(reason == "Total unemployed") |>
  select(year, demographic, total_unemployed = value)

# join and calculate percentage
perm_pct <- perm_counts |>
  left_join(total_unemployed, by = c("year", "demographic")) |>
  mutate(pct_of_total = round(perm_losers / total_unemployed * 100, 1))



# ------ plot permanent job losers as percentage of unemployed ------

ggplot(perm_pct, aes(x = factor(year), y = pct_of_total, fill = demographic)) +
  geom_col(position = "dodge") +
  scale_fill_manual(
    values = c("Women 20+" = "lightslateblue", "Men 20+" = "mediumspringgreen"),
    # no idea why label vector needed to be reversed
    labels = c("Men", "Women"),
    name   = NULL
  ) +
  ylim(0, 100) +
  labs(
    title   = "Proportion of unemployed due to permanent job loss",
    x       = NULL,
    y       = "% of total unemployed",
    caption = "Source: Bureau of Labor Statistics CPS Table 27. Civilian labor force age 20+."
  ) +
  theme_bw() +
  theme(legend.position = "top", legend.justification = "left")



# ------ plot absolute numbers of permanent job losers ------

ggplot(perm_pct, aes(x = factor(year), y = perm_losers, fill = demographic)) +
  geom_col(position = "dodge") +
  scale_fill_manual(
    values = c("Women 20+" = "lightslateblue", "Men 20+" = "mediumspringgreen"),
    # no idea why label vector needed to be reversed
    labels = c("Men", "Women"),
    name   = NULL
  ) +
  ylim(0, 5000) +
  labs(
    title   = "Number of unemployed people due to permanent job loss",
    x       = NULL,
    y       = "# of unemployed (thousands)",
    caption = "Source: Bureau of Labor Statistics CPS Table 27. Civilian labor force age 20+."
  ) +
  theme_bw() +
  theme(legend.position = "top", legend.justification = "left")




# -----------------------------------------------------------------------------
# ------ table 9 - employed people by occupation (not detailed occupation)
# -----------------------------------------------------------------------------

# bring in csv
table9 <- read.csv(file.path(root, "bls-cleaned/table09_all_years.csv"))
glimpse(table9)

# filter to computer and mathematical occupations only (occ_level == 2)
computer_jobs <- table9 |>
  filter(occ_level == 2,
         occ_subgroup == "professional_related",
         occupation == "Computer and mathematical occupations"
         ) |>
  select(year, occupation, women_20plus, men_20plus)

# pivot to long to plot it
computer_jobs_long <- computer_jobs |>
  pivot_longer(
    cols = c(men_20plus, women_20plus),
    names_to = "demographic",
    values_to = "employed"
  )

# ------ line graph of employed tech numbers ------

ggplot(computer_jobs_long, aes(x = year, y = employed, color = demographic, group = demographic)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_x_continuous(breaks = 2019:2025) +
  scale_color_manual(
    values = c("women_20plus" = "lightslateblue", "men_20plus" = "mediumspringgreen"),
    labels = c("Men", "Women"),
    name   = NULL
  ) +
  labs(
    title   = "Employed people in tech jobs",
    x       = NULL,
    y       = "# of people (thousands)",
    caption = "Source: Bureau of Labor Statistics CPS Table 9. Civilian labor force age 20+."
  ) +
  theme_bw() +
  theme(legend.position = "top", legend.justification = "left")


# -----------------------------------------------------------------------------
# ------ table 31 - duration of unemployment
# -----------------------------------------------------------------------------

table31 <- read.csv(file.path(root, "bls-cleaned/table31_all_years.csv"))
glimpse(table31)

# ------ plot durations of unemployment ------

duration_intervals <- c(
  "Less than 5 weeks"  = "less_5_weeks",
  "5 to 14 weeks"      = "X5_to_14_weeks",
  "15 to 26 weeks"     = "X15_to_26_weeks",
  "27 weeks or more"   = "X27_plus_weeks"
)

for (label in names(duration_intervals)) {
  col <- duration_intervals[[label]]
  
  unemployed <- table31 |>
    filter(
      section   == "age_sex",
      sex       %in% c("Men", "Women"),
      age_group %in% c("25-34", "35-44", "45-54")
    ) |>
    select(year, sex, age_group, total_unemployed,
           less_5_weeks, X5_to_14_weeks, X15_to_26_weeks, X27_plus_weeks) |>
    mutate(pct_unemployed = round(.data[[col]] / total_unemployed * 100, 1))
  
  p <- ggplot(unemployed, aes(x = year, y = pct_unemployed, color = age_group, group = age_group)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2.5) +
    scale_color_manual(
      values = c("25-34" = "darkgoldenrod1", "35-44" = "chartreuse4", "45-54" = "orangered"),
      name   = NULL
    ) +
    scale_x_continuous(breaks = 2019:2025) +
    ylim(0, 100) +
    facet_wrap(~ sex) +
    labs(title   = paste("Unemployed for", label, "by age"),
         x = NULL, y = "% of all unemployed",
         caption = "Source: Bureau of Labor Statistics CPS Table 31") +
    theme_bw() +
    theme(legend.position = "top", legend.justification = "left")
  
  print(p)
}

# ------ correlation test

# correlate year with median unemployment duration for each sex x age group
unemployed_cor <- unemployed |>
  group_by(sex, age_group) |>
  summarise(
    r       = round(cor(year, median_duration, method = "pearson"), 3),
    p_value = round(cor.test(year, median_duration)$p.value, 3),
    .groups = "drop"
  )

unemployed_cor

# ------ plot median unemployment duration by sex and age group ------

unemployed <- table31 |>
  filter(
    section   == "age_sex",
    sex       %in% c("Men", "Women"),
    age_group %in% c("25-34", "35-44", "45-54")
  ) |>
  select(year, sex, age_group, total_unemployed, median_duration, avg_duration)

ggplot(unemployed, aes(x = year, y = median_duration, color = age_group, group = age_group)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(
    values = c("25-34" = "darkgoldenrod1", "35-44" = "chartreuse4", "45-54" = "orangered"),
    name   = NULL
  ) +
  scale_x_continuous(breaks = 2019:2025) +
  ylim(0, 52) +
  facet_wrap(~ sex) +
  labs(
    title   = "Median duration of unemployment by age",
    x       = NULL,
    y       = "# of weeks",
    caption = "Source: Bureau of Labor Statistics CPS Table 31"
  ) +
  theme_bw() +
  theme(legend.position = "top", legend.justification = "left")



# ------ women's share of employment in tech computer occupations

computer_jobs <- computer_jobs |>
  mutate(womens_share = round(women_20plus / (women_20plus + men_20plus) * 100, 1))

cor.test(computer_jobs$year, computer_jobs$womens_share, method = "pearson")

ggplot(computer_jobs, aes(x = year, y = womens_share)) +
  geom_line(linewidth = 1, color = "lightslateblue") +
  geom_point(size = 2.5, color = "lightslateblue") +
  scale_x_continuous(breaks = 2019:2025) +
  ylim(0, 100) +
  labs(
    title   = "Women's share of employment in computer occupations",
    x       = NULL,
    y       = "% of total employed",
    caption = "Source: BLS CPS Table 9. Civilian labor force age 20+."
  ) +
  theme_bw()


# ------ t-test

unemployed_ttest <- table31 |>
  mutate(pct_27_plus = round(X27_plus_weeks / total_unemployed * 100, 1)) |>
  group_by(sex, age_group) |>
  summarize(
    mean_pre    = round(mean(pct_27_plus[year <= 2021], na.rm = TRUE), 1),
    mean_post   = round(mean(pct_27_plus[year >= 2022], na.rm = TRUE), 1),
    t_statistic = round(t.test(pct_27_plus[year <= 2021],
                               pct_27_plus[year >= 2022])$statistic, 3),
    p_value     = round(t.test(pct_27_plus[year <= 2021],
                               pct_27_plus[year >= 2022])$p.value, 3),
    .groups     = "drop"
  )

unemployed_ttest

# print all the rows to copy for the report
print(unemployed_ttest, n = 30)
