library(tidyverse)
library(broom)

# What is the effect of *treatment* on endline preference?

# Reading in data: Large
df_epd_clean = read_csv("/Users/teorichard/Downloads/UCD Research/AQ UCD/cleaned_data/LARGE_df_epd_clean.csv")
df_paqi_clean = read_csv("/Users/teorichard/Downloads/UCD Research/AQ UCD/cleaned_data/LARGE_df_paqi_clean.csv")
df_full_clean = read_csv("/Users/teorichard/Downloads/UCD Research/AQ UCD/cleaned_data/LARGE_df_full_clean.csv")


# Treatment effect on Preference -------------------------------------------------

# Getting rid of outcome variables we aren't looking at
drop_vars_pref_endline = c("pref_baseline", "wtp_paqi", "wtp_epd")
df_full_clean_pref_endline = df_full_clean %>% dplyr::select(-all_of(drop_vars_pref_endline))

# Checking correlation just to make sure nothing's weird
cor(df_full_clean$pref_baseline, df_full_clean$epd_treatment_baseline, method = "pearson")

# Endline preference
mod = glm(pref_endline ~ epd_treatment_baseline, 
           data = df_full_clean_pref_endline, 
           family = binomial)
summary(mod)

logodds = predict(mod, type = "link", se.fit = TRUE)

# Converting to Probability
prob = exp(logodds$fit) / (1 + exp(logodds$fit))
prob_vals = c(min(prob), max(prob))

prob_lower = exp(logodds$fit - 1.96*logodds$se.fit) / (1 + exp(logodds$fit - 1.96*logodds$se.fit))
prob_lower_bounds = c(min(prob_lower), max(prob_lower))

prob_upper = exp(logodds$fit + 1.96*logodds$se.fit) / (1 + exp(logodds$fit + 1.96*logodds$se.fit))
prob_upper_bounds = c(min(prob_upper), max(prob_upper))

# Log Odds Plot
log_odds_tibble = tibble(prob = prob_vals, prob_lower = prob_lower_bounds, prob_upper = prob_upper_bounds, treatment = c("PAQI", "EPD"))

log_odds_plot = ggplot(log_odds_tibble, aes(x = treatment, y = prob_vals)) +
  geom_point(size=3) +
  geom_errorbar(aes(ymin=prob_lower_bounds, ymax=prob_upper_bounds), width=0.2) +
  labs(y="Probability of Preferring EPD Forecasting (CIs on Probability Scale)", x="Treatment")


ggsave("linear_regression_treatment_effect/log_odds_plot.png", log_odds_plot, width = 5, height = 5)



