library(tidyverse)
library(broom)
library(effsize)
library(texreg)
library(sandwich)
library(lmtest)
library(glue)

# What is the effect of *treatment* on willingness to pay? (WTP for PAQI and EPD)

# Note: using categorical education

id = read_csv("original_files/csv files/Analysis_data_saved.csv") %>% select(hhid, grid_id_baseline)


# Reading in data: Large
# df_epd_clean = read_csv("/Users/teorichard/Downloads/UCD Research/AQ UCD/cleaned_data/LARGE_df_epd_clean.csv") %>% select(-hhid)
# df_paqi_clean = read_csv("/Users/teorichard/Downloads/UCD Research/AQ UCD/cleaned_data/LARGE_df_paqi_clean.csv") %>% select(-hhid)
# df_full_clean_contedu = read_csv("/Users/teorichard/Downloads/UCD Research/AQ UCD/cleaned_data/LARGE_df_full_clean.csv") %>% select(-hhid) # this one has continuous education
df_full_clean_catedu = read_csv("/Users/teorichard/Downloads/UCD Research/AQ UCD/cleaned_data/LARGE_df_full_clean_catedu.csv")
df_full_clean_catedu = left_join(df_full_clean_catedu, id, by = "hhid") %>% select(-hhid)

# Dropping outcome variables we are not looking at
drop_vars_wtp_paqi = c("pref_baseline", "pref_endline", "wtp_epd", "wtp_dif")
drop_vars_wtp_epd = c("pref_baseline", "pref_endline", "wtp_paqi", "wtp_dif")


df_full_clean_wtp_paqi = df_full_clean_catedu %>% dplyr::select(-all_of(drop_vars_wtp_paqi))
df_full_clean_wtp_epd = df_full_clean_catedu  %>% dplyr::select(-all_of(drop_vars_wtp_epd))
df_full_clean_catedu_wtp_dif = df_full_clean_catedu %>% 
                            dplyr::select(-all_of(c("pref_baseline", "pref_endline", "wtp_paqi", "wtp_epd")))
# ***IMPORTANT*** Observation 709 (hhid = 2110) is a high leverage observation in df_full_clean_catedu_wtp_dif, so I am going to remove it in the original data
# df_full_clean_contedu_wtp_dif = df_full_clean_contedu %>% 
#                            dplyr::select(-all_of(c("pref_baseline", "pref_endline", "wtp_paqi", "wtp_epd")))       

#######
# Overview:
# Look at 3 models to predict relative WTP:
# 1. Treatment only 
# 2. All covariates but NO treatment 
# 3. Treatment AND all covariates
# Then we will run all three models again using sample splitting (randomly get half the data and see if coefs and R^2 are similar)
 #######


# ::::::::::::::::::::::::: PREDICTING *RELATIVE* WTP :::::::::::::::::::::::::

# --------------- FULL DATASET (as opposed to the randomly split halves of the data) ---------------------------------------------

# ///// PREDICT RELATIVE WTP, *TREATMENT ONLY* \\\\\
treatment_only_mod = lm(wtp_dif ~ epd_treatment_baseline,
            data = df_full_clean_catedu_wtp_dif)

treatment_only_mod_robust = coeftest(treatment_only_mod, vcov = vcovCL(treatment_only_mod, cluster = ~grid_id_baseline))

summary(treatment_only_mod)
# R-squared = 0.424, pvalue = 0
# Coef = -32.075

texreg(
  treatment_only_mod_robust,
  format = "latex",
  custom.coef.names = c("Intercept", "EPD Treatment"),
  caption = "Model Name",
  label = "tab:ols",
  omit.coef = "^(?!epd_treatment_baseline$)",
  float.pos = "H",
    caption.above = TRUE
)

# ///// PREDICT RELATIVE WTP, NO TREATMENT ALL OTHER VARIABLES \\\\\



dif_no_treat_mod = lm(wtp_dif ~ ., data = df_full_clean_catedu_wtp_dif %>% select(-epd_treatment_baseline))
dif_no_treat_mod_robust = coeftest(dif_no_treat_mod, vcov = vcovCL(dif_no_treat_mod, cluster = ~grid_id_baseline))
summary(dif_no_treat_mod)

# R-squared = 0.0709, F-statistic p-value 0.3805

# ///// PREDICT RELATIVE WTP, ALL VARIABLES \\\\\
full_mod = lm(wtp_dif ~ ., data = df_full_clean_catedu_wtp_dif)
full_mod_robust = coeftest(full_mod, vcov = vcovCL(full_mod, cluster = ~grid_id_baseline))
summary(full_mod)
# epd_treatment_baseline is still highly significant (p = 0)
# R-squared = 0.460 (so adding all other variables to a model with treatment only increases R-squared by .036)
# F-statistic P-value = 0
# Coef = -32.003



texreg(
  full_mod,
  format = "latex",
  custom.coef.names = c("EPD Treatment"),
  caption = "Model Name",
  label = "tab:ols",
  omit.coef = "^(?!epd_treatment_baseline$)",
  float.pos = "H",
caption.above = TRUE
)




# -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# --------------- More random splits ---------------------------------------------
# Get bootstrap samples, run regression with all variables, get the min coefficient, max coefficient, sd, and average

set.seed(123)
coefs = c()
r_sq_vals = c()
pvals = c()
for (i in 1:10000) {
  boot = df_full_clean_catedu_wtp_dif[sample(nrow(df_full_clean_catedu_wtp_dif), size = nrow(df_full_clean_catedu_wtp_dif), replace = TRUE), ] # size = 929
  mod = lm(wtp_dif ~ ., data = boot)
  mod_robust = tryCatch(
    coeftest(mod, vcov = vcovCL(mod, cluster = ~grid_id_baseline)),
    error = function(e) NULL
  )
  if (is.null(mod_robust)) next
  tidy_robust = tidy(mod_robust)
  row = tidy_robust[tidy_robust$term == "epd_treatment_baseline", ]
  if (nrow(row) == 0 || is.nan(row$p.value)) next
  coefs = c(coefs, row$estimate)
  r_sq_vals = c(r_sq_vals, summary(mod)$r.squared)
  pvals = c(pvals, row$p.value)
}


# The below is not updated with the cluster SEs
print("\nCoefficients:")
print(glue("Min: {min(coefs)}, Mean: {mean(coefs)}, Max: {max(coefs)}, SD: {sd(coefs)}"))
# Min: -37.6862643093335, Mean: -32.2478280018987, Max: -26.9929110608757, SD: 1.61589066251891
print("\nR-Squared")
print(glue("Min: {min(r_sq_vals)}, Mean: {mean(r_sq_vals)}, Max: {max(r_sq_vals)}"))
# Min: 0.437636689988627, Mean: 0.495745645110446, Max: 0.562690955093505
print("\nP-values")
print(glue("Min: {min(pvals)}, Mean: {mean(pvals)}, Max: {max(pvals)}"))
# Min: 2.49878788334361e-113, Mean: 1.01647757974899e-80, Max: 1.01552702668007e-77
# -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


# ::::::::::::::::::::::::: GENERAL PLOTTING :::::::::::::::::::::::::

# ---- PLOTTING WTP BY TREATMENT -----

# Plotting the data for visuals
df = df_full_clean_catedu %>% mutate(wtp_dif = wtp_dif) %>% select(wtp_paqi, wtp_epd, wtp_dif, epd_treatment_baseline)
df_long = df %>% 
    pivot_longer(cols = c("wtp_paqi", "wtp_epd", "wtp_dif"), names_to = "pay_to", values_to = "pay") %>% 
    mutate(treatment = ifelse(epd_treatment_baseline == 1, "EPD", "PAQI")) %>% 
    select(-epd_treatment_baseline)

# this plot is still fine tbh
plot = ggplot(df_long, aes(x = factor(treatment), y = pay, fill = treatment)) +
    geom_violin(position = position_dodge(width = 1.7), alpha = 0.6) +
    geom_jitter(aes(color = treatment), width = 0.03, height = 0, alpha = 0.5) +
    scale_color_manual(values = c("#044778", "#580303")) +
    facet_wrap(~pay_to, labeller = labeller(pay_to = c(wtp_dif = "WTP DIF", wtp_epd = "WTP EPD", wtp_paqi = "WTP PAQI"))) +
    scale_fill_manual(values = c("#31aefc", "red")) +
    labs(title = "WTP for EPD and PAQI forecasting for each treatment group",
    x = "Treatment Group",
    y = "Willingness to Pay") +
    theme(axis.title.y = element_text(margin = margin(r = 20)),
        axis.title.x = element_text(margin = margin(t = 20)),
        plot.title = element_text(margin = margin(b = 20)))

ggsave("linear_regression_treatment_effect/wtp_by_treatment.png", plot, width = 10, height = 10)
# Plot is with categorical education



