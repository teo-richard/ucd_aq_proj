library(tidyverse)
library(broom)
library(effsize)
library(texreg)
library(sandwich)
library(lmtest)

# What is the effect of *treatment* on willingness to pay? (WTP for PAQI and EPD)

# Note: using categorical education


# Reading in data: Large
df_epd_clean = read_csv("/Users/teorichard/Downloads/UCD Research/AQ UCD/cleaned_data/LARGE_df_epd_clean.csv") %>% select(-hhid)
df_paqi_clean = read_csv("/Users/teorichard/Downloads/UCD Research/AQ UCD/cleaned_data/LARGE_df_paqi_clean.csv") %>% select(-hhid)
# df_full_clean_contedu = read_csv("/Users/teorichard/Downloads/UCD Research/AQ UCD/cleaned_data/LARGE_df_full_clean.csv") %>% select(-hhid) # this one has continuous education
df_full_clean_catedu = read_csv("/Users/teorichard/Downloads/UCD Research/AQ UCD/cleaned_data/LARGE_df_full_clean_catedu.csv") %>% select(-hhid)


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

treatment_only_mod_robust = coeftest(treatment_only_mod, vcov = vcovHC(treatment_only_mod, type = "HC3"))



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
dif_no_treat_mod_robust = coeftest(dif_no_treat_mod, vcov = vcovHC(dif_no_treat_mod, type = "HC3"))
summary(dif_no_treat_mod)

# R-squared = 0.0709, F-statistic p-value 0.3805

# ///// PREDICT RELATIVE WTP, ALL VARIABLES \\\\\
full_mod = lm(wtp_dif ~ ., data = df_full_clean_catedu_wtp_dif)
full_mod_robust = coeftest(full_mod, vcov = vcovHC(full_mod, type = "HC3"))
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


# --------------- SPLIT DATASET INTO RANDOM HALVES ---------------------------------------------
# to see if we get same results using less observations

set.seed(123)
idx1 = sample(1:nrow(df_full_clean_catedu_wtp_dif), size = 0.5*nrow(df_full_clean_catedu_wtp_dif), replace = FALSE)
first_half_dat = df_full_clean_catedu_wtp_dif[idx1, ]
second_half_dat = df_full_clean_catedu_wtp_dif[-idx1, ]

# ///// PREDICT RELATIVE WTP, TREATMENT ONLY \\\\\
treatment_only_mod_1st_half = lm(wtp_dif ~ epd_treatment_baseline,
            data = first_half_dat)
treatment_only_mod_1st_half_robust = coeftest(treatment_only_mod_1st_half, vcov = vcovHC(treatment_only_mod_1st_half, type = "HC3"))
summary(treatment_only_mod_1st_half)
# R-squared = 0.4355, F-statistic pvalue = 0
# Coef = -31.264

treatment_only_mod_2nd_half = lm(wtp_dif ~ epd_treatment_baseline,
            data = second_half_dat)
treatment_only_mod_2nd_half_robust = coeftest(treatment_only_mod_2nd_half, vcov = vcovHC(treatment_only_mod_2nd_half, type = "HC3"))
summary(treatment_only_mod_2nd_half)
# R-squared = 0.4144, F-statistic pvalue = 0
# Coef = -32.9039

# Average R-squared = 0.425


# ///// PREDICT RELATIVE WTP, NO TREATMENT ALL OTHER VARIABLES \\\\\
dif_no_treat_1st_half = lm(wtp_dif ~ ., data = first_half_dat %>% select(-epd_treatment_baseline))
dif_no_treat_1st_half_robust = coeftest(dif_no_treat_1st_half, vcov = vcovHC(dif_no_treat_1st_half, type = "HC3"))

summary(dif_no_treat_1st_half)
# R-squared = 0.1232, F-statistic p-value 0.7053

dif_no_treat_2nd_half = lm(wtp_dif ~ ., data = second_half_dat %>% select(-epd_treatment_baseline))
dif_no_treat_2nd_half_robust = coeftest(dif_no_treat_2nd_half, vcov = vcovHC(dif_no_treat_2nd_half, type = "HC3"))

summary(dif_no_treat_2nd_half)
# R-squared = 0.1424, F-statistic p-value 0.3675

# Average R-squared: 0.133


# ///// PREDICT RELATIVE WTP, ALL VARIABLES \\\\\
full_mod_1st_half = lm(wtp_dif ~ ., data = first_half_dat)
full_mod_1st_half_robust = coeftest(full_mod_1st_half, vcov = vcovHC(full_mod_1st_half, type = "HC3"))
summary(full_mod_1st_half)
# epd_treatment_baseline is still highly significant (p = 0)
# R-squared = 0.5139
# F-statistic P-value = 0
# Coef = -31.176

full_mod_2nd_half = lm(wtp_dif ~ ., data = second_half_dat)
full_mod_2nd_half_robust = coeftest(full_mod_2nd_half, vcov = vcovHC(full_mod_2nd_half, type = "HC3"))
summary(full_mod_2nd_half)
# R-squared = 0.4841
# F-statistic P-value = 0
# Coef = -33.7

# Average R-squared = 0.499

# So adding other variables to the model with treatment only increases avg R-squared from 0.425 to 0.499 = 0.074 increase
# So not trivial but not a lot either... and epd_treatment_baseline is highly significant (p is about 0) in both models


# Overall despite using half of the data to mimic the smaller sample size when splitting by treatment group, the R-squared is about the same ish 
#   (still in the 0.4 to 0.52 range). And the coefs are all similar (about -30 to -34 range)

# The R-squared increase from adding all other variables to a model with treatment only is slightly higher, but fairly close (about 0.074 increase vs. 0.036)


# -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
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



