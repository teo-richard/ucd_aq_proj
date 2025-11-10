library(tidyverse)
library(broom)
library(effsize)
library(texreg)

# NOTE!!!! DF_FULL_CLEAN DOES **NOT** HAVE EDUCATION IN YEARS, JUST EDUCATION AS A CATEGORICAL VARIABLE

# What is the effect of *treatment* on willingness to pay? (WTP for PAQI and EPD)


# Reading in data: Large
df_epd_clean = read_csv("/Users/teorichard/Downloads/UCD Research/AQ UCD/cleaned_data/LARGE_df_epd_clean.csv") %>% select(-hhid)
df_paqi_clean = read_csv("/Users/teorichard/Downloads/UCD Research/AQ UCD/cleaned_data/LARGE_df_paqi_clean.csv") %>% select(-hhid)
df_full_clean_contedu = read_csv("/Users/teorichard/Downloads/UCD Research/AQ UCD/cleaned_data/LARGE_df_full_clean.csv") %>% select(-hhid) # this one has continuous education
df_full_clean = read_csv("/Users/teorichard/Downloads/UCD Research/AQ UCD/cleaned_data/LARGE_df_full_clean_catedu.csv") %>% select(-hhid)





# Dropping outcome variables we are not looking at
drop_vars_wtp_paqi = c("pref_baseline", "pref_endline", "wtp_epd", "wtp_dif")
drop_vars_wtp_epd = c("pref_baseline", "pref_endline", "wtp_paqi", "wtp_dif")


df_full_clean_wtp_paqi = df_full_clean  %>% dplyr::select(-all_of(drop_vars_wtp_paqi))
df_full_clean_wtp_epd = df_full_clean  %>% dplyr::select(-all_of(drop_vars_wtp_epd))
df_full_clean_wtp_dif = df_full_clean %>% 
                            dplyr::select(-all_of(c("pref_baseline", "pref_endline", "wtp_paqi", "wtp_epd")))

# ::::::::::::::::::::::::: PREDICTING ABSOLUTE WTP :::::::::::::::::::::::::

# ----- PREDICTING WTP PAQI, TREATMENT ONLY-----
paqi_mod = lm(wtp_paqi ~ epd_treatment_baseline, 
           data = df_full_clean_wtp_paqi)
s_paqi_mod = summary(paqi_mod)
paqi_coefs = s_paqi_mod$coefficients
paqi_results = tibble(estimate = paqi_coefs[2, 1], se = paqi_coefs[2, 2], pval = paqi_coefs[2, 4])

# cohen's d 
cohen_d = cohen.d(wtp_paqi ~ epd_treatment_baseline, data = df_full_clean_wtp_paqi)

# ----- PREDICTING WTP EPD, TREATMENT ONLY -----
# this was glm() idk why? Shouldn't it be lm()?
epd_mod = glm(wtp_epd ~ epd_treatment_baseline, 
           data = df_full_clean_wtp_epd)
summary(epd_mod)
s_epd_mod = summary(epd_mod)
epd_coefs = s_epd_mod$coefficients
epd_results = tibble(estimate = epd_coefs[2, 1], se = epd_coefs[2, 2], pval = epd_coefs[2, 4])



# ::::::::::::::::::::::::: PREDICTING RELATIVE WTP, NO INTERACTIONS :::::::::::::::::::::::::

# ----- PREDICT RELATIVE WTP, TREATMENT ONLY -----
treatment_only_mod = lm(wtp_dif ~ epd_treatment_baseline,
            data = df_full_clean_wtp_dif)
summary_treatment_only_mod = summary(treatment_only_mod)
dif_coefs = summary(treatment_only_mod)$coefficients
# R-squared = 0.4164, F-statistic pvalue = 0

texreg(
  treatment_only_mod,
  format = "latex",
  custom.coef.names = c("Intercept", "EPD Treatment"),
  caption = "Model Name",
  label = "tab:ols",
  omit.coef = "^(?!epd_treatment_baseline$)",
  float.pos = "H",
    caption.above = TRUE
)

# ----- PREDICT RELATIVE WTP, NO TREATMENT ALL OTHER VARIABLES -----
dif_no_treat_mod = lm(wtp_dif ~ ., data = df_full_clean_wtp_dif %>% select(-epd_treatment_baseline))
summary(dif_no_treat_mod)
# R-squared = 0.06, F-statistic p-value 0.3941 (0.3917 when I had continuous education)

# ----- PREDICT RELATIVE WTP, ALL VARIABLES -----
full_mod = lm(wtp_dif ~ ., data = df_full_clean_wtp_dif)
summary(full_mod)
# R-squared hardly increases (0.4564), epd_treatment_baseline is still highly significant
# F-statistic P-value = 0

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

# ::::::::::::::::::::::::: LOOKING AT INTERACTIONS :::::::::::::::::::::::::
# Theory says: income/income source, education, concern about air quality, vulnerability (elderly/kids)
# My hypotheses: political affiliation (use news as a proxy), ability to clean own air (fan)
interactions_mod = lm(wtp_dif ~ 
    epd_treatment_baseline : (
        s2_q3_baseline + # Age
        asset_index_baseline + # asset index
        dem_q31_baseline + # Source of income
        dem_q7_baseline + # Categorical education
        air_worry_baseline + # worried about AQ?
        dem_q3_baseline + # Elderly live with you?
        s9_q7_field_count_baseline + # Num social media
        dem_q5_baseline + # Young children live with you?
        s9_q5_3_7_baseline + # ARY News
        dem_q30_baseline # Num fans
        ),
    data = df_full_clean_wtp_dif)
summary(interactions_mod)
# Some nice potential interaction effects on asset index

# ::::::::::::::::::::::::: INTERACTION PLOTS :::::::::::::::::::::::::
plot_interact = function(fac, i_var, name_i_var) {
    dat = df_full_clean_wtp_dif
  if (fac) {
    dat = dat %>% mutate({{i_var}} := as.factor({{i_var}}))
  }
    dat %>% ggplot(aes(x = factor(epd_treatment_baseline), wtp_dif, group = {{i_var}}, color = {{i_var}})) +
        geom_line() +
        geom_point() +
        labs(
            x = "Treatment",
            y = "Relative WTP (PAQI WTP - EPD WTP)",
            color = name_i_var
        )
}

plot_interact(TRUE, s2_q3_baseline, "Age")
plot_interact(TRUE, dem_q31_baseline, "Income Source")
plot_interact(TRUE, dem_q7_baseline, "Type of Education")
plot_interact(FALSE, asset_index_baseline, "Asset Index")
plot_interact(FALSE, air_worry_baseline, "Worried About AQ")
plot_interact(TRUE, dem_q3_baseline, "Elderly live with you?")
plot_interact(TRUE, s9_q7_field_count_baseline, "Num Social Media")
plot_interact(TRUE, dem_q5_baseline, "Young children live with you?")
plot_interact(TRUE, s9_q5_3_7_baseline, "Watch ARY News")
plot_interact(TRUE, dem_q30_baseline, "Number of Fans")

# ::::::::::::::::::::::::: GENERAL PLOTTING :::::::::::::::::::::::::

# ---- PLOTTING WTP BY TREATMENT -----

# Plotting the data for visuals
df = df_full_clean %>% mutate(wtp_dif = wtp_dif) %>% select(wtp_paqi, wtp_epd, wtp_dif, epd_treatment_baseline)
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

ggsave("linear_regression_treatment_effect/temp_plot.png", plot, width = 10, height = 10)
