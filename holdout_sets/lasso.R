library(tidyverse)
library(glmnet)
library(readr)
library(glue)
library(readxl)
library(here)

# Reading in data: LARGE
df_epd_clean = read_csv("/Users/teorichard/Downloads/UCD Research/AQ UCD/cleaned_data/LARGE_df_epd_clean.csv") %>% 
                  mutate(across(where(is.character), as.factor)) %>% select(-hhid, -yrs_educ_formal_baseline)
df_paqi_clean = read_csv("/Users/teorichard/Downloads/UCD Research/AQ UCD/cleaned_data/LARGE_df_paqi_clean.csv") %>% 
                  mutate(across(where(is.character), as.factor)) %>% select(-hhid, -yrs_educ_formal_baseline)
df_full_clean = read_csv("/Users/teorichard/Downloads/UCD Research/AQ UCD/cleaned_data/LARGE_df_full_clean.csv") %>% 
                  mutate(across(where(is.character), as.factor)) %>% select(-hhid, -yrs_educ_formal_baseline)

# Dropping outcome variables we are not looking at
# drop_vars_wtp_paqi = c("pref_baseline", "pref_endline", "wtp_epd", "wtp_dif", "epd_treatment_baseline")
# drop_vars_wtp_epd = c("pref_baseline", "pref_endline", "wtp_paqi", "wtp_dif", "epd_treatment_baseline")
# drop_vars_

source("holdout_sets/holdout_fns.R")
map = read_excel(here("cleaned_data/variable_labels_filtered.xlsx"))

get_model_info = function(data, outcome, holdout_percent, lambda_choice, holdout) {
    if (deparse(substitute(data)) == "df_paqi_clean") {
        treatment = "pd"
    } else {
        treatment = "ed"
    }
    if (outcome == "wtp_paqi") {
        wtp =  "wp"
    } else if (outcome == "wtp_epd") {
        wtp = "we"
    } else if (outcome == "wtp_dif") {
        wtp = "wd"
    } else (outcome == "NUH UH")

    code = paste(treatment, wtp, sep = "_")
    info = lasso_out(
        data = data, 
        outcome = outcome, 
        holdout_percent = holdout_percent, 
        lambda_choice = lambda_choice, 
        holdout = holdout)
    info_f = info[[1]]
    info_f_sig = info_f %>% filter(pvals <= 0.05)
    num_sel = nrow(info_f)
    num_sel2 = nrow(info_f_sig)
    info_s = info[[2]] %>% mutate(type = paste("l", code, sep = "_"), selected = paste(num_sel, num_sel2, collapse = "; "), .before = r.squared)
    return(list(info_f, info_f_sig, info_s))
}



# TREATMENT PAQI, WTP PAQI
pd_wp = get_model_info(
    data = df_paqi_clean, 
    outcome = "wtp_paqi", 
    holdout_percent = 0.7, 
    lambda_choice = "lambda.min",
    holdout = FALSE
)
pd_wp_s = pd_wp[[3]]
pd_wp_f = pd_wp[[2]]

# TREATMENT PAQI, WTP EPD
pd_we = get_model_info(
    data = df_paqi_clean, 
    outcome = "wtp_epd", 
    holdout_percent = 0.7, 
    lambda_choice = "lambda.min",
    holdout = FALSE
)
pd_we_s = pd_we[[3]]
pd_we_f = pd_we[[2]]

# TREATMENT PAQI, WTP DIF
pd_wd = get_model_info(
    data = df_paqi_clean, 
    outcome = "wtp_dif", 
    holdout_percent = 0.7, 
    lambda_choice = "lambda.min",
    holdout = FALSE
)
pd_wd_s = pd_wd[[3]]
pd_wd_f = pd_wd[[2]]

# TREATMENT EPD, WTP PAQI
ed_wp = get_model_info(
    data = df_epd_clean, 
    outcome = "wtp_paqi", 
    holdout_percent = 0.7, 
    lambda_choice = "lambda.min",
    holdout = FALSE
)
ed_wp_s = ed_wp[[3]]
ed_wp_f = ed_wp[[2]]

# TREATMENT EPD, WTP EPD
ed_we = get_model_info(
    data = df_epd_clean, 
    outcome = "wtp_epd", 
    holdout_percent = 0.7, 
    lambda_choice = "lambda.min",
    holdout = FALSE
)
ed_we_s = ed_we[[3]]
ed_we_f = ed_we[[2]]

# TREATMENT EPD, WTP DIF
ed_wd = get_model_info(
    data = df_epd_clean, 
    outcome = "wtp_dif", 
    holdout_percent = 0.7, 
    lambda_choice = "lambda.min",
    holdout = FALSE
)
ed_wd_s = ed_wd[[3]]
ed_wd_f = ed_wd[[2]]

l_final = bind_rows(pd_wp_s, pd_we_s, pd_wd_s, ed_wp_s, ed_we_s, ed_wd_s)
write_csv(l_final, "final_stuff/l_final_glance.csv")


# Dif treatment, PAQI WTP:
common_vars_d_p = tibble(common_vars = Reduce(intersect, list(pd_wp_f$name, ed_wp_f$name)))
est1 = pd_wp_f %>% filter(name %in% common_vars_d_p$common_vars) %>% select(estimate)
est2 = ed_wp_f %>% filter(name %in% common_vars_d_p$common_vars) %>% select(estimate)
collapsed <- paste(round(est1$estimate, 1), round(est2$estimate, 1), sep = " / ")
common_vars_d_p = common_vars_d_p %>% mutate(estimates = collapsed)



# Dif treatment, EPD WTP:
common_vars_d_e = tibble(common_vars = Reduce(intersect, list(pd_we_f$name, ed_we_f$name)))
est1 = pd_we_f %>% filter(name %in% common_vars_d_e$common_vars) %>% select(estimate)
est2 = ed_we_f %>% filter(name %in% common_vars_d_e$common_vars) %>% select(estimate)
collapsed <- paste(round(est1$estimate, 1), round(est2$estimate, 1), sep = " / ")
common_vars_d_e = common_vars_d_e %>% mutate(estimates = collapsed)

# PAQI treatment treatment, both WTP:
common_vars_p_d = tibble(common_vars = Reduce(intersect, list(pd_wp_f$name, pd_we_f$name)))
est1 = pd_wp_f %>% filter(name %in% common_vars_p_d$common_vars) %>% select(estimate)
est2 = pd_we_f %>% filter(name %in% common_vars_p_d$common_vars) %>% select(estimate)
collapsed <- paste(round(est1$estimate, 1), round(est2$estimate, 1), sep = " / ")
common_vars_p_d = common_vars_p_d %>% mutate(estimates = collapsed)

# EPD treatment, both WTP: 
common_vars_e_d = tibble(common_vars = Reduce(intersect, list(ed_wp_f$name, ed_we_f$name)))
est1 = ed_wp_f %>% filter(name %in% common_vars_e_d$common_vars) %>% select(estimate)
est2 = ed_we_f %>% filter(name %in% common_vars_e_d$common_vars) %>% select(estimate)
collapsed <- paste(round(est1$estimate, 1), round(est2$estimate, 1), sep = " / ")
common_vars_e_d = common_vars_e_d %>% mutate(estimates = collapsed)


# Dif treatment, rel WTP:
common_vars_d_r = tibble(common_vars = Reduce(intersect, list(pd_wd_f$name, ed_wd_f$name)))
est1 = pd_wd_f %>% filter(name %in% common_vars_d_r$common_vars) %>% select(estimate)
est2 = ed_wd_f %>% filter(name %in% common_vars_d_r$common_vars) %>% select(estimate)
collapsed <- paste(round(est1$estimate, 1), round(est2$estimate, 1), sep = " / ")
common_vars_d_r = common_vars_d_r %>% mutate(estimates = collapsed)



common_vars_p_d %>%
  kable(
    format = "latex",
    booktabs = TRUE,
    col.names = c("Variables Selected", "Estimates"),
    caption = "Summary of Models",
    escape = FALSE  # Allow LaTeX formatting
  ) %>%
  kable_styling(
    latex_options = c("HOLD_position"),
    full_width = FALSE
  )

