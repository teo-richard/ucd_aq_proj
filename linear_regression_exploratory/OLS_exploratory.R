library(tidyverse)
library(broom)
library(readxl)
library(here)
library(kableExtra)

# What variables affect willingness to pay?

# # Reading in data: small
# df_epd_clean = read_csv("/Users/teorichard/Downloads/UCD Research/AQ UCD/cleaned_data/df_epd_clean.csv")
# df_paqi_clean = read_csv("/Users/teorichard/Downloads/UCD Research/AQ UCD/cleaned_data/df_paqi_clean.csv")
# df_full_clean = read_csv("/Users/teorichard/Downloads/UCD Research/AQ UCD/cleaned_data/df_full_clean.csv")

# Reading in data: Large
df_epd_clean = read_csv("cleaned_data/LARGE_df_epd_clean.csv") %>% select(-hhid, -yrs_educ_formal_baseline)
df_paqi_clean = read_csv("cleaned_data/LARGE_df_paqi_clean.csv") %>% select(-hhid, -yrs_educ_formal_baseline)
df_full_clean = read_csv("cleaned_data/LARGE_df_full_clean.csv") %>% select(-hhid, -yrs_educ_formal_baseline)

map = read_excel(here("cleaned_data/variable_labels_filtered.xlsx"))

# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Dropping outcome variables we are not looking at
drop_vars_wtp_paqi = c("pref_baseline", "pref_endline", "wtp_epd", "wtp_dif", "epd_treatment_baseline")
drop_vars_wtp_epd = c("pref_baseline", "pref_endline", "wtp_paqi", "wtp_dif", "epd_treatment_baseline")


# creating function
lr_out = function(outcome_var, data, drop_vars) {
    treatment = str_extract(outcome_var, "(?<=_)[a-z]*")
    x_vars = setdiff(names(data), c(paste("wtp", treatment, sep = "_"), drop_vars))
    form = as.formula(paste0(outcome_var, " ~  ", paste(x_vars, collapse = " + ")))
    mod = lm(form, data = data %>% dplyr::select(-all_of(drop_vars)))
    return(mod)
}

tidy_up = function(mod) {
    summary_mod = summary(mod)
    tidy_mod = tidy(summary_mod)
    adj_pvalues = p.adjust(tidy_mod$p.value, method = "BH")
    tidy_mod = tidy_mod %>% mutate(adj_pvals = adj_pvalues)
    sign_mod = tidy_mod %>% filter(p.value < 0.05)
    meaning = map$Label[match(sign_mod$term, map$Variable)]

    final_mod = sign_mod %>% 
        mutate(meaning = meaning, .after = term) %>% 
        filter(term != "(Intercept)")

    stats = broom::glance(summary_mod) %>% 
        dplyr::select(all_of(c("r.squared", "statistic", "p.value", "df", "nobs")))
    stats = stats %>% mutate(
        MSE = mean(residuals(mod)^2, na.rm = TRUE)
    )

    return(list(final_mod, stats, adj_pvalues))
}

get_model_info = function(data, outcome, drop_vars) {
    data_name = deparse(substitute(data))
    if (data_name == "df_paqi_clean") {
        treatment = "pd"
    } else if (data_name == "df_epd_clean") {
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
    info = lr_out(
        data = data, 
        outcome_var = outcome, 
        drop_vars = drop_vars)
    tidied = tidy_up(info)[[1]]
    tidied_f = tidied %>% dplyr::select(-term)
    num_sel = nrow(tidied_f)
    info_s = tidy_up(info)[[2]] %>% mutate(type = paste("ols", code, sep = "_"), selected = num_sel, .before = r.squared)
    return(list(tidied_f, info_s, data_name))
}



# ----- PAQI Data: WTP_PAQI -----
pd_wp = get_model_info(data = df_paqi_clean, outcome = "wtp_paqi", drop_vars = drop_vars_wtp_paqi)
pd_wp_s = pd_wp[[2]]
pd_wp_f = pd_wp[[1]]
write_csv(pd_wp_s, "linear_regression_exploratory/ols_expl_rmd_files/pd_wp_s.csv")


# ----- PAQI Data: WTP_EPD -----
pd_we = get_model_info(data = df_paqi_clean, outcome = "wtp_epd", drop_vars = drop_vars_wtp_epd)
pd_we_s = pd_we[[2]]
pd_we_f = pd_we[[1]]
write_csv(pd_we_s, "linear_regression_exploratory/ols_expl_rmd_files/pd_we_s.csv")

# PAQI Treatment, WTP Dif
pd_wd = get_model_info(data = df_paqi_clean, outcome = "wtp_dif", drop_vars = c(drop_vars_wtp_paqi[-c(4)], "wtp_paqi"))
pd_wd_s = pd_wd[[2]]
pd_wd_f = pd_wd[[1]]
write_csv(pd_wd_f, "linear_regression_exploratory/ols_expl_rmd_files/p_dif_f.csv")
write_csv(pd_wd_s, "linear_regression_exploratory/ols_expl_rmd_files/p_dif_s.csv")

# ----- EPD Data: WTP_PAQI -----
ed_wp = get_model_info(data = df_epd_clean, outcome = "wtp_paqi", drop_vars = drop_vars_wtp_paqi)
ed_wp_s = ed_wp[[2]]
ed_wp_f = ed_wp[[1]]
write_csv(ed_wp_s, "linear_regression_exploratory/ols_expl_rmd_files/ed_wp_s.csv")

# ----- EPD Data: WTP_EPD -----
ed_we = get_model_info(data = df_epd_clean, outcome = "wtp_epd", drop_vars = drop_vars_wtp_epd)
ed_we_s = ed_we[[2]]
ed_we_f = ed_we[[1]]
write_csv(ed_we_s, "linear_regression_exploratory/ols_expl_rmd_files/ed_we_s.csv")

# EPD Treatment, WTP Dif
ed_wd = get_model_info(data = df_epd_clean, outcome = "wtp_dif", drop_vars = c(drop_vars_wtp_paqi[-c(4)], "wtp_paqi"))
ed_wd_s = ed_wd[[2]]
ed_wd_f = ed_wd[[1]]
write_csv(ed_wd_f, "linear_regression_exploratory/ols_expl_rmd_files/e_dif_f.csv")
write_csv(ed_wd_s, "linear_regression_exploratory/ols_expl_rmd_files/e_dif_s.csv")


ols_final_glance = bind_rows(pd_wp_s, pd_we_s, pd_wd_s, ed_wp_s, ed_we_s, ed_wd_s)
ols_final_glance <- ols_final_glance[, c(ncol(ols_final_glance), 1:(ncol(ols_final_glance)-1))]
ols_final_glance = ols_final_glance %>% dplyr::select(type, selected, r.squared, statistic, p.value, df, nobs, MSE)
write_csv(ols_final_glance, "final_stuff/ols_final_glance.csv")



# Dif treatment, PAQI WTP:
common_vars_d_p = tibble(common_vars = Reduce(intersect, list(pd_wp_f$meaning, ed_wp_f$meaning)))
est1 = pd_wp_f %>% filter(meaning %in% common_vars_d_p$common_vars) %>% select(estimate)
est2 = ed_wp_f %>% filter(meaning %in% common_vars_d_p$common_vars) %>% select(estimate)
collapsed <- paste(round(est1$estimate, 1), round(est2$estimate, 1), sep = " / ")
common_vars_d_p = common_vars_d_p %>% mutate(estimates = collapsed)



# Dif treatment, EPD WTP:
common_vars_d_e = tibble(common_vars = Reduce(intersect, list(pd_we_f$meaning, ed_we_f$meaning)))
est1 = pd_we_f %>% filter(meaning %in% common_vars_d_e$common_vars) %>% select(estimate)
est2 = ed_we_f %>% filter(meaning %in% common_vars_d_e$common_vars) %>% select(estimate)
collapsed <- paste(round(est1$estimate, 1), round(est2$estimate, 1), sep = " / ")
common_vars_d_e = common_vars_d_e %>% mutate(estimates = collapsed)

# PAQI treatment treatment, both WTP:
common_vars_p_d = tibble(common_vars = Reduce(intersect, list(pd_wp_f$meaning, pd_we_f$meaning)))
est1 = pd_wp_f %>% filter(meaning %in% common_vars_p_d$common_vars) %>% select(estimate)
est2 = pd_we_f %>% filter(meaning %in% common_vars_p_d$common_vars) %>% select(estimate)
collapsed <- paste(round(est1$estimate, 1), round(est2$estimate, 1), sep = " / ")
common_vars_p_d = common_vars_p_d %>% mutate(estimates = collapsed)

# EPD treatment, both WTP: 
common_vars_e_d = tibble(common_vars = Reduce(intersect, list(ed_wp_f$meaning, ed_we_f$meaning)))
est1 = ed_wp_f %>% filter(meaning %in% common_vars_e_d$common_vars) %>% select(estimate)
est2 = ed_we_f %>% filter(meaning %in% common_vars_e_d$common_vars) %>% select(estimate)
collapsed <- paste(round(est1$estimate, 1), round(est2$estimate, 1), sep = " / ")
common_vars_e_d = common_vars_e_d %>% mutate(estimates = collapsed)


# Dif treatment, rel WTP:
common_vars_d_r = tibble(common_vars = Reduce(intersect, list(pd_wd_f$meaning, ed_wd_f$meaning)))
est1 = pd_wd_f %>% filter(meaning %in% common_vars_d_r$common_vars) %>% select(estimate)
est2 = ed_wd_f %>% filter(meaning %in% common_vars_d_r$common_vars) %>% select(estimate)
collapsed <- paste(round(est1$estimate, 1), round(est2$estimate, 1), sep = " / ")
common_vars_d_r = common_vars_d_r %>% mutate(estimates = collapsed)



common_vars_d_p %>%
  kable(
    format = "latex",
    booktabs = TRUE,
    col.names = c("Variables Selected", "Estimate"),
    caption = "Summary of Models",
    escape = FALSE  # Allow LaTeX formatting
  ) %>%
  kable_styling(
    latex_options = c("HOLD_position"),
    full_width = FALSE
  )



all_vars = bind_rows(
    pd_wp_f %>% mutate(type = "PAQI treatment, WTP PAQI"), 
    pd_we_f %>% mutate(type = "PAQI treatment, WTP EPD"), 
    pd_wd_f %>% mutate(type = "PAQI treatment, relative WTP"), 
    ed_wp_f %>% mutate(type = "EPD treatment, WTP PAQI"),
    ed_we_f %>% mutate(type = "EPD treatment, WTP EPD"),
    ed_wd_f %>% mutate(type = "EPD treatment, relative WTP")
)

all_vars = all_vars %>% 
  group_by(meaning) %>% 
  summarize(
    n = n(),
    variables = paste(type, collapse = " // ")
  )




