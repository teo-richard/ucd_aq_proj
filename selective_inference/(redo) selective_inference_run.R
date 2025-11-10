library(tidyverse)
library(readr)
library(broom)

# Reading in data: Large
df_epd_clean = read_csv("/Users/teorichard/Downloads/UCD Research/AQ UCD/cleaned_data/LARGE_df_epd_clean.csv") %>% select(-hhid, -yrs_educ_formal_baseline)
df_paqi_clean = read_csv("/Users/teorichard/Downloads/UCD Research/AQ UCD/cleaned_data/LARGE_df_paqi_clean.csv") %>% select(-hhid, -yrs_educ_formal_baseline)
df_full_clean = read_csv("/Users/teorichard/Downloads/UCD Research/AQ UCD/cleaned_data/LARGE_df_full_clean.csv") %>% select(-hhid, -yrs_educ_formal_baseline)


source("selective_inference/(redo) selective_inference_fns.R")

# note the ones a, b, etc. won't work currently bc you haven't updated to work
#   with the new matrix function that allows you to get the original scales back for
#   your variables (previously had standardized vars but wanted the orignal ones)

# a = fixedLassoInf_fit(df_paqi_clean, "wtp_paqi") %>% mutate(type = "T: PAQI / WTP: PAQI", wtp = "paqi")
pp_all = fixedLassoInf_fit_keepall(df_paqi_clean, "wtp_paqi") %>% mutate(type = "T: PAQI / WTP: PAQI", wtp = "paqi")
pp_significant = pp_all %>% filter(pval <= 0.05)

# b = fixedLassoInf_fit(df_paqi_clean, "wtp_epd") %>% mutate(type = "T: PAQI / WTP: EPD", wtp = "epd")
pe_all = fixedLassoInf_fit_keepall(df_paqi_clean, "wtp_epd") %>% mutate(type = "T: PAQI / WTP: EPD", wtp = "epd")
pe_significant = pe_all %>% filter(pval <= 0.05)

# c = fixedLassoInf_fit(df_paqi_clean, "wtp_dif") %>% mutate(type = "T: PAQI / WTP: DIF", wtp = "dif")
pd_all = fixedLassoInf_fit_keepall(df_paqi_clean, "wtp_dif") %>% mutate(type = "T: PAQI / WTP: DIF", wtp = "dif")
pd_significant = pd_all %>% filter(pval <= 0.05)


# d = fixedLassoInf_fit(df_epd_clean, "wtp_paqi") %>% mutate(type = "T: EPD / WTP: PAQI", wtp = "paqi")
ep_all = fixedLassoInf_fit_keepall(df_epd_clean, "wtp_paqi") %>% mutate(type = "T: EPD / WTP: PAQI", wtp = "paqi")
ep_significant = ep_all %>% filter(pval <= 0.05)

# e = fixedLassoInf_fit(df_epd_clean, "wtp_epd") %>% mutate(type = "T: EPD / WTP: EPD", wtp = "epd")
ee_all = fixedLassoInf_fit_keepall(df_epd_clean, "wtp_epd") %>% mutate(type = "T: EPD / WTP: EPD", wtp = "epd")
ee_significant = ee_all %>% filter(pval <= 0.05)


# f = fixedLassoInf_fit(df_epd_clean, "wtp_dif") %>% mutate(type = "T: EPD / WTP: DIF", wtp = "dif") 
ed_all = fixedLassoInf_fit_keepall(df_epd_clean, "wtp_dif") %>% mutate(type = "T: EPD / WTP: DIF", wtp = "dif") 
ed_significant = ed_all %>% filter(pval <= 0.05)


bind_tibble = function(a, b, c, d, e, f) {
    max_len = max(nrow(a), nrow(b), nrow(c), nrow(d), nrow(e), nrow(f))
    bound_tib = tibble(.rows = max_len)
    common = Reduce(intersect, list(a$name, b$name, c$name, d$name, e$name, f$name))

    for (z in list(a, b, c, d, e, f)) {
        names(z)[1] = z[[1, 4]]
        z = z %>% dplyr::select(-wtp, -type)
        empty_rows = max_len - nrow(z)
        empty_tib = tibble(.rows = empty_rows)
        z_new = bind_rows(z, empty_tib)
        bound_tib = bind_cols(bound_tib, z_new, .name_repair = "minimal")

    }

    return(bound_tib)
}

bound_tib = bind_tibble(pp_all, pe_all, pd_all, ep_all, ee_all, ed_all)
filtered_tib = bind_tibble(pp_significant, pe_significant, pd_significant, ep_significant, ee_significant, ed_significant)
write_csv(bound_tib, "selective_inference/selective_inference_all_selected_vars.csv")
write_csv(filtered_tib, "selective_inference/selective_inference_significant_vars.csv")

get_lm = function(outcome_var, pred_vars, data) {
    if (outcome == "wtp_paqi") {
    drop_vars = c("wtp_epd", "wtp_dif", "epd_treatment_baseline", "pref_baseline", "pref_endline")
    } else if (outcome == "wtp_epd") {
    drop_vars = c("wtp_paqi", "wtp_dif", "epd_treatment_baseline", "pref_baseline", "pref_endline")
    } else if (outcome == "wtp_dif") {
    drop_vars = c("wtp_paqi", "wtp_epd", "epd_treatment_baseline", "pref_baseline", "pref_endline")
    } else if (outcome == "pref_baseline") {
    drop_vars = c("wtp_paqi", "wtp_epd", "wtp_dif", "epd_treatment_baseline", "pref_endline")
    } else if (outcome == "pref_endline") {
    drop_vars = c("wtp_paqi", "wtp_epd", "wtp_dif", "epd_treatment_baseline", "pref_baseline")
    } else {
    stop("wrong outcome variable")
    }

    form = as.formula(paste(outcome_var, " ~ ", paste(pred_vars, collapse = " + ")))
    model = lm(form, data = data)
    model_summary = summary(model)
    MSE = sum(model_summary$residual^2)
    glance_it = glance(model_summary) %>% dplyr::select(r.squared, statistic, p.value, df, nobs) %>% mutate(MSE = MSE)
    tidy_it = tidy(model_summary)


    residuals_df = tibble(
        actual = data[[outcome_var]],
        fitted = fitted(model),
        residuals = residuals(model)
    )

    return(list(glance_it, tidy_it, residuals_df))
}

pd_significant$name[5] = "dem_q31_baseline"
ed_significant$name[5] = "dem_q31_baseline"

pp_out = get_lm("wtp_paqi", pp_significant$name, df_paqi_clean)
num_sel_pp = length(pp_out[[2]]$term) - 1

pe_out = get_lm("wtp_epd", pe_significant$name, df_paqi_clean)
num_sel_pe = length(pe_out[[2]]$term) - 1

pd_out = get_lm("wtp_dif", pd_significant$name, df_paqi_clean)
num_sel_pd = length(pd_out[[2]]$term) - 1

ep_out = get_lm("wtp_paqi", ep_significant$name, df_epd_clean)
num_sel_ep = length(ep_out[[2]]$term) - 1

ee_out = get_lm("wtp_epd", ee_significant$name, df_epd_clean)
num_sel_ee = length(ee_out[[2]]$term) - 1

ed_out = get_lm("wtp_dif", ed_significant$name, df_epd_clean)
num_sel_ed = length(ed_out[[2]]$term) - 1

pp_glance = pp_out[[1]] %>% mutate(type = "si_pp", selected = num_sel_pp, .before = `r.squared`)
pe_glance = pe_out[[1]] %>% mutate(type = "si_pe", selected = num_sel_pe, .before = `r.squared`)
pd_glance = pd_out[[1]] %>% mutate(type = "si_pd", selected = num_sel_pd, .before = `r.squared`)
ep_glance = ep_out[[1]] %>% mutate(type = "si_ep", selected = num_sel_ep, .before = `r.squared`)
ee_glance = ee_out[[1]] %>% mutate(type = "si_ee", selected = num_sel_ee, .before = `r.squared`)
ed_glance = ed_out[[1]] %>% mutate(type = "si_ed", selected = num_sel_ed, .before = `r.squared`)

si_final_glance = bind_rows(pp_glance, pe_glance, pd_glance, ep_glance, ee_glance, ed_glance)
write_csv(si_final_glance, "final_stuff/si_final_glance.csv")


r = pp_out[[3]]
r %>% ggplot(aes(x = fitted, y = residuals)) + geom_point()


# Check the extreme residuals
r %>% 
  summarise(
    max_resid = max(abs(residuals)),
    mean_resid = mean(residuals),
    sd_resid = sd(residuals),
    n_extreme = sum(abs(residuals) > 100)
  )

# Compare to a similar plot for OLS
# (you'd need to run OLS and get its residuals)