library(tidyverse)
library(leaps)
library(broom)

# Read in SMALL
df_epd_clean = read_csv("/Users/teorichard/Downloads/UCD Research/AQ UCD/cleaned_data/df_epd_clean.csv") %>% 
                  mutate(across(where(is.character), as.factor)) %>% select(-hhid, -yrs_educ_formal_baseline)
df_paqi_clean = read_csv("/Users/teorichard/Downloads/UCD Research/AQ UCD/cleaned_data/df_paqi_clean.csv") %>% 
                  mutate(across(where(is.character), as.factor)) %>% select(-hhid, -yrs_educ_formal_baseline)
df_full_clean = read_csv("/Users/teorichard/Downloads/UCD Research/AQ UCD/cleaned_data/df_full_clean.csv") %>% 
                  mutate(across(where(is.character), as.factor)) %>% select(-hhid, -yrs_educ_formal_baseline)


# Reading in data: LARGE
# df_epd_clean = read_csv("/Users/teorichard/Downloads/UCD Research/AQ UCD/cleaned_data/LARGE_df_epd_clean.csv") %>% 
#                   mutate(across(where(is.character), as.factor))
# df_paqi_clean = read_csv("/Users/teorichard/Downloads/UCD Research/AQ UCD/cleaned_data/LARGE_df_paqi_clean.csv") %>% 
#                   mutate(across(where(is.character), as.factor))
# df_full_clean = read_csv("/Users/teorichard/Downloads/UCD Research/AQ UCD/cleaned_data/LARGE_df_full_clean.csv") %>% 
#                   mutate(across(where(is.character), as.factor))


source("holdout_sets/holdout_fns.R")


# note: not doing holdout because the train set doesn't always capture some of the more rare factors so in the
#   train set their variance is 0 which fucks everything up
# also: can't do best subset on the large ones

# *_full is list(variable names, test data matrix)
# * is variable names
# *_x is the test data matrix

a_full = get_vars(make_matrix(data = df_paqi_clean, outcome = "wtp_paqi", holdout_percent = 0.7, holdout = TRUE))
a = names(a_full[[1]])
a_x = a_full[[2]]
b_full = get_vars(make_matrix(data = df_epd_clean, outcome = "wtp_paqi", holdout_percent = 0.7, holdout = TRUE))
b = names(b_full[[1]])
b_x = b_full[[2]]
c_full = get_vars(make_matrix(data = df_paqi_clean, outcome = "wtp_epd", holdout_percent = 0.7, holdout = TRUE))
c = names(c_full[[1]])
c_x = c_full[[2]]
d_full = get_vars(make_matrix(data = df_epd_clean, outcome = "wtp_epd", holdout_percent = 0.7, holdout = TRUE))
d = names(d_full[[1]])
d_x = d_full[[2]]
e_full = get_vars(make_matrix(data = df_full_clean, outcome = "pref_baseline", holdout_percent = 0.7, holdout = TRUE))
e = names(e_full[[1]])
e_x = e_full[[2]]

# Making sure the one common to all is at the top
common = Reduce(intersect, list(a, b, c, d, e))
aa = c(common, setdiff(a, common))
bb = c(common, setdiff(b, common))
cc = c(common, setdiff(c, common))
dd = c(common, setdiff(d, common))
ee = c(common, setdiff(e, common))


max_len = max(length(a), length(b), length(c), length(d), length(e))




a_info = linear_regression_fit("wtp_paqi", a, a_x)
a_t = a_info[[1]]
b_info = linear_regression_fit("wtp_paqi", b, b_x)
b_t = b_info[[1]]
c_info = linear_regression_fit("wtp_epd", c, c_x)
c_t = c_info[[1]]
d_info = linear_regression_fit("wtp_epd", d, d_x)
d_t = d_info[[1]]
e_info = linear_regression_fit("pref_baseline", e, e_x)
e_t = e_info[[1]]



t = tibble(
    `T: PAQI / WTP: PAQI` = c(aa, rep(NA, max_len - length(a))),
    `coef1` = c(a_t$estimates, rep(NA, max_len - length(a_t$estimates))),
    pval1 = c(a_t$pvals, rep(NA, max_len - length(a_t$estimates))),
    `T: EPD / WTP: PAQI` = c(bb, rep(NA, max_len - length(b))),
    coef2 = c(b_t$estimates, rep(NA, max_len - length(b_t$estimates))),
    pval2 = c(b_t$pvals, rep(NA, max_len - length(b_t$estimates))),
    `T: PAQI / WTP: EPD` = c(cc, rep(NA, max_len - length(c))),
    coef3 = c(c_t$estimates, rep(NA, max_len - length(c_t$estimates))),
    pval3 = c(c_t$pvals, rep(NA, max_len - length(c_t$estimates))),
    `T: EPD / WTP: EPD` = c(dd, rep(NA, max_len - length(d))),
    coef4 = c(d_t$estimates, rep(NA, max_len - length(d_t$estimates))),
    pval4 = c(d_t$pvals, rep(NA, max_len - length(d_t$estimates))),
    `T: Full dataset / Outcome: Baseline Preference` = c(ee, rep(NA, max_len - length(e))),
    coef5 = c(e_t$estimates, rep(NA, max_len - length(e_t$estimates))),
    pval5 = c(e_t$pvals, rep(NA, max_len - length(e_t$estimates)))

  
)

write_csv(t, "holdout_sets/best_subset_table.csv")



