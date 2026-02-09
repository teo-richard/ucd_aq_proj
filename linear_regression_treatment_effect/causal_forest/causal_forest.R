library(grf)
library(tidyverse)
library(readxl)
library(haven)

# Note: Using categorical education because this makes more sense intuitively (vs. continuous education)

df_epd_clean = read_csv("cleaned_data/LARGE_df_epd_clean.csv") %>% select(-hhid)
df_paqi_clean = read_csv("cleaned_data/LARGE_df_paqi_clean.csv") %>% select(-hhid)
# df_full_clean_contedu = read_csv("/Users/teorichard/Downloads/UCD Research/AQ UCD/cleaned_data/LARGE_df_full_clean.csv") %>% select(-hhid, -dem_q7_baseline) # this one has continuous education
df_full_clean_catedu = read_csv("leaned_data/LARGE_df_full_clean_catedu.csv") %>% select(-hhid)

full_analysis = read_dta("original_files/stata files/Analysis_data_saved.dta")
original_mean <- mean(full_analysis$s3_q15v16_baseline_r, na.rm = TRUE)
original_sd <- sd(full_analysis$s3_q15v16_baseline_r, na.rm = TRUE)
a = (df_full_clean_catedu$s3_q15v16_baseline_r_std * original_sd) + original_mean
table(a)

map = read_excel("cleaned_data/variable_labels_filtered.xlsx")

source("linear_regression_treatment_effect/causal_forest/causal_forest_fns.R")

# ----- RUNNING CAUSAL FOREST -----
df_ready_modelling = df_full_clean_catedu %>% dplyr::select(-all_of(c("wtp_paqi", "wtp_epd", "wtp_dif", "epd_treatment_baseline", "pref_baseline", "pref_endline")))
x_vars = model.matrix(~., data = df_ready_modelling)
x_vars = x_vars[, -1] # remove intercept
wtp_dif = df_full_clean_catedu$wtp_dif
treatment = df_full_clean_catedu$epd_treatment_baseline

set.seed(123)
# Fit causal forest
cf = causal_forest(X = x_vars,
                    Y = wtp_dif,
                    W = treatment,
                    num.trees = 4000)

# ----- VARIABLES DF -----
var_imp = round(variable_importance(cf), 3)
var_imp_codes = colnames(x_vars)
meaning = map$Label[match(colnames(x_vars), map$Variable)]
var_imp_df = tibble(code = var_imp_codes, variable = meaning, importance = var_imp)
var_imp_df_ar = var_imp_df %>% arrange(desc(importance))
var_imp_df_ar[, 3] = unlist(var_imp_df_ar[, 3])

write_csv(var_imp_df_ar, "linear_regression_treatment_effect/causal_forest/variable_importance_df.csv")


# ----- TREATMENT EFFECT PREDICTIONS -----
tau_hat = predict(cf)$predictions
avg_importance = 1 / ncol(x_vars) # if all vars are equally important

# ----- VARIABLE IMPORTANCE PLOT -----
# Horizontal bars
ggplot(var_imp_df, aes(x = reorder(variable, importance), y = importance)) +
  geom_bar(stat = "identity") +
    geom_hline(yintercept = avg_importance, color = "red", linetype = "dashed") +
    coord_flip() +
  labs(x = "Variable", y = "Importance", 
       title = "Variable Importance for Treatment Effect Heterogeneity",
       caption = "Dashed line represents importance if all variables were of equal importance") +
  theme_minimal()
  ggsave("linear_regression_treatment_effect/causal_forest/images/var_importance.png")

# Vertical bars
ggplot(var_imp_df, aes(x = reorder(variable, -importance), y = importance)) +
  geom_bar(stat = "identity") +
    geom_hline(yintercept = avg_importance, color = "red", linetype = "dashed") +
  labs(x = "Variable", y = "Importance", 
       title = "Variable Importance for Treatment Effect Heterogeneity",
       subtitle = "Dashed line represents importance if all variables were of equal importance") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1),
    axis.title.x = element_text(margin = margin(t = 30, r = 0, b = 0, l = 0)),
    plot.title = element_text(hjust = 0.5, face = "bold", margin = margin(t = 0, r = 0, b = 20, l = 0)), 
    plot.margin = margin(t = 30, r = 30, b = 30, l = 30, unit = "pt")
    )
ggsave("linear_regression_treatment_effect/causal_forest/images/var_importance_vertical.png", height = 8, width = 10, dpi = 300)



# ----- BAR PLOTS -----
original_mean = mean(full_analysis$s3_q15v16_baseline_r, na.rm = TRUE)
original_sd = sd(full_analysis$s3_q15v16_baseline_r, na.rm = TRUE)
a = round((df_full_clean_catedu$s3_q15v16_baseline_r_std * original_sd) + original_mean, 0)
table(a)

df_for_bar =
  tibble(
    work_total_hrs_baseline = df_full_clean_catedu$work_total_hrs_baseline,
    tehsil_n_baseline = ifelse(df_full_clean_catedu$tehsil_n_baseline == 0, "Shalamar Tehsil", "City Center"),
    s3_q6_7_baseline = df_full_clean_catedu$s3_q6_7_baseline, 
    s9_q7_field_count_baseline = df_full_clean_catedu$s9_q7_field_count_baseline, 
    s3_q15v16_baseline_r_std = a
  )



create_bar_plot(df = df_full_clean_catedu, var_name = "work_total_hrs_baseline")
ggsave("linear_regression_treatment_effect/causal_forest/images/work_hrs.png", width = 5, height = 2.5, dpi = 300)
ggsave("linear_regression_treatment_effect/causal_forest/images/work_hrs_whitebg.png", width = 5, height = 2.5, dpi = 300, bg = "white") # white background

create_bar_plot(df = df_full_clean_catedu, "tehsil_n_baseline")
ggsave("linear_regression_treatment_effect/causal_forest/images/tehsil.png", width = 5, height = 2.5, dpi = 300)

create_bar_plot(df = df_full_clean_catedu, "s3_q15v16_baseline_r_std")
ggsave("linear_regression_treatment_effect/causal_forest/images/gov_approval.png", width = 5, height = 2.5, dpi = 300)

create_bar_plot(df = df_full_clean_catedu, "s3_q6_7_baseline")
ggsave("linear_regression_treatment_effect/causal_forest/images/ap_info_myobs.png", width = 5, height = 2.5, dpi = 300)

create_bar_plot(df = df_full_clean_catedu, "s9_q7_field_count_baseline")
ggsave("linear_regression_treatment_effect/causal_forest/images/num_social_media.png", width = 5, height = 2.5, dpi = 300)


# ----- BEST LINEAR PROJECTION -----
blp = best_linear_projection(cf, x_vars[,c(
    "work_total_hrs_baseline")])

saveRDS(blp, "blp_results.rds")


summary(df_full_clean_catedu$work_total_hrs_baseline)


# ----- TABLE SUMMARY -----

t = tibble(max_tau_hat = max(tau_hat))


# ---- CORRELATIONS ----

target_var = "work_total_hrs_baseline"

correlations = df_full_clean_catedu %>%
  summarise(across(where(is.numeric), ~cor(.x, .data[[target_var]], use = "complete.obs"))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "correlation") %>%
  filter(variable != target_var) %>% 
  arrange(desc(abs(correlation)))

names = map$Label[match(correlations$variable, map$Variable)]
correlations$variable = names
view(correlations)


hist(df_full_clean_catedu$work_total_hrs_baseline)
table(df_full_clean_catedu$work_total_hrs_baseline)





# ----- HETEROGENEITY ANALYSIS -----

# ///// Subgroup Analysis (Quartiles):  \\\\\

# .. Informal OlS tests by subgroup ..

df_subgroup_analysis = df_full_clean_catedu %>% mutate( 
  quarts = ntile(df_full_clean_catedu$work_total_hrs_baseline, 4)
)
df_sa_1 = df_subgroup_analysis %>% filter(quarts == 1)
df_sa_2 = df_subgroup_analysis %>% filter(quarts == 2)
df_sa_3 = df_subgroup_analysis %>% filter(quarts == 3)
df_sa_4 = df_subgroup_analysis %>% filter(quarts == 4)

mod_sa_1 = lm(wtp_dif ~ epd_treatment_baseline, data = df_sa_1)
summary(mod_sa_1)

mod_sa_2 = lm(wtp_dif ~ epd_treatment_baseline, data = df_sa_2)
summary(mod_sa_2)

mod_sa_3 = lm(wtp_dif ~ epd_treatment_baseline, data = df_sa_3)
summary(mod_sa_3)

mod_sa_4 = lm(wtp_dif ~ epd_treatment_baseline, data = df_sa_4)
summary(mod_sa_4)


# .. Formal Interaction Test ..

mod_inter_work = lm(wtp_dif ~ epd_treatment_baseline*work_total_hrs_baseline, data = df_full_clean_catedu)
summary(mod_inter_work)
