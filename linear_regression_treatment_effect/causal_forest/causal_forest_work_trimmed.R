library(grf)
library(tidyverse)
library(readxl)
library(haven)

df_full_clean_catedu = read_csv("/Users/teorichard/Downloads/UCD Research/AQ UCD/cleaned_data/LARGE_df_full_clean_catedu.csv") %>% select(-hhid)

# ----- TRIMMING OFF EXTREME VALUES OF WORK HOURS -----
summary(df_full_clean_catedu$work_total_hrs_baseline)
df_full_work_trimmed = df_full_clean_catedu %>%
  filter(work_total_hrs_baseline >= quantile(work_total_hrs_baseline, 0.05) &
         work_total_hrs_baseline <= quantile(work_total_hrs_baseline, 0.95))

summary(df_full_work_trimmed$work_total_hrs_baseline)

hist(df_full_work_trimmed$work_total_hrs_baseline)

map = read_excel("/Users/teorichard/Downloads/UCD Research/AQ UCD/cleaned_data/variable_labels_filtered.xlsx")

source("/Users/teorichard/Downloads/UCD Research/AQ UCD/linear_regression_treatment_effect/causal_forest/causal_forest_fns.R")

# ----- RUNNING CAUSAL FOREST -----
df_ready_modelling = df_full_work_trimmed %>% dplyr::select(-all_of(c("wtp_paqi", "wtp_epd", "wtp_dif", "epd_treatment_baseline", "pref_baseline", "pref_endline")))
x_vars = model.matrix(~., data = df_ready_modelling)
x_vars = x_vars[, -1] # remove intercept
wtp_dif = df_full_work_trimmed$wtp_dif
treatment = df_full_work_trimmed$epd_treatment_baseline

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


# ----- TREATMENT EFFECT PREDICTIONS -----
tau_hat = predict(cf)$predictions
avg_importance = 1 / ncol(x_vars) # if all vars are equally important

# ----- VARIABLE IMPORTANCE PLOT -----
ggplot(var_imp_df, aes(x = reorder(variable, importance), y = importance)) +
  geom_bar(stat = "identity") +
    geom_hline(yintercept = avg_importance, color = "red", linetype = "dashed") +
  coord_flip() +
  labs(x = "Variable", y = "Importance", 
       title = "Variable Importance for Treatment Effect Heterogeneity",
       caption = "Dashed line represents importance if all variables were of equal importance") +
  theme_minimal()




# ----- BEST LINEAR PROJECTION -----
blp = best_linear_projection(cf, x_vars[,c(
    "work_total_hrs_baseline")])


# ----- TABLE SUMMARY -----

t = tibble(max_tau_hat = max(tau_hat))


# ---- CORRELATIONS ----

target_var = "work_total_hrs_baseline"

correlations = df_full_work_trimmed %>%
  summarise(across(where(is.numeric), ~cor(.x, .data[[target_var]], use = "complete.obs"))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "correlation") %>%
  filter(variable != target_var) %>% 
  arrange(desc(abs(correlation)))

names = map$Label[match(correlations$variable, map$Variable)]
correlations$variable = names
view(correlations)


hist(df_full_work_trimmed$work_total_hrs_baseline)



# ----- HETEROGENEITY ANALYSIS -----

# ///// Subgroup Analysis (Quartiles):  \\\\\

# .. Informal OlS tests by subgroup ..

df_subgroup_analysis = df_full_work_trimmed %>% mutate( 
  quarts = ntile(df_full_work_trimmed$work_total_hrs_baseline, 4)
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

mod_inter_work = lm(wtp_dif ~ epd_treatment_baseline*work_total_hrs_baseline, data = df_full_work_trimmed)
summary(mod_inter_work)
