library(tidvyerse)
library(kableExtra)
library(stringr)

ols = read_csv("final_stuff/ols_final_glance.csv") %>% mutate(selected = as.character(selected))
las = read_csv('final_stuff/l_final_glance.csv')

# add a 'method' column to each table
ols = ols %>% mutate(method = "OLS") %>% dplyr::select(names(ols), method)
las = las %>% mutate(method = "LASSO→OLS") %>% dplyr::select(names(ols), method)


results = bind_rows(ols, las)

results = results %>%
  mutate(
    treatment = str_extract(type, "(?<=_)[a-z]"),
    outcome = str_extract(type, "(?<=_[a-z])[a-z]")
  )

results = results %>%
  mutate(
    treatment = recode(treatment,
      "p" = "PAQI",
      "e" = "EPD"
    ),
    outcome = recode(outcome,
      "p" = "WTP(PAQI)",
      "e" = "WTP(EPD)",
      "d" = "Relative WTP"
    )
  )

results = results %>%
  dplyr::select(treatment, outcome, method, selected, r.squared, MSE, statistic, p.value, df, nobs)

results = results %>%
  arrange(treatment, outcome, factor(method, levels = c("OLS", "SelectiveInference", "LASSO→OLS")))

results_relative_wtp = results[c(1:2, 7:8), ]
results_wtp_epd = results[c(3:4, 9:10), ]
results_wtp_paqi = results[c(5:6, 11:12), ]




results_wtp_paqi %>%
  kable(
    format = "latex",
    booktabs = TRUE,
    col.names = c("Treatment", "Outcome", "Model", "N Vars", "R-Squared", "MSE", "F", "P-Value", "DF", "Num Obs."),
    caption = "Summary of Models",
    digits = 3,
    escape = FALSE  # Allow LaTeX formatting
  ) %>%
  kable_styling(
    latex_options = c("HOLD_position"),
    full_width = FALSE
  ) %>%
  row_spec(
    which(results$method == "OLS"),  # Select rows where method is OLS
    bold = TRUE                       # Make the entire row bold
  )
