library(tidyverse)
library(kableExtra)

d_e_agg = read_csv("linear_regression_exploratory/ols_expl_rmd_files/d_e_agg.csv")
d_p_agg = read_csv("linear_regression_exploratory/ols_expl_rmd_files/d_p_agg.csv")
e_d_agg = read_csv("linear_regression_exploratory/ols_expl_rmd_files/e_d_agg.csv")
e_dif_f = read_csv("linear_regression_exploratory/ols_expl_rmd_files/e_dif_f.csv")
e_dif_s = read_csv("linear_regression_exploratory/ols_expl_rmd_files/e_dif_s.csv")
ed_we_s = read_csv("linear_regression_exploratory/ols_expl_rmd_files/ed_we_s.csv")
ed_wp_s = read_csv("linear_regression_exploratory/ols_expl_rmd_files/ed_wp_s.csv")
p_d_agg = read_csv("linear_regression_exploratory/ols_expl_rmd_files/p_d_agg.csv")
p_dif_f = read_csv("linear_regression_exploratory/ols_expl_rmd_files/p_dif_f.csv")
p_dif_s = read_csv("linear_regression_exploratory/ols_expl_rmd_files/p_dif_s.csv")
pd_we_s = read_csv("linear_regression_exploratory/ols_expl_rmd_files/pd_we_s.csv")
pd_wp_s = read_csv("linear_regression_exploratory/ols_expl_rmd_files/pd_wp_s.csv")


library(kableExtra)
library(knitr)


# :::::::::: Predicting WTP ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# ---------- Different Treatments, PAQI WTP --------------------------------------------------

pd_wp_s %>% 
  kable(
    format = "latex",
    booktabs = TRUE,
    col.names = c("R-Squared", "F-Statistic", "P-value", "DF", "Sample Size", "MSE"),
    caption = "PAQI Treatment, PAQI WTP",
    digits = 3
  ) %>% 
  kable_styling(
    latex_options = c("hold_position"), 
    full_width = FALSE 
  )

ed_wp_s %>% 
  kable(
    format = "latex",
    booktabs = TRUE,
    col.names = c("R-Squared", "F-Statistic", "P-value", "DF", "Sample Size", "MSE"),
    caption = "EPD Treatment, PAQI WTP",
    digits = 3
  ) %>% 
  kable_styling(
    latex_options = c("hold_position"), 
    full_width = FALSE 
  )


d_p_agg %>% 
  kable(
    format = "latex",
    booktabs = TRUE,
    col.names = c("Variable"),
    caption = "Different Treatments, Predict WTP PAQI",
    digits = 3
  ) %>% 
  kable_styling(
    latex_options = c("hold_position"), 
    full_width = FALSE 
  )


# ---------- Different Treatments, EPD WTP --------------------------------------------------

pd_we_s %>% 
  kable(
    format = "latex",
    booktabs = TRUE,
    col.names = c("R-Squared", "F-Statistic", "P-value", "DF", "Sample Size", "MSE"),
    caption = "PAQI Treatment, EPD WTP",
    digits = 3
  ) %>% 
  kable_styling(
    latex_options = c("hold_position"), 
    full_width = FALSE 
  )



ed_we_s %>% 
  kable(
    format = "latex",
    booktabs = TRUE,
    col.names = c("R-Squared", "F-Statistic", "P-value", "DF", "Sample Size", "MSE"),
    caption = "EPD Treatment, EPD WTP",
    digits = 3
  ) %>% 
  kable_styling(
    latex_options = c("hold_position"), 
    full_width = FALSE 
  )

d_e_agg %>% 
  kable(
    format = "latex",
    booktabs = TRUE,
    col.names = c("Variable"),
    caption = "Different Treatments, Predict WTP EPD",
    digits = 3
  ) %>% 
  kable_styling(
    latex_options = c("hold_position"), 
    full_width = FALSE 
  )


# :::::::::: By Treatment ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ---------- PAQI Treatment --------------------------------------------------
pd_wp_s %>% 
  kable(
    format = "latex",
    booktabs = TRUE,
    col.names = c("R-Squared", "F-Statistic", "P-value", "DF", "Sample Size", "MSE"),
    caption = "PAQI Treatment, PAQI WTP",
    digits = 3
  ) %>% 
  kable_styling(
    latex_options = c("hold_position"), 
    full_width = FALSE 
  )

pd_we_s %>% 
  kable(
    format = "latex",
    booktabs = TRUE,
    col.names = c("R-Squared", "F-Statistic", "P-value", "DF", "Sample Size", "MSE"),
    caption = "PAQI Treatment, EPD WTP",
    digits = 3
  ) %>% 
  kable_styling(
    latex_options = c("hold_position"), 
    full_width = FALSE 
  )


p_d_agg %>% 
    kable(
        format = "latex",
        booktabls = TRUE,
        col.names = c("Variable", "PAQI WTP Estimate", "EPD WTP Estimate"),
        digits = 3,
        caption = "Treatment = PAQI") %>% 
  kable_styling(
    latex_options = c("hold_position"), 
    full_width = FALSE 
  )


# ---------- PAQI Treatment --------------------------------------------------
ed_wp_s %>% 
  kable(
    format = "latex",
    booktabs = TRUE,
    col.names = c("R-Squared", "F-Statistic", "P-value", "DF", "Sample Size", "MSE"),
    caption = "EPD Treatment, PAQI WTP",
    digits = 3
  ) %>% 
  kable_styling(
    latex_options = c("hold_position"), 
    full_width = FALSE 
  )

ed_we_s %>% 
  kable(
    format = "latex",
    booktabs = TRUE,
    col.names = c("R-Squared", "F-Statistic", "P-value", "DF", "Sample Size", "MSE"),
    caption = "Treatment = EPD",
    digits = 3
  ) %>% 
  kable_styling(
    latex_options = c("hold_position"), 
    full_width = FALSE 
  )


e_d_agg %>% 
    kable(
        format = "latex",
        booktabls = TRUE,
        col.names = c("Variable", "PAQI WTP Estimate", "EPD WTP Estimate"),
        digits = 3,
        caption = "Treatment = EPD, Predict Different WTP") %>% 
  kable_styling(
    latex_options = c("hold_position"), 
    full_width = FALSE 
  )


# :::::::::: WTP Difference ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ---------- PAQI Treatment --------------------------------------------------
p_dif_f %>% 
  kable(
    format = "latex",
    booktabs = TRUE,
    col.names = c("Variable", "Estimate", "Std. Error", "Statistic", "P-Value", "Adj. P-Value"),
    caption = "PAQI Treatment, Difference in WTP",
    digits = 3
  ) %>% 
  kable_styling(
    latex_options = c("hold_position"), 
    full_width = FALSE 
  )

p_dif_s %>% 
  kable(
    format = "latex",
    booktabs = TRUE,
    col.names = c("R-Squared", "F-Statistic", "P-value", "DF", "Sample Size", "MSE"),
    caption = "PAQI Treatment, Difference in WTP",
    digits = 3
  ) %>% 
  kable_styling(
    latex_options = c("hold_position"), 
    full_width = FALSE 
  )

# ---------- PAQI Treatment --------------------------------------------------
e_dif_f %>% 
  kable(
    format = "latex",
    booktabs = TRUE,
    col.names = c("Variable", "Estimate", "Std. Error", "Statistic", "P-Value", "Adj. P-Value"),
    caption = "EPD Treatment, Difference in WTP",
    digits = 3
  ) %>% 
  kable_styling(
    latex_options = c("hold_position"), 
    full_width = FALSE 
  )

e_dif_s %>% 
  kable(
    format = "latex",
    booktabs = TRUE,
    col.names = c("R-Squared", "F-Statistic", "P-value", "DF", "Sample Size", "MSE"),
    caption = "EPD Treatment, Difference in WTP",
    digits = 3
  ) %>% 
  kable_styling(
    latex_options = c("hold_position"), 
    full_width = FALSE 
  )
