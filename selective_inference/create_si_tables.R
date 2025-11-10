library(tidyverse)
library(kableExtra)

bound_tib = read_csv("selective_inference/selective_inference_all_selected_vars.csv")
filtered_tib = read_csv("selective_inference/selective_inference_significant_vars.csv")

p_e = filtered_tib[, 1:3] %>% filter(!is.na(.[[1]]))
p_p = filtered_tib[, 4:6] %>% filter(!is.na(.[[1]]))
c = filtered_tib[, 7:9] %>% filter(!is.na(.[[1]]))
e_p = filtered_tib[, 10:12] %>% filter(!is.na(.[[1]]))
e_e = filtered_tib[, 13:15] %>% filter(!is.na(.[[1]]))
f = filtered_tib[, 16:18] %>% filter(!is.na(.[[1]]))


p_e %>% 
    kable(
        format = "latex",
        booktabls = TRUE,
        col.names = c("Variable", "Estimate", "Std. P-Value"),
        digits = 3,
        caption = "Treatment = PAQI, Predict WTP EPD: Significant Variables (P < 0.05)") %>% 
  kable_styling(
    latex_options = c("HOLD_position"), 
    full_width = FALSE 
  )


p_p %>% 
    kable(
        format = "latex",
        booktabls = TRUE,
        col.names = c("Variable", "Estimate", "Std. P-Value"),
        digits = 3,
        caption = "Treatment = PAQI, Predict WTP PAQI: Significant Variables (P < 0.05)") %>% 
  kable_styling(
    latex_options = c("HOLD_position"), 
    full_width = FALSE 
  )

e_p %>% 
    kable(
        format = "latex",
        booktabls = TRUE,
        col.names = c("Variable", "Estimate", "Std. P-Value"),
        digits = 3,
        caption = "Treatment = EPD, Predict WTP PAQI: Significant Variables (P < 0.05)") %>% 
  kable_styling(
    latex_options = c("HOLD_position"), 
    full_width = FALSE 
  )

e_e %>% 
    kable(
        format = "latex",
        booktabls = TRUE,
        col.names = c("Variable", "Estimate", "Std. P-Value"),
        digits = 3,
        caption = "Treatment = EPD, Predict WTP EPD: Significant Variables (P < 0.05)") %>% 
  kable_styling(
    latex_options = c("HOLD_position"), 
    full_width = FALSE 
  )
