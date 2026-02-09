library(tidyverse)
library(broom)
library(effsize)
library(texreg)
library(sandwich)
library(lmtest)


# Reading in data: Large
df_epd_clean = read_csv("/Users/teorichard/Downloads/UCD Research/AQ UCD/cleaned_data/LARGE_df_epd_clean.csv") %>% select(-hhid)
df_paqi_clean = read_csv("/Users/teorichard/Downloads/UCD Research/AQ UCD/cleaned_data/LARGE_df_paqi_clean.csv") %>% select(-hhid)
df_full_clean_catedu = read_csv("/Users/teorichard/Downloads/UCD Research/AQ UCD/cleaned_data/LARGE_df_full_clean_catedu.csv") %>% select(-hhid)



mean(full_df_clean$wtp_paqi)
mean(full_df_clean$wtp_epd)
mean(full_df_clean$wtp_dif)
sd_wtp_df = sd(full_df_clean$wtp_dif) # 24.65408
24.65408 / 228


# Average coefficient between treatment only and treatment + covariates using full data when predicting wtp_dif: 
#   - epd_treatment_baseline = -32.039

coef = -32.039

# Standard deviation of wtp_dif = 24.65408
-32.039 / 26.65408
# Number of SDs: -1.20203

# Interpretation: This treatment effect of about -32 PKR represents 1.2 standard deviations of the relative WTP distribution (SD = 24.654). 
# This indicates a very large effect size.

# The relevant question is not: how much is this in absolute terms?
# The relevant question is: How much variation is treatment associated with versus natural variation in preferences?

# Cohen's d for treatment only to predict wtp_dif:

treatment_only_mod_cohen_d = cohen.d(wtp_dif ~ epd_treatment_baseline, data = df_full_clean_catedu_wtp_dif)

# d estimate: 1.712696 (large)
# 95 percent confidence interval:
#    lower    upper 
# 1.562148 1.863243 


ggplot(df_full_clean_catedu) +
    geom_density(aes(x = wtp_dif - coef, fill = "WTP - Est. Treatment Effect"), color = "#ff8737", alpha = 0.4) +
    geom_density(aes(x = wtp_dif + coef, fill = "WTP + Est. Treatment Effect"), color = "#8564e8", alpha = 0.4) +
    geom_density(aes(x = wtp_dif, fill = "Observed WTP"), color = "#717171", alpha = 0.07) +
    geom_vline(xintercept = mean(wtp_dif) - coef, color = "black") +
    geom_vline(xintercept = mean(wtp_dif) + coef, color = "black") +
    geom_vline(xintercept = mean(wtp_dif) - coef - sd_wtp_df, linetype = "dashed", color = "#ff8737") +
    geom_vline(xintercept = mean(wtp_dif) - coef + sd_wtp_df, linetype = "dashed", color = "#ff8737") +
    geom_vline(xintercept = mean(wtp_dif) + coef - sd_wtp_df, linetype = "dashed", color = "#8564e8") +
    geom_vline(xintercept = mean(wtp_dif) + coef + sd_wtp_df, linetype = "dashed", color = "#8564e8") +
    labs(
        x = "Relative WTP",
        y = "Density",
        title = "Distributions of Relative Willingness to Pay",
        subtitle = "Dashed lines indicate ±1SD",
        fill = "Treatment Effect Distribution \n(Est. Effect = -32 PKR)"
    ) +
    scale_fill_manual(
        values = c(
        "WTP - Est. Treatment Effect" = "#ff8737",
        "WTP + Est. Treatment Effect" = "#8564e8",
        "Observed WTP" = "#848484"
    )) +
    theme_minimal() +
    theme(
        axis.title.x = element_text(margin = margin(t = 15)),
        axis.title.y = element_text(margin = margin(r = 15)),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(margin = margin(b = 15), hjust = 0.5, face = "italic")
    )

ggsave("linear_regression_treatment_effect/treatment_effect_distr.png", width = 12, height = 5, dpi = 300)



treat_only_rsquared = 0.424
treat_with_vars_rsquared = 0.460
no_treat_rsquared = 0.0709
treat_only_rsquared / no_treat_rsquared # 5.980254

# Treatment explains about 6x more variance in wtp_dif than all variables combined


# Interpretation: Treatment creates preference shifts that are larger than the natural variation in service preferences across the population. 
# This dominance explains why treatment alone accounts for 42% of variance while all other factors combined account for only 7%.