library(tidyverse)
library(broom)
library(effsize)
library(texreg)
library(sandwich)
library(lmtest)

# This will analyze absolute WTP: for just PAQI alone, just EPD alone, and total WTP (WTP PAQI + WTP EPD)

# Note: using categorical education


# Reading in data: Large
df_epd_clean = read_csv("/Users/teorichard/Downloads/UCD Research/AQ UCD/cleaned_data/LARGE_df_epd_clean.csv") %>% select(-hhid)
df_paqi_clean = read_csv("/Users/teorichard/Downloads/UCD Research/AQ UCD/cleaned_data/LARGE_df_paqi_clean.csv") %>% select(-hhid)
# df_full_clean_contedu = read_csv("/Users/teorichard/Downloads/UCD Research/AQ UCD/cleaned_data/LARGE_df_full_clean.csv") %>% select(-hhid) # this one has continuous education
df_full_clean_catedu = read_csv("/Users/teorichard/Downloads/UCD Research/AQ UCD/cleaned_data/LARGE_df_full_clean_catedu.csv") %>% select(-hhid)


# Dropping outcome variables we are not looking at
drop_vars_wtp_paqi = c("pref_baseline", "pref_endline", "wtp_epd", "wtp_dif")
drop_vars_wtp_epd = c("pref_baseline", "pref_endline", "wtp_paqi", "wtp_dif")


df_full_clean_wtp_paqi = df_full_clean_catedu %>% dplyr::select(-all_of(drop_vars_wtp_paqi))
df_full_clean_wtp_epd = df_full_clean_catedu  %>% dplyr::select(-all_of(drop_vars_wtp_epd))
df_full_clean_wtp_total = df_full_clean_catedu %>% 
    mutate(wtp_total = wtp_paqi + wtp_epd) %>% 
    dplyr::select(-all_of(c("pref_baseline", "pref_endline", "wtp_paqi", "wtp_epd")))
# df_full_clean_contedu_wtp_dif = df_full_clean_contedu %>% 
#                            dplyr::select(-all_of(c("pref_baseline", "pref_endline", "wtp_paqi", "wtp_epd")))       


#######
# Overview:
# Look at 1 models to predict absolute total WTP (WTP PAQI, WTP EPD and WTP TOTAL = WTP PAQI + WTP EPD):
# 1. Treatment only 
# Only this model because we are just asking: 
#   - Does treatment explain as much variation in absolute WTP as it did in relative WTP?

# ::::::::::::::::::::::::: PREDICTING ABSOLUTE WTP *PAQI* :::::::::::::::::::::::::

# ----- PREDICTING WTP PAQI, TREATMENT ONLY -----
paqi_mod = lm(wtp_paqi ~ epd_treatment_baseline, 
           data = df_full_clean_wtp_paqi)
paqi_mod_robust = coeftest(paqi_mod, vcov = vcovHC(paqi_mod, type = "HC3"))
summary(paqi_mod)

# R-squared = 0.012
# F statistic pvalue = 0.001
# epd_treatment_baseline coefficient = -15.137, p = 0.001



# ----- PREDICTING WTP EPD, TREATMENT ONLY -----
epd_mod = lm(wtp_epd ~ epd_treatment_baseline, 
           data = df_full_clean_wtp_epd)
epd_mod_robust = coeftest(epd_mod, vcov = vcovHC(epd_mod, type = "HC3"))
summary(epd_mod)

# R-squared = 0.015
# F-statistic p-value = 0.000
# epd_tratment_baseline estimate = 16.939, pvalue = 0.000


# ----- PREDICTING WTP TOTAL, TREATMENT ONLY -----

total_wtp_mod = lm(wtp_total ~ epd_treatment_baseline, 
           data = df_full_clean_wtp_total)
total_wtp_mod_robust = coeftest(total_wtp_mod, vcov = vcovHC(total_wtp_mod, type = "HC3"))
summary(total_wtp_mod)

# R-squared = about 0
# F statistic pvaleu = 0.839
# epd_treatment_baseline estimate = 1.802, pvalue = 0.839



# So basically the R-squared for absolute WTP PAQI and absolute WTP EPD is quite low and is about 0 for total WTP
# Treatment does *not* predict absolute and total willingness top pay very well


print("Done.")