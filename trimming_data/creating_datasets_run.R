library(purrr)
library(readr)
library(tidyverse)
library(haven)
library(car)
library(recipes)
library(forcats)

# Find the actual problem: duplicate or perfectly correlated variables

# find_perfect_correlations <- function(data) {
#   numeric_data <- data[sapply(data, is.numeric)]
  
#   cat("Checking", ncol(numeric_data), "numeric variables for perfect correlations\n\n")
  
#   # Calculate correlation matrix
#   cor_matrix <- cor(numeric_data, use = "pairwise.complete.obs")
  
#   # Find near-perfect correlations (>0.9999)
#   cor_matrix_abs <- abs(cor_matrix)
#   diag(cor_matrix_abs) <- 0  # Ignore diagonal
  
#   # Find pairs
#   high_cor_idx <- which(cor_matrix_abs > 0.9999, arr.ind = TRUE)
#   high_cor_idx <- high_cor_idx[high_cor_idx[,1] < high_cor_idx[,2], , drop = FALSE]
  
#   if(nrow(high_cor_idx) == 0) {
#     cat("No perfect correlations found!\n")
#     cat("Checking for correlations > 0.999:\n\n")
    
#     high_cor_idx <- which(cor_matrix_abs > 0.999, arr.ind = TRUE)
#     high_cor_idx <- high_cor_idx[high_cor_idx[,1] < high_cor_idx[,2], , drop = FALSE]
    
#     if(nrow(high_cor_idx) == 0) {
#       cat("No correlations > 0.999 found either.\n")
#       cat("The problem is not perfect multicollinearity.\n")
#       cat("Check if you have the right data or if imputation is causing issues.\n")
#       return(invisible(NULL))
#     }
#   }
  
#   cat("Variables with correlations > 0.999:\n\n")
  
#   problem_vars <- character()
  
#   for(i in 1:nrow(high_cor_idx)) {
#     var1 <- rownames(cor_matrix)[high_cor_idx[i,1]]
#     var2 <- colnames(cor_matrix)[high_cor_idx[i,2]]
#     cor_val <- cor_matrix[high_cor_idx[i,1], high_cor_idx[i,2]]
    
#     cat(sprintf("%.8f : %s <-> %s\n", cor_val, var1, var2))
    
#     problem_vars <- c(problem_vars, var1, var2)
#   }
  
#   problem_vars <- unique(problem_vars)
  
#   cat("\n\nSuggested variables to remove (appear in multiple perfect correlations):\n")
  
#   # Count how many times each variable appears
#   var_counts <- table(c(
#     rownames(cor_matrix)[high_cor_idx[,1]],
#     colnames(cor_matrix)[high_cor_idx[,2]]
#   ))
  
#   var_counts_sorted <- sort(var_counts, decreasing = TRUE)
#   print(var_counts_sorted[var_counts_sorted > 1])
  
#   cat("\n\nRECOMMENDATION: Remove these variables from your dataset:\n")
#   cat(paste(names(var_counts_sorted)[1:min(5, length(var_counts_sorted))], collapse = "\n"))
#   cat("\n")
  
#   return(invisible(list(
#     problem_pairs = high_cor_idx,
#     problem_vars = problem_vars,
#     var_counts = var_counts
#   )))
# }

# find_perfect_correlations(analysis_orig)

# Run this on your data BEFORE any filtering:
# Load your original dataset, then:
# find_perfect_correlations(analysis_orig)

# Or run it after imputation to see if imputation is causing the issue:
# find_perfect_correlations(full_df_imputed)

source("trimming_data/create_fns.R")

# Full dataset ------------------------------------------------------------
full_analysis = read_dta("original_files/stata files/Analysis_data_saved.dta")
hhid = full_analysis$hhid
dem_q7_baseline = full_analysis$dem_q7_baseline


# # SMALL dataset:
# analysis_orig = read_dta("original_files/stata files/Analysis_data_small/Analysis_data_sheared_baseline.dta")
# dataset_type = "small"

# LARGE dataset:
analysis_orig = read_dta("original_files/stata files/Analysis_data_large/Analysis_data_basline_all_reasonable copy.dta")
analysis_orig = analysis_orig %>% 
  dplyr::select(-diff_donation_govt_W, -a_dr_epd, -a_dr_paqi) %>% 
  mutate(hhid = hhid, dem_q7_baseline = full_analysis$dem_q7_baseline)
#analysis_orig = analysis_orig[analysis_orig$s2_q3_baseline != 150, ]

analysis = analysis_orig
# ----- RECODE VARIABLES -----
analysis = analysis %>%
  mutate(dem_q7_baseline = case_when(
    dem_q7_baseline %in% c(888, 999) ~ NA_character_,  # Drop don't know/refused
    dem_q7_baseline == 0 | dem_q7_baseline == 17 ~ "No formal education",
    dem_q7_baseline >= 1 & dem_q7_baseline <= 5 ~ "Primary (1-5)",
    dem_q7_baseline >= 6 & dem_q7_baseline <= 8 ~ "Middle (6-8)",
    dem_q7_baseline >= 9 & dem_q7_baseline <= 10 ~ "Secondary/Matric",
    dem_q7_baseline == 11 ~ "Intermediate",
    dem_q7_baseline == 12 ~ "University",
    dem_q7_baseline == 13 ~ "Postgraduate",
    dem_q7_baseline %in% c(14, 15, 16) ~ "Other education",
    TRUE ~ NA_character_
  ))

# Make it an ordered factor
analysis = analysis %>%
  mutate(dem_q7_baseline = factor(dem_q7_baseline, 
                                  levels = c("No formal education", 
                                            "Primary (1-5)", 
                                            "Middle (6-8)", 
                                            "Secondary/Matric",
                                            "Intermediate",
                                            "University",
                                            "Postgraduate",
                                            "Other education"),
                                  ordered = TRUE))
dataset_type = "large"
# Note: for the LARGE dataset, I already did some cleaning i.e. dropped
#   vars with high freq of same value or high freq of missing values


# ----- GET RID OF COLLINEAR VARIABLES -----

if (dataset_type == "large") {
  analysis = analysis %>% dplyr::select(
    -s9_q5_1_baseline, # Too many factors
    -inside_total_hrs_baseline, # correlated with outside total hours
     -dem_q4_baseline, # correlated with "are there any elderly living with you?"
     -dem_q6_baseline, # correlated with "are there any young children living with you?"
     -s3_q11_baseline, # correlated with s3_q13_baseline
 # correlated with s3_q13_baseline
    -s3_q15_baseline # correlated with s3_q13_baseline
    ) 

}
# ----- I FORGOT TO DROP IN STATA -----
analysis = analysis %>% 
  dplyr::select(-formdef_version_baseline, -own_house_baseline, -mean_pm2_5_airnow_baseline)

# I'm adding these variables to the "small" dataset because I didn't do any cleaning in 
#   stata for that and wanted to include these in my pre-cleaned "small" dataset.
# But for the "large" one, these were all included and any that are no longer there
#   were just dropped in Stata due to Missing Values cleaning or High Freq cleaning
# if (dataset_type == "small"){
#   nums = c(13:30)
#   vars = c()
#   for (i in nums){
#     new_var = paste0("dem_q", i, "_baseline")
#     if (new_var %in% names(full_analysis) && !(new_var %in% names(analysis)))
#     vars = c(vars, new_var)
#   }
#   vars = c(vars, "dem_q21a_baseline")
#   # dem_q21a_baseline is Rikshaw

#   added_vars = full_analysis %>% 
#     dplyr::select(all_of(vars)) %>% 
#     mutate(leisure_total_hrs_baseline = full_analysis$leisure_total_hrs_baseline)

#   analysis = analysis %>% bind_cols(added_vars)
# }

labeled_vars = map_lgl(analysis, ~ !is.null(attr(.x, "labels")))
var_names = names(labeled_vars[which(labeled_vars == TRUE)])

analysis = analysis %>% 
  mutate(across(where(~ is.labelled(.x) & !is.null(.x)), as_factor))


likert_cols = get_lgl(analysis, "agree|disagree")
yesno_cols = setdiff(get_lgl(analysis, "\\byes\\b|\\bno\\b"), "dem_q7_baseline")
avg_cols = get_lgl(analysis, "average|avereage")

analysis = analysis %>%
  mutate(across(all_of(likert_cols), ~ case_when(
    str_to_lower(.x) == "strongly disagree" ~ 1,
    str_to_lower(.x) == "disagree" ~ 2,
    str_to_lower(.x) == "neither agree nor disagree" ~ 3,
    str_to_lower(.x) == "agree" ~ 4,
    str_to_lower(.x) == "strongly agree" ~ 5,
    TRUE ~ NA_real_
  )))

analysis = analysis %>% 
  mutate(across(all_of(yesno_cols), ~ case_when(
    str_to_lower(.x) == "yes" ~ 1,
    str_to_lower(.x) == "no" ~ 0,
    TRUE ~ NA
  )))

analysis = analysis %>%
  mutate(across(all_of(avg_cols), ~ case_when(
    str_to_lower(.x) %in% c("a lot more than average", "a lot more than avereage") ~ 1,
    str_to_lower(.x) %in% c("somewhat more than average", "somewhat more than avereage") ~ 2,
    str_to_lower(.x) %in% c("average", "avereage") ~ 3,
    str_to_lower(.x) %in% c("somewhat less than average", "somewhat less than avereage") ~ 4,
    str_to_lower(.x) %in% c("a lot less than average", "a lot less than avereage") ~ 5,
    TRUE ~ NA_real_
  )))


analysis = analysis %>% 
  mutate(s7_q10_baseline = case_when(
    s7_q10_baseline == "None" ~ 0,
    s7_q10_baseline == "One" ~ 1,
    s7_q10_baseline == "two" ~ 2,
    s7_q10_baseline == "three" ~ 3,
    s7_q10_baseline == "four" ~ 4,
    s7_q10_baseline == "five" ~ 5,
    s7_q10_baseline == "six" ~ 6,
    s7_q10_baseline == "seven" ~ 7,
    TRUE ~ NA_real_  # Handle any unexpected values
  ))


# ----- DROPPING NA VALUES -----

# Calculate proportion of NAs for each column
na_prop = colMeans(is.na(analysis))
names(analysis)[na_prop >= 0.15]
# Keep only columns with less than 85% NAs
analysis = analysis[, na_prop < 0.85]

# Fixing the tehsil_n_baseline one to be numeric
analysis = analysis %>% 
  mutate(tehsil_n_baseline = case_when(
    tehsil_n_baseline == "Shalamar" ~ 0,
    tehsil_n_baseline == "City" ~ 1,
    TRUE ~ NA
  ))




# ------ CREATE pref_diff AND pref COLUMNS ------
pref_diff_baseline = analysis_orig$pref_cit_group_index_baseline - analysis_orig$pref_govt_index_baseline
pref_diff_endline = analysis_orig$pref_cit_group_index_endline - analysis_orig$pref_govt_index_endline

analysis = analysis %>% 
  mutate(
    pref_baseline = ifelse(pref_diff_baseline > 0, 1, 0),
    pref_endline = ifelse(pref_diff_endline > 0, 1, 0)) %>%
    dplyr::select(-c("pref_govt_index_baseline", "pref_govt_index_endline", "pref_cit_group_index_baseline", "pref_cit_group_index_endline"))

# ------ CREATE wtp_paqi AND wtp_epd COLUMNS ------
analysis = analysis %>% 
  mutate(
    wtp_paqi = ifelse(epd_treatment_baseline == 0, wtp_bdm_1_endline, wtp_bdm_1a_endline),
    wtp_epd = ifelse(epd_treatment_baseline == 1, wtp_bdm_1_endline, wtp_bdm_1a_endline)
  ) %>% 
  dplyr::select(
    -all_of(c("wtp_bdm_1_endline", "wtp_bdm_1a_endline"))
  )

# ------ DROPPING OUTCOME NAs ------

analysis = analysis[!is.na(analysis$wtp_paqi), ]
analysis = analysis[!is.na(analysis$wtp_epd), ]

analysis = analysis[!is.na(analysis$pref_baseline), ]
analysis = analysis[!is.na(analysis$pref_endline), ]




# ------ SPLIT INTO TREATMENT GROUPS ------
# keep epd_treatment_baseline for now because we need it for OLS laster

epd_df = analysis %>% 
  filter(epd_treatment_baseline == 1) 

paqi_df = analysis %>% 
  filter(epd_treatment_baseline == 0)

full_df = analysis



# ------ CHECK HIGH FREQUENCY ------

drop_epd = drop_high_freq(epd_df)
drop_paqi = drop_high_freq(paqi_df)
drop_full = drop_high_freq(full_df)

drop_vars_high_freq = unique(c(drop_epd, drop_paqi, drop_full))

epd_df_no_high_freq = epd_df %>% dplyr::select(-all_of(drop_vars_high_freq))
paqi_df_no_high_freq = paqi_df %>% dplyr::select(-all_of(drop_vars_high_freq))
full_df_no_high_freq = full_df %>% dplyr::select(-all_of(drop_vars_high_freq))


# ------ IMPUTE ------

epd_df_imputed = impute(epd_df_no_high_freq)
paqi_df_imputed = impute(paqi_df_no_high_freq)
full_df_imputed = impute(full_df_no_high_freq)

# ------ SET UP FOR VIF ------
# Drop the outcome variables not looking at because they aren't predictor variables so I don't want them in my VIF calculations
# Some of these are unused but I've left them in case I want to replicate my earlier problems with the VIF calculations

vif_pref_b = c("wtp_paqi", "wtp_epd", "pref_endline", "epd_treatment_baseline")
vif_pref_e = c("wtp_paqi", "wtp_epd", "pref_baseline", "epd_treatment_baseline")

vif_wtp_paqi = c("wtp_epd", "pref_baseline", "pref_endline", "epd_treatment_baseline")
vif_wtp_epd = c("wtp_paqi", "pref_baseline", "pref_endline", "epd_treatment_baseline")

full_pref_b = full_df_imputed %>% dplyr::select(-all_of(vif_pref_b))
full_pref_e = full_df_imputed %>% dplyr::select(-all_of(vif_pref_e))
full_wtp_paqi = full_df_imputed %>% dplyr::select(-all_of(vif_wtp_paqi))
full_wtp_epd= full_df_imputed %>% dplyr::select(-all_of(vif_wtp_epd))

# Checking to make sure matrix is invertible
vif_prune_diagnostic(full_pref_b, "pref_baseline", threshold = 2.5)[[2]]
vif_prune_diagnostic(full_pref_e, "pref_endline", threshold = 2.5)[[2]]
vif_prune_diagnostic(full_wtp_paqi, "wtp_paqi", threshold = 2.5)[[2]]
vif_prune_diagnostic(full_wtp_epd, "wtp_epd", threshold = 2.5)[[2]]





# ^ The same variables are an issue in each one (in the LARGE data)
#s3_q11_baseline
#s3_q13_baseline
# Removed s3_q11_baseline at top after loading in dataset


# pd_pref_b = epd_df_imputed %>% dplyr::select(-all_of(vif_pref_b))
# epd_pref_e = epd_df_imputed %>% dplyr::select(-all_of(vif_pref_e))
# paqi_pref_b = paqi_df_imputed %>% dplyr::select(-all_of(vif_pref_b))
# paqi_pref_e = paqi_df_imputed %>% dplyr::select(-all_of(vif_pref_e))

# epd_wtp1_e = epd_df_imputed %>% dplyr::select(-all_of(vif_wtp1_e))
# epd_wtp1a_e = epd_df_imputed %>% dplyr::select(-all_of(vif_wtp1a_e))
# paqi_wtp1_e = paqi_df_imputed %>% dplyr::select(-all_of(vif_wtp1_e))
# aqi_wtp1a_e = paqi_df_imputed %>% dplyr::select(-all_of(vif_wtp1a_e))

# ------ CHECK VIF ------

a = vif_prune(data = full_pref_b, outcome = "pref_baseline")
b = vif_prune(full_pref_e, "pref_endline")
c = vif_prune(full_wtp_paqi, "wtp_paqi")
d = vif_prune(full_wtp_epd, "wtp_epd")




# This is just if the VIF checks fail due to perfect multicollinearity.
# It doesn't do anything just prints out correlation above 0.9 and then
#   go drop variables in Stata
check_cor_table = "false"
if (check_cor_table == "true") {
  # this part isn't correlation but looking at variance
  sapply(full_pref_b, function(x) var(x, na.rm = TRUE))
  # Calculate correlation matrix (numeric variables only)
  cor_matrix <- cor(full_pref_b[sapply(full_pref_b, is.numeric)], use = "complete.obs")

  # Find pairs with correlation > 0.9
  high_cor <- which(abs(cor_matrix) > 0.9 & upper.tri(cor_matrix), arr.ind = TRUE)

  # Get variable names and correlation values
  if(nrow(high_cor) > 0) {
    high_cor_pairs <- data.frame(
      var1 = rownames(cor_matrix)[high_cor[,1]],
      var2 = colnames(cor_matrix)[high_cor[,2]],
      correlation = cor_matrix[high_cor]
    )
    print(high_cor_pairs)
  } else {
    print("No variable pairs with correlation > 0.9")
  }
}

vif_to_drop = setdiff(unique(c(a[[2]], b[[2]], c[[2]], d[[2]])), "hhid")

full_df_viffed = full_df_imputed %>% dplyr::select(-all_of(vif_to_drop)) %>% 
                  mutate(wtp_dif = wtp_paqi - wtp_epd) %>% 
                  mutate(across(where(is.character), as.factor))
epd_df_viffed = epd_df_imputed %>% dplyr::select(-all_of(vif_to_drop)) %>% 
                  mutate(wtp_dif = wtp_paqi - wtp_epd) %>% 
                  mutate(across(where(is.character), as.factor))
paqi_df_viffed = paqi_df_imputed %>% dplyr::select(-all_of(vif_to_drop)) %>% 
                  mutate(wtp_dif = wtp_paqi - wtp_epd) %>% 
                  mutate(across(where(is.character), as.factor))

which(sapply(full_df_clean, function(x) class(x)) == "character")

# ------ COMBINE RARE FACTOR LEVELS ------
combine_rare_levels = function(df, min_prop = 0.01) {
  df %>%
    mutate(across(where(~ is.factor(.) || is.character(.)), 
                  ~ fct_lump_prop(as.factor(.), prop = min_prop)))
}

full_df_clean = combine_rare_levels(full_df_viffed, min_prop = 0.02)
epd_df_clean = combine_rare_levels(epd_df_viffed, min_prop = 0.02)
paqi_df_clean = combine_rare_levels(paqi_df_viffed, min_prop = 0.02)

# ------ WRITING (keep commented out unless you want to write for sure) ------

# write_csv(epd_df_clean, "cleaned_data/LARGE_df_epd_clean.csv")
# write_csv(paqi_df_clean, "cleaned_data/LARGE_df_paqi_clean.csv")
# write_csv(full_df_clean, "cleaned_data/LARGE_df_full_clean.csv")

# write_csv(epd_df_clean, "cleaned_data/df_epd_clean.csv")
# write_csv(paqi_df_clean, "cleaned_data/df_paqi_clean.csv")
# write_csv(full_df_clean, "cleaned_data/df_full_clean.csv")

ncol(df_full_clean)

# df_full_clean = read_csv("/Users/teorichard/Downloads/UCD Research/AQ UCD/cleaned_data/df_full_clean.csv",)
which(sapply(df_full_clean, function(x) class(x)) == "character")
