df_full_clean = read_csv("/Users/teorichard/Downloads/UCD Research/AQ UCD/cleaned_data/LARGE_df_full_clean.csv")

original_analysis = read_dta("original_files/stata files/Analysis_data_saved.dta")
edu_tib = tibble(hhid = original_analysis$hhid, dem_q7_baseline = original_analysis$dem_q7_baseline)

# Remove duplicates from edu_tib - keep first occurrence of each hhid
edu_tib = edu_tib %>% distinct(hhid, .keep_all = TRUE)



df_full_clean = df_full_clean %>%
  select(-yrs_educ_formal_baseline, -dem_q7_baseline) %>%  # Remove old columns first
  left_join(edu_tib, by = "hhid")

df_full = df_full_clean %>%
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
df_full = df_full %>%
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

cat("Cleaning df_full\n")
lvg_df_full = check_and_remove_high_leverage(
  data = df_full,
  outcome_var = "wtp_dif",
  exclude_vars = c("epd_treatment_baseline", "wtp_paqi", "wtp_epd",
                   "pref_baseline", "pref_endline"),
  leverage_threshold = 0.99,
  verbose = TRUE
)
df_full = lvg_df_full$cleaned_data

write_csv(df_full, "/Users/teorichard/Downloads/UCD Research/AQ UCD/cleaned_data/LARGE_df_full_clean_catedu.csv")
