get_lgl = function(data, grepl_phrase) {
  cols_lgl = map_lgl(analysis, ~ any(grepl(grepl_phrase, .x, ignore.case = TRUE)))
  names = names(which(cols_lgl == TRUE))
  return(names)
}

# Check NA values -------------------------------------------------------



# Check Frequencies -------------------------------------------------------

# Check frequencies
check_frequency = function(df) {
  # Don't analyze these
  dont_analyze = c("pref_endline", "epd_treatment_baseline")
  cols_to_check = setdiff(names(df), dont_analyze)
  
  map_dfr(cols_to_check, function(col) {
    val = df[[col]]
    unique_vals = n_distinct(val)
    
    ptab = base::sort(prop.table(table(val, useNA = "no")), decreasing = TRUE)
    freq = round(ptab[1], 2)
    mode_val = names(ptab)[1]
    
    tibble(
      variable = col,
      freq = freq,
      mode = mode_val,  # Keep as character to avoid conversion issues
      unique = unique_vals
    )
  })
}

# Run check_frequency() and then drop the high frequency vars
# **Must be ran with a df of the correct structure, e.g. a column called "variable"
# ^ here, we're running it with the tibble from check_frequency
drop_high_freq = function(df) {
  frequencies = check_frequency(df)
  high_freq_vars = frequencies %>% filter(freq >= 0.85)
  high_freq_vars = unlist(high_freq_vars[, 1])

  return(high_freq_vars)
}



# Imputing ----------------------------------------------------------------

impute = function(df) {
  na_vars = df %>%
    dplyr::select(where(~ sum(is.na(.x)) != 0)) %>%
    summarize(across(everything(), ~ sum(is.na(.x)))) %>%
    unlist()

  rec = recipe(df) %>%  # No formula needed for imputation
    step_impute_median(all_numeric()) %>%
    step_impute_mode(all_nominal())

  prep_rec = prep(rec)
  df_full = bake(prep_rec, new_data = NULL)  # Also use bake() instead of juice()
  return(df_full)
}


# Checking VIF ------------------------------------------------------------

vif_prune_old = function(data, outcome, threshold = 2.5, data_type) {
  # All variables except the outcome variable
  predictors = setdiff(names(data), outcome)
  # Initializing the list we will keep all the dataframes in as we iterate
  history = list()
  dropped_vars = c()
  iter = 1

  # Looping until VIF is under the threshold
  while (TRUE) {

    if (length(predictors) < 2) {
      warning("Length of predictors is less than 2 (btw this is a manual warning)")
      break
    }

    form = as.formula(paste(outcome, "~", paste(predictors, collapse = " + ")))

    # Fit VIF on model
    if (data_type == "continuous") {
      fit = lm(form, data = data)
    } else if (data_type == "categorical") {
      fit = glm(form, data = data, family = binomial())
    }

    fit_vif = vif(fit)

    # Create dataframe with variable and VIF value
    vif_df = tibble(var = names(fit_vif[, 1]), fit_vif = fit_vif[, 3]) %>% arrange(desc(fit_vif))

    # Add the dataframe to our list of dataframes
    history[[iter]] = vif_df

    max_vif_df = vif_df[[1, 2]]
    if (max_vif_df <= threshold) {
      break
    }

    # Excluding the variable with the highest VIF from our potential predictors
    worst_var = vif_df[[1, 1]]
    dropped_vars = c(dropped_vars, worst_var)
    predictors = setdiff(predictors, worst_var)
    iter = iter + 1

  }


  # Return the predictors we kept as well as the history of dataframes
  list(
    kept_predictors = predictors,
    dropped_predictors = dropped_vars,
    vif_history = history
  )
}

vif_prune_diagnostic = function(data, outcome, threshold = 2.5) {
  # Keep only numeric variables
  numeric_data = data[sapply(data, is.numeric)]
  predictors = setdiff(names(numeric_data), outcome)
  
  # Check 1: Remove zero variance variables
  variances = sapply(numeric_data[predictors], var, na.rm = TRUE)
  zero_var = names(variances[variances < 1e-10])
  if(length(zero_var) > 0) {
    message(paste("Removing zero variance variables:", paste(zero_var, collapse = ", ")))
    predictors = setdiff(predictors, zero_var)
  }
  
  # Check 2: Remove variables with too many NAs
  na_prop = sapply(numeric_data[predictors], function(x) mean(is.na(x)))
  high_na = names(na_prop[na_prop > 0.9])
  if(length(high_na) > 0) {
    message(paste("Removing high NA variables:", paste(high_na, collapse = ", ")))
    predictors = setdiff(predictors, high_na)
  }
  
  # Check 3: Check condition number
  X = as.matrix(numeric_data[, predictors])
  X = X[complete.cases(X), ]  # Remove NA rows
  cor_matrix = cor(X)
  
  # Check condition number
  eigenvalues = eigen(cor_matrix)$values
  condition_number = max(eigenvalues) / min(eigenvalues)
  message(paste("Condition number:", condition_number))
  
  if(condition_number > 1e15) {
    message("Matrix is singular. Removing variables one by one...")
    
    # Remove variables iteratively until matrix is invertible
    while(length(predictors) > 2) {
      X = as.matrix(numeric_data[, predictors])
      X = X[complete.cases(X), ]
      cor_matrix = cor(X)
      
      # Try to invert
      invertible = tryCatch({
        solve(cor_matrix)
        TRUE
      }, error = function(e) FALSE)
      
      if(invertible) break
      
      # Remove variable with highest correlation sum
      cor_sums = rowSums(abs(cor_matrix)) - 1  # Subtract diagonal
      worst_var = names(which.max(cor_sums))
      message(paste("Removing:", worst_var))
      predictors = setdiff(predictors, worst_var)
    }
  }

}


vif_prune = function(data, outcome, threshold = 2.5) {
  # Keep only numeric variables
  numeric_data = data[sapply(data, is.numeric)]
  predictors = setdiff(names(numeric_data), outcome)
  
  history = list()
  dropped_vars = c()
  iter = 1
  
  while (length(predictors) >= 2) {
    # Calculate VIF from correlation matrix - no model needed!
    X = as.matrix(numeric_data[, predictors])
    cor_matrix = cor(X, use = "complete.obs")
    vif_values = diag(solve(cor_matrix))
    
    vif_df = tibble(
      var = names(vif_values),
      fit_vif = vif_values
    ) %>% arrange(desc(fit_vif))
    
    history[[iter]] = vif_df
    
    if (vif_df[[1, 2]] <= threshold) {
      break
    }
    
    # Drop highest VIF variable
    worst_var = vif_df[[1, 1]]
    dropped_vars = c(dropped_vars, worst_var)
    predictors = setdiff(predictors, worst_var)
    iter = iter + 1
  }
  
  list(
    kept_predictors = predictors,
    dropped_predictors = dropped_vars,
    vif_history = history
  )
}


find_extreme_outliers = function(data, z_threshold = 4) {
  numeric_data = data[sapply(data, is.numeric)]
  
  # Track which rows have extreme outliers
  rows_to_drop = c()
  
  outlier_summary = map_dfr(names(numeric_data), function(var) {
    x = data[[var]]  # Use original data to keep row indices
    
    if(!is.numeric(x)) return(NULL)
    
    # get statistics
    min = min(x, na.rm = TRUE)
    mean = mean(x, na.rm = TRUE)
    med = median(x, na.rm = TRUE)
    max = max(x, na.rm = TRUE)

    # Calculate z-scores
    z_scores = abs((x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE))
    
    # Find rows with extreme values
    extreme_rows = which(z_scores > z_threshold & !is.na(z_scores))  # Added !is.na check
    extreme_values = x[extreme_rows]
    
    # Add to rows to drop
    if(length(extreme_rows) > 0) {
      rows_to_drop <<- unique(c(rows_to_drop, extreme_rows))
    }
    
    tibble(
      variable = var,
      n_extreme = length(extreme_rows),
      pct_extreme = round(length(extreme_rows) / sum(!is.na(x)) * 100, 2),
      extreme_row_numbers = ifelse(length(extreme_rows) > 0, 
                                    paste(head(extreme_rows, 10), collapse = ", "),
                                    "none"),
      extreme_values = ifelse(length(extreme_rows) > 0,
                             paste(head(round(extreme_values, 2), 10), collapse = ", "),
                             "none"),
      min = min,
      mean = mean,
      median = med,
      max = max
    )
  })
  
  outlier_summary = outlier_summary %>% 
    filter(n_extreme > 0) %>%
    arrange(desc(n_extreme))
  
  if(nrow(outlier_summary) > 0) {
    cat("Variables with extreme outliers (z-score >", z_threshold, "):\n\n")
    print(outlier_summary, n = min(20, nrow(outlier_summary)))
  } else {
    cat("No extreme outliers found (z-score >", z_threshold, ")\n")
  }
  
  cat("\n\nTotal unique rows with extreme outliers:", length(rows_to_drop), "\n")
  cat("Percentage of data:", round(length(rows_to_drop) / nrow(data) * 100, 2), "%\n\n")
  
  return(list(
    summary = outlier_summary,
    rows_to_drop = rows_to_drop
  ))
}




check_and_remove_high_leverage = function(data, outcome_var, exclude_vars, 
                                          leverage_threshold = 0.99,
                                          verbose = TRUE) {
  # Purpose: Identify and remove observations with perfect or near-perfect leverage
  # These observations have unique combinations of predictors that make them
  # perfectly predictable, causing hat value = 1 and numerical errors
  
  # Prepare data for model
  # Remove outcome variable and any variables we don't want as predictors
  model_data = data %>% select(-all_of(exclude_vars))
  
  # Fit model to calculate leverage
  # Using all available predictors to predict the outcome
  formula_str = paste(outcome_var, "~ .")
  mod = lm(as.formula(formula_str), data = model_data)
  
  # Extract leverage values (hat values)
  # Hat value measures how unusual this observation's X values are
  # Range: 1/n to 1, where 1 = perfect leverage (unique combination)
  lev = hatvalues(mod)
  
  # Get model diagnostics
  n = nobs(mod)  # Number of observations in fitted model
  k = length(coef(mod))  # Number of parameters (including intercept)
  avg_leverage = k / n  # Average leverage across all observations
  
  # Find problematic observations
  # Threshold = 0.99 catches observations with perfect or near-perfect leverage
  # These cause numerical instability in robust SE estimation
  high_lev_obs = which(lev >= leverage_threshold)
  
  if(verbose) { # Claude created this vebose to help me figure out what is GOING ON
    cat("\n")
    cat("=================================================================\n")
    cat("HIGH LEVERAGE DIAGNOSTIC\n")
    cat("=================================================================\n")
    cat("Outcome variable:", outcome_var, "\n")
    cat("Sample size:", n, "\n")
    cat("Number of predictors (k):", k, "\n")
    cat("Average leverage (k/n):", round(avg_leverage, 4), "\n")
    cat("Maximum leverage:", round(max(lev), 4), "\n")
    cat("High leverage threshold:", leverage_threshold, "\n\n")
    
    # Report findings
    if(length(high_lev_obs) > 0) {
      cat("WARNING: Found", length(high_lev_obs), 
          "observation(s) with leverage >=", leverage_threshold, "\n")
      cat("  These observations have unique covariate patterns\n")
      cat("  Observation numbers:", paste(high_lev_obs, collapse = ", "), "\n")
      cat("  Leverage values:", paste(round(lev[high_lev_obs], 4), collapse = ", "), "\n\n")
      cat("REMOVING these observations to prevent numerical instability\n")
      cat("=================================================================\n\n")
    } else {
      cat("✓ No high-leverage observations detected\n")
      cat("  All observations have leverage <", leverage_threshold, "\n")
      cat("=================================================================\n\n")
    }
  }
  
  # Return results
  if(length(high_lev_obs) > 0) {
    return(list(
      cleaned_data = data[-high_lev_obs, ],  # Data with high-leverage obs removed
      removed_obs = high_lev_obs,            # Which observations were removed
      removed_leverage = lev[high_lev_obs],  # Their leverage values
      n_removed = length(high_lev_obs),      # Count of removed observations
      max_leverage_before = max(lev)         # Maximum leverage before removal
    ))
  } else {
    return(list(
      cleaned_data = data,                   # No changes needed
      removed_obs = integer(0),              # Nothing removed
      removed_leverage = numeric(0),
      n_removed = 0,
      max_leverage_before = max(lev)
    ))
  }
}
