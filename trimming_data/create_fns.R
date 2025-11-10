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


