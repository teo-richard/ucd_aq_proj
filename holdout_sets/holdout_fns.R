library(tidyverse)
library(broom)

holdout_percent = 0.7


make_sets = function(data, 
                    outcome,
                    holdout_percent = 0.7,
                    holdout = TRUE) {
    
    require(tidyverse)

     
    # Drop the other outcome variables
    if (outcome == "wtp_paqi") {
        drop_vars = c("wtp_epd", "wtp_dif", "epd_treatment_baseline", "pref_baseline", "pref_endline")
    } else if (outcome == "wtp_epd") {
        drop_vars = c("wtp_paqi", "wtp_dif", "epd_treatment_baseline", "pref_baseline", "pref_endline")
    } else if (outcome == "wtp_dif") {
        drop_vars = c("wtp_paqi", "wtp_epd", "epd_treatment_baseline", "pref_baseline", "pref_endline")
    } else if (outcome == "pref_baseline") {
        drop_vars = c("wtp_paqi", "wtp_epd", "wtp_dif", "epd_treatment_baseline", "pref_endline")
    } else if (outcome == "pref_endline") {
        drop_vars = c("wtp_paqi", "wtp_epd", "wtp_dif", "epd_treatment_baseline", "pref_baseline")
    } else {
        stop("wrong outcome variable")
    }

    data = data %>% dplyr::select(-all_of(drop_vars))

    if (holdout == TRUE) {
        # Train and testing sets
        n = floor(holdout_percent*nrow(data))
        set.seed(123)
        full_vec = sample(c(1:nrow(data)), replace = FALSE)
        vec = full_vec[1:n]
        train_data = data[vec, ]
        test_data = data[-vec, ]

        return(list(train_data, test_data))
    } else if (holdout == FALSE) {
        return(data)
    }

}


make_matrix = function(data, outcome, holdout_percent = 0.7, holdout = TRUE) {
    holdout = holdout
    holdout_percent = holdout_percent
    sets = make_sets(data = data, outcome = outcome, holdout_percent = holdout_percent, holdout = holdout)
    form = as.formula( "~ . + 0") # No intercept
    if (holdout == TRUE) {
        train_data = sets[[1]]
        test_data = sets[[2]]
        x = model.matrix(form, data = train_data %>% dplyr::select(-outcome))
        y = train_data[[outcome]] # Turn y into a vector
    } else if (holdout == FALSE) {
        data = sets
        x = model.matrix(form, data = data %>% dplyr::select(-outcome))
        y = data[[outcome]]
        test_data = data
        train_data = data
    }

    # x = scale(x, TRUE, TRUE) # Standardize the matrix

    return(list(x, y, train_data, test_data))
}

# IMPORTANT NOTE: with the holdout, sometimes the factor variables with rare levels aren't captured
#   in the training set so their variance is 0 which breaks the scaling matrix bc can't divide by 0


# Best Subset --------------------------------------------------------------------------------

get_fit = function(info, big) {
    require(leaps)
    x = info[[1]]
    y = info[[2]]
    regsubsets_fit = regsubsets(x, y, nvmax = 20, method = "exhaustive", really.big = big)
    return(list(regsubsets_fit, x))
}

get_vars = function(info, big = FALSE) {
    fit_whole = get_fit(info = info, big = big)
    fit = fit_whole[[1]]
    x = fit_whole[[2]]
    summary_fit = summary(fit)
    best_size = which.min(summary_fit$cp) # using cp to balance simplicity and complexity
    best_vars = which(summary_fit$which[best_size, -1])

    test_data = info[[4]]
    return(list(best_vars, test_data))
}


# Best subset WITH intercept
linear_regression_fit = function(outcome_var, sel_vars, matrix) {
    y = matrix[[outcome_var]]
    
    form = as.formula("~ . + 0")
    x_full = model.matrix(form, data = matrix %>% dplyr::select(-outcome_var))
    x_selected = x_full[, sel_vars, drop = FALSE]
    
    model_data = as.data.frame(x_selected) %>% mutate(!!outcome_var := y)
    sel_vars_clean = paste0("`", colnames(x_selected), "`")
    
    # WITH INTERCEPT (remove the - 1)
    form = as.formula(paste(outcome_var, " ~ ", paste(sel_vars_clean, collapse = " + ")))
    
    mod = lm(form, data = model_data)
    tidied = broom::tidy(mod)[-1, ]  # Remove intercept row
    tidied = tidied %>% mutate(p.value_adj = p.adjust(p.value, method = "BH"))

    return(list(tibble(estimates = tidied$estimate, pvals = tidied$p.value_adj), 
                summary(mod)))
}






# Lasso --------------------------------------------------------------------------------


lasso_out = function(data,
                    outcome,
                    holdout_percent,
                    lambda_choice = c("lambda.min", "lambda.1se"),
                    holdout = TRUE) {
    

    lambda = match.arg(lambda_choice)

    if (holdout == TRUE) {
        outcome_and_matrix = make_matrix(data = data, outcome = outcome, 
                                        holdout_percent = holdout_percent, 
                                        holdout = TRUE)
        train_data = outcome_and_matrix[[3]]
        test_data = outcome_and_matrix[[4]]
        x = outcome_and_matrix[[1]]
        y = outcome_and_matrix[[2]]
        
    } else {
        # Use make_matrix with holdout = FALSE
        outcome_and_matrix = make_matrix(data = data, outcome = outcome, 
                                        holdout_percent = holdout_percent, 
                                        holdout = FALSE)
        train_data = outcome_and_matrix[[3]]  # Will be same as test_data
        test_data = outcome_and_matrix[[4]]   # Will be same as train_data
        x = outcome_and_matrix[[1]]
        y = outcome_and_matrix[[2]]
    }

    

    set.seed(123)
    fit = cv.glmnet(x, y, alpha = 1, family = "gaussian")

    coefs = coef(fit, s = lambda)
    coef_idx = which(coefs[-1] != 0)
    base_var_names = unique(str_extract(rownames(coefs)[coef_idx], "^.*_.*(?=_)"))
    pattern = paste0("^(", paste(base_var_names, collapse = "|"), ")")
    master_names = names(train_data)
    sel_idx = grep(pattern, master_names)
    sel_names = master_names[sel_idx]


    coefs_collapsed = paste(sel_names, collapse = " + ")
    form = as.formula(glue(outcome, " ~ ", coefs_collapsed))

    mod = lm(form, data = test_data)
    pvals = summary(mod)$coefficients[, 4]
    adj_pvals = p.adjust(pvals, method = "hochberg")[-1]

    var_summary = summary(mod)$coefficients[-1,]
    final_names = rownames(var_summary)
    meaning = map$Label[match(final_names, map$Variable)]

    tib = tibble(
            name = meaning,
            estimate = var_summary[,1],
            se = var_summary[,2],
            pvals = var_summary[,4],
            adj_pvals = adj_pvals)

    summary_mod = summary(mod)
    stats = broom::glance(summary_mod) %>% 
    dplyr::select(all_of(c("r.squared", "statistic", "p.value", "df", "nobs")))
    stats = stats %>% mutate(
        MSE = mean(residuals(mod)^2, na.rm = TRUE)
    )

    return(list(tib, stats))
}
