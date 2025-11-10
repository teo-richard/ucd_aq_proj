library(selectiveInference)
library(tidyverse)
library(glmnet)
library(Matrix)
library(broom)
library(corrplot)

df_epd_clean = read_csv("/Users/teorichard/Downloads/UCD Research/AQ UCD/cleaned_data/df_epd_clean.csv")
df_paqi_clean = read_csv("/Users/teorichard/Downloads/UCD Research/AQ UCD/cleaned_data/df_paqi_clean.csv")
df_full_clean = read_csv("/Users/teorichard/Downloads/UCD Research/AQ UCD/cleaned_data/df_full_clean.csv")


drop_other_vars = function(data, outcome) {
    if (outcome == "wtp_paqi") {
        drop_vars = c("wtp_epd", "wtp_dif", "pref_baseline", "pref_endline")
    } else if (outcome == "wtp_epd") {
        drop_vars = c("wtp_paqi", "wtp_dif", "pref_baseline", "pref_endline")
    } else if (outcome == "wtp_dif") {
        drop_vars = c("wtp_paqi", "wtp_epd", "pref_baseline", "pref_endline")
    } else {
        stop("wrong outcome variable")
    }
    return(drop_vars)
}

make_matrix = function(data, outcome) {
    drop_vars = c(drop_other_vars(data, outcome), "epd_treatment_baseline") # epd_treatment_baseline is a constant for the datasets split by treatment group
    data = data %>% dplyr::select(-all_of(drop_vars)) # nolint: object_usage_linter.

    form = as.formula(paste(outcome, "~ ."))
    m_matrix = model.matrix(form, data = data)[, -1]
    m_matrix = scale(m_matrix, TRUE, TRUE)
    center = attr(m_matrix, "scaled:center")
    scale_factor = attr(m_matrix, "scaled:scale")
    return(list(mat = m_matrix, sf = scale_factor))
}

fixedLassoInf_fit = function(data, outcome) {

# *** out of date!!! not rly, it just gives you standardized coefficients but everything else is fine :)


    y = data[[outcome]]
    x = make_matrix(data, outcome)

    cv_fit = cv.glmnet(x, y, family = "gaussian")
    lambdamin = cv_fit$lambda.min
    set.seed(123)
    fit = glmnet(x, y, lambda = lambdamin)
    b = coef(fit, s = lambdamin)[-1]
    fli_fit = fixedLassoInf(x, y, beta = b, lambda = lambdamin)
    
    names = colnames(x)[which(fli_fit$pv < 0.05)]
    coefs = fli_fit$coef0[which(fli_fit$pv < 0.05)]
    pvals = fli_fit$pv[which(fli_fit$pv < 0.05)]

    final_summary = tibble(name = names, coef = coefs, pval = pvals) # nolint: object_usage_linter.
    return(final_summary)
}


fixedLassoInf_fit_keepall = function(data, outcome) {
    y = data[[outcome]]
    make_mat_out = make_matrix(data, outcome)
    scale_factor = make_mat_out$sf
    x = make_mat_out$mat

    # Use the actual fitted coefficients from glmnet's path
    fit = glmnet(x, y)
    cv_fit = cv.glmnet(x, y, family = "gaussian")
    b = coef(cv_fit, s = "lambda.min")[-1]

    # Verify it's sparse (has zeros)
    cat("Number of non-zero coefficients:", sum(b != 0), "\n")

    # Try fixedLassoInf with the CV-fitted coefficients
    fli_fit = fixedLassoInf(x, y, beta = b, lambda = cv_fit$lambda.min)
    
    names = names(fli_fit$vars)
    coefs_std = as.vector(fli_fit$coef0)
    coefs_transformed = coefs_std / scale_factor[names]
    pvals = fli_fit$pv

    final_summary = tibble(name = names, coef = coefs_transformed, pval = pvals) # nolint: object_usage_linter.
    return(final_summary)
}

data = df_paqi_clean
outcome = "wtp_epd"

