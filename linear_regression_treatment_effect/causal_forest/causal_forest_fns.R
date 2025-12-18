create_bar_plot = function(var_name) {
    baseline_wtp_paqi = mean(df_full_clean_contedu$wtp_paqi, na.rm = TRUE)
    baseline_wtp_epd = mean(df_full_clean_contedu$wtp_epd, na.rm = TRUE)
    avg_wtp = mean(c(baseline_wtp_paqi, baseline_wtp_epd))

    var = df_for_bar %>% pull(var_name)
    var_meaning = map$Label[match(var_name, map$Variable)]
    if (length(unique(var)) != 2 & length(unique(var)) > 10) {
        breaks = quantile(var, probs = c(0, 0.25, .5, .75, 1)) # nonbinary, continuous
        bins = cut(
        var,
        breaks = breaks,
        labels = c(
            paste(round(breaks[1], 2), round(breaks[2], 2), sep = " - "), 
            paste(round(breaks[2], 2) + 0.01, round(breaks[3], 2), sep = " - "), 
            paste(breaks[3] + 0.01, breaks[4], sep = " - "),
            paste(breaks[4] + 0.01, breaks[5], sep = " - ")
        ),
        include.lowest = TRUE)
    } else if (length(unique(var)) == 2) {
        bins = factor(
            var,
            levels = unique(var),
            labels = c(
                unique(var)[1],
                unique(var)[2])
        )
    } else {
        bins = factor(
            var,
            levels = unique(var)
        )
    }

    results = tibble(
        bins = bins,
        tau = tau_hat,
        ) %>% 
        group_by(bins) %>% 
        summarize(
            avg_tau = mean(tau),
            se_tau = sd(tau) / sqrt(n()),
            n = n(),
            ci_lower = avg_tau - 1.96 * se_tau,
            ci_upper = avg_tau + 1.96 * se_tau
        )
        
    if (var_name == "s3_q15v16_baseline_r_std") {
        results$bins = factor(results$bins, levels = c("-3", "-2", "-1", "0", "1", "2", "3"))
    } else if (var_name == "s9_q7_field_count_baseline") {
    results$bins <- factor(results$bins, 
                          levels = c("0", "1", "2", "3"))    
    }


    plot = ggplot(results, aes(x = bins, y = avg_tau)) + 
        geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7) +
        geom_errorbar(
            aes(ymin = ci_lower, ymax = ci_upper),
            width = 0.2, size = 1) +
        geom_hline(yintercept = 0, linetype = "dashed") +
        ylim(-95, 0) +
        geom_hline(yintercept = -0.25*avg_wtp, linetype = "dashed", color = "orange") +
        labs(
            x = var_meaning, 
            y = "Pred. Treatment Effect",
            subtitle = "With 95% confidence intervals",
            caption = "Negative values of treatment effect indicate preference for EPD over PAQI") +
            theme_minimal()

    return(plot)
}
