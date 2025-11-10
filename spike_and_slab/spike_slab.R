library(tidyverse)
library(BoomSpikeSlab)

# Reading in data: Large
df_epd_clean = read_csv("/Users/teorichard/Downloads/UCD Research/AQ UCD/cleaned_data/LARGE_df_epd_clean.csv") %>% select(-hhid, -yrs_educ_formal_baseline)
df_paqi_clean = read_csv("/Users/teorichard/Downloads/UCD Research/AQ UCD/cleaned_data/LARGE_df_paqi_clean.csv") %>% select(-hhid, -yrs_educ_formal_baseline)
df_full_clean = read_csv("/Users/teorichard/Downloads/UCD Research/AQ UCD/cleaned_data/LARGE_df_full_clean.csv") %>% select(-hhid, -yrs_educ_formal_baseline)


dat = df_full_clean %>% dplyr::select(-c(epd_treatment_baseline, wtp_epd, wtp_paqi, pref_baseline, pref_endline))

model = lm.spike(wtp_dif ~ .,
    data = dat,
    niter = 10000,
    ping = 1000,
    expected.model.size = 10)

posterior_means <- colMeans(model$beta)
# Get coefficient samples
dim(model$beta)  # rows = iterations, columns = variables
head(model$beta)

# Calculate posterior means
posterior_means <- colMeans(model$beta)
posterior_means

# Calculate inclusion probabilities
# (proportion of iterations where coefficient != 0)
inclusion_probs <- colMeans(model$beta != 0)
inclusion_probs

# Combine into a nice data frame
results <- data.frame(
  variable = colnames(model$beta),
  coefficient = posterior_means,
  inclusion_prob = inclusion_probs
) %>%
  arrange(desc(inclusion_prob))

plot(model)


view(results)

spikeslabcoefs = results %>% ggplot(aes(inclusion_prob, coefficient)) +
  geom_point() +
  labs(
    x = "Probability of Inclusion",
    y = "Coefficient value",
    title = "Coefficient values and inclusion probability") +
    theme_minimal() +
    theme(
      axis.line = element_line(color = "gray"),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA)
  )


ggsave("spike_and_slab/spikeslabcoefs.png", spikeslabcoefs)
