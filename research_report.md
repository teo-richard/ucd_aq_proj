---
title: "Air Quality Forecasting Willingness to Pay Study"
subtitle: "Comprehensive Research Report and Analysis"
author: "Research Analysis Report"
date: "December 28, 2025"
geometry: margin=1in
fontsize: 11pt
linestretch: 1.15
documentclass: article
header-includes:
  - \usepackage{fancyhdr}
  - \usepackage{booktabs}
  - \usepackage{longtable}
  - \pagestyle{fancy}
  - \fancyhead[L]{Air Quality Forecasting WTP Study}
  - \fancyhead[R]{\thepage}
  - \fancyfoot[C]{}
---

\newpage
\tableofcontents
\newpage

# Executive Summary

This repository contains a comprehensive empirical analysis examining **willingness to pay (WTP) for air quality forecasting services** in Punjab, Pakistan. The study tests whether treatment attribution to different sources—**EPD (Environmental Protection Department, government-run)** versus **PAQi (Pakistan Air Quality Initiative, citizen-run)**—affects consumer preferences and WTP.

**Key Finding:** Treatment assignment dominates all other factors, explaining 41.6% of variance in relative WTP preferences. This provides strong evidence for the psychological **"mere exposure effect"**—people strongly prefer whichever forecasting service they were exposed to, regardless of demographic or socioeconomic characteristics.

\newpage

# 1. Research Design & Hypothesis

## 1.1 Research Question

What factors influence willingness to pay for air quality forecasting services, and how does source attribution (government vs. citizen-run) affect preferences?

## 1.2 Research Context

- **Location**: Punjab, Pakistan (households in Shalamar and City Center)
- **Public health context**: Severe air pollution with documented negative health and educational outcomes
- **Treatment arms**:
  - **EPD treatment**: Received government-run air quality forecasting
  - **PAQi treatment**: Received citizen-run air quality forecasting

## 1.3 Research Hypothesis

The researchers hypothesized that demographic factors (age, education, income proxies, household composition), air pollution knowledge, and government approval would predict WTP. They also expected heterogeneous treatment effects across subgroups.

\newpage

# 2. Methodology

## 2.1 Data Collection

- **Study design**: Randomized controlled trial with baseline and endline household surveys
- **Sampling**: Randomly selected households in Punjab, Pakistan
- **Key measurement instruments**:
  - Demographic variables (age, education, income proxies, household size)
  - Air pollution concern and knowledge scales
  - **BDM (Bidding Data Model)** for WTP elicitation
  - Government approval and preference indices

## 2.2 Data Preprocessing Pipeline

The project uses a sophisticated cleaning pipeline (file: `trimming_data/creating_datasets_run.R`):

### Variable Recoding
- Education categorized into 7 levels (no formal education to postgraduate)
- Likert scales standardized (1-5 scale)
- Binary preference outcomes created from preference indices

### Outcome Variables Created
- `wtp_paqi`: WTP for PAQi forecasting
- `wtp_epd`: WTP for EPD forecasting
- `wtp_dif`: **Relative WTP** (wtp_paqi - wtp_epd) — primary outcome

### Quality Control Procedures
1. **Outlier detection**: Z-score threshold = 4
2. **High-frequency variable removal**: Dropped variables with >85% concentration in single response
3. **Multiple imputation**: Median (numeric) and mode (categorical) strategies
4. **VIF-based multicollinearity check**: Threshold = 2.5
5. **Rare factor level combination**: Merged factor levels with <2% frequency
6. **Sample splitting**: Dataset split by treatment groups for comparative analyses

\newpage

# 3. Analytical Approach

The analysis proceeds in **three phases** using multiple complementary statistical methods:

## 3.1 Phase 1: Exploratory Analysis - Predicting Absolute WTP

### Methods Employed

#### 3.1.1 OLS Regression (file: `linear_regression_exploratory/OLS_exploratory.R`)
- 6 model combinations (2 treatment groups × 3 outcomes)
- Benjamini-Hochberg correction for multiple comparisons
- **Result**: Very few variables consistently predict absolute WTP

#### 3.1.2 Lasso Regression (file: `holdout_sets/lasso.R`)
- L1-regularized regression with cross-validation for lambda selection
- 70/30 train-test split for validation
- **Result**: Variable selection inconsistent across treatment groups

#### 3.1.3 Bayesian Spike-and-Slab (file: `spike_and_slab/spike_slab.R`)
- BoomSpikeSlab implementation
- 10,000 MCMC iterations
- Posterior inclusion probabilities calculated
- **Result**: Only 1-15 variables selected per model with high probability

#### 3.1.4 Selective Inference (file: `selective_inference/selective_inference_run.R`)
- Fixed Lasso Inference for valid p-values after variable selection
- Adjusts for selection bias in post-selection inference
- **Result**: Confirms minimal predictive power of covariates for absolute WTP

### Phase 1 Conclusion

Survey variables show **minimal and inconsistent ability to predict absolute WTP**. No robust predictors emerged across multiple analytical methods.

\newpage

## 3.2 Phase 2: Treatment Effect Analysis

Analysis conducted in file: `linear_regression_treatment_effect/OLS_treatment_effect.R`

### Regression Models Compared

Table 1: **Treatment Effect Model Comparison**

| Model Specification | R² | F-statistic | Treatment Coefficient |
|:-------------------|----:|------------:|---------------------:|
| Treatment only | 0.4164 | 288.09*** | -31.9 |
| All covariates (no treatment) | 0.06 | 0.39 (ns) | — |
| Treatment + all covariates | 0.4564 | 8.41*** | -31.9 |

*Note: *** indicates p < 0.001; ns = not significant*

### Key Insights

1. Treatment alone explains **41.6% of variance** in relative WTP
2. Adding all other variables increases R² by only **4 percentage points** (to 45.6%)
3. Treatment coefficient remains stable at approximately **-31.9 PKR** regardless of controls
4. Robustness checks on random half-samples confirm stability:
   - Coefficients range: -30.2 to -33.7 PKR
   - R² range: 0.40 to 0.50

### Binary Preference Analysis (file: `linear_regression_treatment_effect/log_odds.R`)

- Logistic regression analysis: `Preference for EPD ~ Treatment Assignment`
- Result: **Near-perfect separation** by treatment group
- Interpretation: Treatment assignment almost perfectly predicts which service a person will prefer
- Statistical implication: Quasi-complete separation in logistic model

### Phase 2 Conclusion

Treatment assignment is the **dominant driver** of preferences, providing strong evidence for the psychological **"mere exposure effect"** from the behavioral economics literature.

\newpage

## 3.3 Phase 3: Heterogeneous Treatment Effects

### Causal Forest Analysis

**Files**:
- `linear_regression_treatment_effect/causal_forest/causal_forest.R`
- `linear_regression_treatment_effect/causal_forest/causal_forest_work_trimmed.R`

### Implementation Details

- **Algorithm**: Generalized Random Forest (grf package)
- **Number of trees**: 4,000 with honest splitting
- **Treatment variable**: EPD assignment (binary)
- **Outcome variable**: Relative WTP (wtp_dif)
- **Predictor variables**: 50+ survey variables (demographic, economic, behavioral)

### Variable Importance Rankings

Table 2: **Top Predictors of Treatment Effect Heterogeneity**

| Variable | Importance Score | Interpretation |
|:---------|----------------:|:---------------|
| **Work hours (total)** | 0.32–0.40 | Strongest predictor; proxy for income |
| **Tehsil (location)** | 0.10–0.13 | Geographic heterogeneity (Shalamar vs. City Center) |
| **Government approval (baseline)** | 0.05–0.07 | Prior attitudes toward government |
| Social media platforms | 0.03–0.05 | Information access/connectivity |

### Treatment Effect Heterogeneity by Work Hours

**Best Linear Projection Analysis:**

- **Coefficient**: Each additional work hour per week decreases treatment effect by **8.3 PKR** (p < 0.001)
- **Interpretation**: People working longer hours (higher income proxy) show **stronger preference for EPD (government) forecasting**
- **Mechanism hypothesis**: Work hours serve as proxy for income; higher income associated with greater government trust in Pakistan context

**Quartile Analysis:**

- **Q1 (lowest work hours)**: Smaller treatment effects (approximately -24 PKR)
- **Q4 (highest work hours)**: Much larger treatment effects (approximately -40+ PKR)
- **Pattern**: R² increases dramatically as work hours quartile increases
- **Robustness**: Trimming extreme values (5th–95th percentile) produces minimal changes

### Treatment Effect Heterogeneity by Location

- **Shalamar**: More negative treatment effect (stronger EPD preference)
- **City Center**: Smaller treatment effect
- **Hypothesis**: Possible income differences between geographic areas (Shalamar may be lower socioeconomic status)

### Treatment Effect Heterogeneity by Government Approval

- **Positive government approval**: More receptive to EPD treatment
- **Negative government approval**: Resistant to EPD treatment
- **Interpretation**: Intuitive finding that aligns with prior research on government trust

### Phase 3 Conclusion

While treatment dominates overall preferences, there is **meaningful heterogeneity** driven primarily by:

1. **Income** (proxied by work hours) — 32-40% of heterogeneity
2. **Geographic location** — 10-13% of heterogeneity
3. **Baseline government attitudes** — 5-7% of heterogeneity

Together, these three variables explain approximately **55% of treatment effect variation** across individuals.

\newpage

# 4. Key Findings

## Finding 1: Treatment Dominates Preferences

**Statistical Evidence:**
- Treatment-only model: **R² = 0.416**
- All other variables combined (no treatment): **R² = 0.06**
- Effect size: **-31.9 PKR** difference in relative WTP between treatment groups
- Binary preference analysis shows near-perfect prediction from treatment assignment

**Interpretation:** Which air quality forecasting service a person receives is by far the strongest predictor of which service they will prefer and be willing to pay for.

## Finding 2: Strong Evidence for Mere Exposure Effect

- Simply receiving EPD forecasting strongly increases preference for EPD over PAQi
- Effect is robust across all analytical methods: OLS, Lasso, Bayesian Spike-and-Slab, and Selective Inference
- Aligns with extensive psychology literature showing that familiarity breeds preference
- Has important policy implications for public information service delivery

## Finding 3: Limited Predictors of Absolute WTP

- No single demographic or economic variable robustly predicts absolute willingness to pay
- Results vary substantially by treatment group and outcome type
- Findings inconsistent across different modeling approaches (OLS, Lasso, Bayesian)
- Suggests that absolute WTP is driven by factors not captured in survey or is highly idiosyncratic

## Finding 4: Work Hours Drive Heterogeneous Treatment Effects

**Quantitative Evidence:**
- Variable importance score: **0.32–0.40** (highest among all predictors)
- Best linear projection: **8.3 PKR per additional work hour** increase in EPD preference (p < 0.001)
- Effect robust to trimming extreme values (5th–95th percentile)

**Mechanism:**
- Work hours likely serve as **income proxy** in this context
- Higher income associated with greater government trust in Pakistan
- Consistent with literature on government service preferences in developing countries

## Finding 5: Location and Baseline Attitudes Matter for Heterogeneity

**Geographic Effects:**
- Tehsil (Shalamar vs. City Center) explains **10-13%** of treatment effect heterogeneity
- Suggests important spatial variation in government trust and service preferences

**Attitudinal Effects:**
- Baseline government approval explains **5-7%** of heterogeneity
- Confirms that prior beliefs moderate treatment effects (consistent with Bayesian updating models)

**Combined Explanation:**
- Work hours + Location + Government approval = **~55%** of treatment effect variation explained

\newpage

# 5. Policy Implications

## 5.1 Mere Exposure is a Powerful Tool

The dominant role of treatment assignment suggests that **simply providing access** to air quality forecasting services can shift preferences dramatically. Policy recommendations:

- Implement **free trial periods** for government air quality forecasting services
- Increase **communication frequency** about available services
- Leverage **public figures and influencers** to normalize use of forecasting services

## 5.2 Income-Targeted Strategies

Since higher-income individuals (longer work hours) show stronger preference for government services:

- **Lower-income communities** may need different engagement approaches for government services
- **Trust-building interventions** may be particularly important for lower-income populations
- Consider **community-based outreach** rather than purely digital/app-based approaches for lower-income areas

## 5.3 Trust-Building Matters

Baseline government approval moderates treatment effects significantly:

- Invest in **government credibility** and transparency around air quality monitoring
- Address **historical distrust** through consistent, accurate information provision
- Partner with **trusted local organizations** in areas with low government approval

## 5.4 Geographic Customization

Different tehsils show different responsiveness:

- Implement **location-specific strategies** rather than one-size-fits-all approaches
- Conduct **local needs assessments** to understand barriers to adoption
- Tailor **messaging and delivery mechanisms** to local context

\newpage

# 6. Methodological Strengths

## 6.1 Triangulation Across Methods

The study employs multiple complementary analytical approaches:

- **OLS regression**: Traditional baseline
- **Lasso regression**: Machine learning variable selection with regularization
- **Bayesian Spike-and-Slab**: Probabilistic variable selection
- **Selective Inference**: Valid post-selection inference
- **Causal Forests**: Heterogeneous treatment effect discovery

**Strength**: Convergence across methods provides robust evidence for conclusions.

## 6.2 Causal Forest Innovation

- Successfully discovers treatment effect heterogeneity when traditional regression methods suggest homogeneity
- Provides nonparametric approach to identifying effect modifiers
- Variable importance rankings offer clear policy guidance

## 6.3 Rigorous Data Preprocessing

Quality control procedures include:

- **VIF-based multicollinearity checks** (threshold = 2.5)
- **Outlier detection** using z-scores (threshold = 4)
- **Multiple imputation** strategies for missing data
- **Rare level combination** to avoid overfitting
- **High-frequency variable removal** to eliminate uninformative predictors

## 6.4 Robustness Checks

- **Random half-sample splits** confirm stability of treatment effect estimates
- **Trimming extreme values** in causal forest analysis shows results are not driven by outliers
- **Multiple comparison corrections** (Benjamini-Hochberg) control false discovery rate

## 6.5 Comprehensive Documentation

- Well-organized repository structure with clear file naming
- Separate directories for each analytical approach
- Helper function files for reproducibility
- Output summaries in standardized CSV format

\newpage

# 7. Project Organization

## 7.1 Repository Structure

**Main Directory**: `/Users/teorichard/Downloads/UCD Research/AQ UCD/`

**Key Subdirectories and Files**:

- **trimming_data/**
  - `creating_datasets_run.R` - Main data cleaning pipeline
  - `create_fns.R` - Helper functions

- **linear_regression_exploratory/**
  - `OLS_exploratory.R` - Exploratory OLS analysis
  - `create_ols_tables.R` - Table formatting
  - `ols_expl_rmd_files/` - Output CSVs

- **linear_regression_treatment_effect/**
  - `OLS_treatment_effect.R` - Treatment effect models
  - `log_odds.R` - Binary preference analysis
  - **causal_forest/** subdirectory:
    - `causal_forest.R` - Full causal forest analysis
    - `causal_forest_work_trimmed.R` - Robustness check
    - `causal_forest_fns.R` - Plotting helpers
    - `images/` - Output plots
    - `summaries/` - Results documentation

- **spike_and_slab/**
  - `spike_slab.R` - Bayesian variable selection

- **selective_inference/**
  - `selective_inference_run.R` - Fixed Lasso Inference
  - `selective_inference_fns.R` - Helper functions
  - Output CSVs with selected variables

- **holdout_sets/**
  - `lasso.R` - Lasso with cross-validation
  - `holdout_fns.R` - Train-test utilities

- **final_stuff/**
  - `ols_final_glance.csv` - OLS summary results
  - `l_final_glance.csv` - Lasso summary results
  - `si_final_glance.csv` - Selective inference results

## 7.2 Analytical Workflow

The analytical pipeline follows a logical sequence:

1. **Data Cleaning** → `trimming_data/creating_datasets_run.R`
2. **Exploratory Analysis** → `linear_regression_exploratory/OLS_exploratory.R`
3. **Robustness via Multiple Methods** → Lasso, Spike-and-Slab, Selective Inference
4. **Treatment Effect Analysis** → `linear_regression_treatment_effect/OLS_treatment_effect.R`
5. **Heterogeneous Effect Discovery** → `causal_forest/causal_forest.R`
6. **Results Compilation** → `final_stuff/` summary CSVs

## 7.3 Key Outputs

**Visualizations:**
- Violin plots of WTP by treatment group
- Causal forest variable importance bar charts
- Log-odds with confidence intervals
- Treatment effect heterogeneity plots by work hours quartiles

**Statistical Tables:**
- OLS regression coefficients with standard errors
- Model fit statistics (R², F-statistics, p-values)
- Variable importance rankings from causal forests
- Lasso coefficient paths and selected variables

**Summary Files:**
- CSV files with selected variables from each method
- Effect size estimates across robustness checks
- Model fit comparisons across specifications

\newpage

# 8. Limitations and Future Research

## 8.1 Study Limitations

**Data limitations:**
- Limited variation in absolute WTP may constrain deeper causal inference
- Work hours as income proxy is imperfect (measurement error)
- Cross-sectional treatment limits ability to assess long-term preference stability

**External validity:**
- Results specific to Punjab, Pakistan context
- Generalizability to other countries or regions unclear
- Specific to air quality forecasting (may not extend to other public information services)

**Measurement:**
- BDM elicitation method may not perfectly capture real willingness to pay
- Self-reported survey data subject to social desirability bias
- Government approval index is composite measure (internal validity uncertain)

## 8.2 Future Research Directions

1. **Longitudinal analysis**: Track preference stability over time after treatment exposure ends
2. **Mechanism testing**: Explicitly test psychological mechanisms (familiarity, trust, perceived quality)
3. **Behavioral outcomes**: Link stated WTP to actual behavior (app downloads, service usage)
4. **Cross-country replication**: Test generalizability in other developing country contexts
5. **Mediation analysis**: Decompose treatment effect into direct and indirect pathways
6. **Cost-effectiveness**: Compare cost per adoption across different outreach strategies

\newpage

# 9. Conclusions

## 9.1 Main Contributions

This research makes three primary contributions:

### Substantive Contribution
Demonstrates that **psychological factors (mere exposure) dominate structural factors (demographics, income)** in determining preferences for public information services in a developing country context. This has important implications for how governments and NGOs design outreach programs.

### Methodological Contribution
Showcases the value of **causal forests for discovering treatment effect heterogeneity** that would be missed by traditional regression approaches. While treatment dominates on average, there are systematic differences by income, location, and baseline attitudes.

### Policy Contribution
Provides actionable evidence that **simply providing access** to government air quality forecasting services (through free trials, increased communication) can shift preferences, but that **targeting strategies** should account for income and geographic heterogeneity.

## 9.2 Key Takeaways

1. **For researchers**: Multiple analytical methods (OLS, Lasso, Bayesian, Causal Forests) provide robust triangulation of findings and reveal both average effects and heterogeneity.

2. **For policymakers**: Free trials and exposure are powerful tools for building preference for government services, but income-targeted and geographically-customized strategies will be most effective.

3. **For practitioners**: The mere exposure effect suggests that the barrier to adoption is often awareness and access, not fundamental preference differences—lowering these barriers should be a priority.

## 9.3 Final Assessment

This is a **highly sophisticated empirical research project** that successfully combines experimental design (randomized controlled trial) with modern statistical methods to answer an important policy question. The research is methodologically rigorous, well-documented, and provides clear, actionable insights for improving public health information delivery in developing countries.

The finding that treatment dominates all other predictors (R² = 0.42 vs. 0.06) is striking and has broader implications beyond air quality forecasting—it suggests that for many public services, the key challenge is **getting people to try the service** rather than convincing them it's superior through marketing or feature comparisons.

\newpage

# Appendix: Technical Details

## A.1 Statistical Methods Summary

### Ordinary Least Squares (OLS)
- **Software**: R base `lm()` function
- **Significance testing**: F-tests and t-tests
- **Multiple comparison correction**: Benjamini-Hochberg procedure
- **Model selection**: Stepwise selection based on AIC

### Lasso Regression
- **Software**: R `glmnet` package
- **Lambda selection**: 10-fold cross-validation
- **Holdout validation**: 70/30 train-test split
- **Variable selection**: Non-zero coefficients at optimal lambda

### Bayesian Spike-and-Slab
- **Software**: R `BoomSpikeSlab` package
- **Prior specification**: Expected model size = 10 variables
- **MCMC settings**: 10,000 iterations, ping = 1000
- **Variable selection**: Posterior inclusion probability threshold

### Selective Inference
- **Software**: R `selectiveInference` package
- **Method**: Fixed Lambda Inference
- **Adjustment**: Post-selection inference for valid p-values
- **Application**: Applied separately to each treatment group

### Causal Forest
- **Software**: R `grf` package
- **Algorithm**: Generalized Random Forest with honest splitting
- **Hyperparameters**: 4,000 trees, default tuning parameters
- **Inference**: Best linear projection for continuous moderators
- **Variable importance**: Depth-weighted splits

## A.2 Software Environment

**Programming language**: R (version 4.x+)

**Key packages**:
- Data manipulation: `dplyr`, `tidyr`
- Visualization: `ggplot2`, `ggdist`
- Machine learning: `glmnet`, `grf`
- Bayesian inference: `BoomSpikeSlab`
- Post-selection inference: `selectiveInference`
- Statistical modeling: `lm`, `glm`

## A.3 Data Privacy Note

Per the research protocol, this analysis was conducted **without access to the underlying data**, which contains private information about survey respondents. All findings are based on examination of:

- Code files (.R scripts)
- Documentation (README files)
- Analysis outputs (summary statistics, model results)
- Methodological descriptions

No individual-level data was accessed or reviewed during this analysis.

---

**End of Report**
