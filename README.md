# Air Quality Forecasting: Willingness to Pay Study

A faculty-supervised independent undergraduate research project investigating willingness to pay (WTP) for air quality forecasting in Pakistan. This was mainly an exploratory analysis where I looked at which factors influence WTP, then used causal forests to investigate heterogeneous treatment effects.

**Note:** The data used in this project is private and this code cannot be run without the correct data. For the purpose of explaining the project, I'll still assume you have access to it.

## The Data

The data comes from a survey of randomly selected households in Lahore, Pakistan with various questions including demographics, air pollution awareness, and air quality forecasting preferences. There are two treatments—PAQI and EPD—each corresponding to a type of air quality forecasting an individual received after the baseline survey was given.

## Project Structure

```
├── trimming_data/               # Data cleaning pipeline
│   ├── creating_datasets_run.R  # Main cleaning script (outlier removal, imputation,
│   │                            #   high-leverage checks, train/test splits)
│   └── create_fns.R             # Helper functions (VIF pruning, outlier detection,
│                                #   high-leverage removal)
│
├── cleaned_data/                # Cleaned CSVs (LARGE_* = all variables, others = pre-selected)
│
├── linear_regression_exploratory/
│   ├── OLS_exploratory.R        # Exploratory OLS to see what predicts WTP
│   └── create_ols_tables.R      # Table formatting
│
├── holdout_sets/
│   ├── lasso.R                  # Lasso regression with holdout validation
│   └── best_subset/             # Best subset selection with holdout validation
│
├── selective_inference/         # Selective inference (honorable mention—see below)
│
├── spike_and_slab/
│   └── spike_slab.R             # Bayesian Spike and Slab variable selection
│
├── linear_regression_treatment_effect/
│   ├── OLS_treatment_effect_relWTP.R   # Treatment effect on relative WTP (PAQI - EPD)
│   ├── OLS_treatment_effect_absWTP.R   # Treatment effect on absolute WTP
│   │                                   #   (PAQI alone, EPD alone, total)
│   ├── investigating_treatment_effect.R # Effect size calculations
│   ├── log_odds.R                      # Formal test of treatment preference
│   ├── get_cat_edu.R                   # Categorical education variable creation
│   └── causal_forest/                  # Heterogeneous treatment effects
│       ├── causal_forest.R             # Main causal forest analysis
│       ├── causal_forest_fns.R         # Plotting functions
│       └── causal_forest_work_trimmed.R # CF with trimmed work hours
│
├── final_stuff/                 # Summary/glance tables for final results
├── original_files/              # Raw data files (Stata .dta format)
└── research_report_updated.pdf  # Full research report
```

## Data Cleaning Pipeline

The cleaning pipeline in `trimming_data/creating_datasets_run.R` does a lot of the heavy lifting:

1. **Recoding** — Standardizing variables (e.g. reverse-coding, combining levels)
2. **Outlier removal** — Z-score based detection (threshold = 4) for extreme univariate outliers
3. **Imputation** — KNN imputation for missing values
4. **VIF pruning** — Iteratively drops variables with high variance inflation (threshold = 2.5)
5. **High-leverage removal** — Identifies observations with near-perfect leverage (hat values >= 0.99) that cause numerical instability, especially with robust SE estimation
6. **Rare level combining** — Lumps together rare factor levels (< 2% of observations)

The result is three cleaned datasets: full data, EPD treatment only, and PAQI treatment only.

## Analysis & Findings

### What predicts WTP?

I used OLS, Lasso, and Bayesian Spike and Slab to figure out what variables impact willingness to pay. The `selectiveInference` package also gets an honorable mention, but it's not a main part of this analysis because I didn't meet the assumptions and the package is not being maintained to my knowledge.

**Finding:** No variable included in the survey has much of an effect on WTP for air quality forecasting—except treatment itself.

### The treatment effect

Once I realized treatment was the main driver, I started investigating it more closely. It is easy to see from the violin plots below that if a person is assigned EPD treatment, they will always prefer EPD forecasting (i.e., WTP for PAQI minus WTP for EPD will always be negative), and vice versa for PAQI treatment.

<img width="600" height="600" alt="WTP by treatment group" src="linear_regression_treatment_effect/wtp_by_treatment.png"/>

The code in `log_odds.R` verifies this formally with logistic regression. The predicted probability of preferring EPD forecasting is ~97% for those in the EPD treatment group and ~5% for those in the PAQI group—a near-complete separation.

<img width="500" height="500" alt="Log odds of preferring EPD forecasting" src="linear_regression_treatment_effect/log_odds_plot.png"/>

This is a very interesting finding and supports the "mere exposure effect" that is well-documented in psychology.

#### Technical details (relative WTP)

Using OLS with robust standard errors (HC1), the treatment effect on relative WTP (PAQI minus EPD) is strong:

| Model | R² | Treatment Coefficient | p-value |
|---|---|---|---|
| Treatment only | 0.424 | -32.08 | < 0.001 |
| All covariates, no treatment | 0.071 | — | 0.381 |
| Full model (treatment + covariates) | 0.460 | -32.00 | < 0.001 |

Treatment alone explains 42.4% of the variance in relative WTP—about **6x more** than all other covariates combined (R² = 0.071, which is not even significant). Adding 65+ baseline covariates on top of treatment only increases R² from 0.424 to 0.460.

The effect size is **Cohen's d = 1.71** (95% CI: 1.56 to 1.86), which is a large effect by conventional standards (d > 0.8). In standard deviation terms, the treatment shifts relative WTP by about 1.2 SD.

These results are robust to random sample splitting: the treatment-only R² averages 0.425 across halves, and the coefficient remains stable around -31 to -34.

<img width="700" height="350" alt="Treatment effect distribution" src="linear_regression_treatment_effect/treatment_effect_distr.png"/>

#### Absolute WTP

I also looked at the treatment effect on absolute WTP using OLS with robust standard errors (HC1). Treatment weakly predicts absolute WTP for PAQI (R² = 0.012, p = 0.001) and for EPD (R² = 0.015, p < 0.001), but does **not** predict total WTP (R² ≈ 0, p = 0.831). This means treatment shifts which service people prefer, but not how much they're willing to spend overall.

### Heterogeneous treatment effects

Finally, I used causal forests (4,000 trees, `grf` package) to look at heterogeneous treatment effects. The top variable importance scores are:

| Variable | Importance |
|---|---|
| Hours spent on work | 0.356 |
| Tehsil (location) | 0.124 |
| Relative stated pref for govt approval | 0.081 |
| Received AP info from own observation | 0.062 |
| Num social media used for social issues | 0.054 |

<img width="600" height="300" alt="Variable importance" src="linear_regression_treatment_effect/causal_forest/images/var_importance.png"/>

Work hours is by far the dominant moderator. The treatment effect grows monotonically across work hour quartiles—from about -10 PKR for those working 1-9.5 hours to about -60 PKR for those working 13.5-18 hours:

<img width="600" height="300" alt="Treatment effect by work hours" src="linear_regression_treatment_effect/causal_forest/images/work_hrs_whitebg.png"/>

Location also matters: the treatment effect is substantially larger in Shalamar Tehsil (~-38 PKR) compared to City Center (~-5 PKR):

<img width="600" height="300" alt="Treatment effect by tehsil" src="linear_regression_treatment_effect/causal_forest/images/tehsil.png"/>

Government approval preference shows a clear gradient—those who approve of the government more strongly show larger treatment effects (up to -55 PKR), while those who disapprove show much smaller effects (~-5 PKR):

<img width="600" height="300" alt="Treatment effect by government approval" src="linear_regression_treatment_effect/causal_forest/images/gov_approval.png"/>

See `causal_forest.R` for causal forests on the full data and `causal_forest_work_trimmed.R` for causal forests on data which trims off extreme values of the hours worked variable.

Overall, I could not investigate this matter further due to a lack of data. But this finding is quite interesting as it provides important insights into how individuals decide what kind of air quality forecasting they value most. Perhaps free trials, increased communication, or seeing public figures using air quality forecasting could push individuals to also use this forecasting. Pakistan is very polluted and it has been well-established that air pollution has various negative effects on health and education. It benefits individuals to know the air quality around them to make informed decisions on daily activities.

## Tools & Packages

This project is written in R. Key packages: `tidyverse`, `glmnet` (Lasso), `BoomSpikeSlab` (Spike and Slab), `grf` (causal forests), `sandwich` + `lmtest` (robust SEs), `recipes` (imputation), `car` (VIF).

## Learning Outcomes

I learned quite a bit from this project, which is to be expected as this is my first "brain cell intensive" project. First, I learned about the difficulties of cleaning data and the decisions to be made regarding variables that are not high quality. I was also able to practice applying statistical techniques to a real question on real data. I learned more about OLS and was able to learn about and implement machine learning techniques, namely Lasso, Bayesian Spike and Slab, and causal forests. I practiced good data visualization to effectively communicate my results and had to solve problems when my results didn't make sense, weren't satisfactory, or I felt I could push further. In addition, as this was my first real project, I had to navigate unfamiliar territory, but I learned that I really enjoy the process of being challenged.

But perhaps the most important learning outcome (I saved the best for last) was that of the importance of organization, documentation, functions, and version control. Not paying attention to these four ideas definitely made all this way more difficult than it needed to be. While I would say that my directories are fairly well-organized by the type of analysis, the code itself could be better in its organization. Some functions are commented out, some are unused, and functions often reference another function that is nowhere near it. In addition, there's redundant code that simply does not need to exist. In the future I should make functions and utilize a utils file or something similar. The code is also sometimes well-commented, and sometimes not. Sometimes I went back to old code and spent time figuring out what I was thinking, as I had neglected to comment it, then yet again neglected to comment it. Staring at your old code with your head in your hands is definitely a canon event but that does not mean it is a good thing.

I give this project a 10/10 in terms of learning opportunity. Thanks to Professor Arman Rezaee at UC Davis for having patience, especially when I abandoned the project for three months to go study abroad in New Zealand!

## Extra Note

I had "finished" this project in December 2025 but decided to present at the UC Davis Undergraduate Research Conference. When reviewing my project I realized there were some kind of important stones I'd left unturned, so I spent a bit of time updating the project. The second most important of these "updates" was looking into the treatment effect of absolute willingness to pay, both specific to PAQI and EPD, as well as the total willingness to pay. It didn't add too much but I think it is important to have in the project. But the *most* important was definitely also a huge learning opportunity—I didn't remove high-leverage points OR use robust SEs even though I was working with survey data! In the spirit of honesty, I'm going to out myself here for the world to read and judge me, but after I got over my embarrassment, I fixed it all up. It's not perfect as I don't have any geographic variables so I can't cluster SEs but hey, the world is a mess and I think the fact that I didn't cluster my standard errors is the least of your worries.
