# Overview

This repository is for an independent research project in which I was investigating willingness to pay for air quality pollution. It was mainly an exploratory analysis in which I investigated
which factors influence willingness to pay, and then used causal forests to look at heterogenous treatment effects. 

The data used in this project is private and this code is unable to be run without the correct data. For the purpose of explaining this project, I will still assume you have access to the correct data.

# Cleaning, Analysis, and Findings

The data comes from a survey of randomly selected households in Pakistan with various questions including demographic questions and questions about air pollution and air quality forecasting. In addition, there are two treatments: PAQI and EPD, with each correpsonding to the type of air quality forecasting an individual recieved after the baseline survey was given. The `creating_datasets_run.R` does the majority of the cleaning. It includes checking for outliers, imputing data, and dropping necessary rows and variables. It also results in three separate cleaned datasets: the full data, data with PAQI treatment, and data with EPD treatment. 

The first goal was to figure out what variables impact willingness to pay for air quality forecasting. I used OLS, Lasso, and Spike and Slab to try to answer this question. The `selectiveInference` package also gets an honorable mention, but it is not a main part of this analysis because I did not meet the assumptions and the package is not being maintained to my knowledge. I found that no variable included in the survey has much of an effect on willingness to pay for air quality forecasting except treatment itself. 

I then started investigating the treatment effect. It is easy to see from the plot below that if a person is assigned EPD treatment, then they will always prefer EPD forecasting (i.e. WTP for PAQI minus WTP for EPD will always be negative), and vice versa for PAQI treatment. The code in `log_odds.R` verifies this formally.\

<img width="600" height="600" alt="temp_plot" src="https://github.com/user-attachments/assets/03ff7ec9-4113-4892-b3ab-ef290ef7956c"/>


This is a very interseting finding and supports the "mere exposure effect" that is well-documented in psychology. Finally, I looked into the heterogenous treatment effects using causal forests and found that work hours is assigned the highest variable importance score, at about 0.32 (see `causal_forest.R` for causal forests on the full data and `causal_forests_work_trimmed.R` for causal forests on data which trims off extreme values of the hours worked variable). 

<img width="600" height="300" alt="image" src="https://github.com/user-attachments/assets/1597ae0b-8460-4f43-b67d-02aba25492d6"/>

Overall, I could not investigate this matter further due to a lack of data. But this finding is quite interesting as it provides important insights into how individuals decide what kind of air quality forecasting they value most. Perhaps free trials, increased communication, or seeing public figures using air quality forecasting could push individuals to also use this forecasting. Pakistan is very polluted and it has been well-established that air pollution has various negative effects on health and education. It benefits individuals to know the air quality around them to make informed decisions on daily activities.

# Learning Outcomes

I learned quite a bit from this project. First, I learned about the difficultes of cleaning data and the decisions to be made regarding variables that are not high quality. I was also able to practice applying statistical techniques to a real question on real data. I learned more about OLS and was able to learn about and implement machine learning techniques, namely Lasso, Bayesian Spike and Slab, and causal forests. I practiced good data visualization to effectively communicate my results and had to solve problems when my results didn't make sense, weren't satisfactory, or I felt I could push further. In addition, as this was my first real project, I had to navigate unfamiliar territory, but I learned that I really enjoy the process of being challenged. 

In addition, perhaps the most important outcome (I saved the best for last) was that of organization, documentation, and version control. While my directories are fairly well-organized by the type of analysis, the code itself could be better in its organization. Some functions are commented out, some are unused, and functions often reference another function that is nowhere near it. The code is sometimes well-commented, and sometimes not. Sometimes I went back to old code and spent time figuring out what I was thinking, as I had neglected to comment it, then yet again neglected to comment it. In addition, I learned that github is a little confusing but is worth wading through because it turns out that version control is quite useful.

I give this project a 10/10 in terms of learning opportunity. Thanks to Professor Arman Rezaee at UC Davis for having patience with me, especially when I abandoned the project for three months to go study abroad in New Zealand!


