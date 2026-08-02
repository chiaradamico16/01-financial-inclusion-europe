# Financial Inclusion in Europe

## Motivation
This project builds on the extensive literature concerning financial inclusion, a key element for economic development and poverty reduction. 
While many European economies have high levels of account ownership, still disparities persist. Thus, this project aims at exploring financial inclusion across European economies, with a specific focus on identifying potential gaps related to gender, age, education, and income.
Understanding these disparities is crucial when designing policies to ensure access to financial services to all European citizens. 

## Research Question
How does account ownership vary across European economies, and how are demographic and socioeconomic characteristics associated with different levels of financial inclusion within countries?

## Dataset
The data used in this project is sourced from the updated individual-level release of the Global Findex 2021 Database. It is composed of interviews with individuals aged 15 or older. The standardized questionnaire covers access to financial instruments, the use of digital payments, savings, credit, and financial resilience.
The data are weighted to represent the national adult population and cover 139 economies worldwide, with 143,887 individuals interviewed.

## Methodology
The analysis proceeds in three stages.
First, the raw data are audited and cleaned. Variables are selected, recoded, labelled, and checked for missing values. The sample is then restricted to 38 European economies.
Second, weighted descriptive statistics and visualizations are used to examine the demographic composition of the sample and the distribution of financial inclusion indicators.
Third, weighted Linear Probability Models are estimated for four binary outcomes:
* account ownership;
* use of digital payments;
* saving;
* borrowing.

The main regressors are gender, age, education, income quintile, and workforce status. All regressions include country fixed effects and standard errors clustered at the country level.
The country fixed effects account for characteristics shared by individuals living in the same economy, such as institutional conditions, financial infrastructure, and the overall level of financial development. The estimated coefficients should therefore be interpreted as conditional associations within countries rather than causal effects.

## Main Results
The results reveal socioeconomic differences in financial inclusion within European economies.

Higher education, income, and workforce participation are positively associated with account ownership, digital payment use, and saving. These relationships are strongest for saving, where the differences across income groups are larger than for the other outcomes.

In the main account ownership specification:

- secondary education is associated with an approximately 7.9 percentage point higher probability of owning an account relative to primary education or less;
- tertiary education is associated with an approximately 9.6 percentage point higher probability;
- individuals in the richest income quintile have an approximately 6.4 percentage point higher probability than those in the poorest quintile;
- workforce participation is associated with an approximately 5.1 percentage point higher probability.

The estimated gender gap in account ownership becomes smaller and statistically insignificant after education, income, and workforce status are included. A similar result emerges for digital payments. Women are, however, less likely to report saving and borrowing after controlling for the other observed characteristics.

Borrowing follows a different pattern from the other outcomes and this may be because borrowing reflects both access to credit and the need to borrow money.

These results show associations between the variables, not causal effects.

## Repository Structure
```text
.
├── code/
│   ├── data_audit.R
│   ├── data_cleaning.R
│   ├── descriptive_analysis.R
│   └── regression_analysis.R
├── data/
│   └── processed/
│       ├── europe_analysis.rds
│       └── micro_world_clean.rds
├── figures/
├── tables/
├── paper/
├── presentation/
├── .gitignore
├── LICENSE
└── README.md
```

## Reproducibility
To reproduce the analysis download the Global Findex 2021 microdata and run the scripts in the order indicated in the `code/` folder.
The scripts generate the processed datasets, figures, and regression tables used in the report. The analysis was conducted in R using `tidyverse`, `fixest`, and `scales`.

