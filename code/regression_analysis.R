library(tidyverse)
library(fixest)

europe <- readRDS(
  "data/processed/europe_analysis.rds"
)

dir.create(
  "tables",
  recursive = TRUE,
  showWarnings = FALSE
)

#prepare variables for regression
regression_data <- europe %>%
  mutate(
    #center age to make the quadratic term easier to handle
    age_centered = age - 45,
    age_centered_sq = age_centered^2 #to capture non-linear relationships
  )

#main regression sample
main_sample <- regression_data %>%
  filter(
    !is.na(account),
    !is.na(female),
    !is.na(age_centered),
    !is.na(educ),
    !is.na(inc_q),
    !is.na(emp_in),
    !is.na(wgt),
    !is.na(economy)
  )

cat("Regression sample\n")
cat("Observations:", nrow(main_sample), "\n")
cat(
  "Countries:",
  n_distinct(main_sample$economy),
  "\n"
)

stopifnot(
  all(!is.na(main_sample$account)),
  all(!is.na(main_sample$anydigpayment)),
  all(!is.na(main_sample$saved)),
  all(!is.na(main_sample$borrowed))
)

##outcome = account
#model 1: demographics and country fixed effects
model_account_demographics <- feols(
  account ~
    female +
    age_centered +
    age_centered_sq |
    economy,
  data = main_sample,
  weights = ~ wgt,
  cluster = ~ economy
)

#model 2: main specification
model_account_full <- feols(
  account ~
    female +
    age_centered +
    age_centered_sq +
    i(educ, ref = 1) +
    i(inc_q, ref = 1) +
    i(emp_in, ref = 2) |
    economy,
  data = main_sample,
  weights = ~ wgt,
  cluster = ~ economy
)

#create main regression table
table_main <- etable(
  "Demographic controls" = model_account_demographics,
  "Full specification" = model_account_full,
  digits = 3,
  fitstat = ~ n + r2,
  signif.code = c(
    "***" = 0.001,
    "**" = 0.01,
    "*" = 0.05,
    "." = 0.1
  )
)
print(table_main)

#save LaTeX version
etable(
  "Demographic controls" = model_account_demographics,
  "Full specification" = model_account_full,
  digits = 3,
  fitstat = ~ n + r2,
  signif.code = c(
    "***" = 0.001,
    "**" = 0.01,
    "*" = 0.05,
    "." = 0.1
  ),
  caption = "Account ownership: main regression results",
  label = "tab:main_regressions",
  file = "tables/table_01_main_regressions.tex",
  replace = TRUE
)



##alternative outcomes

#digital payments
model_digital <- feols(
  anydigpayment ~
    female +
    age_centered +
    age_centered_sq +
    i(educ, ref = 1) +
    i(inc_q, ref = 1) +
    i(emp_in, ref = 2) |
    economy,
  data = main_sample,
  weights = ~ wgt,
  cluster = ~ economy
)


#saving
model_saving <- feols(
  saved ~
    female +
    age_centered +
    age_centered_sq +
    i(educ, ref = 1) +
    i(inc_q, ref = 1) +
    i(emp_in, ref = 2) |
    economy,
  data = main_sample,
  weights = ~ wgt,
  cluster = ~ economy
)


#borrowing
model_borrowing <- feols(
  borrowed ~
    female +
    age_centered +
    age_centered_sq +
    i(educ, ref = 1) +
    i(inc_q, ref = 1) +
    i(emp_in, ref = 2) |
    economy,
  data = main_sample,
  weights = ~ wgt,
  cluster = ~ economy
)

#create alternative-outcomes table
table_outcomes <- etable(
  "Account ownership" = model_account_full,
  "Digital payments" = model_digital,
  "Saving" = model_saving,
  "Borrowing" = model_borrowing,
  digits = 3,
  fitstat = ~ n + r2,
  signif.code = c(
    "***" = 0.001,
    "**" = 0.01,
    "*" = 0.05,
    "." = 0.1
  )
)
print(table_outcomes)


#save LaTeX version
etable(
  "Account ownership" = model_account_full,
  "Digital payments" = model_digital,
  "Saving" = model_saving,
  "Borrowing" = model_borrowing,
  digits = 3,
  fitstat = ~ n + r2,
  signif.code = c(
    "***" = 0.001,
    "**" = 0.01,
    "*" = 0.05,
    "." = 0.1
  ),
  caption = "Financial inclusion: alternative outcomes",
  label = "tab:alternative_outcomes",
  file = "tables/table_02_alternative_outcomes.tex",
  replace = TRUE
)

