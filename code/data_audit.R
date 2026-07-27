library(tidyverse)
file_path <- "data/raw/micro_world_139countries.csv"

micro_world_139countries <- read.csv(
  file_path, 
  fileEncoding = "UTF-8"
)

micro_world_139countries <- as_tibble(micro_world_139countries)

View(micro_world_139countries)

dim(micro_world_139countries)

nrow(micro_world_139countries)
ncol(micro_world_139countries)

glimpse(micro_world_139countries)

names(micro_world_139countries)[grep("^fin", names(micro_world_139countries))]
summary(micro_world_139countries$fin22a)
table(is.na(micro_world_139countries$fin22a))


summary(micro_world_139countries$female)
table(micro_world_139countries$female)
sum(is.na(micro_world_139countries$female))

summary(micro_world_139countries$educ)
table(micro_world_139countries$educ)
sum(is.na(micro_world_139countries$educ))

summary(micro_world_139countries$age)
table(micro_world_139countries$age)
sum(is.na(micro_world_139countries$age))


vars_project <- c(
  "economy",
  "economycode",
  "regionwb",
  "pop_adult",
  "wgt",
  "female",
  "age",
  "educ",
  "inc_q",
  "emp_in",
  "urbanicity_f2f",
  "account",
  "account_fin",
  "account_mob",
  "borrowed",
  "saved",
  "anydigpayment",
  "internetaccess"
)

audit_project <- tibble(
  Variable = vars_project,
  Type = sapply(micro_world_139countries[vars_project], class),
  Missing = sapply(micro_world_139countries[vars_project], function(x) sum(is.na(x))),
  Missing_pct = round(
    sapply(micro_world_139countries[vars_project], function(x) mean(is.na(x))) * 100,
    2
  )
)

view(audit_project)

vars <- c(
  "female",
  "age",
  "educ",
  "inc_q",
  "emp_in",
  "account",
  "account_fin",
  "borrowed",
  "saved",
  "anydigpayment",
  "internetaccess"
)

for(v in vars){
  cat("\n============================\n")
  cat(v, "\n")
  print(table(micro_world_139countries[[v]], useNA = "ifany"))
}