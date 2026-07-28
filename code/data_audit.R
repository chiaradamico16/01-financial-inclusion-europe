library(tidyverse)

file_path <- "data/raw/micro_world_139countries.csv"

micro_world_139countries <- read.csv(
  file_path,
  fileEncoding = "Windows-1252"
) %>%
  as_tibble()

#verify that the entire dataset was imported
stopifnot(nrow(micro_world_139countries) == 143887)
stopifnot(ncol(micro_world_139countries) == 128)
stopifnot(n_distinct(micro_world_139countries$economy) == 139)

cat("Dataset successfully imported.\n")
cat("Observations:", nrow(micro_world_139countries), "\n")
cat("Variables:", ncol(micro_world_139countries), "\n")
cat(
  "Countries:",
  n_distinct(micro_world_139countries$economy),
  "\n"
)

#check the structure of the dataset 
glimpse(micro_world_139countries)


#check variables relevant for the project and verify they exist
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

missing_variables <- setdiff(
  vars_project,
  names(micro_world_139countries)
)

stopifnot(length(missing_variables) == 0)


#check the quality of the variables, checking types and missing values
audit_project <- tibble(
  variable = vars_project,
  type = map_chr(
    micro_world_139countries[vars_project],
    ~ class(.x)[1]
  ),
  missing = map_int(
    micro_world_139countries[vars_project],
    ~ sum(is.na(.x))
  ),
  missing_pct = map_dbl(
    micro_world_139countries[vars_project],
    ~ round(mean(is.na(.x)) * 100, 2)
  )
)

print(audit_project, n = Inf)

#check some of the variables more in detail

#categorical
categorical_variables <- c(
  "female",
  "educ",
  "inc_q",
  "emp_in",
  "account",
  "account_fin",
  "account_mob",
  "borrowed",
  "saved",
  "anydigpayment",
  "internetaccess"
)

for (variable in categorical_variables) {
  
  cat("\n")
  cat("============================================================\n")
  cat("Variable:", variable, "\n")
  cat("============================================================\n")
  
  print(
    table(
      micro_world_139countries[[variable]],
      useNA = "ifany"
    )
  )
}

#numerical
summary(micro_world_139countries$age)


#analyse the geographical coverage of the dataset
country_coverage <- micro_world_139countries %>%
  distinct(economy, economycode, regionwb) %>%
  arrange(economy)

cat("\nNumber of countries by World Bank regional category:\n")

country_coverage %>%
  count(regionwb, name = "countries") %>%
  arrange(desc(countries)) %>%
  print(n = Inf)
