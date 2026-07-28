file_path <- "data/raw/micro_world_139countries.csv"

micro_world_139countries <- read.csv(
  file_path,
  fileEncoding = "Windows-1252"
) %>%
  as_tibble()

#verify that the complete dataset was imported
stopifnot(nrow(micro_world_139countries) == 143887)
stopifnot(n_distinct(micro_world_139countries$economy) == 139)

#select variables of interest
variables_to_keep <- c(
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
  "account",
  "account_fin",
  "borrowed",
  "saved",
  "anydigpayment",
  "internetaccess"
)

#verify that all required variables exist
missing_variables <- setdiff(
  variables_to_keep,
  names(micro_world_139countries)
)

stopifnot(length(missing_variables) == 0)

micro_clean <- micro_world_139countries %>%
  select(all_of(variables_to_keep))

#remove countries without country identifier
micro_clean <- micro_clean %>%
  filter(!is.na(economycode))


#clean, recode and label variables and create labelled factors
micro_clean <- micro_clean %>%
  mutate(
    
    #original coding: 1 = female, 2 = male
    female = case_when(
      female == 1 ~ 1L,
      female == 2 ~ 0L,
      TRUE ~ NA_integer_
    ),
    
    gender = factor(
      female,
      levels = c(0, 1),
      labels = c("Male", "Female")
    ),
    
    #the codebook reports three education categories, so undocumented values 4 and 5 are treated as missing.
    educ = case_when(
      educ %in% 1:3 ~ educ,
      TRUE ~ NA_integer_
    ),
    
    education = factor(
      educ,
      levels = c(1, 2, 3),
      labels = c(
        "Primary or less",
        "Secondary",
        "Tertiary or more"
      ),
      ordered = TRUE
    ),
    
    income_quintile = factor(
      inc_q,
      levels = 1:5,
      labels = c(
        "Poorest",
        "Second",
        "Middle",
        "Fourth",
        "Richest"
      ),
      ordered = TRUE
    ),
    
    employment_status = factor(
      emp_in,
      levels = c(1, 2),
      labels = c(
        "In workforce",
        "Out of workforce"
      )
    ),
    
    #original coding: 1 = yes, 2 = no, 3 = don't know, 4 = refused
    internetaccess = case_when(
      internetaccess == 1 ~ 1L,
      internetaccess == 2 ~ 0L,
      internetaccess %in% c(3, 4) ~ NA_integer_,
      TRUE ~ NA_integer_
    )
  )

#control the cleaned dataset to make sure evreything is ok
stopifnot(n_distinct(micro_clean$economy) == 139)

stopifnot(
  all(
    unique(na.omit(micro_clean$female)) %in% c(0, 1)
  )
)

stopifnot(
  all(
    unique(na.omit(micro_clean$educ)) %in% c(1, 2, 3)
  )
)

stopifnot(
  all(
    unique(na.omit(micro_clean$internetaccess)) %in% c(0, 1)
  )
)

cat("Cleaning completed successfully.\n")
cat("Observations:", nrow(micro_clean), "\n")
cat("Variables:", ncol(micro_clean), "\n")
cat(
  "Countries:",
  n_distinct(micro_clean$economy),
  "\n"
)

glimpse(micro_clean)

#save cleaned dataset
dir.create(
  "data/processed",
  recursive = TRUE,
  showWarnings = FALSE
)

saveRDS(
  micro_clean,
  "data/processed/micro_world_clean.rds"
)
