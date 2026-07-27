library(tidyverse)

#select variables of interest
micro_clean <- micro_world_139countries %>%
  select(
    economy,
    economycode,
    regionwb,
    pop_adult,
    wgt,
    female,
    age,
    educ,
    inc_q,
    emp_in,
    account,
    account_fin,
    borrowed,
    saved,
    anydigpayment,
    internetaccess
  )

micro_clean <- micro_clean %>%
  filter(!is.na(economycode))

micro_clean <- micro_clean %>%
  mutate(
    
    # make it binary since original coding is 1 = female, 2 = male
    female = case_when(
      female == 1 ~ 1,
      female == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    
    # again make it binary since internet access: 1 = yes, 2 = no,
    # 3 = don't know, 4 = refused
    internetaccess = case_when(
      internetaccess == 1 ~ 1,
      internetaccess == 2 ~ 0,
      internetaccess %in% c(3, 4) ~ NA_real_,
      TRUE ~ NA_real_
    ),
    
    # add labels
    gender = factor(
      female,
      levels = c(0, 1),
      labels = c("Male", "Female")
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
      labels = c("In workforce", "Out of workforce")
    )
)

glimpse(micro_clean)

colSums(is.na(micro_clean))

table(micro_clean$educ, useNA = "ifany")

attributes(micro_world_139countries$educ)

haven::as_factor(micro_world_139countries$educ) %>%
  table(useNA = "ifany")

#The education variable contained 192 observations coded as 4 or 5, although the official codebook documents only three education categories. These observations were treated as missing values to ensure consistency with the documented coding scheme.



micro_clean <- micro_clean %>%
  mutate(
    educ = if_else(educ %in% c(4,5), NA_integer_, educ),
    
    education = factor(
      educ,
      levels = c(1,2,3),
      labels = c(
        "Primary or less",
        "Secondary",
        "Tertiary or more"
      ),
      ordered = TRUE
    )
  )

saveRDS(
  micro_clean,
  "data/processed/micro_world_clean.rds"
)