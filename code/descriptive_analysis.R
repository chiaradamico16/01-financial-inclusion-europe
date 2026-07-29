library(tidyverse)

micro_clean <- readRDS("data/processed/micro_world_clean.rds")

#cannot filter using variable region because Europe is together with central asia

micro_clean %>%
  distinct(regionwb) %>%
  arrange(regionwb)

#we see that Europe and Central Asia exclude high income so we need to manually select the countries

candidate_countries <- micro_clean %>%
  distinct(economy, regionwb) %>%
  filter(
    regionwb %in% c(
      "Europe & Central Asia (excluding high income)",
      "High income"
    )
  ) %>%
  arrange(regionwb, economy)
print(candidate_countries, n = Inf)


#I want to focus only on European countries
european_countries <- c(
  "Albania",
  "Austria",
  "Belgium",
  "Bosnia and Herzegovina",
  "Bulgaria",
  "Croatia",
  "Cyprus",
  "Czechia",
  "Denmark",
  "Estonia",
  "Finland",
  "France",
  "Germany",
  "Greece",
  "Hungary",
  "Iceland",
  "Ireland",
  "Italy",
  "Kosovo",
  "Latvia",
  "Lithuania",
  "Malta",
  "Moldova",
  "Netherlands",
  "North Macedonia",
  "Norway",
  "Poland",
  "Portugal",
  "Romania",
  "Russian Federation",
  "Serbia",
  "Slovak Republic",
  "Slovenia",
  "Spain",
  "Sweden",
  "Switzerland",
  "Ukraine",
  "United Kingdom"
)

europe <- micro_clean %>%
  filter(economy %in% european_countries)

#check everything is good
stopifnot(n_distinct(europe$economy) == length(european_countries))

cat("European sample\n")
cat("Observations:", nrow(europe), "\n")
cat("Countries:", n_distinct(europe$economy), "\n")


saveRDS(
  europe,
  "data/processed/europe_analysis.rds"
)

#start descriptive statistics

#tables

##sample characteristics
sample_summary <- europe %>%
  summarise(
    observations = n(),
    countries = n_distinct(economy),
    mean_age = weighted.mean(
      age,
      wgt,
      na.rm = TRUE
    ),
    sd_age = sd(
      age,
      na.rm = TRUE
    ),
    female_share = weighted.mean(
      female,
      wgt
    ),
    workforce_share = weighted.mean(
      emp_in == 1,
      wgt,
      na.rm = TRUE
    ),
    internet_access_share = weighted.mean(
      internetaccess,
      wgt
    )
  )
print(sample_summary)


##financial inclusion indicators
financial_summary <- europe %>%
  summarise(
    account_share = weighted.mean(
      account,
      wgt
    ),
    formal_account_share = weighted.mean(
      account_fin,
      wgt
    ),
    saved_share = weighted.mean(
      saved,
      wgt
    ),
    borrowed_share = weighted.mean(
      borrowed,
      wgt
    ),
    digital_payment_share = weighted.mean(
      anydigpayment,
      wgt
    )
  )

print(financial_summary, width = Inf)


##categorical distribution

##gender
gender_distribution <- europe %>%
  count(gender, wt = wgt, name = "weighted_count") %>%
  mutate(share = weighted_count / sum(weighted_count)  )
print(gender_distribution)

##income
income_distribution <- europe %>%
  count(income_quintile, wt = wgt, name = "weighted_count") %>%
  mutate(share = weighted_count / sum(weighted_count))
print(income_distribution)


##employment
employment_distribution <- europe %>%
  count(employment_status, wt = wgt, name = "weighted_count") %>%
  mutate(
    share = weighted_count / sum(weighted_count)
  )
print(employment_distribution)


##education
education_distribution <- europe %>%
  filter(
    !is.na(education),
    !is.na(wgt)
  ) %>%
  count(
    education,
    wt = wgt,
    name = "weighted_count"
  ) %>%
  mutate(
    share = weighted_count / sum(weighted_count)
  )
print(education_distribution)


#figures

dir.create(
  "figures",
  recursive = TRUE,
  showWarnings = FALSE
)


#define aesthetics
library(scales)

theme_paper <- function(base_size = 12) {
  
  theme_classic(
    base_size = base_size,
    base_family = "serif"
  ) +
    theme(
      plot.title = element_text(
        size = 14,
        face = "bold",
        hjust = 0.5,
        margin = margin(b = 10)
      ),
      
      plot.subtitle = element_text(
        size = 11,
        hjust = 0.5,
        margin = margin(b = 10)
      ),
      
      axis.title = element_text(
        size = 11
      ),
      
      axis.text = element_text(
        size = 10,
        colour = "black"
      ),
      
      axis.line = element_line(
        colour = "black",
        linewidth = 0.4
      ),
      
      axis.ticks = element_line(
        colour = "black",
        linewidth = 0.4
      ),
      
      legend.title = element_blank(),
      
      legend.position = "bottom",
      
      plot.caption = element_text(
        size = 9,
        hjust = 0,
        colour = "grey30",
        margin = margin(t = 10)
      ),
      
      plot.margin = margin(
        t = 15,
        r = 20,
        b = 15,
        l = 20
      )
    )
}


##age
figure_age <- ggplot(
  europe,
  aes(
    x = age,
    weight = wgt
  )
) +
  geom_histogram(
    aes(
      y = after_stat(count / sum(count))
    ),
    binwidth = 5,
    boundary = 15,
    fill = "grey35",
    colour = "white",
    linewidth = 0.3,
    na.rm = TRUE
  ) +
  scale_x_continuous(
    breaks = seq(15, 100, by = 10),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "Age distribution",
    x = "Age",
    y = "Weighted share of respondents",
    caption = "Note: Survey weights are applied. Bin width: five years."
  ) +
  theme_paper()
print(figure_age)

ggsave(
  filename = "figures/figure_01_age_distribution.png",
  plot = figure_age,
  width = 8,
  height = 5,
  dpi = 300,
  bg = "white"
)



#gender
figure_gender <- ggplot(
  gender_distribution,
  aes(
    x = gender,
    y = share
  )
) +
  geom_col(
    width = 0.65,
    fill = "grey35"
  ) +
  geom_text(
    aes(
      label = percent(
        share,
        accuracy = 0.1
      )
    ),
    vjust = -0.5,
    family = "serif",
    size = 3.8
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(
      0,
      max(gender_distribution$share) *1.12
    ),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    title = "Gender distribution",
    x = NULL,
    y = "Weighted share of respondents",
    caption = "Note: Survey weights are applied."
  ) +
  theme_paper()
print(figure_gender)

ggsave(
  filename = "figures/figure_02_gender_distribution.png",
  plot = figure_gender,
  width = 8,
  height = 5,
  dpi = 300,
  bg = "white"
)


#education 
figure_education <- ggplot(
  education_distribution,
  aes(
    x = education,
    y = share
  )
) +
  geom_col(
    width = 0.65,
    fill = "grey35"
  ) +
  geom_text(
    aes(
      label = percent(
        share,
        accuracy = 0.1
      )
    ),
    vjust = -0.5,
    family = "serif",
    size = 3.8
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(
      0,
      max(education_distribution$share) * 1.12
    ),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    title = "Educational attainment",
    x = NULL,
    y = "Weighted share of respondents",
    caption = "Note: Survey weights are applied."
  ) +
  theme_paper()
print(figure_education)

ggsave(
  filename = "figures/figure_03_education_distribution.png",
  plot = figure_education,
  width = 8,
  height = 5,
  dpi = 300,
  bg = "white"
)


#income quintile
figure_income <- ggplot(
  income_distribution,
  aes(
    x = income_quintile,
    y = share
  )
) +
  geom_col(
    width = 0.65,
    fill = "grey35"
  ) +
  geom_text(
    aes(
      label = percent(
        share,
        accuracy = 0.1
      )
    ),
    vjust = -0.5,
    family = "serif",
    size = 3.8
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(
      0,
      max(income_distribution$share) * 1.12
    ),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    title = "Income quintile",
    x = NULL,
    y = "Weighted share of respondents",
    caption = "Note: Survey weights are applied."
  ) +
  theme_paper()
print(figure_income)

ggsave(
  filename = "figures/figure_04_income_quintile_distribution.png",
  plot = figure_income,
  width = 8,
  height = 5,
  dpi = 300,
  bg = "white"
)


#employment
figure_employment <- ggplot(
  employment_distribution,
  aes(
    x = employment_status,
    y = share
  )
) +
  geom_col(
    width = 0.65,
    fill = "grey35"
  ) +
  geom_text(
    aes(
      label = percent(
        share,
        accuracy = 0.1
      )
    ),
    vjust = -0.5,
    family = "serif",
    size = 3.8
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(
      0,
      max(employment_distribution$share) * 1.12
    ),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    title = "Employment status",
    x = NULL,
    y = "Weighted share of respondents",
    caption = "Note: Survey weights are applied."
  ) +
  theme_paper()
print(figure_employment)

ggsave(
  filename = "figures/figure_05_employment_status.png",
  plot = figure_employment,
  width = 8,
  height = 5,
  dpi = 300,
  bg = "white"
)


#financial inclusion (first prepare data)
financial_plot <- financial_summary %>%
  pivot_longer(
    everything(),
    names_to = "indicator",
    values_to = "share"
  ) %>%
  mutate(
    indicator = recode(
      indicator,
      account_share = "Account",
      formal_account_share = "Formal account",
      saved_share = "Saved",
      borrowed_share = "Borrowed",
      digital_payment_share = "Digital payment"
    )
  ) %>%
  arrange(desc(share)) %>%
  mutate(
    indicator = factor(
      indicator,
      levels = indicator
    )
  )


figure_financial <- ggplot(
  financial_plot,
  aes(
    x = indicator,
    y = share
  )
) +
  geom_col(
    width = 0.65,
    fill = "grey35"
  ) +
  geom_text(
    aes(
      label = percent(
        share,
        accuracy = 0.1
      )
    ),
    vjust = -0.5,
    family = "serif",
    size = 3.8
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 1),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    title = "Financial inclusion indicators",
    x = NULL,
    y = "Weighted share of respondents",
    caption = "Note: Survey weights are applied."
  ) +
  theme_paper()
print(figure_financial)

ggsave(
  filename = "figures/figure_06_financial_inclusion.png",
  plot = figure_financial,
  width = 8,
  height = 5,
  dpi = 300,
  bg = "white"
)



