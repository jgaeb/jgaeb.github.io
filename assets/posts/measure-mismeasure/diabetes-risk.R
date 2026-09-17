options(tidyverse.quiet = TRUE)
library(tidyverse)

# Set seed
set.seed(31371648)

#################################### LOAD ######################################
# NOTE: Thanks to Madison Coots for the data loading code.

# 2011-2012
download.file("https://wwwn.cdc.gov/nchs/nhanes/2011-2012/DEMO_G.XPT",
              demo <- tempfile(), mode="wb", quiet = TRUE)
download.file("https://wwwn.cdc.gov/nchs/nhanes/2011-2012/DIQ_G.XPT",
              diq <- tempfile(), mode="wb", quiet = TRUE)
download.file("https://wwwn.cdc.gov/nchs/nhanes/2011-2012/BMX_G.XPT",
              bmx <- tempfile(), mode="wb", quiet = TRUE)
download.file("https://wwwn.cdc.gov/nchs/nhanes/2011-2012/GHB_G.XPT",
              ghb <- tempfile(), mode="wb", quiet = TRUE)
raw_demographics_11_12 <- foreign::read.xport(demo) %>% 
  janitor::clean_names()
raw_survey_responses_11_12 <- foreign::read.xport(diq) %>% 
  janitor::clean_names()
raw_body_measurements_11_12 <- foreign::read.xport(bmx) %>% 
  janitor::clean_names()
raw_glycohemoglobin_11_12 <- foreign::read.xport(ghb) %>% 
  janitor::clean_names()

# 2013-2014
download.file("https://wwwn.cdc.gov/nchs/nhanes/2013-2014/DEMO_H.XPT",
              demo <- tempfile(), mode="wb", quiet = TRUE)
download.file("https://wwwn.cdc.gov/nchs/nhanes/2013-2014/DIQ_H.XPT",
              diq <- tempfile(), mode="wb", quiet = TRUE)
download.file("https://wwwn.cdc.gov/nchs/nhanes/2013-2014/BMX_H.XPT",
              bmx <- tempfile(), mode="wb", quiet = TRUE)
download.file("https://wwwn.cdc.gov/nchs/nhanes/2013-2014/GHB_H.XPT",
              ghb <- tempfile(), mode="wb", quiet = TRUE)
raw_demographics_13_14 <- foreign::read.xport(demo) %>% 
  janitor::clean_names()
raw_survey_responses_13_14 <- foreign::read.xport(diq) %>% 
  janitor::clean_names()
raw_body_measurements_13_14 <- foreign::read.xport(bmx) %>% 
  janitor::clean_names()
raw_glycohemoglobin_13_14 <- foreign::read.xport(ghb) %>% 
  janitor::clean_names()

# 2015-2016
download.file("https://wwwn.cdc.gov/nchs/nhanes/2015-2016/DEMO_I.XPT",
              demo <- tempfile(), mode="wb", quiet = TRUE)
download.file("https://wwwn.cdc.gov/nchs/nhanes/2015-2016/DIQ_I.XPT",
              diq <- tempfile(), mode="wb", quiet = TRUE)
download.file("https://wwwn.cdc.gov/nchs/nhanes/2015-2016/BMX_I.XPT",
              bmx <- tempfile(), mode="wb", quiet = TRUE)
download.file("https://wwwn.cdc.gov/nchs/nhanes/2015-2016/GHB_I.XPT",
              ghb <- tempfile(), mode="wb", quiet = TRUE)
raw_demographics_15_16 <- foreign::read.xport(demo) %>% 
  janitor::clean_names()
raw_survey_responses_15_16 <- foreign::read.xport(diq) %>% 
  janitor::clean_names()
raw_body_measurements_15_16 <- foreign::read.xport(bmx) %>% 
  janitor::clean_names()
raw_glycohemoglobin_15_16 <- foreign::read.xport(ghb) %>% 
  janitor::clean_names()

# 2017-2018
download.file("https://wwwn.cdc.gov/nchs/nhanes/2017-2018/DEMO_J.XPT",
              demo <- tempfile(), mode="wb", quiet = TRUE)
download.file("https://wwwn.cdc.gov/nchs/nhanes/2017-2018/DIQ_J.XPT",
              diq <- tempfile(), mode="wb", quiet = TRUE)
download.file("https://wwwn.cdc.gov/nchs/nhanes/2017-2018/BMX_J.XPT",
              bmx <- tempfile(), mode="wb", quiet = TRUE)
download.file("https://wwwn.cdc.gov/nchs/nhanes/2017-2018/GHB_J.XPT",
              ghb <- tempfile(), mode="wb", quiet = TRUE)
raw_demographics_17_18 <- foreign::read.xport(demo) %>% 
  janitor::clean_names()
raw_survey_responses_17_18 <- foreign::read.xport(diq) %>% 
  janitor::clean_names()
raw_body_measurements_17_18 <- foreign::read.xport(bmx) %>% 
  janitor::clean_names()
raw_glycohemoglobin_17_18 <- foreign::read.xport(ghb) %>% 
  janitor::clean_names()

# Demographics data
raw_demographics_all <- bind_rows(
    raw_demographics_11_12,
    raw_demographics_13_14,
    raw_demographics_15_16,
    raw_demographics_17_18
  ) %>%
  as_tibble()

# Survey data
raw_survey_responses_all <- bind_rows(
    raw_survey_responses_11_12,
    raw_survey_responses_13_14,
    raw_survey_responses_15_16,
    raw_survey_responses_17_18
  ) %>%
  as_tibble()

# Body measurements data
raw_body_measurements_all <- bind_rows(
    raw_body_measurements_11_12,
    raw_body_measurements_13_14,
    raw_body_measurements_15_16,
    raw_body_measurements_17_18
  ) %>%
  as_tibble()

# Glycohemoglobin data
raw_glycohemoglobin_all <- bind_rows(
    raw_glycohemoglobin_11_12,
    raw_glycohemoglobin_13_14,
    raw_glycohemoglobin_15_16,
    raw_glycohemoglobin_17_18
  ) %>%
  as_tibble()

# Join into one dataset and add outcome label
df <- raw_demographics_all %>%
  full_join(raw_survey_responses_all, by = "seqn") %>%
  full_join(raw_body_measurements_all, by = "seqn") %>%
  full_join(raw_glycohemoglobin_all, by = "seqn") %>%
  mutate(
    lbxgh = as.numeric(as.character((lbxgh))),
    diq010 = as.numeric(as.character((diq010))),
    a1c = cut(lbxgh,breaks=c(0,5.7,6.5,1000),right=FALSE),
    diabetes_diagnosis = case_when(
      diq010 %in% 1 ~ 1,
      diq010 %in% c(2,3,9) ~ 0,
      diq010 %in% 7 ~ as.numeric(NA),
    ),
    diabetes = diabetes_diagnosis,
    diabetes = if_else(a1c=="[6.5,1e+03)" &!is.na(a1c), 1, diabetes),
    diabetes = as.integer(diabetes),
    diabetes = if_else(diabetes == 1, TRUE, FALSE),
    # Normalize weights
    weights = wtmec2yr / sum(wtmec2yr),
    # Normalize race
    race = case_when(
      ridreth3 == 1 ~ "Hispanic",
      ridreth3 == 2 ~ "Hispanic",
      ridreth3 == 3 ~ "White",
      ridreth3 == 4 ~ "Black",
      ridreth3 == 6 ~ "Asian",
      ridreth3 == 7 ~ "Other"
    )
  )

# Simplify the dataframe
df_simp <- df %>%
  select(diabetes, age = ridageyr, bmi = bmxbmi, race = race, weights) %>%
  filter(!is.na(diabetes), !is.na(bmi))

#################################### TRAIN #####################################

# Fit logistic regression
m_full <- df_simp %>%
  glm(diabetes ~ age + bmi + race, data = ., family = binomial(),
      weights = weights)

risk <- df_simp %>%
  predict(m_full, newdata = ., type = "response")

df_simp$risk <- risk

################################### ANALYZE ####################################
  
# Calculate means
means <- df_simp %>%
  group_by(race) %>%
  summarize(mean = weighted.mean(risk, weights), .groups = "drop") %>%
  mutate(density_type = "Distribution of risk") %>%
  filter(race %in% c("Asian", "White"))

# Plot the risk distributions
p_density <- df_simp %>%
  filter(race %in% c("Asian", "White")) %>%
  group_by(race) %>%
  mutate(weights = weights / sum(weights)) %>%
  ggplot(aes(x = risk, color = race)) +
  geom_density(aes(weight = weights), bw = 0.01) +
  geom_vline(aes(xintercept = mean, color = race), linetype = "dashed", data = means) +
  geom_vline(xintercept = .015) +
  labs(
    x = "Probability of having diabetes",
    y = "Density",
    color = NULL
  ) +
  scale_color_brewer(palette = "Set1") +
  scale_x_continuous(
    label = scales::label_percent(),
    limits = c(0, 0.2),
    expand = c(0,0)
  ) +
  scale_y_continuous(label = NULL, expand = c(0,1)) +
  theme_bw() +
  theme(
    legend.justification = c(1, 1),
    legend.position = c(1, 1),
    legend.background = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title = element_text(size = 9),
    axis.text = element_text(size = 8),
    legend.text = element_text(size = 9),
    panel.spacing = unit(1.5, "lines"),
    plot.margin = margin(10, 10, 10, 10, "pt")
  )

suppressWarnings(ggsave(
  "risk-distributions.svg",
  plot = p_density,
  width = 4,
  height = 4,
))
