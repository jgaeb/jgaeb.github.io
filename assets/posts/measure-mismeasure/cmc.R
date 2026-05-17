options(tidyverse.quiet = TRUE)
library(gridExtra)
library(tidyverse)

`%T>%` <- magrittr::`%T>%`

# Set seed
set.seed(31371648)

#################################### LOAD ######################################

data_url <- "https://gitlab.com/labsysmed/dissecting-bias/-/raw/master/data/data_new.csv"

# Load data frame
df <- read_csv(data_url, show_col_types = FALSE) %>%
  # Is the patient in top 10% of cost?
  mutate(Y = cost_t > quantile(cost_t, 0.9))

#################################### TRAIN #####################################

# Train the model without race
m <- df %>%
  select(-ends_with("_t"), -race) %>%
  glm(Y ~ ., data = ., family = binomial)

df$risk <- predict(m, type = "response")

################################### ANALYZE ####################################

# Generate Pareto frontier for all patients
# 2% of people enrolled in program
n_prog <- as.integer(nrow(df) * 0.02)
# Threshold is risk of marginal person
t_black <- with(
  df,
  sort(risk[race == "black"], decreasing = TRUE)[1:n_prog]
)
t_white <- rev(with(
  df,
  sort(risk[race == "white"], decreasing = TRUE)[1:n_prog]
))
# Number of expected high-cost individuals from each group
n_black <- c(0, cumsum(t_black))
n_white <- rev(c(0, cumsum(rev(t_white))))
# Proportion of positive decisions
p_black <- with(df, seq(0, n_prog) / sum(race == "black"))
p_white <- with(df, seq(n_prog, 0) / sum(race == "white"))
# False negative rate. NOTE: For smoothing, we use model predictions.
fnr_black <- 1 - n_black / with(df, sum(risk[race == "black"]))
fnr_white <- 1 - n_white / with(df, sum(risk[race == "white"]))
# False positve rate. NOTE: For smoothing, we use model predictions.
fpr_black <- with(
  df,
  (seq(0, n_prog) - n_black)
    / (sum(race == "black") - sum(risk[race == "black"]))
)
fpr_white <- with(
  df,
  (seq(n_prog, 0) - n_white)
    / (sum(race == "white") - sum(risk[race == "white"]))
)

# Calculate the positions of the maximum objective, demographic parity,
# equalized false positive rate, and equalized false negative rate points.
# NOTE: Because of one-indexing, these are off by 1.
n_max <- which(n_black + n_white == max(n_black + n_white))
n_dp <- which(abs(p_black - p_white) == min(abs(p_black - p_white)))
n_fpr <- which(abs(fpr_black - fpr_white) == min(abs(fpr_black - fpr_white)))
n_fnr <- which(abs(fnr_black - fnr_white) == min(abs(fnr_black - fnr_white)))

# Convert them into data frames for easy plotting.
# NOTE: Because arrays are one-indexed, these are off by one.
df_dp <- tibble(
  obj = (n_black + n_white)[n_dp],
  n_black = n_dp - 1,
  label = "DP,\nFNR"
)
df_fpr <- tibble(
  obj = (n_black + n_white)[n_fpr],
  n_black = n_fpr - 1,
  label = "FPR"
)
# NOTE: FNR is almost on top of DP, so we don't plot it.
df_fnr <- tibble(
  obj = (n_black + n_white)[n_fnr],
  n_black = n_fnr - 1,
  label = "FNR"
)

df_left <- tibble(
    # Number of high cost patients admitted
    obj = n_black + n_white,
    # Number of black patients admitted
    n_black = seq(p_black) - 1
  ) %>%
  filter(n_black < n_max)
df_right <- tibble(
    # Number of high cost patients admitted
    obj = n_black + n_white,
    # Number of black patients admitted
    n_black = seq(p_black) - 1
  ) %>%
  filter(n_black >= n_max)
p_pareto_full <- ggplot(mapping = aes(x = n_black, y = obj)) +
  geom_line(data = df_left, linetype = "dotted") +
  geom_line(data = df_right, linetype = "solid") +
  # NOTE: Because arrays are one indexed, n_max is off by 1.
  geom_vline(xintercept = n_max - 1, linetype = "dashed") +
  geom_point(data = df_dp, color = RColorBrewer::brewer.pal(3, "Dark2")[[1]]) +
  geom_point(data = df_fpr, color = RColorBrewer::brewer.pal(3, "Dark2")[[2]]) +
  geom_text(
    aes(label = label),
    data = df_dp,
    size = 2.5,
    color = RColorBrewer::brewer.pal(3, "Dark2")[[1]],
    nudge_x = -60,
    nudge_y = 10
  ) +
  geom_text(
    aes(label = label),
    data = df_fpr,
    size = 2.5,
    color = RColorBrewer::brewer.pal(3, "Dark2")[[2]],
    nudge_x = -10,
    nudge_y = 15
  ) +
  scale_color_brewer(palette = "Set1") +
  labs(
    x = "Total num. Black patients",
    y = "Total num. high-cost patients"
  ) +
  coord_cartesian(xlim = c(0, 667), ylim = c(600, 800), expand = FALSE) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.title = element_text(size = 9),
    axis.text = element_text(size = 8),
    legend.text = element_text(size = 9)
  )

# Generate Pareto frontier for women between ages 25 and 34
# 2% of people enrolled in program
n_prog <- with(
  df,
  as.integer(sum(dem_female == 1 & `dem_age_band_25-34_tm1` == 1) * 0.02 + 10)
)
# Threshold is risk of marginal person
t_black <- with(
  filter(df, dem_female == 1, `dem_age_band_25-34_tm1` == 1),
  sort(risk[race == "black"], decreasing = TRUE)[1:n_prog]
)
t_white <- rev(with(
  filter(df, dem_female == 1, `dem_age_band_25-34_tm1` == 1),
  sort(risk[race == "white"], decreasing = TRUE)[1:n_prog]
))
# Number of expected high-cost individuals from each group
n_black <- c(0, cumsum(t_black))
n_white <- rev(c(0, cumsum(rev(t_white))))
# Proportion of positive decisions
p_black <- with(
  filter(df, dem_female == 1, `dem_age_band_25-34_tm1` == 1),
  seq(0, n_prog) / sum(race == "black")
)
p_white <- with(
  filter(df, dem_female == 1, `dem_age_band_25-34_tm1` == 1),
  seq(n_prog, 0) / sum(race == "white")
)
# False negative rate. NOTE: For smoothing, we use model predictions.
fnr_black <- 1 - n_black / with(
  filter(df, dem_female == 1, `dem_age_band_25-34_tm1` == 1),
  sum(risk[race == "black"])
)
fnr_white <- 1 - n_white / with(
  filter(df, dem_female == 1, `dem_age_band_25-34_tm1` == 1),
  sum(risk[race == "white"])
)
# False positve rate. NOTE: For smoothing, we use model predictions.
fpr_black <- with(
  filter(df, dem_female == 1, `dem_age_band_25-34_tm1` == 1),
  (seq(0, n_prog) - n_black)
    / (sum(race == "black") - sum(risk[race == "black"]))
)
fpr_white <- with(
  filter(df, dem_female == 1, `dem_age_band_25-34_tm1` == 1),
  (seq(n_prog, 0) - n_white)
    / (sum(race == "white") - sum(risk[race == "white"]))
)

# Calculate the positions of the maximum objective, demographic parity,
# equalized false positive rate, and equalized false negative rate points.
# NOTE: Because of one-indexing, these are off by 1.
n_max <- which(n_black + n_white == max(n_black + n_white))
n_dp <- which(abs(p_black - p_white) == min(abs(p_black - p_white)))
n_fpr <- which(abs(fpr_black - fpr_white) == min(abs(fpr_black - fpr_white)))
n_fnr <- which(abs(fnr_black - fnr_white) == min(abs(fnr_black - fnr_white)))

# Convert them into data frames for easy plotting.
# NOTE: Because arrays are one-indexed, these are off by one.
df_dp <- tibble(
  obj = (n_black + n_white)[n_dp],
  n_black = n_dp - 1,
  label = "DP,\nFPR"
)
# NOTE: FPR is on top of DP, so we don't plot it.
df_fpr <- tibble(
  obj = (n_black + n_white)[n_fpr],
  n_black = n_fpr - 1,
  label = "FPR"
)
df_fnr <- tibble(
  obj = (n_black + n_white)[n_fnr],
  n_black = n_fnr - 1,
  label = "FNR"
)

df_left <- tibble(
    # Number of high cost patients admitted
    obj = n_black + n_white,
    # Number of black patients admitted
    n_black = seq(p_black) - 1
  ) %>%
  filter(n_black < n_max)
df_right <- tibble(
    # Number of high cost patients admitted
    obj = n_black + n_white,
    # Number of black patients admitted
    n_black = seq(p_black) - 1
  ) %>%
  filter(n_black >= n_max)
p_pareto_sub <- ggplot(mapping = aes(x = n_black, y = obj)) +
  geom_line(data = df_left, linetype = "dotted") +
  geom_line(data = df_right, linetype = "solid") +
  # NOTE: Because arrays are one indexed, n_max is off by 1.
  geom_vline(xintercept = n_max - 1, linetype = "dashed") +
  geom_point(data = df_dp, color = RColorBrewer::brewer.pal(3, "Dark2")[[1]]) +
  geom_point(data = df_fnr, color = RColorBrewer::brewer.pal(3, "Dark2")[[3]]) +
  geom_text(
    aes(label = label),
    data = df_dp,
    size = 2.5,
    color = RColorBrewer::brewer.pal(3, "Dark2")[[1]],
    nudge_x = 0,
    nudge_y = 1.5
  ) +
  geom_text(
    aes(label = label),
    data = df_fnr,
    size = 2.5,
    color = RColorBrewer::brewer.pal(3, "Dark2")[[3]],
    nudge_x = 3,
    nudge_y = 1
  ) +
  scale_color_brewer(palette = "Set1") +
  labs(
    x = "Total num. Black patients",
    y = "Total num. high-cost patients"
  ) +
  coord_cartesian(xlim = c(0, 42), ylim = c(45, 60), expand = FALSE) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.title = element_text(size = 9),
    axis.text = element_text(size = 8),
    legend.text = element_text(size = 9)
  )

p <- grid.arrange(
  p_pareto_full,
  p_pareto_sub,
  ncol = 2
)

ggsave(
  "cmc.svg",
  plot = p,
  height = 3,
  width = 6
)
