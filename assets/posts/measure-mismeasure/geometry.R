options(tidyverse.quiet = TRUE)
library(progress)
library(fs)
library(tidyverse)

# Set the ggplot theme
theme_set(theme_bw())

# Set seed
set.seed(31371648)

# Define the simulation parameters
POP_SIZE_TRAIN = 10000000
POP_SIZE_TEST = 1000000
FRAC_MINORITY = 0.33
DISCRETE = TRUE
E_0 = 1
E_R = -1
E_NOISE = 1
M_0 = -1.0
M_E = 1.0
M_NOISE = 1.0
T_0 = 50
T_M = 4
T_E = 4
T_E_M = 1.0
T_NOISE = 7
Y_REJECT_NOISE_SD = 0.3
Y_REJECT_NOISE_MEAN = -0.5
DIVERSITY_UTILITY = 0.25
FRAC_ADMIT = 0.25

# Load the test data
# df_test <- read_csv([ BIG FILE ])

############################## PARETO FRONTIER #################################

POP_SIZE_TEST_ADMIT <- FRAC_ADMIT * POP_SIZE_TEST
N <- round(FRAC_ADMIT * POP_SIZE_TEST)  * FRAC_MINORITY * 2.0

MAX_MINORITY_ADMIT <- POP_SIZE_TEST_ADMIT * FRAC_MINORITY * 2.0
pareto_ideal <- NULL
pareto_grid <- seq(from = 1, to = FRAC_ADMIT * POP_SIZE_TEST,
                   by = as.integer(POP_SIZE_TEST_ADMIT / 1000))
# Create a progress bar
pb <- progress_bar$new(
  total = length(pareto_grid),
  format = "Generating Pareto Frontier: [:bar] :percent :eta",
  clear = FALSE
)

for (n_minority in pareto_grid) {
  df_test_minority <- df_test %>%
    arrange(desc(T)) %>%
    filter(R == 1)
  df_test_majority <- df_test %>%
    arrange(desc(T)) %>%
    filter(R == 0)
  agg_ac_index <- with(df_test_minority, sum(Y[1:n_minority])) +
    with(df_test_majority, sum(Y[1:(POP_SIZE_TEST_ADMIT - n_minority)]))
  pareto_ideal <- rbind(pareto_ideal, tibble(n_minority, agg_ac_index))
  pb$tick()
}

max_graduation <- pareto_ideal[which.max(pareto_ideal$agg_ac_index), ]
MAX_GRADUATION <- list(
  "Max Graduation",
  0,
  max_graduation$n_minority,
  max_graduation$agg_ac_index
)

max_lambda <- pareto_ideal[which.max(pareto_ideal$agg_ac_index + pareto_ideal$n_minority / 4), ]
MAX_LATENT <- list(
  "Max Latent",
  0,
  max_lambda$n_minority,
  max_lambda$agg_ac_index
)

pareto_ideal$cutoff <- pareto_ideal$n_minority >=
  as.double(MAX_GRADUATION[[3]])
lo <- loess(
  agg_ac_index ~ n_minority,
  pareto_ideal,
  span = 0.5
)

pareto_ideal$smooth_y <- predict(lo, pareto_ideal$n_minority, se = FALSE)

#################################### PLOT ######################################

# Counterfactual fairness
# grid <- read_csv([ BIG FILE ])

grid_ <- grid %>%
  group_by(min_minority_admits) %>%
  summarize( 
    y_max = max(max_graduates),
    y_min = min(min_graduates)
  )

p <- ggplot() +
  geom_line(
    data = filter(pareto_ideal, cutoff),
    mapping = aes(x = n_minority, y = smooth_y),
    linewidth = 1.5,
    color = "violet",
    alpha = 0
  ) + 
  geom_line(
    data = filter(pareto_ideal, !cutoff), 
    mapping = aes(x = n_minority, y = smooth_y),
    linetype = "dotted",
    linewidth = 1.5,
    color = "violet",
    alpha = 0
  ) +
  coord_cartesian(
    xlim = c(0.0, FRAC_ADMIT * POP_SIZE_TEST + 10000),
    ylim = c(90000, 195000),
    expand = FALSE
  ) + 
  scale_x_continuous("Admitted Applicants from Target Group", labels = scales::comma) + 
  scale_y_continuous("Bachelor's degree attainment", labels = scales::comma) +
  theme(
    legend.title = element_blank(),
    legend.position = "none",
    panel.grid = element_line(
      color = rgb(235, 235, 235, 100, maxColorValue = 255)
    ),
    axis.ticks.y = element_blank(),
    axis.title = element_text(size = 9),
    axis.text = element_text(size = 8),
    legend.text = element_text(size = 9),
    panel.spacing = unit(1.5, "lines"),
    plot.margin = margin(10, 10, 10, 10, "pt")
  ) +
  geom_line(
    data = filter(pareto_ideal, cutoff),
    mapping = aes(x = n_minority, y = smooth_y),
    linewidth = 1.5,
    color = "violet"
  ) + 
  geom_line(
    data = filter(pareto_ideal, !cutoff), 
    mapping = aes(x = n_minority, y = smooth_y),
    linetype = "dotted",
    linewidth = 1.5,
    color = "violet"
  ) +
  geom_point(
    mapping = aes(x = max_graduation$n_minority, y = max_graduation$agg_ac_index),
    color = "red",
    size = 4,
  ) +
  annotate("text",
    size = 3,
    lineheight = 0.85, 
    hjust = 1,
    x = max_graduation$n_minority + 60000,
    y = max_graduation$agg_ac_index + 5000,
    label ='Max Graduation'
  ) +
  geom_point(
    aes(x = max_lambda$n_minority, y = max_lambda$agg_ac_index),
    color = "blue",
    size = 4
  ) +
  annotate("text",
    size = 3,
    lineheight = 0.85, 
    hjust = 1,
    x = max_lambda$n_minority + 80000,
    y = max_lambda$agg_ac_index + 5000,
    # Label with the equivalent of the LaTeX expression \text{Max Graduation} \\
    # + \lambda \cdot \text{Diversity}
    label = expression(paste("Max Graduation"))
  ) +
  annotate("text",
    size = 3,
    lineheight = 0.85, 
    hjust = 1,
    x = max_lambda$n_minority + 80000,
    y = max_lambda$agg_ac_index + 5000 - 5000,
    label = expression(+ lambda %.% "Diversity")
  ) + 
  geom_segment(aes(x = 0, y = 0, xend = 82250, yend = 111000), color = "#C77CFF", size = 1.5) +
  geom_point(aes(x = 82250, y = 111000), color = "#C77CFF", size = 4) +
  annotate("text",
    size = 3,
    lineheight = 0.85, 
    hjust = 1,
    x = 82250 + 120000,
    y = 111000,
    label ='Counterfactual Fairness'
  )

suppressWarnings(ggsave(
  plot = p,
  filename = "frontier.svg",
  height = 4,
  width = 4
))
