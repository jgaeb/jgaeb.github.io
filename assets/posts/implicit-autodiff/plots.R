library(gridExtra)
library(scales)
library(tidyverse)

d_old <- read_csv("old.csv", show_col_types = FALSE)
d_new <- read_csv("new.csv", show_col_types = FALSE)

plot_raw <- ggplot(mapping = aes(x = n, y = runtime)) +
  scale_color_manual(
    name = "Algorithm",
    values = c(Naïve = "blue", Adjoint = "red")
  ) +
  geom_point(
    aes(color = "Naïve"),
    data = d_old,
    alpha = 1/10,
    position = position_nudge(x = -2),
    show.legend = TRUE
  ) +
  geom_smooth(aes(color = "Naïve"), data = d_old, method = "loess") +
  geom_point(
    aes(color = "Adjoint"),
    data = d_new,
    alpha = 1/10,
    position = position_nudge(x = 2)
  ) +
  geom_smooth(aes(color = "Adjoint"), data = d_new, method = "loess") +
  labs(
    title = "Raw runtime of model fits",
    x = "Number of patients",
    y = "Runtime (seconds)"
  ) +
  guides(colour = guide_legend(override.aes = list(alpha = 1)))

# Convenience function to generate quotient CIs
gen_ci <- function(mu_old, sigma_old, mu_new, sigma_new, n, reps = 1e5) {
  fake_old <- rnorm(reps, mu_old, sigma_old / sqrt(n))
  fake_new <- rnorm(reps, mu_new, sigma_new / sqrt(n))
  quantile((fake_old - fake_new) / fake_new, c(1/20, 19/20)) %>%
    set_names(c("ci_lower", "ci_upper")) %>%
    list()
}

plot_quot <- bind_rows(mutate(d_new, type = "new"), mutate(d_old, type = "old")) %>%
  group_by(n, type) %>%
  summarize(
    mu = mean(runtime),
    sigma = sd(runtime) / 10, # 10 = sqrt(100)
    .groups = "drop"
  ) %>%
  pivot_wider(id_cols = n, names_from = type, values_from = c(mu, sigma)) %>%
  mutate(speedup = (mu_old - mu_new) / mu_new) %>%
  rowwise() %>%
  mutate(ci = gen_ci(mu_old, sigma_old, mu_new, sigma_new, 100)) %>%
  ungroup() %>%
  unnest_wider(ci) %>%
  ggplot(aes(x = n, y = speedup)) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper)) +
  geom_line() +
  scale_x_log10() +
  scale_y_continuous(label = label_percent()) +
  labs(
    title = "Relative speedup",
    x = "Number of patients",
    y = "Speedup"
  )

# Save the plots
ggsave("plot_raw.svg", plot_raw, height = 4, width = 6)
ggsave("plot_quot.svg", plot_quot, height = 4, width = 6)
