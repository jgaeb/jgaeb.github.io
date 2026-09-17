library(latex2exp)
library(scales)
library(tidyverse)

theme_set(theme_bw())

# Axis labels
a_0 = "A == a[0]"
a_1 = "A == a[1]"
y_0 = "Y(1) == 0"
y_1 = "Y(1) == 1"

ggplot() +
  scale_x_continuous(breaks = c(0:4 / 4), labels = label_percent(),
                     limits = c(0, 1)) +
  scale_y_continuous(breaks = NULL) +
  ylab(NULL) +
  xlab("Probability of graduating") +
  geom_area(stat = "function", fun = ~ dbeta(., 2, 2), xlim = c(0.5, 1),
            data = tibble(a = a_0, y_ = y_0)) +
  geom_function(fun = ~ dbeta(., 2, 2), data = tibble(a = a_0, y_ = y_0)) +
  geom_area(stat = "function", fun = ~ dbeta(., 3, 3), xlim = c(0.5, 1),
            data = tibble(a = a_1, y_ = y_0)) +
  geom_function(fun = ~ dbeta(., 3, 3), data = tibble(a = a_1, y_ = y_0)) +
  geom_area(stat = "function", fun = ~ dbeta(., 3, 3), xlim = c(0.5, 1),
            data = tibble(a = a_0, y_ = y_1)) +
  geom_function(fun = ~ dbeta(., 3, 3), data = tibble(a = a_0, y_ = y_1)) +
  geom_area(stat = "function", fun = ~ dbeta(., 4, 4), xlim = c(0.5, 1),
            data = tibble(a = a_1, y_ = y_1)) +
  geom_function(fun = ~ dbeta(., 4, 4), data = tibble(a = a_1, y_ = y_1)) +
  annotate("text", x = 2/3, y = 1/2, label = "50%", color = "white") +
  facet_grid(vars(y_), vars(a), labeller = label_parsed)

ggsave("balanced.svg", height = 4, width = 6)

ggplot() +
  scale_x_continuous(breaks = c(0:4 / 4), labels = label_percent(),
                     limits = c(0, 1)) +
  scale_y_continuous(breaks = NULL) +
  ylab(NULL) +
  xlab("Probability of graduating") +
  geom_area(stat = "function", fun = ~ dbeta(., 2, 2), xlim = c(0.5, 1),
            data = tibble(a = a_0, y_ = y_0)) +
  geom_function(fun = ~ dbeta(., 2, 2), data = tibble(a = a_0, y_ = y_0)) +
  geom_text(label = str_c(format(100 - 100 * pbeta(1/2, 2, 2), digits = 2), "%"),
            data = tibble(a = a_0, y_ = y_0, x = 2/3, y = 1/2), color = "white",
            mapping = aes(x = x, y = y)) +
  geom_area(stat = "function", fun = ~ dbeta(., 3, 3), xlim = c(0.5, 1),
            data = tibble(a = a_1, y_ = y_0)) +
  geom_function(fun = ~ dbeta(., 3, 3), data = tibble(a = a_1, y_ = y_0)) +
  geom_text(label = str_c(format(100 - 100 * pbeta(1/2, 3, 3), digits = 2), "%"),
            data = tibble(a = a_1, y_ = y_0, x = 2/3, y = 1/2), color = "white",
            mapping = aes(x = x, y = y)) +
  geom_area(stat = "function", fun = ~ dbeta(., 3.1, 2.9), xlim = c(0.5, 1),
            data = tibble(a = a_0, y_ = y_1)) +
  geom_function(fun = ~ dbeta(., 3.1, 2.9), data = tibble(a = a_0, y_ = y_1)) +
  geom_text(label = str_c(format(100 - 100 * pbeta(1/2, 3.1, 2.9), digits = 2), "%"),
            data = tibble(a = a_0, y_ = y_1, x = 2/3, y = 1/2), color = "white",
            mapping = aes(x = x, y = y)) +
  geom_area(stat = "function", fun = ~ dbeta(., 3.9, 4.1), xlim = c(0.5, 1),
            data = tibble(a = a_1, y_ = y_1)) +
  geom_function(fun = ~ dbeta(., 3.9, 4.1), data = tibble(a = a_1, y_ = y_1)) +
  geom_text(label = str_c(format(100 - 100 * pbeta(1/2, 3.9, 4.1), digits = 2), "%"),
            data = tibble(a = a_1, y_ = y_1, x = 2/3, y = 1/2), color = "white",
            mapping = aes(x = x, y = y)) +
  facet_grid(vars(y_), vars(a), labeller = label_parsed)

ggsave("unbalanced.svg", height = 4, width = 6)
