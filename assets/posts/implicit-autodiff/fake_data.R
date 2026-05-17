library(cmdstanr)
library(furrr)
library(tidyverse)

set.seed(1213)

# Set up parallelization.
max_cores <- 40
plan(multicore(workers = min(availableCores(), max_cores)))

# Calculate the drug concentration from a given initial state and diffusion
# parameters.
drug_conc <- function(y_cen, y_per, kappa_cen, kappa_per, ts) {
  y_cen_out <- exp(-kappa_cen * ts) * y_cen
  y_per_out <- (kappa_cen / (kappa_per - kappa_cen)) *
                 (exp(-kappa_cen * ts) - exp(-kappa_per * ts)) *
                  y_cen + exp(-kappa_per * ts) * y_per

  tibble(y_cen = y_cen_out, y_per = y_per_out)
}

# Calculate the fixed point given a dosage, interval, and diffusion parameters.
fixed_point <- function(kappa_cen, kappa_per, delta, tau, n) {
  y_cen <- double(n)
  y_per <- double(n)
  y_cen <- delta / (1 - exp(-kappa_cen * tau))
  y_per <- y_cen * (kappa_cen / (kappa_per - kappa_cen)) *
              (exp(-kappa_cen * tau) - exp(-kappa_per * tau)) /
              (1 - exp(-kappa_per * tau))

  tibble(y_cen = y_cen, y_per = y_per)
}

# Simulate fake data.
gen_fake_data <- function(n = 10, m = 100 * n, delta = 10, tau = 5) {
  # Generate the diffusion parameters
  kappas <- tibble(kappa_cen = rlnorm(n, 0, 1/4), kappa_per = rlnorm(n, 0, 1/4))

  # Generate the equilibrium concentrations
  y_steady <- with(kappas, fixed_point(kappa_cen, kappa_per, delta, tau, n))

  # Generate times of observations
  ts <- runif(m, 0, tau)

  # Generate identity of patient observed
  idx <- sample.int(n, m, replace = TRUE)

  # Generate concentrations.
  # NOTE: Only the peripheral compartment concentrations are observed.
  obs <- bind_cols(kappas, y_steady) %>%
    slice(idx) %>%
    with(drug_conc(y_cen, y_per, kappa_cen, kappa_per, ts)) %>%
    pull(y_per) %>%
    log() %>%
    rlnorm(m, ., 1/4)

  # Return generated fake data as a list.
  list(
    delta       = delta,
    tau         = tau,
    n           = n,
    m           = m,
    ts          = ts,
    idx         = idx,
    obs         = obs,
    # Here we guess the true steady state as our starting point for the
    # algebraic solver.
    # NOTE: The steady state for the _true_ parameters will not, in general, be
    # the steady state for the sampled parameters; this is merely a good guess
    # that reduces the computation necessary for solution, but not autodiff.
    y_guess_cen = y_steady$y_cen,
    y_guess_per = y_steady$y_per
  )
}

# Compile the model
model <- cmdstan_model("model.stan")

# Convenience function for fitting the model
fit <- function(n) {
  fake_data <- gen_fake_data(n)
  # The parallelization is treated at the top level.
  fit <- model$sample(data = fake_data, chains = 1)
  # Return the total time
  fit$time()$total
}

# Setup test parameters
ns <- as.integer(2^(4:21 / 3))
reps <- 100
test_params <- expand_grid(rep = seq(reps), n = ns)

test_params %>%
  mutate(
    runtime = future_map(
      n,
      purrr::quietly(fit),
      .options = future_options(seed = TRUE, scheduling = Inf)
    ),
    runtime = map_dbl(runtime, "result")
  ) %>%
  write_csv(str_c(str_replace(Sys.time(), "\\s+", "_"), ".csv"))
