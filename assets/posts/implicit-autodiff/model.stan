functions {
  /* Solution to drug concentration ODE given inital concentration, time elapsed,
   * and diffusion parameters.
   */
  vector[] drug_conc(vector y_cen, vector y_per, vector kappa_cen,
                     vector kappa_per, vector ts, int N) {
    vector[N] y_cen_out = exp(-kappa_cen .* ts) .* y_cen;
    vector[N] y_per_out = (kappa_cen ./ (kappa_per - kappa_cen))
                            .* (exp(-kappa_cen .* ts) - exp(-kappa_per .* ts))
                            .* y_cen + exp(-kappa_per .* ts) .* y_per;

    return { y_cen_out, y_per_out };
  }

  // Functor with appropriate signature for algebraic solver.
  vector f(vector y, vector kappas, real[] x_r, int[] x_i) {
    // Unpack x_r.
    real delta = x_r[1];
    real tau = x_r[2];

    // Unpack x_i.
    int n = x_i[1];

    // All of the intervals are tau.
    vector[n] ts = rep_vector(tau, n);

    /* The first n entries of y are the concentrations in the central
     * compartment, while the last n entries of y are the concentrations in the
     * peripheral compartments. Likewise for kappa.
     */
    vector[n] y_cen = y[:n];
    vector[n] y_per = y[(n+1):];
    vector[n] kappa_cen = kappas[:n];
    vector[n] kappa_per = kappas[(n+1):];

    // Calculate the concentrations after tau.
    vector[n] y_res[2] = drug_conc(y_cen, y_per, kappa_cen, kappa_per, ts, n);

    /* If a steady state has been reached, the difference between the
     * concentration after tau (with a dose delta) should be the same as the
     * current state.
     */
    return append_row(y_res[1] + rep_vector(delta, n), y_res[2]) - y;
  }
}

data {
  real<lower=0> delta;              // Dosage
  real<lower=0> tau;                // Dose interval
  int n;                            // Number of patients
  int m;                            // Number of observations
  vector<lower=0,upper=tau>[m] ts;  // Times of observations (since last dose)
  int<lower=0,upper=n> idx[m];      // Patient corresponding to observation
  vector<lower=0>[m] obs;           // Observed concetrations
  vector<lower=0>[n] y_guess_cen;   // Guess for central compartment.
  vector<lower=0>[n] y_guess_per;   // Guess for peripheral compartment.
}

transformed data {
  // Reshape the data for the algebra solver.
  vector[2*n] y_guess = append_row(y_guess_cen, y_guess_per);
  real x_r[2]         = { delta, tau };
  int  x_i[1]         = { n };
}

parameters {
  vector<lower=0>[n] kappa_cen; // Dispersion from central compartment
  vector<lower=0>[n] kappa_per; // Dispersion from peripheral compartment
}

transformed parameters {
  // Reshape dispersion parameters for algebra solver.
  vector[2*n] kappas = append_row(kappa_cen, kappa_per);

  // Get the steady-state for each patient.
  vector[2*n] y_steady = algebra_solver(f, y_guess, kappas, x_r, x_i);
  vector[n] y_steady_cen = y_steady[:n];
  vector[n] y_steady_per = y_steady[(n+1):];

  /* Get the concentrations in the central compartment at each time observation,
   * given the currently sampled diffusion parameters.
   */
  vector[m] y_true[2] = drug_conc(y_steady_cen[idx], y_steady_per[idx],
                                  kappa_cen[idx], kappa_per[idx], ts, m);
}

model {
  kappa_cen ~ lognormal(0, 1.0/4);
  kappa_per ~ lognormal(0, 1.0/4);
  obs ~ lognormal(log(y_true[2]), 1.0/4);
}

generated quantities {
  real fake_obs[m] = lognormal_rng(log(y_true[2]), 1.0/4);
}
