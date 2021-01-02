#' Use priors and covariates to generate a prior predictive distribution
#'
#' @inheritParams fit_brms_binomial
#'
#' @export
prior_pd_binomial <- function(.formula,
                              .data,
                              verbose = TRUE,
                              .prior = c(prior_string("normal(0, 1)", class = "b"),
                                         prior_string("normal(0, 1)", class = "sd"),
                                         prior_string("normal(0, 1)", class = "Intercept")),
                              .iter = 1e3,
                              .warmup = floor(.iter/2),
                              .cores = 2,
                              .chains = 2,
                              .backend = "rstan",
                              .seed = 02138) {

  brm(formula = .formula,
      data = .data,
      family = binomial,
      prior = .prior,
      sample_prior = "only",
      cores = .cores,
      chains = .chains,
      iter = .iter,
      warmup = .warmup,
      refresh = ifelse(verbose, 200, 0),
      seed = .seed)
}
