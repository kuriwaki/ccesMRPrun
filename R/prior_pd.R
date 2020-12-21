

#' Use priors and covariates to generate a prior predictive distribution
#'
#' @inheritParams fit_brms_binomial
#'
#' @export
prior_pd_binomial <- function(.formula,
                              .data,
                              .prior = c(prior_string("normal(0, 1)", class = "b"),
                                         prior_string("normal(0, 1)", class = "sd"),
                                         prior_string("normal(0, 1)", class = "Intercept")),

                              .seed = 02138) {

  brm(formula = .formula,
      data = .data,
      family = binomial,
      prior = .prior,
      sample_prior = "only",
      cores = 2,
      chains = 2,
      iter = 1.0e3,
      warmup = 0.5e3,
      refresh = ifelse(verbose, 200, 0),
      seed = .seed)
}
