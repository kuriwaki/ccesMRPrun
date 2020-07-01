#' Fit a binomial brms model
#'
#'
#' @param .formula model specification
#' @param .data collapsed survey dataset, built from ccesMRPprep::build_counts
#' @param .prior prior specification that can be interepreted by brms. The default
#'   is a standard normal prior, which is tighter than the brms default but has
#'   shown to have good prior posterior draws
#' @param .iter Number of total iterations.
#' @param .warmup Of the iterations, how much are burn-ins. Defaults to half.
#' @param verbose Whether to show iteration messages
#' @param .seed seed for randomization to pass into brm
#' Fit brms regressions based on input data
#'
#' @importFrom brms brm prior_string
#'
#' @export
fit_brms_binomial <- function(.formula,
                     .data,
                     .prior = c(prior_string("normal(0, 1)", class = "b"),
                                prior_string("normal(0, 1)", class = "sd"),
                                prior_string("normal(0, 1)", class = "Intercept")),
                     .iter = 2e3,
                     .warmup = floor(.iter/2),
                     .cores = 4,
                     .chains = 4,
                     verbose = TRUE,
                     .seed = 02138) {
    fit <- brm(formula = .formula,
               data = .data,
               family = binomial,
               prior = .prior,
               chains = .chains,
               cores = .cores,
               iter = .iter,
               warmup = .warmup,
               control = list(adapt_delta = 0.95,
                              max_treedepth = 10),
               refresh = ifelse(verbose, 200, 0),
               seed = .seed)
  return(fit)
}



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
