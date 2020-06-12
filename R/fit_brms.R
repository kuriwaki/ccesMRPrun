#' Fit a binomial brms model
#'
#'
#' @param .formula model specification
#' @param .data collapsed survey dataset, built from ccesMRPprep::build_counts
#' @param .prior prior specification that can be interepreted by brms. The default
#'   is a standard normal prior, which is tighter than the brms default but has
#'   shown to have good prior posterior draws
#' @param prior_PD whether to etimate the prior predictive distribution
#' @param .seed seed for randomization 02138
#' Fit brms regressions based on input data
#'
#' @importFrom brms brm prior_string
#'
#' @export
fit_brms <- function(.formula,
                     .data,
                     .prior = c(prior_string("normal(0, 1)", class = "b"),
                                prior_string("normal(0, 1)", class = "sd"),
                                prior_string("normal(0, 1)", class = "Intercept")),
                     prior_PD = FALSE,
                     .seed = 02138) {

  if (prior_PD) {
    fit <- brm(formula = .formula,
               data = .data,
               family = binomial,
               prior = .prior,
               sample_prior = "only",
               cores = 2,
               chains = 2,
               iter = 1.0e3,
               warmup = 0.5e3,
               seed = .seed)
  }

  if (!prior_PD) {
    fit <- brm(formula = .formula,
               data = .data,
               family = binomial,
               prior = .prior,
               cores = 4,
               chains = 4,
               iter = 2e3,
               control = list(adapt_delta = 0.95,
                              max_treedepth = 10),
               seed = .seed)
  }
  # attr(fit, "question") <- attr(.data, "question")

  return(fit)
}

