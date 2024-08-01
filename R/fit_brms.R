#' Fit a brms model with count data
#'
#'
#' @param .formula model specification
#' @param .data collapsed survey dataset, built from ccesMRPprep::build_counts
#' @param .prior prior specification that can be interpreted by brms. The default
#'   is a standard normal prior, which is tighter than the brms default but has
#'   shown to have good prior posterior draws
#' @param .cores Number of cores to uses
#' @param .chains Number of chains to pass on fit_brms
#' @param .iter Number of total iterations.
#' @param .warmup Of the iterations, how much are burn-ins. Defaults to half.
#' @param verbose Whether to show iteration messages
#' @param .seed seed for randomization to pass into brm
#' @param .backend The backend argument of brms. Defaults to `"rstan"`, can also
#'  be `"cmdstanr"`
#' @param .threads The number of threads to do within-chain parallelization. Defaults
#'  to not using, which is NULL
#'
#'
#'
#' @importFrom brms brm prior_string threading
#'
#' @export
fit_brms_binomial <- function(.formula,
                              .data,
                              verbose = TRUE,
                              .prior = c(prior_string("normal(0, 1)", class = "b"),
                                         prior_string("normal(0, 1)", class = "sd"),
                                         prior_string("normal(0, 1)", class = "Intercept")),
                              .iter = 2e3,
                              .warmup = floor(.iter/2),
                              .cores = 4,
                              .chains = 4,
                              .threads = NULL,
                              .backend = "rstan",
                              .seed = 02138) {


  RHS <- attr(terms(as.formula(.formula)), "term.labels")

  # no "b" for RE only model
  if (all(grepl(x = RHS, pattern = "|"))) {
    .prior <- subset(.prior, class != "b")
  }

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
               backend = .backend,
               thread = threading(.threads),
               seed = .seed)
  return(fit)
}


#' Fit a binary model for MRP
#'
#' Internally, it creates a count version of the individual-data via
#' `ccesMRPprep::build_counts` and then runs the regression in `fit_brms_binomial`.
#'
#' @param .data Data for the bernoulli `fit` function is an indidividual-level dataset,
#' rather than a collapsed version
#'
#'
#' @inheritParams fit_brms_binomial
#' @inheritDotParams fit_brms_binomial
#' @param .formula Formula in `binary y ~ (1|x1) + (1|x2)` form.
#' @param .data Individual-level dataset
#' @param name_trls_as The name for the variable name of the number of trials
#' @param name_ones_as The name for the variable name for the number of successes
#'
#' @importFrom ccesMRPprep build_counts
#' @importFrom glue glue
#'
#'
#' @examples
#' \dontrun{
#' fit <- fit_brms(response ~ (1|educ) + (1|cd), cces_GA)
#' }
#'
#'
#' @export
fit_brms <- function(.formula,
                     .data,
                     name_ones_as = "yes",
                     name_trls_as = "n_response",
                     ...) {

  df_count <- build_counts(formula = .formula,
                           data = .data,
                           name_ones_as = name_ones_as,
                           name_trls_as = name_trls_as)

  formula_binomial <-
    glue("{name_ones_as} | trials({name_trls_as}) ~ {as.character(as.formula(.formula)[3])}")


  fit <- fit_brms_binomial(.formula = formula_binomial,
                           .data = df_count,
                           ...)

}


