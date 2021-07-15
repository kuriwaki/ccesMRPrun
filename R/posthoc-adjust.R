


#' A logit transformation as implemented by Ghitza
#'
#' This logit transformation does some trimming around
#' extreme values.
#'
#' @author Yair Ghitza
#'
#' @examples
#'  arm::logit(0.000001)
#'  logit_ghitza(0.000001, digits = 5) # the default
#'  logit_ghitza(0.000001, digits = 1)
#'  logit_ghitza(0.000001, digits = 10)
#'
logit_ghitza <- function(x, digits=5) {
  return(-1 * log(1/pmin(
    1 - 1 * 10^(-1 * digits),
    pmax(1 * 10^(-1 * digits), x)) -
      1))
}


#' Compute absolute deviation
#'
#' @param delta The parameter of interest
#' @param y The true target
#' @param yhat A vector of current estimate
#'
#' @author Yair Ghitza
#' @source https://github.com/Catalist-LLC/unemployment/blob/master/unemployment_cps_mrp/helper_functions/GetYHat.R
#'
#'
#' @importFrom arm invlogit
posthoc_error <- function(delta, y, n, yhat) {
  y <- pmin(logit_ghitza(1), pmax(logit_ghitza(0), y))
  abs((sum(invlogit(logit_ghitza(y) + delta) * n) /
         sum(n) - yhat))
}


#' Find intercept correction for cell estimates
#'
#'
#' For a given geography g, there may be a value y_g which is
#' the ground truth. You have C cells with estiamtes of y that
#' may be biased. This function, prosed by Ghitza and Gelman
#' and Ghitza, will find a intercept shift for all C cells
#' to best fit the estimand y. It is the argmin of the sum of
#' absolute values of the deviation.
#'
#' @author Yair Ghitza
#' @source https://github.com/Catalist-LLC/unemployment/blob/master/unemployment_cps_mrp/helper_functions/GetYHat.R
#'
#'
#' @examples
#'  cell_sims <- poststrat_draws(fit_GA, poststrat_tgt = acs_GA)
#'  mrp_by_edu <- summ_sims(cell_sims, area_var = c("cd", "educ"))
#'
#' @export
posthoc_intercept <- function(y, n, yhat) {
  optimize(posthoc_error, interval = c(-5, 5), y, n, yhat)$minimum
}

