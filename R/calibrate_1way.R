#' Compute absolute deviation
#'
#' @param delta The parameter of interest
#' @param xi The true target
#' @param ests A vector of current estimate, by cell
#' @param n The sample size of the estimates, by cell
#'
#' @author Yair Ghitza and Shiro Kuriwaki
#' @source Modified from AbsError in https://github.com/Catalist-LLC/unemployment/blob/master/unemployment_cps_mrp/helper_functions/GetYHat.R
#'
#' @seealso logit_ghitza posthoc_intercept
#'
#' @examples
#'  biased_ests <- ccesMRPrun:::invlogit(rnorm(n = 100, mean = 1, sd = 1))
#'  sizes <- rbinom(n = 100, size = 100, prob = 0.1)
#'  tru <- 0.5
#'
#'  # one delta
#'  posthoc_error(delta = 0.1, xi = tru, ests = biased_ests, n = sizes)
#'
#'  # test multiple deltas
#'  deltas <- seq(-3, 3, by = 0.1)
#'  abserr <- purrr::map_dbl(.x = deltas,
#'          .f = ~posthoc_error(.x, xi = tru, ests= biased_ests, n = sizes))
#'  plot(deltas, abserr, bty = "n")
#'
#' @export
posthoc_error <- function(delta, xi, ests, n) {
  ests_adj <- pmin(logit_ghitza(1), pmax(logit_ghitza(0), ests))

  ests_d_pr <- invlogit(logit_ghitza(ests_adj) + delta)

  abs(xi - (sum(ests_d_pr * n) / sum(n)))
}


#' Find intercept correction for cell estimates
#'
#'
#' For a given geography g, there may be a value y_g which is
#' the ground truth. You have C cells with estimates of y that
#' may be biased. This function, proposed by Ghitza and Gelman
#' and Ghitza, will find a intercept shift for all C cells
#' to best fit the estimand y. It is the argmin of the sum of
#' absolute values of the deviation.
#'
#' @param search The lower and upper endpoints of the interval to search
#' @inheritParams posthoc_error
#'
#' @author Yair Ghitza
#' @source FindDelta function at https://github.com/Catalist-LLC/unemployment/blob/master/unemployment_cps_mrp/helper_functions/GetYHat.R
#' @seealso posthoc_error
#'
#' @returns The value of delta or the intercept that minimizes
#'  the absolute deviation in total. The value is on the logit scale.
#'  To translate to a probability, use invlogit.
#'
#' @examples
#'
#'  biased_ests <- ccesMRPrun:::invlogit(rnorm(n = 100, mean = 1, sd = 1))
#'  sizes <- rbinom(n = 100, size = 100, prob = 0.1)
#'  tru <- 0.5
#'
#'  posthoc_intercept(tru, biased_ests, sizes)
#'
#' @export
posthoc_intercept <- function(xi, ests, n, search = c(-5, 5)) {
  optimize(posthoc_error,
           interval = search,
           xi = xi,
           ests = ests,
           n = n)$minimum
}

