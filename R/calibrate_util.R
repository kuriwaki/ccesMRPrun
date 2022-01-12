
#' Traditional inverse logit. Turn logit scale to probability.
#' @param x input
#' @keywords internal
invlogit <- function(x) {1/(1 + exp(-x))}

#' A logit transformation as implemented by Ghitza
#'
#' This logit transformation (from a probability scale to unbounded) does some
#' trimming around extreme values.
#'
#'
#' @param x input
#' @param digits The number of digits to round by. Larger values indicate
#'  more closer to a theoretical inverse logit.
#'
#' @author Yair Ghitza
#'
#' @examples
#'  logit <- function(x) log(x/(1 - x))
#'  logit(0.000001)
#'  logit_ghitza(0.000001, digits = 5) # the default
#'  logit_ghitza(0.000001, digits = 1)
#'  logit_ghitza(0.000001, digits = 10)
#'
#' @seealso posthoc_error calib_oneway
#' @export
logit_ghitza <- function(x, digits=5) {
  return(-1 * log(1/pmin(
    1 - 1 * 10^(-1 * digits),
    pmax(1 * 10^(-1 * digits), x)) -
      1))
}
