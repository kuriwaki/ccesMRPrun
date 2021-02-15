#' Fits and tidies MRP outputs in one step
#'
#' @details Combines `fit_brms`, `poststrat_draws`, and `direct_est`. See `scatter_45`
#'  for options on visualization
#'
#' @param add_on Any area-level data to be merged with the output,
#'  for example validation data
#' @param ... Additional arguments to pass to the model fitting function, `fit_brms()`
#'
#' @inheritParams fit_brms
#' @inheritParams direct_ests
#' @inheritParams poststrat_draws
#' @examples
#' \dontrun{
#' mrp_fit <- mrp_onestep(response ~ (1|educ) + (1|cd),
#'                        .data = cces_GA,
#'                        poststrat_tgt = acs_GA,
#'                        area_var = "cd",
#'                        count_var = "count",
#'                        weight_var = "weight_post",
#'                        add_on = elec_GA)
#'
#' scatter_45(mrp_fit, clinton_vote, p_mrp_est,
#'            xlab = "Clinton Vote", ylab = "MRP Estimate")
#' }
#'
#' @export
mrp_onestep <- function(.formula, .data,
                        poststrat_tgt,
                        area_var = "cd",
                        count_var = "count",
                        weight_var = NULL,
                        add_on = NULL,
                        ...) {

  # direct
  drct_fit <- direct_ests(.formula, .data, area_var = area_var, weight_var = weight_var)

  # brms
  brms_fit <- fit_brms(.formula, .data, ...)

  # P-step
  post_fit <- poststrat_draws(brms_fit,
                              poststrat_tgt = poststrat_tgt,
                              orig_data = .data,
                              area_var = area_var, count_var = count_var)

  # summarize
  post_sum <- summ_sims(post_fit, area_var = area_var)

  # combine
  out <- left_join(drct_fit, post_sum, by = area_var)

  # optional
  if (!is.null(add_on)) {
    out <- left_join(out, add_on, by = area_var)
  }

  return(out)
}
