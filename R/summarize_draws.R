#' Get summary statistics from draws (of counts)
#'
#' @param sims output of \link{poststrat_draws}
#' @param est_var input column name with estimates. Following the output of
#' \link{poststrat_draws}, defaults to `p_mrp`.
#'
#' @returns A tibble with the following statistics by each variable specified in `area_var`:
#'  `p_mrp_est`: posterior mean,
#'  `p_mrp_se`: standard deviation of posterior,
#'  `p_mrp_050`: 5% quantile,
#'  `p_mrp_100`: 10% quantile,
#'  `p_mrp_900`: 90% quantile, and
#'  `p_mrp_950`: 95% quantile.
#'
#' @inheritParams poststrat_draws
#' @importFrom dplyr group_by summarize across all_of as_tibble
#' @importFrom stats quantile
#' @import dtplyr
#'
#' @examples \dontrun{
#' drw_GA <- poststrat_draws(fit_GA, poststrat_tgt = acs_GA)
#' summ_sims(drw_GA, area_var = "cd") # also saved as data(`summ_GA`)
#'
#' # To create summaries by both CD and education
#' drw_GA_educ <- poststrat_draws(fit_GA, poststrat_tgt = acs_GA, area_var = c("cd", "educ"))
#' summ_sims(drw_GA_educ, area_var = c("cd", "educ"))
#'
#' }
#'
#' @export
summ_sims <- function(sims, area_var = "cd", est_var = "p_mrp", dtplyr = TRUE) {
  grp_by_vars <- area_var

  sims_grouped <- group_by(as_tibble(sims), across(all_of(grp_by_vars)))

  if (dtplyr) {
    sims <- lazy_dt(sims)
  }

  summarize(sims_grouped,
            p_mrp_est = mean(.data[[est_var]]),
            p_mrp_se  = sd(.data[[est_var]]),
            p_mrp_050 = quantile(.data[[est_var]], 0.050),
            p_mrp_100 = quantile(.data[[est_var]], 0.100),
            p_mrp_900 = quantile(.data[[est_var]], 0.900),
            p_mrp_950 = quantile(.data[[est_var]], 0.950)) %>%
    as_tibble()
}
