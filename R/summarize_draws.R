#' Get summary statistics from draws (of counts)
#'
#' @param sims output of \link{poststrat_draws}
#' @param est_var column name with estimates. Following the output of
#' \link{poststrat_draws}, defaults to `p_mrp`.
#'
#' @inheritParams poststrat_draws
#' @importFrom dplyr group_by summarize across all_of as_tibble
#' @importFrom stats quantile
#' @import dtplyr
#'
#' @examples
#' drw_GA <- poststrat_draws(fit_GA, poststrat_tgt = acs_GA)
#'
#' summ_sims(drw_GA) # also saved as  data(`summ_GA`)
#'
#' @export
summ_sims <- function(sims, area_var = "cd", est_var = "p_mrp", dtplyr = TRUE) {
  grp_by_vars <- area_var

  if ("qID" %in% colnames(sims))
    grp_by_vars <- c("qID", grp_by_vars)

  sims_grouped <- group_by(sims, across(all_of(grp_by_vars)))

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
