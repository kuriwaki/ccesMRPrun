#' Get summary statistics from draws (of counts)
#'
#' @param  sims output of \link{poststrat_draws}
#'
#' @importFrom dplyr group_by summarize across all_of
#' @export
summ_sims <- function(sims) {

  grp_by_vars <- "cd"

  if ("qID" %in% colnames(sims))
    grp_by_vars <- c("qID", grp_by_vars)


  sims %>%
    group_by(across(all_of(grp_by_vars))) %>%
    summarize(p_mrp_est = mean(p_mrp),
              p_mrp_025 = quantile(p_mrp, 0.025),
              p_mrp_050 = quantile(p_mrp, 0.050),
              p_mrp_100 = quantile(p_mrp, 0.100),
              p_mrp_500 = quantile(p_mrp, 0.500),
              p_mrp_900 = quantile(p_mrp, 0.900),
              p_mrp_950 = quantile(p_mrp, 0.950),
              p_mrp_975 = quantile(p_mrp, 0.975))
}
