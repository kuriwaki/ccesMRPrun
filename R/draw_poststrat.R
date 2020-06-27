#' Get MCMC draws of post-stratified estimate of demog x cd cells
#'
#'
#' @param model stan model from fit_brms
#' @param poststrat_tgt The poststratification target. It must contain the column
#' `count`, which is treated as the number of \code{trials} in the binomial model.
#' @param orig_data original survey data, only used to subset the poststratification,
#' containing question data.
#' @param new_levels If there are new levels in the poststrat table that do not have
#' coefficients in the survey data, should there be an extrapolation or assignment to 0s?
#' The answer should almost always be No in MRP.
#'
#'
#' @return A tidy dataset with `qID` x `cd` x `iter` number of rows,
#' where `qID` is the number of questions (outcomes), `cd` is
#' the number of geographies, and `iter` is the number of iterations estimated in
#' the MCMC model.  The demographic cells within a district are averaged across,
#' and a MRP estimate is computed.
#' It contains the columns
#' \describe{
#' \item{iter}{The number of iterations}
#' \item{cd}{The geography}
#' \item{qID}{The question}
#' \item{p_mrp_est}{The proportion of success, estimated by MRP.}
#' }
#'
#'
#'
#' @importFrom dplyr mutate group_by summarize filter rename bind_cols matches
#' @importFrom tibble as_tibble
#' @importFrom tidyr pivot_longer
#' @importFrom readr parse_number
#' @importFrom brms pp_expect
#'
#'
#' @export
poststrat_draws <- function(model, poststrat_tgt, orig_data, new_levels = FALSE) {

  # districts (CDs) to loop through
  cds <- intersect(unique(poststrat_tgt$cd), unique(orig_data$cd))

  # subset to predict on
  cd_strat <- filter(poststrat_tgt, cd %in% cds) %>%
    rename(n_response = count)

  # draw, then reshape to tidy form
  p_draws <- pp_expect(model,
                       newdata = cd_strat,
                       allow_new_levels = new_levels,
                       summary = FALSE)

  # format
  cds_draws <- p_draws %>%
    t() %>%
    as_tibble() %>%
    mutate(cell = 1:n()) %>%
    bind_cols(cd_strat, .) %>%
    pivot_longer(cols = matches("^V"), names_to = "iter", values_to = "pred_n_yes") %>%
    mutate(iter = parse_number(iter))


  # relabel, compute MRP
  question <- attr(orig_data, "question")
  cd_est <- cds_draws %>%
    mutate(qID = question) %>%
    group_by(qID, cd, iter) %>%
    summarize(p_mrp = sum(pred_n_yes) / sum(n_response),
              .groups = "drop")

  cd_est
}
