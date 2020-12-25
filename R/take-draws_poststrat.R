#' Get MCMC draws of post-stratified estimate of demog x cd cells
#'
#'
#' @param model stan model from fit_brms
#' @param poststrat_tgt The poststratification target. It must contain the column
#' `count`, which is treated as the number of \code{trials} in the binomial model.
#' @param orig_data original survey data. This defaults to NULL but if supplied
#'  be used to (1) subset the poststratification, to areas only in the survey, and
#'  (2) label the question outcome.
#' @param question_lbl A character string that indicates the outcome, e.g.
#'  a shorthand for the outcome variable. This is useful when you want to preserve
#'  the outcome or description of multiple models.
#' @param area_var A character string for the variable name(s) for area to group
#'  and aggregate by. That is, the area of interest in MRP. Defaults to `"cd"`
#' @param count_var A character string for the variable name for the population
#'  count in the `poststrat_tgt` dataframe. This will be renamed as if it is
#'  a trial count in the model. Defaults to `"count"`.
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
#'  across all_of
#' @importFrom tibble as_tibble
#' @importFrom brms posterior_epred
#'
#'
#' @export
poststrat_draws <- function(model,
                            poststrat_tgt,
                            orig_data = NULL,
                            question_lbl = attr(orig_data, "question"),
                            area_var =  "cd",
                            count_var = "count",
                            new_levels = FALSE) {

  # districts (CDs) to loop through
  if (!is.null(orig_data)) {
    areas <- intersect(unique(poststrat_tgt[[area_var]]),
                     unique(orig_data[[area_var]]))

    # subset to predict on
    poststrat_tgt <- poststrat_tgt[poststrat_tgt[[area_var]] %in% areas, ]
  }


  # rename variable for MRP est
  cd_strat <- rename(poststrat_tgt, n_response = {{count_var}})

  # draw, then reshape to tidy form
  p_draws <- posterior_epred(model,
                             newdata = cd_strat,
                             allow_new_levels = new_levels,
                             summary = FALSE)

  # group by variables
  iter_grp_vars <- c(area_var, "iter")

  if ("question_lbl" %in% colnames(cds_draws))
    iter_grp_vars <- c("qID", iter_grp_vars)


  # binomial (yes | n_response model)
  if (model$family$family == "binomial") {
    areas_draws <- pivot_celldraws_longer(
      mod_draws = p_draws,
      data_strat = cd_strat,
      yhat_name = "pred_n_yes")

    if ("question_lbl" %in% colnames(areas_draws))
      areas_draws <-  mutate(areas_draws, qID = question_lbl)

    areas_grp <- cds_draws %>%
      group_by(across(all_of(iter_grp_vars)))

    # mean estimator
    areas_est <- areas_grp %>%
      summarize(p_mrp = sum(pred_n_yes) / sum(n_response),
                .groups = "drop")
  }

  # bernoulli
  if (model$family$family == "bernoulli") {

    areas_draws <- pivot_celldraws_longer(p_draws,
                                        cd_strat,
                                        yhat_name = "pred_yes")

    # mean estimator
    areas_est <- areas_grp %>%
      mutate(qID = question_lbl) %>%
      group_by(across(all_of(iter_grp_vars))) %>%
      summarize(p_mrp =  sum(pred_yes*n_response) / sum(n_response), # n_response still a misnomer, more like N in this case
                .groups = "drop")

  }
  cd_est
}


#' Take output from brms prediction and turn to tidy form
#'
#' @param model_draws Output from `posterior_*pred`, which is of dimension
#'  `Iter` (in rows) by `Cells` (in columns)
#' @param data_strat Covariates to append to. The number of rows should be
#'  the number of `Cells`
#' @param yhat_name The name of the value for the predicted outcome.
#'
#' @return A tidy long dataset with `Iter` x `Cells` rows. Columns
#'  will include the columns of `data_strat`, the iteration and cell ID, and
#'  the predicted value for that cell at that iteration.
#'
#' @importFrom tidyr pivot_longer
#' @importFrom readr parse_number
#'
#' @export
pivot_celldraws_longer <- function(mod_draws, data_strat, yhat_name = "pred_n_yes") {

  # mod_draws is Iter by Cell
  stopifnot(ncol(mod_draws) == nrow(data_strat))

  # Transpose to Cell by Iter
  draws_transposed <- t(mod_draws)
  colnames(draws_transposed) <- paste0("V", 1:ncol(draws_transposed))

  as_tibble(draws_transposed) %>%
    mutate(cell = 1:n()) %>%
    bind_cols(data_strat, .) %>%
    pivot_longer(cols = matches("^V"), names_to = "iter", values_to = yhat_name) %>%
    mutate(iter = parse_number(iter))
}
