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
#' @param calibrate Adjust each cell's posthoc estimates so they add up to
#'  a pre-specified, user input? Logical, defaulting to FALSE. See  the `calib_area_to` argument.
#' @param calib_area_to A dataset with area-level correct values to calibrate to in the last
#'  column. It should contain the variables set in `calib_join_var` and `calib_to_var`.
#'  See `posthoc_error()` for details.
#' @param calib_join_var The variable that defines the level of the calibration dataframe
#'  that can be joined, e.g. the area
#' @param calib_to_var The variable to calibrate to, e.g. the voteshare
#' @param dtplyr Whether to use a data.table/dtplyr backend for processing for
#'  slightly faster dataframe wrangling. Currently does not apply to anything within the function.
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
#'  across all_of select
#' @importFrom tibble as_tibble
#' @importFrom brms posterior_epred
#' @importFrom glue glue
#' @import dtplyr
#'
#' @examples
#' class(fit_GA) # brms object
#' head(acs_GA) # dataset
#'
#' drw_GA <- poststrat_draws(fit_GA, poststrat_tgt = acs_GA, area_var = "cd")
#' drw_GA
#'
#' if (FALSE)  {
#'
#' # 1. get MRP estimates by CD, while calibrating the overall cd results to
#' # the election
#' ## Each takes about 75 secs
#' drw_GA_fix <- poststrat_draws(fit_GA, poststrat_tgt = acs_GA, calibrate = TRUE,
#'                               calib_area_to = elec_GA,
#'                               calib_join_var = "cd",
#'                               calib_to_var = "clinton_vote_2pty")
#'
#' # to get MRP estimates by CD and sex, while calibrating the overall
#' # cd result to the eleciton
#' drw_GA_sex <- poststrat_draws(fit_GA, poststrat_tgt = acs_GA, calibrate = TRUE,
#'                               calib_area_to = select(elec_GA, cd, clinton_vote_2pty),
#'                               area_var = c("cd", "female"),
#'                               calib_join_var = "cd",
#'                               calib_to_var = "clinton_vote_2pty")
#'
#'
#' ## take some examples
#' samp_ests <- drw_GA_sex %>% filter(cd == "GA-01", iter == 1:5) %>% arrange(iter)
#'
#' ## Gender balance in poststratification target is 48.7 - 51.3
#' sex_wt <- acs_GA %>%
#'   filter(cd == "GA-01") %>%
#'   count(cd, clinton_vote_2pty, female, wt = count) %>%
#'   mutate(frac = n/sum(n))
#'
#' ## In all iterations, the MRP estimates should add up to the calibration target
#' samp_ests %>%
#'   left_join(sex_wt, by = c("cd", "female")) %>%
#'   group_by(cd, iter, clinton_vote_2pty) %>%
#'   summarize(implied_vote = sum(p_mrp*frac) / sum(frac))
#'
#' }
#'
#'
#' @export
poststrat_draws <- function(model,
                            poststrat_tgt,
                            orig_data = NULL,
                            question_lbl = attr(orig_data, "question"),
                            area_var =  "cd",
                            count_var = "count",
                            calibrate = FALSE,
                            calib_area_to = NULL,
                            calib_to_var = NULL,
                            calib_join_var = NULL,
                            dtplyr = FALSE,
                            new_levels = FALSE) {

  # districts (CDs) to loop through. Remove extra in post-strat target
  if (!is.null(orig_data)) {
    areas <- intersect(unique(poststrat_tgt[[area_var]]),
                     unique(orig_data[[area_var]]))

    # subset to predict on
    poststrat_tgt <- poststrat_tgt[poststrat_tgt[[area_var]] %in% areas, ]
  }


  # rename variable for MRP est
  areas_strat <- rename(poststrat_tgt, n_response = {{count_var}})

  # draw, then reshape to tidy form
  p_draws <- posterior_epred(model,
                             newdata = areas_strat,
                             allow_new_levels = new_levels,
                             summary = FALSE)

  # group by variables
  iter_grp_vars <- c(area_var, "iter")
  iter_join_vars <- c(calib_join_var, "iter")

  if ("question_lbl" %in% colnames(areas_strat))
    iter_grp_vars <- c("qID", iter_grp_vars)


  # binomial (yes | n_response model)
  if (model$family$family == "binomial") {
    areas_draws <- pivot_celldraws_longer(
      mod_draws = p_draws,
      data_strat = areas_strat,
      yhat_name = "pred_n_yes")

    if ("question_lbl" %in% colnames(areas_draws))
      areas_draws <-  mutate(areas_draws, qID = question_lbl)

    if (calibrate) {
      message(glue("Calibrating for results in {n_distinct(calib_area_to[[calib_join_var]])} districts, {n_distinct(areas_draws$iter)} iterations each."))

      correct_add <- areas_draws %>%
        select(-matches(calib_to_var)) %>%
        left_join(calib_area_to, by = calib_join_var) %>%
        group_by(across(all_of(iter_join_vars))) %>%
        summarize(
          delta = calib_oneway(tgt = unique(.data[[calib_to_var]]),
                               ests = pred_n_yes / n_response,
                               n = n_response)
        )
      areas_grp <- areas_draws %>%
        left_join(correct_add, by = iter_join_vars) %>%
        mutate(pred_n_yes = n_response * invlogit(delta + logit_ghitza(pred_n_yes / n_response))) %>%
        group_by(across(all_of(iter_grp_vars)))

    }

    if (!calibrate) {
      areas_grp <- areas_draws %>%
        group_by(across(all_of(iter_grp_vars)))
    }


    # mean estimator
    areas_est <- areas_grp %>%
      summarize(p_mrp = sum(.data$pred_n_yes) / sum(.data$n_response),
                .groups = "drop")
  }

  # bernoulli
  if (model$family$family == "bernoulli") {

    areas_draws <- pivot_celldraws_longer(
      p_draws,
      areas_strat,
      yhat_name = "pred_yes")

    if (calibrate) {
      stop("Cannot allow for any bernoulli / logit model now")
    }
    if (!calibrate) {
      areas_grp <- areas_draws %>%
        group_by(across(all_of(iter_grp_vars)))
    }

    # mean estimator
    areas_est <- areas_grp %>%
      mutate(qID = question_lbl) %>%
      group_by(across(all_of(iter_grp_vars))) %>%
      summarize(p_mrp =  sum(.data$pred_yes*.data$n_response) / sum(.data$n_response), # n_response still a misnomer, more like N in this case
                .groups = "drop")

  }
  areas_est
}


#' Take output from brms prediction and turn to tidy form
#'
#' @param mod_draws Output from `posterior_*pred`, which is of dimension
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
#' @importFrom dplyr `%>%` n
#'
#' @examples
#' library(dplyr)
#' library(brms)
#'
#' pred_df <- rename(acs_GA, n_response = count) # match variables with model call
#' posterior_draws <- posterior_epred(object = fit_GA,  # stanfit
#'                                    newdata = pred_df)
#'
#' # output is matrix, not tidy form
#' class(posterior_draws)
#' dim(posterior_draws)
#'
#' # tidy out
#' out <- pivot_celldraws_longer(mod_draws = posterior_draws,
#'                               data_strat = pred_df,
#'                               yhat_name = "pred_n_yes")
#'
#' dim(out)
#' select(out, cd, n_response, cell, iter, pred_n_yes)
#'
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
    pivot_longer(cols = matches("^V[0-9]"), names_to = "iter", values_to = yhat_name) %>%
    mutate(iter = parse_number(.data$iter))
}
