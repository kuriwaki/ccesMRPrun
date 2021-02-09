#' Direct Estimator
#'
#' Collapses survey data to get direct estimates (i.e. non-pooled sample porportions)
#'
#' @param .formula MRP formula. Only thing that will be used is the outcome
#'  variable (a binary variable)
#' @param .data Survey data to be collapsed
#' @param area_var Character for the variable(s) that corresponds to the area to
#'  aggregate to.
#' @param weight_var Character for the variable that corresponds to weights.
#'
#' @return A wide dataframe where each row is a area, `p_raw` indicates the
#'  raw average, `p_wt` indicates the weighted average( if `weight_var` is provided),
#'  and `n_raw` is the raw sampe size.
#'
#' @importFrom Formula as.Formula
#' @importFrom dplyr group_by summarize relocate left_join
#' @export
#'
#' @examples
#'  direct_ests(response ~ (1|cd), cces_GA,
#'              area_var = "cd",
#'              weight_var = "weight_post")
#'
direct_ests <- function(.formula, .data, area_var, weight_var = NULL) {
  Form <- as.Formula(.formula)
  outcome_var <- all.vars(formula(Form, lhs = 1, rhs = 0))


  out <- .data %>%
    group_by(!!!syms(area_var)) %>%
    summarize(p_raw = mean(.data[[outcome_var]],
                           na.rm = TRUE),
              n_raw = sum(!is.na(.data[[outcome_var]])),
              .groups = "drop")

  # weighted prop
  if (!is.null(weight_var)) {
    w_df <- .data %>%
      group_by(!!!syms(area_var)) %>%
      summarize(p_wt = weighted.mean(.data[[outcome_var]],
                                     .data[[weight_var]],
                                     na.rm = TRUE))

      out <- left_join(out, w_df, by = area_var) %>%
        relocate(starts_with("p_"))
  }
 out
}


