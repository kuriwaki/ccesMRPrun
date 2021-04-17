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
#' @param shape Whether to return the output in \code{"wide"} (with one row per states)
#'  or \code{"long"} (one row per state x estimate). Defaults to \code{"wide"}.
#'
#' @return A wide dataframe where each row is a area, `p_raw` indicates the
#'  raw average, `p_wt` indicates the weighted average (if `weight_var` is provided),
#'  and `n_raw` is the raw sample size.  We also provide standard errors in the form `se_`
#'  `se_raw` follows the standard standard error for proportion `sqrt(p*(1-p)/n)`.
#'  The `se_wt` implements the weighted standard error, where the sample size
#'  is replaced with the effective sample size, `sum(wt)^2 / sum(wt^2)`.
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
direct_ests <- function(.formula, .data, area_var, weight_var = NULL, shape = "wide") {
  Form <- as.Formula(.formula)
  outcome_var <- all.vars(formula(Form, lhs = 1, rhs = 0))


  out <- .data %>%
    group_by(!!!syms(area_var)) %>%
    summarize(p_raw = mean(.data[[outcome_var]],
                           na.rm = TRUE),
              n_raw = sum(!is.na(.data[[outcome_var]])),
              se_raw = sqrt(.data$p_raw * (1 - .data$p_raw) / .data$n_raw),
              .groups = "drop")

  # weighted prop
  if (!is.null(weight_var)) {
    w_df <- .data %>%
      group_by(!!!syms(area_var)) %>%
      summarize(p_wt = weighted.mean(.data[[outcome_var]],
                                     .data[[weight_var]],
                                     na.rm = TRUE),
                n_wt = sum(.data[[weight_var]], na.rm = TRUE)^2 /
                  (sum(.data[[weight_var]]^2, na.rm = TRUE)),
                se_wt = sqrt(.data$p_wt * (1 - .data$p_wt) / .data$n_wt)
                )

      out <- left_join(out, w_df, by = area_var) %>%
        relocate(!!syms(area_var), starts_with("p_"))
  }

  if (shape == "wide")
    return(out)

  if (shape == "long") {
    out %>%
      pivot_longer(-st,
                   names_pattern = "(p|n|se)_(raw|wt)",
                   names_to = c(".value", "method")) %>%
      return()
  }
}


