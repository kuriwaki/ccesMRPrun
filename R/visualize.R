#' Standard scatterplot with 45 degree line
#'
#' @param tbl dataset of summary statistics
#' @param xvar Variable to put on x-axis, unquoted
#' @param yvar Variable to put on y-axis, unquoted
#' @param lblvar Variable to use as labels for `geom_text_repel`, unquoted
#' @param ubvar,lbvar Variable to use as upper and lower bounds for `geom_errorbar`, unqoted
#' @param xlab,ylab x and y-axis labels, respectively
#' @param xlim,ylim x and y-axis limits, respectively
#' @param by_form If the dataset is in long form with separate rows for different
#'  model estimates, you can supply a formula to be passed on to `facet_rep_wrap()` to
#'  have separate facets for each model.
#' @param by_nrow If using facets, how many rows should the facet take? Defaults to NULL,
#' which is facet_wrap's default
#' @param by_labels A named vector for the facets, where the names are the names
#' of the unique values of the variable by specified in `by_form` (e.g. "model") and
#' the values are the corresponding characters to recode to.
#' @param alpha.CI The transparency value for the error bars, ranging from 0 to 1.
#' @param alpha.text The transparency value for the labels, ranging from 0 to 1.
#' @param alpha.segment The transparency value for the segments linking the labels to the points, ranging from 0 to 1.
#' @param size.point Size of points to use in ggplot
#' @param size.text Size for the labels
#' @param size.errorstat Size for the error statistic
#' @param max.overlaps To be passed on to \code{geom_text_repel} if a label
#'  is used.
#' @param repeat.axis.text Whether to reproduce the axis texts for every facets in
#'  `facet_rep_wrap()`. Defaults to `FALSE`
#' @param show_error Whether or not to show the error statistic in a caption or a corner of the figure
#' @param expand_axes Whether to expand the axes so that the plot is a square,
#'  even if there is more whitespace. Overrides xlim and ylim.
#' @param ... Additional arguments sent to the \code{error_lbl} function
#'
#'
#'
#' @import ggplot2
#' @importFrom lemon facet_rep_wrap
#' @importFrom scales percent_format
#' @importFrom ggrepel geom_text_repel
#' @importFrom stringr str_replace str_trim str_remove
#' @importFrom dplyr pull enquo `%>%`
#' @importFrom tibble enframe
#' @importFrom purrr is_formula
#' @importFrom stats terms
#'
#' @export
#'
#' @examples
#' library(dplyr)
#'
#' mrp_df <- summ_sims(poststrat_draws(fit_GA, poststrat_tgt = acs_GA)) %>%
#'   left_join(elec_GA)
#'
#'
#' scatter_45(mrp_df,
#'            clinton_vote,
#'            p_mrp_est,
#'            lblvar = cd,
#'            lbvar = p_mrp_050,
#'            ubvar = p_mrp_950,
#'            xlab = "Clinton Vote",
#'            ylab = "MRP Estimate")
#'
#'
scatter_45 <- function(tbl, xvar, yvar, lblvar = NULL,
                       xlab = NULL, ylab = NULL,
                       xlim = NULL,
                       ylim = NULL,
                       size.point = 0.8,
                       size.text = 2,
                       size.errorstat = 2,
                       ubvar = NULL, lbvar = NULL,
                       alpha.CI = 0.8,
                       alpha.text = 0.5,
                       alpha.segment = 0.5,
                       max.overlaps = 20,
                       repeat.axis.text = FALSE,
                       by_form = NULL,
                       by_nrow = NULL,
                       by_labels = NULL,
                       show_error = TRUE,
                       expand_axes = TRUE, ...) {
  # setup
  xvar <- enquo(xvar)
  yvar <- enquo(yvar)

  lblvar <- enquo(lblvar)
  lbl_name <- quo_name(lblvar)

  lbvar <- enquo(lbvar)
  lb_name <- quo_name(lbvar)
  ubvar <- enquo(ubvar)
  ub_name <- quo_name(ubvar)


  if (!is.null(by_form))
    stopifnot(is_formula(by_form))

  axis_lim <- range(c(pull(tbl, !!xvar), pull(tbl, !!yvar)))

  if (expand_axes) {
    xlim = ylim = axis_lim
    }

  # main plot -- defaults
  gg0 <- ggplot(tbl, aes(x = {{xvar}}, y = {{yvar}})) +
    geom_point(size = size.point) +
    coord_equal(xlim = xlim, ylim = ylim) +
    scale_x_continuous(labels = percent_format(accuracy = 1)) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    theme_bw()

  gg1 <- gg0 +
    geom_abline(linetype = "dashed", alpha = 0.75)

  if (!is.null(by_form)) {
    form_char <- str_trim(str_remove(attr(terms(by_form), "term.labels"), "~"))
    formvar <- enquo(form_char)
    gg1 <- gg1 +
      facet_rep_wrap(by_form,
                     labeller = as_labeller(by_labels),
                     repeat.tick.labels = repeat.axis.text,
                     nrow = by_nrow)
  }

  if (ub_name != "NULL" & lb_name != "NULL") {
    gg1 <- gg1 +
      geom_errorbar(aes(ymin = {{lbvar}}, ymax = {{ubvar}}), width = 0, alpha = alpha.CI)
  }

  if (lbl_name != "NULL") {
    gg1 <- gg1 +
      geom_text_repel(aes(label = {{lblvar}}),
                      alpha = alpha.text,
                      size = size.text,
                      segment.alpha = alpha.segment,
                      max.overlaps = max.overlaps)
  }

  if (!is.null(xlab))
    gg1 <- gg1 + labs(x = xlab)

  if (!is.null(ylab))
    gg1 <- gg1 + labs(y = ylab)

  if (show_error) {
    if (is.null(by_form)) {
      err_txt <- error_lbl(truth = pull(tbl, !!xvar),
                           estimate = pull(tbl, !!yvar),
                           ...)
      gg1 <- gg1 +
        labs(caption = err_txt)
    }

    # separate stats by model
    if (!is.null(by_form)) {
      err_df <- tbl %>%
        group_by(across(all_of(form_char))) %>%
        summarize(text_to_show = error_lbl({{xvar}}, {{yvar}}),
                  .groups = "drop")

      gg1 <- gg1 +
        geom_text(data = err_df,
                  mapping = aes(label = text_to_show),
                  x = Inf, y = -Inf, hjust = 1.1, vjust = -0.5,
                  lineheight = 1, size = size.errorstat)
    }
  }

  gg1
}


#' Labels for accuracy metric
#'
#' @param truth Vector of true values
#' @param estimate Vector of estimates, must be the same length as \code{truth}.
#'  In fact, the metrics are invariant to which goes in which.
#' @param show_metrics The metrics to show. Defaults to RMSE and accuracy
#' @param metrics_lbl The labels to show for each metric. Named vaector
#' @param pp_accuracy Significant digits for percentage points. Corresponds to
#' the accuracy argument in scales::percent
#'
#' @importFrom scales percent_format percent
#' @importFrom stringr str_c
#' @importFrom glue glue
#'
#' @export
error_lbl <- function(truth, estimate,
                      show_metrics = c("rmse", "mean", "bias"),
                      metrics_lbl = c(rmse = "RMSE", mean = "Mean Abs. Dev.", bias = "Mean Dev."),
                      pp_accuracy = 0.1) {

  rmse_stat <- sqrt(mean((truth - estimate)^2))
  mean_stat <- mean(abs(truth - estimate))
  bias_stat <- mean(estimate - truth)

  stat_vec <- c(rmse = rmse_stat, mean = mean_stat, bias = bias_stat)

  show_stat <- percent(stat_vec[show_metrics],
                       accuracy = pp_accuracy,
                       suffix = "pp")

  show_lbl <- str_c(str_c(metrics_lbl[show_metrics], show_stat, sep = ": "),
                    collapse = "\n")

  show_lbl
}




