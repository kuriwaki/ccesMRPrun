#' Standard scatterplot with 45 degree line
#'
#' @param tbl dataset of summary statistics
#' @param xvar Variable to put on x-axis, unquoted
#' @param yvar Variable to put on y-axis, unquoted
#' @param lblvar Variable to use as labels for geom_text_repel, unquoted
#' @param xlab,ylab x and y-axis labels, respectively
#' @param xlim,ylim x and y-axis limits, respectively.
#' @param show_error Whether or not to show the accuracy metrics in caption
#' @param expand_axes Whether to expand the axes so that the plot is a square,
#'  even if there is more whitespace. Overrides xlim and ylim.
#' @param ... Additional arguments sent to the \code{error_lbl} function
#'
#'
#'
#' @import ggplot2
#' @importFrom scales percent_format
#' @importFrom ggrepel geom_text_repel
#' @importFrom dplyr pull
#'
#'
#' @export
scatter_45 <- function(tbl, xvar, yvar, lblvar = NULL,
                       xlab = NULL, ylab = NULL,
                       xlim = NULL,
                       ylim = NULL,
                       modelvar = NULL,
                       show_error = TRUE,
                       expand_axes = TRUE, ...) {
  xvar <- enquo(xvar)
  yvar <- enquo(yvar)
  lblvar <- enquo(lblvar)

  axis_lim <- range(c(pull(tbl, !!xvar), pull(tbl, !!yvar)))

  if (expand_axes) {
    xlim = ylim = axis_lim
    }

  gg0 <- ggplot(tbl, aes(x = {{xvar}}, y = {{yvar}})) +
    geom_point() +
    coord_equal(xlim = xlim, ylim = ylim) +
    scale_x_continuous(labels = percent_format(accuracy = 1)) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    theme_bw()

  gg1 <- gg0 +
    geom_abline(linetype = "dashed", alpha = 0.75)

  if (!is.null(lblvar)) {
    gg1 <- gg1 +
      geom_text_repel(aes(label = {{lblvar}}), alpha = 0.5)
  }

  if (!is.null(xlab))
    gg1 <- gg1 + labs(x = xlab)

  if (!is.null(ylab))
    gg1 <- gg1 + labs(y = ylab)

  if (show_error) {
    err_txt <- error_lbl(truth = pull(tbl, !!xvar),
                         estimate = pull(tbl, !!yvar),
                         ...)
    gg1 <- gg1 +
      labs(caption = err_txt)
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
#'
error_lbl <- function(truth, estimate, show_metrics = c("rmse", "mean"),
                      metrics_lbl = c(rmse = "RMSE", mean = "MAD"),
                      pp_accuracy = 0.1) {

  rmse_stat <- sqrt(mean((truth - estimate)^2))
  mean_stat <- mean(abs(truth - estimate))

  stat_vec <- c(rmse = rmse_stat, mean = mean_stat)

  show_stat <- percent(stat_vec[show_metrics],
                       accuracy = pp_accuracy,
                       suffix = "pp")


  show_lbl <- str_c(str_c(metrics_lbl[show_metrics], show_stat, sep = ": "),
                    collapse = "\n")

  show_lbl
}




