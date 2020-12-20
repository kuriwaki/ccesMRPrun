% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/draw_poststrat.R
\name{pivot_celldraws_longer}
\alias{pivot_celldraws_longer}
\title{Take output from brms prediction and turn to tidy form}
\usage{
pivot_celldraws_longer(mod_draws, data_strat, yhat_name = "pred_n_yes")
}
\arguments{
\item{data_strat}{Covariates to append to. The number of rows should be
the number of \code{Cells}}

\item{yhat_name}{The name of the value for the predicted outcome.}

\item{model_draws}{Output from \code{posterior_*pred}, which is of dimension
\code{Iter} (in rows) by \code{Cells} (in columns)}
}
\value{
A tidy long dataset with \code{Iter} x \code{Cells} rows. Columns
will include the columns of \code{data_strat}, the iteration and cell ID, and
the predicted value for that cell at that iteration.
}
\description{
Take output from brms prediction and turn to tidy form
}