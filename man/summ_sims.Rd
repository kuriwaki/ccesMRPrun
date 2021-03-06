% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summarize_draws.R
\name{summ_sims}
\alias{summ_sims}
\title{Get summary statistics from draws (of counts)}
\usage{
summ_sims(sims, area_var = "cd", est_var = "p_mrp")
}
\arguments{
\item{sims}{output of \link{poststrat_draws}}

\item{area_var}{A character string for the variable name(s) for area to group
and aggregate by. That is, the area of interest in MRP. Defaults to \code{"cd"}}

\item{est_var}{column name with estimates. Following the output of
\link{poststrat_draws}, defaults to \code{p_mrp}.}
}
\description{
Get summary statistics from draws (of counts)
}
\examples{
drw_GA <- poststrat_draws(fit_GA, poststrat_tgt = acs_GA)

summ_sims(drw_GA)

}
