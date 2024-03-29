% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/take-draws_poststrat.R
\name{poststrat_draws}
\alias{poststrat_draws}
\title{Get MCMC draws of post-stratified estimate of demog x cd cells}
\usage{
poststrat_draws(
  model,
  poststrat_tgt,
  orig_data = NULL,
  question_lbl = attr(orig_data, "question"),
  area_var = "cd",
  count_var = "count",
  calibrate = FALSE,
  calib_area_to = NULL,
  calib_to_var = NULL,
  calib_join_var = NULL,
  dtplyr = FALSE,
  new_levels = FALSE
)
}
\arguments{
\item{model}{stan model from fit_brms}

\item{poststrat_tgt}{The poststratification target. It must contain the column
\code{count}, which is treated as the number of \code{trials} in the binomial model.}

\item{orig_data}{original survey data. This defaults to NULL but if supplied
be used to (1) subset the poststratification, to areas only in the survey, and
(2) label the question outcome.}

\item{question_lbl}{A character string that indicates the outcome, e.g.
a shorthand for the outcome variable. This is useful when you want to preserve
the outcome or description of multiple models.}

\item{area_var}{A character string for the variable name(s) for area to group
and aggregate by. That is, the area of interest in MRP. Defaults to \code{"cd"}}

\item{count_var}{A character string for the variable name for the population
count in the \code{poststrat_tgt} dataframe. This will be renamed as if it is
a trial count in the model. Defaults to \code{"count"}.}

\item{calibrate}{Adjust each cell's posthoc estimates so they add up to
a pre-specified, user input? Logical, defaulting to FALSE. See  the \code{calib_area_to} argument.}

\item{calib_area_to}{A dataset with area-level correct values to calibrate to in the last
column. It should contain the variables set in \code{calib_join_var} and \code{calib_to_var}.
See \code{posthoc_error()} for details.}

\item{calib_to_var}{The variable to calibrate to, e.g. the voteshare}

\item{calib_join_var}{The variable that defines the level of the calibration dataframe
that can be joined, e.g. the area}

\item{dtplyr}{Whether to use a data.table/dtplyr backend for processing for
slightly faster dataframe wrangling. Currently does not apply to anything within the function.}

\item{new_levels}{If there are new levels in the poststrat table that do not have
coefficients in the survey data, should there be an extrapolation or assignment to 0s?
The answer should almost always be No in MRP.}
}
\value{
A tidy dataset with \code{qID} x \code{cd} x \code{iter} number of rows,
where \code{qID} is the number of questions (outcomes), \code{cd} is
the number of geographies, and \code{iter} is the number of iterations estimated in
the MCMC model.  The demographic cells within a district are averaged across,
and a MRP estimate is computed.
It contains the columns
\describe{
\item{iter}{The number of iterations}
\item{cd}{The geography}
\item{qID}{The question}
\item{p_mrp_est}{The proportion of success, estimated by MRP.}
}
}
\description{
Get MCMC draws of post-stratified estimate of demog x cd cells
}
\examples{
class(fit_GA) # brms object
head(acs_GA) # dataset

drw_GA <- poststrat_draws(fit_GA, poststrat_tgt = acs_GA, area_var = "cd")
drw_GA

if (FALSE)  {

# 1. get MRP estimates by CD, while calibrating the overall cd results to
# the election
## Each takes about 75 secs
drw_GA_fix <- poststrat_draws(fit_GA, poststrat_tgt = acs_GA, calibrate = TRUE,
                              calib_area_to = elec_GA,
                              calib_join_var = "cd",
                              calib_to_var = "clinton_vote_2pty")

# to get MRP estimates by CD and sex, while calibrating the overall
# cd result to the eleciton
drw_GA_sex <- poststrat_draws(fit_GA, poststrat_tgt = acs_GA, calibrate = TRUE,
                              calib_area_to = select(elec_GA, cd, clinton_vote_2pty),
                              area_var = c("cd", "female"),
                              calib_join_var = "cd",
                              calib_to_var = "clinton_vote_2pty")


## take some examples
samp_ests <- drw_GA_sex \%>\% filter(cd == "GA-01", iter == 1:5) \%>\% arrange(iter)

## Gender balance in poststratification target is 48.7 - 51.3
sex_wt <- acs_GA \%>\%
  filter(cd == "GA-01") \%>\%
  count(cd, clinton_vote_2pty, female, wt = count) \%>\%
  mutate(frac = n/sum(n))

## In all iterations, the MRP estimates should add up to the calibration target
samp_ests \%>\%
  left_join(sex_wt, by = c("cd", "female")) \%>\%
  group_by(cd, iter, clinton_vote_2pty) \%>\%
  summarize(implied_vote = sum(p_mrp*frac) / sum(frac))

}


}
