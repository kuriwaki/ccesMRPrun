#' CCES 2016 from Georgia
#'
#'
#' @format A survey dataset from the 2016 CCES
#' \describe{
#'   \item{year, case_id, ...}{Standard CCES variables. See https://doi.org/10.7910/DVN/II2DB6}
#'   \item{clinton_vote}{Hillary Clinton's voteshare as a portion of all votes}
#'   \item{clinton_vote_2way}{Hillary Clinton's voteshare as a portion of only
#'    Clinton and Trump voters}
#'   \item{weight}{YouGov post-stratification weights for pre-electin wave}
#'   \item{weight_post}{YouGov post-stratification weights for post-electin wave}
#'   \item{response}{A binary variable of Presidential vote for Clinton using
#'    post-election survey. See the source code in \code{data-raw} to see exactly
#'    how.}
#'   ...
#' }
"cces_GA"



#' ACS Data
#'
#'
#' Post-stratification dataset for \code{cces_GA}.
#'
"acs_GA"

#' Election outcome (often the quantity to estimate)
"elec_GA"


#' Fitted brms model
#'
#' @description A fitted multilevel of the formula
#'  `response ~ (1|age) + (1 + female |educ) + clinton_vote + (1|cd)`
#' where `response` is a binary variable for voting for Hilary Clinton in
#' 2016. The sample is `r nrow(cces_GA)` respondents in Georgia in 2016.
#'
#'
"fit_GA"

#' Summary statistics of posterior samples.
#'
#' @examples
#' # Shorthand for
#' drw_GA <- poststrat_draws(fit_GA, poststrat_tgt = acs_GA)
#' summ_GA <- summ_sims(drw_GA)
"summ_GA"
