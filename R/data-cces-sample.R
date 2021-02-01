#' Sample CCES data
#'
#' A dataset containing the prices and other attributes of almost 54,000
#' diamonds.
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
#'
"fit_GA"
