#' Two-way Calibration
#' objective function
#'
#' @importFrom purrr map_dbl
twoway_obj_fn <- function(par, obj) {
  dat <- obj$dat

  ## adjustment factor
  delta <- obj$X %*% par

  ## adjusted value
  dat$pi_adj <- invlogit(dat$pi_logit + delta)

  ## objective wrt district ---------------------------------
  # dat %>%
  #   group_by(!!sym(obj$var_area)) %>%
  #   summarise(pi_group = sum(n_gj * pi_adj) / sum(n_gj))
  by_area <- split(dat, obj$ind_area)
  avg_area <- map_dbl(by_area, function(X) with(X, weighted.mean(pi_adj, n_gj)))
  loss_area <- sum((obj$n_j / obj$n) * (obj$tgt_area - avg_area)^2)


  ## objective wrt racial groups ----------------------------
  by_group <- split(dat, obj$ind_group)
  avg_group <- map_dbl(by_group, function(X) with(X, weighted.mean(pi_adj, n_gj)))
  loss_group <- sum((obj$n_g / obj$n) * (obj$tgt_group - avg_group)^2)


  ## sum of two losses
  loss <- loss_area + loss_group

  return(loss)
}

#' Two-way Calibration
#'
#'
#' @param data
#'    Estimates stored in the long format.
#'    The column should be named `est`.
#' @param var_area
#'    Variable name (char) for area in \code{data}.
#' @param var_group
#'    Variable name (char) for group in \code{data}.
#' @param tgt_area
#'    Vector of true values for area.
#' @param tgt_group
#'    Vector of true values for group.
#' @param X
#'    Design matrix. E.g., \code{data.matrix(~cd+race-1, data = data)}.
#' @param n_area
#'    Vector consists of population sizes in each area.
#' @param n_group
#'    Vector consists of population sizes for each group.
#' @param n_total
#'    Scalar of total number of population.
#' @param delta_init
#'    Initial values of delta.
#' @return
#'    Data frame with new columns \code{"est_corrected"} and \code{"delta"}
#'
#' @source Kuriwaki, S., Ansolabehere, S., Dagonel, A., & Yamauchi, S. (2021).
#'  The Geography of Racially Polarized Voting: Calibrating Surveys at the
#'  District Level. <https://doi.org/10.31219/osf.io/mk9e6>
#'
#'
#' @examples
#' # Single estimate
#'
#'
posthoc_twoway <- function(
  data,
  var_area, var_group,
  tgt_area, tgt_group,
  X, n_area, n_group, n_total,
  delta_init = NULL
) {

  ## convert to logit scale
  data$pi_logit <- logit_ghitza(data$est)

  ## organize inputs
  input_dat <- list(
    dat       = as.data.frame(data[, c("pi_logit", "n_gj")]),
    ind_area  = data[[var_area]],
    ind_group = data[[var_group]],
    tgt_area  = tgt_area,
    tgt_group = tgt_group,
    X         = X,
    n_j       = n_area,
    n_g       = n_group,
    n         = n_total
  )

  # checks
  stopifnot(names(tgt_area) == levels(ind_area))
  stopifnot(names(tgt_group) == levels(ind_group))


  ## set initial values
  if (is.null(delta_init)) {
    par_init <- runif(ncol(X), -0.5, 0.5)
  } else {
    if (length(delta_init) != ncol(X)) stop("Wrong number of initial values.")
    par_init <- delta_init
  }

  ## estimate parameters
  fit <- optim(
    par = par_init,
    fn = twoway_obj_fn,
    method = "BFGS",
    obj = input_dat)


  ## update estimate
  delta <- as.vector(X %*% fit$par)
  data$est_corrected <- invlogit(logit_ghitza(data$est) + delta)
  data$delta <- delta
  return(data)
}


