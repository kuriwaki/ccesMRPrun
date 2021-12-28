#' Two-way Calibration
#' objective function
twoway_obj_fn <- function(par, obj) {
  dat <- obj$dat

  ## convert to logit scale
  pi_logit <- logit_ghitza(dat$est)

  ## adjustment factor
  delta <- obj$X %*% par

  ## adjusted value
  dat$pi_adj <- invlogit(pi_logit + delta)


  ## objective wrt district ---------------------------------
  avg_area <- dat %>%
    group_by(!!sym(obj$var_area)) %>%
    summarise(pi_area = sum(n_gj * pi_adj) / sum(n_gj)) %>%
    pull(pi_area)

  loss_area <- sum((obj$n_j / obj$n) * (obj$tgt_area - avg_area)^2)


  ## objective wrt racial groups ----------------------------
  avg_group <- dat %>%
    group_by(!!sym(obj$var_group)) %>%
    summarise(pi_group = sum(n_gj * pi_adj) / sum(n_gj)) %>%
    pull(pi_group)

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
#'
#' @examples
#' # Single estimate
#'
#' posthoc_twoay <-
#'
posthoc_twoway <- function(
  data,
  var_area, var_group,
  tgt_area, tgt_group,
  X, n_area, n_group, n_total,
  delta_init = NULL
) {


  ## organize inputs
  input_dat <- list(
    dat       = data,
    var_area  = var_area,
    var_group = var_group,
    tgt_area  = tgt_area,
    tgt_group = tgt_group,
    X         = X,
    n_j       = n_area,
    n_g       = n_group,
    n         = n_total
  )


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

