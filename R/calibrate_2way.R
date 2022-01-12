#' Find two-way correction for cell estimates
#'
#'
#' @param data
#'    Estimates stored in the long format. Must be coercible to a non-tibble dataframe.
#'    The column should be named `est` and the sample size should be called `n`.
#' @param var_area
#'    Variable name (char) for area in \code{data}.
#' @param var_group
#'    Variable name (char) for group in \code{data}.
#' @param tgt_area
#'    Vector of true values for area. Must be named so that the names indicate
#'    the area that corresponds to the number.
#' @param tgt_group
#'    Vector of true values for group. Must be named so that the names indicate
#'    the group that corresponds to the number.
#' @param n_area
#'    Vector consists of population sizes in each area.
#' @param n_group
#'    Vector consists of population sizes for each group.
#' @param n_total
#'    Scalar of total number of population.
#' @param delta_init
#'    Initial values of delta.
#' @param use_grad
#'    Whether to use the gradient function to speed up the optimization.
#' @return
#'    Data frame with new columns \code{"est_corrected"} and \code{"delta"}
#'
#' @source Kuriwaki, S., Ansolabehere, S., Dagonel, A., & Yamauchi, S. (2021).
#'  The Geography of Racially Polarized Voting: Calibrating Surveys at the
#'  District Level. <https://doi.org/10.31219/osf.io/mk9e6>
#'
#' @seealso calib_oneway
#'
#' @importFrom glue glue
#' @export
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' library(tibble)
#' library(tictoc)
#'
#' # MRP estimates
#' drw_GA_educ <- poststrat_draws(fit_GA, poststrat_tgt = acs_GA, area_var = c("cd", "educ"))
#' acs_GA_educ <- acs_GA %>% count(cd, educ, wt = count, name = "N")
#'
#' # for a particular draw
#' i <- 1
#'
#' # Data at cell level
#' draw_i <- drw_GA_educ %>%
#'   filter(iter == i) %>%
#'   left_join(acs_GA_educ, by = c("cd", "educ")) %>%
#'   rename(est = p_mrp, n = N)
#'
#'
#' # ys
#' elec_tgt <- deframe(select(elec_GA, cd, clinton_vote_2pty))
#' educ_tgt <- c(`HS or Less` = 0.40, `Some College` = 0.45, `4-Year` = 0.50, `Post-Grad` = 0.60)
#'
#' # Ns
#' area_N <- deframe(count(acs_GA_educ, cd, wt = N))
#' educ_N <- deframe(count(acs_GA_educ, educ, wt = N))
#' totalN <- deframe(count(acs_GA_educ, wt = N))
#'
#' # Run
#' set.seed(02138)
#' out <- calib_twoway(
#'   data = draw_i,
#'   var_area = "cd",
#'   var_group = "educ",
#'   tgt_area  = elec_tgt,
#'   tgt_group = educ_tgt,
#'   n_area = area_N,
#'   n_group = educ_N,
#'   n_total = totalN,
#'   use_grad = TRUE
#' )
#'}
#'
calib_twoway <- function(
  data,
  var_area,
  var_group,
  tgt_area,
  tgt_group,
  n_area,
  n_group,
  n_total,
  delta_init = NULL,
  use_grad   = FALSE
) {

  ## convert to logit scale
  data$pi_logit <- logit_ghitza(data$est)

  # shorten
  dat <- as.data.frame(data[, c("pi_logit", "n")])
  ind_area <- data[[var_area]]
  ind_group <- data[[var_group]]

  # Checks ordering. If the data variable is a factor, check its levels.
  lv_g <- names(tgt_group); lv_j <- names(tgt_area)
  su <- function(x) sort(base::unique(x))
  stopifnot(identical(lv_g, levels(ind_group)) || identical(lv_g, su(ind_group)))
  stopifnot(identical(lv_j, levels(ind_area))  || identical(lv_j, su(ind_area)))

  # Make X matrix
  data_matrix <- cbind(
    model.matrix(as.formula(glue("~ {var_area} - 1")), data),
    model.matrix(as.formula(glue("~ {var_group} - 1")), data)
  )

  ## organize inputs
  input_dat <- list(
    dat       = dat,
    ind_area  = ind_area,
    ind_group = ind_group,
    tgt_area  = tgt_area,
    tgt_group = tgt_group,
    X         = data_matrix,
    n_j       = n_area,
    n_g       = n_group,
    n_total   = n_total
  )

  ## set initial values
  if (is.null(delta_init)) {
    par_init <- runif(ncol(data_matrix), -0.5, 0.5)
  } else {
    if (length(delta_init) != ncol(data_matrix)) stop("Wrong number of initial values.")
    par_init <- delta_init
  }

  if (isTRUE(use_grad)) {
    grad_fn <- twoway_grad_fn
  } else {
    grad_fn <- NULL
  }

  ## estimate parameters
  fit <- optim(
    par = par_init,
    fn = twoway_obj_fn,
    gr = grad_fn,
    method = "BFGS",
    obj = input_dat)


  ## update estimate
  delta <- as.vector(data_matrix %*% fit$par)
  data$est_corrected <- invlogit(logit_ghitza(data$est) + delta)
  data$delta <- delta
  return(data)
}

#' Two-way Calibration objective function
#' @param par The vector of parameters.
#' @param obj A list of inputs See `calib_twoway`.
#'
#' @importFrom purrr map_dbl
#' @keywords internal
twoway_obj_fn <- function(par, obj) {
  dat <- obj$dat

  ## adjustment factor
  delta <- obj$X %*% par

  ## adjusted value
  dat$pi_adj <- invlogit(dat$pi_logit + delta)

  ## objective wrt district ---------------------------------
  # dat %>%
  #   group_by(!!sym(obj$var_area)) %>%
  #   summarise(pi_group = sum(n * pi_adj) / sum(n))
  by_area <- split(dat, obj$ind_area)
  avg_area <- map_dbl(by_area, function(X) with(X, weighted.mean(pi_adj, n)))
  loss_area <- sum((obj$n_j / obj$n_total) * (obj$tgt_area - avg_area)^2)


  ## objective wrt racial groups ----------------------------
  by_group <- split(dat, obj$ind_group)
  avg_group <- map_dbl(by_group, function(X) with(X, weighted.mean(pi_adj, n)))
  loss_group <- sum((obj$n_g / obj$n_total) * (obj$tgt_group - avg_group)^2)


  ## sum of two losses
  loss <- loss_area + loss_group

  return(loss)
}

#' Two-way Calibration gradient function
#'
#' @param par A vector of parameters.
#' @param obj A list of inputs, see `calib_twoway`
#' @importFrom purrr map_dbl map2
#' @keywords internal
twoway_grad_fn <- function(par, obj) {
  dat <- obj$dat

  ## note
  ## - par is a stacked vector par = [delta(area), delta(group)]

  ## adjustment factor
  delta <- obj$X %*% par
  dat$pi_adj <- invlogit(dat$pi_logit + delta)
  dat$pi_adj_sq <- dat$pi_adj * (1 - dat$pi_adj)

  ## fix area and average over groups
  by_area   <- split(dat, obj$ind_area)
  avg_area  <- map_dbl(by_area, function(X) with(X, weighted.mean(pi_adj, n)))
  loss_area <- obj$tgt_area - avg_area

  ## fix groups and average over areas
  by_group   <- split(dat, obj$ind_group)
  avg_group  <- map_dbl(by_group, function(X) with(X, weighted.mean(pi_adj, n)))
  loss_group <- obj$tgt_group - avg_group

  ## grad wrt groups -----------------------------
  ## first term
  ### G by J
  Agj_tmp <- map2(by_area, loss_area, function(x, y) {
    as.vector(x$n * x$pi_adj_sq * y) / obj$n_total
  })

  ### sum over j
  Ag_1 <- colSums(do.call(rbind, Agj_tmp))

  ## second term
  Ag_2 <- map_dbl(by_group, function(x) {
    sum(x$n * x$pi_adj_sq) / obj$n_total
  }) * loss_group

  grad_group <- -2 * (Ag_1 + Ag_2)

  ## grad wrt areas ----------------------------
  ## first term
  Agj_tmp <- map2(by_group, loss_group, function(x, y) {
    as.vector(x$n * x$pi_adj_sq * y) / obj$n_total
  })

  ## sum over group
  Aj_1 <- colSums(do.call(rbind, Agj_tmp))

  ## second term
  Aj_2 <- map_dbl(by_area, function(x) {
    sum(x$n * x$pi_adj_sq) / obj$n_total
  }) * loss_area
  grad_area <- -2 * (Aj_1 + Aj_2)

  ## output gradient
  grad <- c(grad_area, grad_group)
  return(grad)
}
