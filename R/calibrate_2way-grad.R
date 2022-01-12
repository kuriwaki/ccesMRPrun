
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
  by_area <- split(dat, obj$ind_area)
  avg_area <- map_dbl(by_area, function(X) with(X, weighted.mean(pi_adj, n_gj)))
  loss_area <- obj$tgt_area - avg_area

  ## fix groups and average over areas
  by_group <- split(dat, obj$ind_group)
  avg_group <- map_dbl(by_group, function(X) with(X, weighted.mean(pi_adj, n_gj)))
  loss_group <- obj$tgt_group - avg_group

  ## grad wrt areas -----------------------------
  ## first term
  ### G by J
  Agj_tmp <- map2(by_area, loss_area, function(x, y) {
    as.vector(x$n * x$pi_adj_sq * y) / obj$n_total
  })

  ### sum over j
  Ag_1 <- colSums(do.call(rbind, Agj_tmp))

  ## second term
  Ag_2 <- map_dbl(by_group,
            function(x) sum(x$n * x$pi_adj_sq) / obj$n_total
          ) * loss_group

  grad_group <- -2 * (Ag_1 + Ag_2)

  ## grad wrt groups ----------------------------
  ## first term
  Agj_tmp <- map2(by_group, loss_group, function(x, y) {
    as.vector(x$n * x$pi_adj_sq * y) / obj$n_total
  })

  ## sum over group
  Aj_1 <- colSums(do.call(rbind, Agj_tmp))

  ## second term
  Aj_2 <- map_dbl(by_area,
            function(x) sum(x$n * x$pi_adj_sq) / obj$n_total
          ) * loss_area
  grad_area <- -2 * (Aj_1 + Aj_2)
  grad <- c(grad_area, grad_group)
  return(grad)
}
