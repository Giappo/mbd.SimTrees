#' Create the parameter table to generate simulations
#' @inheritParams default_params_doc
#' @export
create_params_table <- function(
  lambdas = c(0.2),
  mus = c(0, 0.1),
  nus = c(0, 0.5, 1.0, 1.5),
  qs = c(0.1, 0.15, 0.2),
  cond = 1,
  crown_age = 8,
  n_replicates = 2
) {

  x <- expand.grid(lambda = lambdas, mu = mus, nu = nus, q = qs)
  no_mbd_lines <-
    apply(X = x, MARGIN = 1, FUN = function(y) y[3] == 0 | y[4] == 0)
  no_mbd_x <- expand.grid(lambda = lambdas, mu = mus, nu = 0, q = 0)
  x[no_mbd_lines, ] <- no_mbd_x
  x <- dplyr::distinct(x)
  x$crown_age <- crown_age
  x$cond <- cond
  x2 <- x[rep(1:nrow(x), rep(n_replicates, nrow(x))), ]
  x2$seed <- 1:nrow(x2)
  rownames(x2) <- NULL
  x2
}
