#' This function does nothing. It is intended to inherit is parameters'
#' documentation.
#' @param bins number of bins
#' @param cond Conditioning
#' @param crown_age The crown age of the tree
#' @param lambda per-lineage speciation rate. See \code{\link[mbd]{mbd_sim}}
#' @param lambdas per-lineage speciation rate. See \code{\link[mbd]{mbd_sim}}
#' @param mu per-species extinction rate. See \code{\link[mbd]{mbd_sim}}
#' @param mus per-species extinction rate. See \code{\link[mbd]{mbd_sim}}
#' @param n_replicates number of replicates
#' @param nu the rate at which a multiple-birth specation is triggered.
#' @param nus the rate at which a multiple-birth specation is triggered.
#' @param q per-species speciation probability in case of occurrance of
#' @param qs per-species speciation probability in case of occurrance of
#' @param saveit do you want to save it?
#' @param seed a random number generator seed
#' @param verbose give more output
#' @author Documentation by Giovanni Laudanno.
#' @note This is an internal function, so it should be marked with
#'   \code{@noRd}. This is not done, as this will disallow all
#'   functions to find the documentation parameters
#'
default_params_doc <- function(
  bins,
  cond,
  crown_age,
  lambda,
  lambdas,
  mu,
  mus,
  n_replicates,
  nu,
  nus,
  q,
  qs,
  saveit,
  seed,
  verbose
) {
  # Nothing
}
