#' Run all
#' @inheritParams default_params_doc
#' @return List with meaningful measures for each parameter setting
#' @author Giovanni Laudanno
#' @export
run_all <- function(
  lambdas = c(0.2),
  mus = c(0, 0.1),
  nus = c(0, 0.5, 1.0, 1.5),
  qs = c(0.1, 0.15, 0.2),
  cond = 1,
  crown_age = 8,
  n_replicates = 1e4,
  bins = 15,
  saveit = TRUE
) {
  measure_taxa_and_mbness(
    lambdas = lambdas,
    mus = mus,
    nus = nus,
    qs = qs,
    cond = cond,
    crown_age = crown_age,
    n_replicates = n_replicates,
    saveit = saveit
  )
  plot_taxa_and_mbness(
    lambdas = lambdas,
    mus = mus,
    nus = nus,
    qs = qs,
    cond = cond,
    crown_age = crown_age,
    bins = bins,
    saveit = saveit
  )
  check_taxa_and_mbness(
    lambdas = lambdas,
    mus = mus,
    nus = nus,
    qs = qs,
    cond = cond,
    crown_age = crown_age
  )
}
