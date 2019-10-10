#' Check results
#' @return Check that results will match conditions
#' @author Giovanni Laudanno
#' @export
check_taxa_and_mbness <- function(
  lambdas = c(0.2),
  mus = c(0, 0.1),
  nus = c(0, 0.5, 1.0, 1.5),
  qs = c(0.1, 0.15, 0.2),
  cond = 1,
  crown_age = 8
) {
  full_filename <- get_full_filename(
    lambdas = lambdas,
    mus = mus,
    nus = nus,
    qs = qs,
    crown_age = crown_age,
    cond = cond
  )
  x <- analyze_taxa_and_mbness(crown_age = crown_age)
  # conditions
  # min taxa
  if (!all(x$median_n_taxas >= min_n_taxa())) {
    print(x$median_n_taxas)
    stop("Not enough taxas!")
  }
  # max taxa
  if (!all(x$limit_n_taxas <= max_n_taxa())) {
    print(x$limit_n_taxas)
    stop("Too many taxas!")
  }
  # min mbness
  if (!any(x$median_mbness >= min_mbness())) {
    print(x$median_mbness)
    stop("Mbness is too weak!")
  }
}
