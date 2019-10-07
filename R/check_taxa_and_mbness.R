#' Check results
#' @return Check that results will match conditions
#' @author Giovanni Laudanno
#' @export
check_taxa_and_mbness <- function(
  crown_age = 8
) {
  x <- analyze_taxa_and_mbness(crown_age = crown_age)
  if (!all(x$median_n_taxas > min_n_taxa())) {
    print(x$median_n_taxas)
    stop("Not enough taxas!")
  }
  if (!all(x$limit_n_taxas <= max_n_taxa())) {
    print(x$limit_n_taxas)
    stop("Too many taxas!")
  }
}
