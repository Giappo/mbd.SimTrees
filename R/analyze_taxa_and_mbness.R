#' Analyze results
#' @inheritParams default_params_doc
#' @return List with meaningful measures for each parameter setting
#' @author Giovanni Laudanno
#' @export
analyze_taxa_and_mbness <- function(
  lambdas = c(0.2),
  mus = c(0, 0.1),
  nus = c(0, 0.5, 1.0, 1.5),
  qs = c(0.1, 0.15, 0.2),
  cond = 1,
  crown_age = 8
) {
  measure <- NULL; rm(measure)
  median <- NULL; rm(median)
  quantile <- NULL; rm(quantile)
  x <- NULL; rm(x)

  full_filename <- mbd.SimTrees::get_full_filename(
    lambdas = lambdas,
    mus = mus,
    nus = nus,
    qs = qs,
    crown_age = crown_age,
    cond = cond
  )
  file <- full_filename
  testit::assert(file.exists(full_filename))
  load(full_filename)

  df <- measure
  settings <- levels(df$setting)
  max_quant <- 0.95
  limit_n_taxas <- rep(0, length(settings))
  names(limit_n_taxas) <- settings
  median_mbness <- median_n_taxas <- limit_n_taxas
  quantiles_n_taxas <- vector("list", length(settings))
  names(quantiles_n_taxas) <- settings
  for (s in seq_along(settings)) {
    setting <- settings[s]
    df1 <- df[df$setting == setting, ]
    median_n_taxas[s] <- median(df1$n_taxas)
    limit_n_taxas[s] <- quantile(df1$n_taxas, max_quant)
    median_mbness[s] <- median(df1$percentage_mb_species)
    quantiles_n_taxas[[s]] <-
      # quantile(x = df1$n_taxas, c(0.50 - 0.34, 0.50, 0.50 + 0.34, 0.90))
      quantile(x = df1$n_taxas, c(0.25, 0.50, 0.75, 0.90))
  }
  list(
    median_n_taxas = median_n_taxas,
    limit_n_taxas = limit_n_taxas,
    median_mbness = median_mbness,
    quantiles_n_taxas = quantiles_n_taxas
  )
}
