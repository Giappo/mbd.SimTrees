#' Measure number of taxa and mb-ness for each parameter setting
#' @return a dataframe with parameter settings, a sample of number of taxa and
#'  a sample of mb-ness (in percentage and absolute).
#' @inheritParams default_params_doc
#' @author Giovanni Laudanno
#' @export
measure_taxa_and_mbness <- function(
  lambdas = c(0.2),
  mus = c(0, 0.1),
  nus = c(0, 0.5, 1.0, 1.5),
  qs = c(0.1, 0.15, 0.2),
  cond = 1,
  crown_age = 8,
  n_replicates = 1e4,
  saveit = TRUE
) {

  measure <- NULL; rm(measure)

  full_filename <- mbd.SimTrees::get_full_filename(
    lambdas = lambdas,
    mus = mus,
    nus = nus,
    qs = qs,
    crown_age = crown_age,
    cond = cond
  )
  if (file.exists(full_filename)) {
    load(full_filename)
    prev_n_replicates <- sum(measure$setting == measure$setting[1])
    if (prev_n_replicates >= n_replicates) {
      return(measure)
    } else {
      file.remove(full_filename)
      rm(measure)
    }
  }

  n_0 <- 2
  params_table <- mbd.SimTrees::create_params_table(
    lambdas = lambdas,
    mus = mus,
    nus = nus,
    qs = qs,
    crown_age = crown_age,
    cond = cond,
    n_replicates = n_replicates
  )
  params_table$crown_age <- crown_age
  params_table <- dplyr::distinct(params_table)
  percentage_mb_species <- n_mb_species <- n_taxas <- rep(NA, nrow(params_table))
  for (m in 1:nrow(params_table)) {
    pars <- params_table[m, ]
    brts <- mbd::mbd_sim(
      pars = c(pars$lambda, pars$mu, pars$nu, pars$q),
      n_0 = n_0,
      age = pars$crown_age,
      cond = pars$cond,
      seed = pars$seed
    )$brts
    n_mb_species[m] <- mbd::count_n_mb_events(brts)
    percentage_mb_species[m] <- mbd::count_percentage_mb_events(brts)
    n_taxas[m] <- length(brts) + n_0 - 1
    if (
      is.nan(n_taxas[m]) ||
      is.nan(n_mb_species[m]) ||
      is.nan(percentage_mb_species[m])
    ) {stop("NaN produced!")}
  }
  params_table$n_taxas <- n_taxas
  params_table$n_mb_species <- n_mb_species
  params_table$percentage_mb_species <- percentage_mb_species
  params_table$setting <- droplevels(interaction(
    params_table$lambda,
    params_table$mu,
    params_table$nu,
    params_table$q,
    params_table$crown_age,
    params_table$cond,
    sep = "-"
  ))
  measure <- params_table
  if (saveit == TRUE) {
    save(measure, file = full_filename)
  }
  measure
}
