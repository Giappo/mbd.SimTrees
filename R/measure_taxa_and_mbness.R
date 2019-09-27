#' Measure number of taxa and mb-ness for each parameter setting
#' @return a dataframe with parameter settings, a sample of number of taxa and
#'  a sample of mb-ness (in percentage and absolute).
#' @author Giovanni Laudanno
#' @export
measure_taxa_and_mbness <- function(
  crown_age = 8,
  n_replicates = 1e4,
  saveit = TRUE
) {

  measure <- NULL; rm(measure)

  # folder structure
  project_folder <- get_pkg_path()
  x <- unlist(strsplit(getwd(), .Platform$file.sep))

  if (get_pkg_name() %in% x) {
    y <- which(x == get_pkg_name())
    z <- paste0(x[1:y], collapse = .Platform$file.sep)
    setwd(z)
  } else {
    dir.create(get_pkg_name())
    setwd(get_pkg_name())
  }

  data_folder <- file.path(project_folder, "data")
  if (!dir.exists(data_folder)) {
    dir.create(data_folder)
  }

  age_folder <- file.path(data_folder, paste0("crown_age_", crown_age))
  if (!dir.exists(age_folder)) {
    dir.create(age_folder)
  }
  filename <- paste0("crown_age=", crown_age, "-measure_taxa.Rdata")
  if (file.exists(filename)) {
    load(file.path(age_folder, filename))
    prev_n_replicates <- sum(measure$setting == measure$setting[1])
    if (prev_n_replicates >= n_replicates) {
      return()
    } else {
      file.remove(file.path(age_folder, filename))
      rm(measure)
    }
  }

  n_0 <- 2
  x <- razzo::create_mbd_params_table(n_replicates = n_replicates)
  x$crown_age <- crown_age
  x <- x %>% dplyr::distinct()
  percentage_mb_species <- n_mb_species <- n_taxas <- rep(NA, nrow(x))
  for (m in 1:nrow(x)) {
    pars <- x[m, ]
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
  x$n_taxas <- n_taxas
  x$n_mb_species <- n_mb_species
  x$percentage_mb_species <- percentage_mb_species
  x$setting <-
    interaction(x$lambda, x$mu, x$nu, x$q, x$crown_age, x$cond, sep = "-")

  measure <- x
  if (saveit == TRUE) {
    save(measure, file = file.path(age_folder, filename))
  }
  measure
}
