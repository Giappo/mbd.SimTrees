#' Analyze results
#' @return List with meaningful measures for each parameter setting
#' @author Giovanni Laudanno
#' @export
analyze_taxa_and_mbness <- function(
  crown_age = 8
) {
  filename <- paste0("crown_age=", crown_age, "-measure_taxa.Rdata")
  x <- unlist(strsplit(getwd(), .Platform$file.sep))
  if (get_pkg_name() %in% x) {
    y <- which(x == get_pkg_name())
    project_folder <- paste0(x[1:y], collapse = .Platform$file.sep)
  } else {
    home_folder <- paste0(x, collapse = .Platform$file.sep)
    project_folder <- file.path(home_folder, get_pkg_name())
    dir.create(project_folder)
  }
  data_folder <- file.path(project_folder, "data")
  if (!dir.exists(data_folder)) {
    stop("data_folder does not exists!")
  }
  age_folder <- file.path(data_folder, paste0("crown_age_", crown_age))
  if (!dir.exists(age_folder)) {
    stop("age_folder does not exists!")
  }
  file <- file.path(age_folder, filename)
  testit::assert(file.exists(file))
  load(file)

  df <- measure
  settings <- levels(df$setting)
  max_quant <- 0.95
  limit_n_taxas <- rep(0, length(settings))
  names(limit_n_taxas) <- settings
  median_mbness <- median_n_taxas <- limit_n_taxas
  for (s in seq_along(settings)) {
    setting <- settings[s]
    df1 <- df[df$setting == setting, ]
    median_n_taxas[s] <- median(df1$n_taxas)
    limit_n_taxas[s] <- quantile(df1$n_taxas, max_quant)
    median_mbness[s] <- median(df1$percentage_mb_species)
  }
  list(
    median_n_taxas = median_n_taxas,
    limit_n_taxas = limit_n_taxas,
    median_mbness = median_mbness
  )
}
