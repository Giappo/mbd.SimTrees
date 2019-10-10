#' @export
get_pkg_name <- function() {
  # pkg_name <- basename(get_pkg_path())
  pkg_name <- "mbd.TaxaAndMbness"
  pkg_name
}

#' @export
get_pkg_path <- function() {
  # rprojroot::find_package_root_file()
  list.files(getwd())
}

#' @export
max_n_taxa <- function() {
  max_n <- 400
  max_n
}

#' @export
min_n_taxa <- function() {
  min_n <- 5
  min_n
}

#' @export
min_mbness <- function() {
  min_mbness <- 0.5
  min_mbness
}

#' @export
get_full_filename <- function(
  lambdas = c(0.2),
  mus = c(0, 0.1),
  nus = c(0, 0.5, 1.0, 1.5),
  qs = c(0.1, 0.15, 0.2),
  cond = 1,
  crown_age = 8
) {
  # folder structure
  x <- unlist(strsplit(getwd(), .Platform$file.sep))
  if (get_pkg_name() %in% x) {
    y <- which(x == get_pkg_name())
    project_folder <- paste0(x[1:y], collapse = .Platform$file.sep)
  } else {
    home_folder <- paste0(x, collapse = .Platform$file.sep)
    project_folder <- file.path(home_folder, get_pkg_name())
  }
  rm(x)
  if (!dir.exists(project_folder)) {
    dir.create(project_folder)
  }

  data_folder <- file.path(project_folder, "data")
  if (!dir.exists(data_folder)) {
    dir.create(data_folder)
  }

  parsetting <- paste0(
    "lambdas=", "[", toString(lambdas), "]",
    "-",
    "mus=", "[", toString(mus), "]",
    "-",
    "nus=", "[", toString(nus), "]",
    "-",
    "qs=", "[", toString(qs), "]",
    "-",
    "crown_age=", "[", toString(crown_age), "]"
  )
  parsetting <- gsub(parsetting, pattern = " ", replacement = "")
  parsetting_folder <- file.path(data_folder, parsetting)
  if (!dir.exists(parsetting_folder)) {
    dir.create(parsetting_folder)
  }
  filename <- paste0(
    "measure_taxa",
    "-",
    parsetting,
    ".Rdata"
  )
  full_filename <- file.path(parsetting_folder, filename)
  full_filename
}
