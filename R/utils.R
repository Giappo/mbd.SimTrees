#' @noRd
get_pkg_name <- function() {
  pkg_name <- "mbd.SimTrees"
  pkg_name
}

#' @noRd
get_pkg_path <- function() {
  list.files(getwd())
}

#' @noRd
max_n_taxa <- function() {
  max_n <- 400
  max_n
}

#' @noRd
min_n_taxa <- function() {
  min_n <- 5
  min_n
}

#' @noRd
min_mbness <- function() {
  min_mbness <- 0.5
  min_mbness
}

#' @noRd
to_string2 <- function(
  var
) {
  gsub(x = toString(var), pattern = " ", replacement = "")
}

#' @noRd
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

  data_folder <- file.path(project_folder, "inst", "extdata")
  if (!dir.exists(data_folder)) {
    dir.create(data_folder)
  }

  parsetting <- paste0(
    "la=", to_string2(lambdas),
    "-",
    "mu=", to_string2(mus),
    "-",
    "nu=", to_string2(nus),
    "-",
    "q=", to_string2(qs),
    "-",
    "age=", to_string2(crown_age)
  )
  parsetting <- gsub(parsetting, pattern = " ", replacement = "")
  parsetting_folder <- file.path(data_folder, parsetting)
  if (!dir.exists(parsetting_folder)) {
    dir.create(parsetting_folder)
  }
  filename <- paste0(
    "result",
    ".Rdata"
  )
  full_filename <- file.path(parsetting_folder, filename)
  full_filename
}
