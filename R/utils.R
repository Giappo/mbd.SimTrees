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
  min_n <- 10
  min_n
}
