#' @export
get_pkg_name <- function() {
  pkg_name <- basename(get_pkg_path())
  pkg_name
}

#' @export
get_pkg_path <- function() {
  rprojroot::find_package_root_file()
}
