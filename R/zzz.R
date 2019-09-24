.onLoad <- function(libname, pkgname) { # nolint .onLoad cannot be snake_case

  suppressPackageStartupMessages(
    lapply(
      c(
        "beautier",
        "beastier",
        "tracerer",
        "mauricer",
        "babette",
        "mcbette",
        "pirouette",
        "razzo"
      ),
      library,
      character.only = TRUE,
      warn.conflicts = FALSE
    )
  )
  invisible()
}
