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
  data_folder <- file.path(project_folder, "data")
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
    ) {stop("")}
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

#' Plot number of taxa and mb-ness for each parameter setting
#' @return plots for all the variables
#' @author Giovanni Laudanno
#' @export
plot_taxa_and_mbness <- function(
  crown_age,
  saveit = TRUE
) {
  measure <- NULL; rm(measure)
  x <- NULL; rm(x)

  filename <- paste0("crown_age=", crown_age, "-measure_taxa.Rdata")
  project_folder <- get_pkg_path()
  data_folder <- file.path(project_folder, "data")
  age_folder <- file.path(data_folder, paste0("crown_age_", crown_age))
  file <- file.path(age_folder, filename)
  testit::assert(file.exists(file))
  load(file)

  df <- measure
  crown_age <- unique(measure$crown_age)
  testit::assert(length(crown_age) == 1)
  n_sims <- nrow(df[df$setting == df$setting[[1]], ])
  mus <- unique(df$mu)
  nus <- unique(df$nu)
  qs <- unique(df$q)
  settings <- expand.grid(q = qs, nu = nus, mu = mus)
  quant <- 0.95
  bin_factor <- 2.0
  fontsize <- 14
  variables <- names(measure)[8:10]
  for (variable in variables) {
    df$x <- df[[variable]]
    mu <- mus[1]; nu <- nus[1]; q <- qs[1]
    plots <- vector("list", nrow(settings))
    for (i in 1:nrow(settings)) {
      mu <- settings[i, "mu"]
      nu <- settings[i, "nu"]
      q <- settings[i, "q"]
      df1 <- df[df$mu == mu & df$nu == nu & df$q == q, ]
      if (all(df1$x == 0)) {
        df1$x[1:length(df1$x)] <- 1e-6
      }
      plots[[i]] <- ggplot2::ggplot(data = df1, ggplot2::aes(x = x)) +
        ggplot2::geom_histogram(
          data = df1,
          binwidth = bin_factor * max(df1$x) / sqrt(n_sims)
        ) +
        ggplot2::scale_x_continuous() +
        ggplot2::coord_cartesian(xlim = c(0, quantile(df1$x, quant))) +
        ggplot2::theme_bw() +
        ggplot2::xlab(label = variable)
    }

    m <- matrix(1:12, nrow = 3, ncol = 4)
    rownames(m) <- paste0("q=", qs); colnames(m) <- paste0("nu=", nus)

    q_grobs <- vector("list", length(qs))
    for (i in seq_along(qs)) {
      q_grobs[[i]] <- grid::textGrob(
        eval(bquote(expression("q = " ~ .(qs[i])))),
        gp = grid::gpar(fontsize = fontsize),
        rot = 90
      )
    }
    nu_grobs <- vector("list", length(nus))
    for (i in seq_along(nus)) {
      nu_grobs[[i]] <- grid::textGrob(
        bquote(nu ~ "=" ~ .(nus[i])),
        gp = grid::gpar(fontsize = fontsize),
        rot = 0
      )
    }

    grob_qlabs <- gridExtra::arrangeGrob(
      q_grobs[[1]], q_grobs[[2]], q_grobs[[3]], ncol = 1
    )
    grob_nulabs <- gridExtra::arrangeGrob(
      nu_grobs[[1]], nu_grobs[[2]], nu_grobs[[3]], nu_grobs[[4]], ncol = 4
    )
    grob_empty <- grid::textGrob("")
    for (i_mu in seq_along(mus)) {
      mu <- mus[i_mu]
      grob_plots <- gridExtra::arrangeGrob(
        grobs = plots[settings$mu == mu],
        layout_matrix = m
      )
      grob_legend <- grid::textGrob(
        bquote(variable, " plots for " ~ mu ~ "=" ~ .(mu)),
        gp = grid::gpar(fontsize = fontsize),
        rot = 0
      )
      g <- gridExtra::arrangeGrob(
        grob_empty, grob_nulabs, grob_empty,
        grob_qlabs, grob_plots, grob_empty,
        grob_empty, grob_legend, grob_empty,
        ncol = 3,
        widths = c(1, 16, 2.5),
        heights = c(1, 16, 1)
      )
      filename_grob <- file.path(age_folder, paste0(
        variable,
        "_plots_mu=",
        mu,
        "_crown_age=",
        crown_age,
        ".png"
      ))
      if (saveit == TRUE) {
        cowplot::save_plot(
          filename = filename_grob, plot = g, base_aspect_ratio = (16 / 9)
        )
      }
      g
    }
  }
}
