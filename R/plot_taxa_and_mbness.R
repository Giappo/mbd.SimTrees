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
  read_crown_age <- unique(measure$crown_age)
  testit::assert(crown_age == read_crown_age)
  n_sims <- nrow(df[df$setting == df$setting[[1]], ])
  mus <- unique(df$mu)
  nus <- unique(df$nu)
  qs <- unique(df$q)
  settings <- expand.grid(q = qs, nu = nus, mu = mus)
  quant <- 0.95
  bin_factor <- 2.0
  fontsize <- 14
  variables <- names(measure)[8:10]
  jj <- 1
  all_plots <- vector("list")
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
      plot_filename <- paste0(
        "crown_age=", crown_age,
        "-",
        variable, "_plots-mu=", mu,
        ".png"
      )
      filename_grob <- file.path(age_folder, plot_filename)
      if (saveit == TRUE) {
        cowplot::save_plot(
          filename = filename_grob, plot = g, base_aspect_ratio = (16 / 9)
        )
      }
      all_plots[[jj]] <- g
      jj <- jj + 1
    }
  }
  all_plots
}
