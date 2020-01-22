#' Plot number of taxa and mb-ness for each parameter setting
#' @inheritParams default_params_doc
#' @return plots for all the variables
#' @author Giovanni Laudanno
#' @export
plot_taxa_and_mbness <- function(
  lambdas = c(0.2),
  mus = c(0, 0.1),
  nus = c(0, 0.5, 1.0, 1.5),
  qs = c(0.1, 0.15, 0.2),
  cond = 1,
  crown_age = 8,
  bins = 15,
  saveit = TRUE
) {
  measure <- NULL; rm(measure)
  median <- NULL; rm(median)
  quantile <- NULL; rm(quantile)
  x <- NULL; rm(x)

  full_filename <- get_full_filename(
    lambdas = lambdas,
    mus = mus,
    nus = nus,
    qs = qs,
    crown_age = crown_age,
    cond = cond
  )
  file <- full_filename
  testit::assert(file.exists(full_filename))
  load(full_filename)

  df <- measure
  read_crown_age <- unique(measure$crown_age)
  testit::assert(crown_age == read_crown_age)
  mus <- unique(df$mu)

  variables <- names(measure)[8:10]
  jj <- 1
  plots <- vector("list")
  for (variable in variables) {
    df$x <- df[[variable]]
    setting <- NULL; rm(setting)
    df2 <- dplyr::group_by(.data = df, setting)
    z <- lapply(
      split(df2, f = df2$setting, drop = T),
      FUN = function(y) length(unique(y$x))
    )
    scales <- "free"
    quant <- 0.99

    for (mu in mus) {

      df0 <- df[df$mu == mu, ]

      ##### Facet Labels
      # mu
      mu_values <- unique(df0$mu)
      mu_labels <- paste0("mu==", mu_values) # what you show
      names(mu_labels) <- mu_values # what's in the df
      # nu
      nu_values <- unique(df0$nu)
      nu_labels <- paste0("nu==", nu_values) # what you show
      names(nu_labels) <- nu_values # what's in the df
      # q
      q_values <- unique(df0$q)
      q_labels <- paste0("q==", q_values) # what you show
      names(q_labels) <- q_values # what's in the df
      df0$nu_q <- interaction(df0$nu, df0$q, sep = ",")
      nu_q_values <- unique(df0$nu_q)
      nu_q_matrix <- expand.grid(nu = nu_values, q = q_values)
      nu_q_labels <- apply(
        nu_q_matrix,
        MARGIN = 1,
        FUN = function(x) paste0("nu==", x[1], "~~", "q==", x[2])
      ) # what you show
      names(nu_q_labels) <- nu_q_values # what's in the df

      pippo <- split(x = df0, f = df0$setting)
      xx <- rep(NA, length(pippo))
      for (b in seq_along(names(pippo))) {
        baudo <- names(pippo)[b]
        pippobaudo <- pippo[[baudo]]
        if (length(unique(pippobaudo$x)) == 1) {
          pippo[[baudo]]$x[1] <- max(df0$x)
          xx[b] <- max(pippo[[baudo]]$x[1])
        } else {
          xx[b] <- quantile(pippobaudo$x, quant)
        }
      }
      for (b in seq_along(names(pippo))) {
        baudo <- names(pippo)[b]
        pippobaudo <- pippo[[baudo]]
        yy <- pippobaudo[pippobaudo$x <= xx[b], ]
        pippo[[baudo]] <- yy
      }
      zz <- do.call("rbind", pippo)
      rownames(zz) <- NULL
      df1 <- zz

      plot <-
        ggplot2::ggplot(
        data = df1,
        ggplot2::aes(x = x)
      ) +
        ggplot2::geom_histogram(
          data = df1,
          bins = bins
        ) +
        ggplot2::facet_wrap(
          nu ~ q,
          labeller = ggplot2::labeller(
            mu = ggplot2::as_labeller(mu_labels, ggplot2::label_parsed),
            nu = ggplot2::as_labeller(nu_labels, ggplot2::label_parsed),
            q = ggplot2::as_labeller(q_labels, ggplot2::label_parsed),
            nu_q = ggplot2::as_labeller(nu_q_labels, ggplot2::label_parsed)
          ),
          strip.position = "top",
          scales = scales,
          ncol = length(q_values)
        ) + ggplot2::theme(
          strip.placement = "outside"
        ) +
        ggplot2::ggtitle(
          label = paste0("Crown age = ", crown_age),
          subtitle = paste0(
            "lambda = ", df1$lambda,
            ", ",
            "mu = ", mu,
            ". Average ", variable, " = ",
            signif(mean(df1$x), 2)
          )
        ) +
        ggplot2::xlab(variable) +
        ggplot2::theme_bw()
      plots[[jj]] <- plot
      if (variable == "n_taxas") {
        variable_name <- "taxa"
      }
      if (variable == "n_mb_species") {
        variable_name <- "n_mb"
      }
      if (variable == "percentage_mb_species") {
        variable_name <- "p_mb"
      }
      variable_name <-
      plot_filename <- paste0(
        variable_name, "_", mu,
        ".png"
      )
      if (saveit == TRUE) {
        ggplot2::ggsave(
          filename = file.path(dirname(full_filename), plot_filename),
          plot = plot,
          width = 10,
          height = 10,
        )
      }
      jj <- jj + 1
    }
  }
  plots
}
