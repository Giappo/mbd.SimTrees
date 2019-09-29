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
  read_crown_age <- unique(measure$crown_age)
  testit::assert(crown_age == read_crown_age)
  n_sims <- nrow(df[df$setting == df$setting[[1]], ])
  mus <- unique(df$mu)
  nus <- unique(df$nu)
  qs <- unique(df$q)

  variables <- names(measure)[8:10]
  jj <- 1
  plots <- vector("list")
  for (variable in variables) {
    df$x <- df[[variable]]
    setting <- NULL; rm(setting)
    df2 <- df %>% dplyr::group_by(setting)
    z <- lapply(
      split(df2, f = df2$setting, drop = T),
      FUN = function(y) length(unique(y$x))
    )
    if (any(unlist(z) == 1)) {
      scales <- "free_y"
    } else {
      scales <- "free"
    }

    for (mu in mus) {

      df1 <- df[df$mu == mu, ]

      ##### Facet Labels
      # mu
      mu_values <- unique(df1$mu)
      mu_labels <- paste0("mu==", mu_values) # what you show
      names(mu_labels) <- mu_values # what's in the df
      # nu
      nu_values <- unique(df1$nu)
      nu_labels <- paste0("nu==", nu_values) # what you show
      names(nu_labels) <- nu_values # what's in the df
      # q
      q_values <- unique(df1$q)
      q_labels <- paste0("q==", q_values) # what you show
      names(q_labels) <- q_values # what's in the df
      # nu+q
      # df1$nu_q <- interaction(df1$nu, df1$q, sep = ",")
      df1$nu_q <- interaction(df1$nu, df1$q, sep = ",")
      nu_q_values <- unique(df1$nu_q)
      # nu_q_matrix <- expand.grid(q = q_values, nu = nu_values)
      nu_q_matrix <- expand.grid(nu = nu_values, q = q_values)
      nu_q_labels <- apply(
        nu_q_matrix,
        MARGIN = 1,
        FUN = function(x) paste0("nu==", x[1], "~~", "q==", x[2])
      ) # what you show
      names(nu_q_labels) <- nu_q_values # what's in the df

      plot <-
        ggplot2::ggplot(
        data = df1,
        ggplot2::aes(x = x)
      ) +
        ggplot2::geom_histogram(
          # data = df1 %>% dplyr::filter(bin == b),
          # ggplot2::aes(x = df1),
          data = df1,
          bins = 30
        ) +
        # ggplot2::facet_grid(
        ggplot2::facet_wrap(
          nu ~ q,
          # . ~ nu_q,
          # nu_q ~ .,
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
          strip.placement = "outside"#,
          # axis.title.x = ggplot2::element_text(variable)
        ) +
        # ggplot2::ggtitle(paste0(variable, " plots for mu = ", mu)) +
        ggplot2::ggtitle(bquote(mu ~ " = " ~ .(mu))) +
        ggplot2::xlab(variable) +
        ggplot2::theme_bw()
      plots[[jj]] <- plot
      plot_filename <- paste0(
        "crown_age=", crown_age,
        "-",
        variable, "_plots-mu=", mu,
        ".png"
      )
      if (saveit == TRUE) {
        ggplot2::ggsave(
          filename = file.path(age_folder, plot_filename),
          plot = plot
        )
      }
      jj <- jj + 1
    }
  }
  # ggplot2::scale_x_continuous(
  #   breaks = round(
  #     seq(min(df1$x), top_x, length.out = 5)
  #     , digits = 2
  #   )
  # ) +
  # ggplot2::coord_cartesian(xlim = c(0, top_x)) +
  # ggplot2::theme(axis.text = ggplot2::element_text(size = fontsize)) +
  # ggplot2::xlab(label = variable)
  plots
}
