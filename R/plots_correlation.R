# Build correlation plot from the original Shiny app

build_plot_correlation <- function(scope, strain) {
  raw_x   <- input$corr_param_x
  raw_y   <- input$corr_param_y
  param_x <- if (isTRUE(input$doNorm)) paste0(raw_x, "_Norm") else raw_x
  param_y <- if (isTRUE(input$doNorm)) paste0(raw_y, "_Norm") else raw_y

  base_df <- if (isTRUE(input$doNorm)) datos_agrupados_norm() else datos_agrupados()

  df_raw <- if (scope == "Por Cepa") {
    base_df %>%
      filter(Strain == strain) %>%
      order_filter_strain() %>%
      filter_reps_strain()
  } else {
    base_df %>% order_filter_group() %>% filter_reps_group()
  }

  df <- if (scope == "Por Cepa") {
    df_raw %>%
      group_by(Media) %>%
      summarise(X = mean(.data[[param_x]], na.rm = TRUE),
                Y = mean(.data[[param_y]], na.rm = TRUE),
                .groups = "drop") %>%
      rename(Label = Media)
  } else {
    df_raw %>%
      group_by(Strain, Media) %>%
      summarise(X = mean(.data[[param_x]], na.rm = TRUE),
                Y = mean(.data[[param_y]], na.rm = TRUE),
                .groups = "drop") %>%
      mutate(Label = if (isTRUE(input$labelMode)) Strain else paste(Strain, Media, sep = "-"))
  }

  validate(need(nrow(df) >= 3, "Se necesitan ≥3 puntos para calcular la correlación"))

  cor_res <- cor.test(df$X, df$Y, method = input$corr_method)
  eq_lab <- NULL
  if (isTRUE(input$corr_show_eq)) {
    fit <- lm(Y ~ X, data = df)
    slope <- coef(fit)[2]
    intercept <- coef(fit)[1]
    r2 <- summary(fit)$r.squared
    eq_lab <- sprintf("y = %.3f·x %+.3f\nR² = %.3f", slope, intercept, r2)
  }

  dx  <- 0.05 * (input$xmax_corr - input$xmin_corr)
  dy  <- 0.04 * (input$ymax_corr - input$ymin_corr)
  x_t <- input$xmax_corr - dx
  y_t <- input$ymax_corr - dy

  ggplot(df, aes(X, Y)) +
    geom_point(size = 3, colour = "black") +
    { if (isTRUE(input$corr_show_line))
        geom_smooth(method = "lm", se = FALSE,
                    colour = "black", linetype = "dashed") } +
    { if (isTRUE(input$corr_show_labels))
        geom_text(aes(label = Label),
                  nudge_y = 0.05 * (input$ymax_corr - input$ymin_corr),
                  size    = input$corr_label_size) } +
    annotate("text", x = x_t, y = y_t, hjust = 1, vjust = 1,
             label = sprintf("r = %.3f\np = %.3g", cor_res$estimate, cor_res$p.value),
             size = 5) +
    { if (!is.null(eq_lab))
        annotate("text", x = x_t, y = y_t - dy*2.3, hjust = 1, vjust = 1,
                 label = eq_lab, size = 5) } +
    labs(title = input$plotTitle,
         x     = if (nzchar(input$corr_xlab)) input$corr_xlab else raw_x,
         y     = if (nzchar(input$corr_ylab)) input$corr_ylab else raw_y) +
    scale_x_continuous(limits = c(input$xmin_corr, input$xmax_corr),
                       breaks = seq(input$xmin_corr, input$xmax_corr, by = input$xbreak_corr),
                       expand = c(0, 0)) +
    scale_y_continuous(limits = c(input$ymin_corr, input$ymax_corr),
                       breaks = seq(input$ymin_corr, input$ymax_corr, by = input$ybreak_corr),
                       expand = c(0, 0)) +
    coord_cartesian(clip = "off") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = input$fs_title, face = "bold"),
      axis.title = element_text(size = input$fs_axis,  face = "bold"),
      axis.text  = element_text(size = input$fs_axis),
      axis.line  = element_line(linewidth = input$axis_line_size),
      axis.ticks = element_line(linewidth = input$axis_line_size),
      panel.grid = element_blank(),
      plot.margin = margin(20, 50, 10, 10)
    )
}
