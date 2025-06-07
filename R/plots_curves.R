# Build growth curves plot as defined in the original app

build_plot_curves <- function(scope, strain) {
  req(curve_data(), curve_settings())

  df_cur <- curve_data() %>%
    mutate(across(-Time, ~ suppressWarnings(as.numeric(.x)))) %>%
    pivot_longer(cols = -Time, names_to = "Well", values_to = "Value") %>%
    left_join(datos_combinados(), by = "Well")

  if (scope == "Por Cepa") {
    df_cur <- df_cur %>%
      filter(Strain == strain) %>%
      order_filter_strain() %>%
      filter_reps_strain()

    media_order <- df_cur %>%
      distinct(Media, Orden) %>%
      arrange(Orden) %>%
      pull(Media)

    df_sum <- df_cur %>%
      group_by(Time, Media) %>%
      summarise(Avg = mean(Value, na.rm = TRUE), .groups = "drop") %>%
      mutate(Label = factor(Media, levels = media_order))

  } else {
    df_cur <- df_cur %>%
      order_filter_group() %>%
      filter_reps_group()

    platemap_labels <- datos_agrupados() %>%
      distinct(Strain, Media, Orden) %>%
      mutate(Label = paste(Strain, Media, sep = "-")) %>%
      arrange(Orden) %>%
      pull(Label)

    user_order <- if (!is.null(input$orderGroups) && nzchar(input$orderGroups)) {
      trimws(strsplit(input$orderGroups, ",")[[1]])
    } else NULL
    final_order <- if (!is.null(user_order)) intersect(user_order, platemap_labels) else platemap_labels

    if (isTRUE(input$labelMode)) {
      strain_order <- datos_agrupados() %>%
        group_by(Strain) %>%
        summarise(minO = min(Orden), .groups = "drop") %>%
        arrange(minO) %>%
        pull(Strain)

      df_sum <- df_cur %>%
        group_by(Time, Strain) %>%
        summarise(Avg = mean(Value, na.rm = TRUE), .groups = "drop") %>%
        mutate(Label = factor(Strain, levels = strain_order))

    } else {
      df_sum <- df_cur %>%
        group_by(Time, Strain, Media) %>%
        summarise(Avg = mean(Value, na.rm = TRUE), .groups = "drop") %>%
        mutate(Label = factor(paste(Strain, Media, sep = "-"), levels = final_order))
    }
  }

  cfg_cur <- curve_settings()[1, ]
  x_lab <- if (nzchar(input$cur_xlab)) input$cur_xlab else cfg_cur$X_Title
  y_lab <- if (nzchar(input$cur_ylab)) input$cur_ylab else cfg_cur$Y_Title

  ggplot(df_sum, aes(x = Time, y = Avg, colour = Label, group = Label)) +
    geom_line(size = 1.5) +
    geom_point(size = 1.5) +
    scale_colour_manual(values = get_palette(nlevels(df_sum$Label)),
                        breaks = levels(df_sum$Label)) +
    labs(title  = input$plotTitle,
         x      = x_lab,
         y      = y_lab,
         colour = NULL) +
    scale_x_continuous(limits = c(0, input$xmax_cur),
                       breaks = seq(0, input$xmax_cur, by = input$xbreak_cur),
                       expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, input$ymax_cur),
                       breaks = seq(0, input$ymax_cur, by = input$ybreak_cur),
                       expand = c(0, 0)) +
    theme_minimal() +
    theme(
      plot.title      = element_text(size = input$fs_title, face = "bold"),
      axis.title      = element_text(size = input$fs_axis, face = "bold"),
      axis.text       = element_text(size = input$fs_axis),
      axis.line       = element_line(linewidth = input$axis_line_size),
      axis.ticks      = element_line(linewidth = input$axis_line_size),
      panel.grid      = element_blank(),
      legend.text     = element_text(size = input$fs_legend),
      legend.key.size = unit(1.5, "lines")
    )
}
