# Build stacked bar plotly chart from the original Shiny app

build_plotly_stack <- function(scope, strain = NULL) {
  num <- function(x) as.numeric(gsub(",", ".", x))

  params_apilar <- input$stackParams
  validate(need(length(params_apilar) > 0,
                "Selecciona ≥1 parámetro en “Parámetros incluidos”"))

  base_df <- if (isTRUE(input$doNorm)) datos_agrupados_norm() else datos_agrupados()
  df_f <- if (scope == "Por Cepa") {
    base_df |>
      filter(Strain == strain) |>
      order_filter_strain()    |>
      filter_reps_strain()
  } else {
    base_df |> order_filter_group()
  }

  eje_x <- if (scope == "Por Cepa") {
    "Media"
  } else if (isTRUE(input$labelMode)) {
    "Strain"
  } else {
    "Label"
  }

  if (is.factor(df_f[[eje_x]])) {
    df_f[[eje_x]] <- droplevels(df_f[[eje_x]])
  }
  df_f[[eje_x]] <- as.character(df_f[[eje_x]])

  order_stack_input <- trimws(strsplit(input$orderStack, ",")[[1]])
  order_levels <- intersect(order_stack_input, params_apilar)

  base_df <- if (isTRUE(input$doNorm)) datos_agrupados_norm() else datos_agrupados()
  df_f <- if (scope == "Por Cepa") {
    base_df %>% filter(Strain == strain) %>% order_filter_strain() %>% filter_reps_strain()
  } else {
    base_df %>% order_filter_group()
  }

  eje_x <- if (scope == "Por Cepa") {
    "Media"
  } else if (isTRUE(input$labelMode)) {
    "Strain"
  } else {
    "Label"
  }

  df_long <- df_f |>  
    pivot_longer(all_of(params_apilar),
                 names_to  = "Parametro",
                 values_to = "Valor") |>  
    group_by(.data[[eje_x]], Parametro) |>  
    summarise(
      Mean = mean(Valor, na.rm = TRUE),
      SD   = sd  (Valor, na.rm = TRUE),
      .groups = "drop"
    ) |>  
    mutate(Parametro = factor(Parametro, levels = order_levels)) |>  
    arrange(.data[[eje_x]], Parametro)

  pal <- get_palette(length(params_apilar))
  names(pal) <- params_apilar

  plt <- plot_ly(
    width  = input$plot_w,
    height = input$plot_h
  )
  for (p in order_levels) {
    sub <- df_long[df_long$Parametro == p, ]
    plt <- add_trace(
      plt,
      x        = sub[[eje_x]],
      y        = sub$Mean,
      type     = "bar",
      name     = p,
      marker   = list(color = pal[[p]],
                      line  = list(color = "black", width = 1))
    )
  }

  if (isTRUE(input$showErrBars)) {
    err_df <- df_long %>%
      group_by(.data[[eje_x]]) %>%
      arrange(Parametro, .by_group = TRUE) %>%
      mutate(y_top = cumsum(Mean)) %>%
      ungroup()

    thick <- num(input$errbar_size) * 1.6

    for (p in order_levels) {
      sub <- err_df[err_df$Parametro == p, ]
      plt <- add_trace(
        plt,
        x          = sub[[eje_x]],
        y          = sub$y_top,
        type       = "scatter",
        mode       = "markers",
        marker     = list(size = 1, opacity = 0),
        showlegend = FALSE,
        hoverinfo  = "skip",
        error_y = list(
          type       = "data",
          symmetric  = FALSE,
          array      = sub$SD,
          arrayminus = rep(0, nrow(sub)),
          color      = "black",
          thickness  = thick,
          width      = 20
        )
      )
    }
  }

  plt <- plt %>%
    layout(
      barmode = "stack",
      margin = list(t = input$fs_title * 2 + 20),
      title = list(text = input$plotTitle,
                   font = list(size = input$fs_title),
                   y    = 0.95),
      yaxis = list(title     = if (nzchar(input$yLab)) input$yLab else "",
                   titlefont = list(size = input$fs_axis),
                   tickfont  = list(size = input$fs_axis),
                   range     = c(0, input$ymax),
                   dtick     = input$ybreak,
                   showline  = TRUE,
                   linecolor = "black",
                   linewidth = input$axis_line_size,
                   ticks     = "outside",
                   ticklen   = 5,
                   tickcolor = "black",
                   showgrid  = FALSE),
      xaxis = list(title         = "",
                   type          = "category",
                   categoryorder = "array",
                   categoryarray = unique(df_long[[eje_x]]),
                   titlefont     = list(size = input$fs_axis),
                   tickfont      = list(size = input$fs_axis),
                   showline      = TRUE,
                   linecolor     = "black",
                   linewidth     = input$axis_line_size,
                   ticks         = "outside",
                   ticklen       = 5,
                   tickcolor     = "black",
                   showgrid      = FALSE),
      legend = list(title      = list(text = ""),
                    traceorder = "normal",
                    font       = list(size = input$fs_legend))
    )
  plt
}
