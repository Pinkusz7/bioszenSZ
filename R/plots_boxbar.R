# Helper functions for boxplots and barplots extracted from app.R

# Draw a single significance bar
add_sigline <- function(p, group1, group2, label = "*",
                        height   = .05,
                        vsize    = .02,
                        tpad     = .01,
                        linewidth = .8,
                        textsize  = 5){
  build  <- ggplot_build(p)
  xbreaks <- build$layout$panel_params[[1]]$x$breaks
  get_x   <- function(g) if (is.numeric(g)) g else match(g, xbreaks)
  x1 <- get_x(group1);  x2 <- get_x(group2)

  dat   <- build$data[[1]]
  ytop  <- if ("ymax" %in% names(dat)) max(dat$ymax, na.rm = TRUE) else max(dat$y, na.rm = TRUE)
  yrng  <- diff(range(build$layout$panel_params[[1]]$y.range))
  ybar  <- ytop + height * yrng
  ycap  <- ybar - vsize * yrng
  ytxt  <- ybar + tpad  * yrng

  p +
    annotate("segment", x=x1,xend=x2, y=ybar,yend=ybar, linewidth=linewidth) +
    annotate("segment", x=x1,xend=x1, y=ybar,yend=ycap, linewidth=linewidth) +
    annotate("segment", x=x2,xend=x2, y=ybar,yend=ycap, linewidth=linewidth) +
    annotate("text",    x=mean(c(x1,x2)), y=ytxt,
             label=label, size=textsize, vjust=0) +
    coord_cartesian(clip = "off") +
    theme(plot.margin = margin(5.5, 20, 5.5, 5.5))
}

# Stack multiple significance bars without overlaps
stack_siglines <- function(p, sigs,
                           sep       = .05,
                           linewidth = .8,
                           vsize     = .02,
                           tpad      = .01,
                           tsize     = 5){
  if (length(sigs) == 0 || length(ggplot_build(p)$data) == 0)
    return(p)

  build  <- ggplot_build(p)
  yrng   <- build$layout$panel_params[[1]]$y.range
  xranks <- build$layout$panel_params[[1]]$x$breaks

  get_span <- function(cmp){
    x1 <- if (is.numeric(cmp$g1)) cmp$g1 else match(cmp$g1, xranks)
    x2 <- if (is.numeric(cmp$g2)) cmp$g2 else match(cmp$g2, xranks)
    c(min(x1,x2), max(x1,x2))
  }

  levels <- list()
  bar_level <- integer(length(sigs))

  for (i in seq_along(sigs)){
    span <- get_span(sigs[[i]])
    placed <- FALSE
    for (lvl in seq_along(levels)){
      overlap <- vapply(levels[[lvl]], function(iv)
        !(span[2] < iv[1] || span[1] > iv[2]), logical(1))
      if (!any(overlap)){
        levels[[lvl]] <- append(levels[[lvl]], list(span))
        bar_level[i]  <- lvl
        placed <- TRUE; break
      }
    }
    if (!placed){
      levels[[length(levels)+1]] <- list(span)
      bar_level[i] <- length(levels)
    }
  }

  extra <- (length(levels)+1) * sep * diff(yrng)
  p <- p + expand_limits(y = max(yrng)+extra)

  for (i in seq_along(sigs)){
    h <- bar_level[i] * sep
    cmp <- sigs[[i]]
    p <- add_sigline(p,
                     group1   = cmp$g1,
                     group2   = cmp$g2,
                     label    = cmp$lab,
                     height   = h,
                     vsize    = vsize,
                     tpad     = tpad,
                     linewidth= linewidth,
                     textsize = tsize)
  }
  p
}

# Build boxplots or barplots depending on 'tipo' (Boxplot/Barras)
build_plot_boxbar <- function(scope, strain, tipo) {
  req(plot_settings(), input$param)
  rawParam <- input$param
  is_norm  <- isTRUE(input$doNorm)
  param_sel <- if (is_norm) paste0(rawParam, "_Norm") else rawParam
  ps <- plot_settings() %>% filter(Parameter == rawParam)
  ylab <- if (nzchar(input$yLab)) input$yLab else ps$Y_Title
  lims <- get_ylim(param_sel)
  ymax   <- lims$ymax
  ybreak <- lims$ybreak
  colourMode <- input$colorMode
  fs_title  <- input$fs_title
  fs_axis   <- input$fs_axis
  axis_size <- input$axis_line_size

  if (scope == "Combinado" && tipo == "Boxplot") {
    df_plot <- datos_agrupados_norm() %>%
      order_filter_group() %>%
      transmute(Label, Valor = .data[[param_sel]])
    cols <- nlevels(df_plot$Label)
    pal  <- get_palette(cols)

    p <- ggplot(df_plot, aes(Label, Valor))
    if (input$colorMode == "Blanco y Negro") {
      p <- p +
        stat_boxplot(geom = "errorbar", width = .2, linewidth = .6, colour = "black") +
        geom_boxplot(fill = "white", colour = "black", width = input$box_w, linewidth = .6) +
        geom_jitter(colour = "black", width = input$pt_jit, size = input$pt_size)
    } else {
      p <- p +
        stat_boxplot(geom = "errorbar", width = .2, linewidth = .6) +
        geom_boxplot(aes(fill = Label), width = input$box_w, colour = "black", linewidth = .6) +
        geom_jitter(aes(colour = Label), width = input$pt_jit, size = input$pt_size) +
        scale_fill_manual(values = alpha(pal, .5)) +
        scale_colour_manual(values = pal)
    }
    p <- p +
      labs(title = input$plotTitle, y = ylab, x = NULL) +
      scale_y_continuous(limits = c(0, ymax), breaks = seq(0, ymax, by = ybreak), expand = c(0,0)) +
      theme_minimal() +
      theme(
        plot.title      = element_text(size = fs_title, face = "bold"),
        axis.title.y    = element_text(size = fs_axis, face = "bold"),
        axis.text.x     = element_text(size = fs_axis, angle = 45, hjust = 1),
        axis.text.y     = element_text(size = fs_axis),
        axis.line       = element_line(linewidth = axis_size),
        axis.ticks      = element_line(linewidth = axis_size),
        panel.grid      = element_blank(),
        legend.position = "none"
      )
    if (isTRUE(input$labelMode)) {
      p <- p + scale_x_discrete(labels = function(x) sub("-.*$", "", x))
    }
    p <- stack_siglines(p, sig_list(),
                        sep       = input$sig_sep,
                        linewidth = input$sig_linewidth,
                        vsize     = .02,
                        tpad      = input$sig_textpad,
                        tsize     = input$sig_textsize)
    return(p)
  }

  if (scope == "Combinado" && tipo == "Barras") {
    df_raw <- datos_agrupados_norm() %>%
      filter_reps_group() %>%
      order_filter_group()
    if (nrow(df_raw) == 0) {
      return(ggplot() + theme_void() +
               annotate("text", 0, 0, label = "Sin datos con la selección actual"))
    }
    resumen <- df_raw %>%
      group_by(Label) %>%
      summarise(
        Mean = mean(.data[[param_sel]], na.rm = TRUE),
        SD   = sd  (.data[[param_sel]], na.rm = TRUE),
        .groups = "drop"
      )
    cols <- max(nlevels(resumen$Label), nlevels(df_raw$Media))
    pal  <- get_palette(cols)
    p <- ggplot(resumen, aes(Label, Mean))
    if (input$colorMode == "Blanco y Negro") {
      p <- p +
        geom_col(fill = "white", colour = "black", width = .6) +
        geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD),
                      width = .2, linewidth = .6, colour = "black") +
        geom_jitter(
          data   = df_raw,
          aes(x = Label,
              y = .data[[param_sel]]),
          colour = "black",
          width  = input$pt_jit,
          size   = input$pt_size,
          inherit.aes = FALSE
        )
    } else {
      p <- p +
        geom_col(aes(fill = Label), width = .6) +
        geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD),
                      width = .2, linewidth = .6) +
        geom_jitter(
          data   = df_raw,
          aes(x = Label,
              y = .data[[param_sel]],
              colour = Media),
          width  = input$pt_jit,
          size   = input$pt_size
        ) +
        scale_fill_manual(values = alpha(pal, .5)) +
        scale_colour_manual(values = pal)
    }
    p <- p +
      labs(title = input$plotTitle, y = ylab, x = NULL) +
      scale_y_continuous(limits = c(0, ymax), breaks = seq(0, ymax, by = ybreak), expand = c(0,0)) +
      theme_minimal() +
      theme(
        plot.title      = element_text(size = fs_title, face = "bold"),
        axis.title.y    = element_text(size = fs_axis, face = "bold"),
        axis.text.x     = element_text(size = fs_axis, angle = 45, hjust = 1),
        axis.text.y     = element_text(size = fs_axis),
        axis.line       = element_line(linewidth = axis_size),
        axis.ticks      = element_line(linewidth = axis_size),
        panel.grid      = element_blank(),
        legend.position = "none"
      )
    if (isTRUE(input$labelMode)) {
      p <- p + scale_x_discrete(labels = function(x) sub("-.*$", "", x))
    }
    p <- stack_siglines(p, sig_list(),
                        sep       = input$sig_sep,
                        linewidth = input$sig_linewidth,
                        vsize     = .02,
                        tpad      = input$sig_textpad,
                        tsize     = input$sig_textsize)
    return(p)
  }

  if (tipo == "Boxplot") {
    df <- datos_agrupados_norm() %>%
      filter(Strain == strain) %>%
      order_filter_strain() %>%
      filter_reps_strain()
    cols <- nlevels(factor(df$Media))
    p <- if (colourMode == "Blanco y Negro") {
      ggplot(df, aes(Media, .data[[param_sel]])) +
        stat_boxplot(geom = "errorbar", width = .2, linewidth = .6, colour = "black") +
        geom_boxplot(fill = "white", colour = "black", width = input$box_w, linewidth = .6) +
        geom_jitter(colour = "black", width = input$pt_jit, size = input$pt_size)
    } else {
      pal <- get_palette(cols)
      ggplot(df, aes(Media, .data[[param_sel]], fill = Media)) +
        stat_boxplot(geom = "errorbar", width = .2, linewidth = .6) +
        geom_boxplot(width = input$box_w, colour = "black", linewidth = .6) +
        scale_fill_manual(values = alpha(pal, .5)) +
        geom_jitter(aes(colour = Media),  width = input$pt_jit, size = input$pt_size) +
        scale_colour_manual(values = pal)
    }
    p <- p +
      labs(title = input$plotTitle, y = ylab, x = NULL) +
      scale_y_continuous(limits = c(0, ymax),
                         breaks = seq(0, ymax, by = ybreak),
                         expand = c(0, 0)) +
      theme_minimal() +
      theme(
        plot.title      = element_text(size = fs_title, face = "bold"),
        axis.title.y    = element_text(size = fs_axis,  face = "bold"),
        axis.text.x     = element_text(size = fs_axis, angle = 45, hjust = 1),
        axis.text.y     = element_text(size = fs_axis),
        axis.line       = element_line(linewidth = axis_size),
        axis.ticks      = element_line(linewidth = axis_size),
        panel.grid      = element_blank(),
        legend.position = "none"
      )
    p <- stack_siglines(p, sig_list(),
                        sep       = input$sig_sep,
                        linewidth = input$sig_linewidth,
                        vsize     = .02,
                        tpad      = input$sig_textpad,
                        tsize     = input$sig_textsize)
    return(p)
  }

  if (tipo == "Barras") {
    df_raw <- datos_agrupados_norm() %>%
      filter(Strain == strain) %>%
      order_filter_strain() %>%
      filter_reps_strain()
    if (nrow(df_raw) == 0) {
      return(ggplot() + theme_void() +
               annotate("text", x = 0, y = 0, label = "Sin datos con la selección actual"))
    }
    resumen <- df_raw %>%
      group_by(Media) %>%
      summarise(
        Mean = mean(.data[[param_sel]], na.rm = TRUE),
        SD   = sd  (.data[[param_sel]], na.rm = TRUE),
        .groups = "drop"
      )
    cols <- nlevels(resumen$Media)
    pal  <- get_palette(cols)
    p <- if (colourMode == "Blanco y Negro") {
      ggplot(resumen, aes(Media, Mean)) +
        geom_col(fill = "white", colour = "black", width = .6) +
        geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD),
                      width = .2, linewidth = .6, colour = "black") +
        geom_jitter(data   = df_raw,
                    aes(x = Media,
                        y = .data[[param_sel]]),
                    colour = "black",
                    width  = input$pt_jit,
                    size   = input$pt_size)
    } else {
      ggplot(resumen, aes(Media, Mean, fill = Media)) +
        geom_col(width = .6) +
        scale_fill_manual(values = alpha(pal, .5)) +
        geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD),
                      width = .2, linewidth = .6) +
        geom_jitter(data = df_raw,
                    aes(x = Media, y = .data[[param_sel]], colour = Media),
                    width = .07, size = 2.5) +
        scale_colour_manual(values = pal)
    }
    p <- p +
      labs(title = input$plotTitle, y = ylab, x = NULL) +
      scale_y_continuous(limits = c(0, ymax),
                         breaks = seq(0, ymax, by = ybreak),
                         expand = c(0, 0)) +
      theme_minimal() +
      theme(
        plot.title      = element_text(size = fs_title, face = "bold"),
        axis.title.y    = element_text(size = fs_axis,  face = "bold"),
        axis.text.x     = element_text(size = fs_axis, angle = 45, hjust = 1),
        axis.text.y     = element_text(size = fs_axis),
        axis.line       = element_line(linewidth = axis_size),
        axis.ticks      = element_line(linewidth = axis_size),
        panel.grid      = element_blank(),
        legend.position = "none"
      )
    p <- stack_siglines(p, sig_list(),
                        sep       = input$sig_sep,
                        linewidth = input$sig_linewidth,
                        vsize     = .02,
                        tpad      = input$sig_textpad,
                        tsize     = input$sig_textsize)
    return(p)
  }

  ggplot() + theme_void()
}
