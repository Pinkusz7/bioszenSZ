# Reactive helpers extracted from the original server

# Initialize global reactive values for storing plots
init_plot_bank <- function() {
  plot_bank  <<- reactiveValues(all = list())
  panel_inserto <<- reactiveVal(FALSE)
  ov_trigger <<- reactiveVal(0)
}

# Add current plot to the composition panel
add_current_plot <- function() {
  id <- paste0("p", as.integer(Sys.time()))
  snap <- plot_base()
  plot_bank$all[[id]] <- list(id = id, plot = snap, overrides = list())
  if (!panel_inserto()) {
    insertTab(inputId = "mainTabs",
              tab = tab_compos,
              target = "Obtención de Parámetros de Crecimiento",
              position = "after",
              select = FALSE)
    panel_inserto(TRUE)
  }
}

# Reactive base plot depending on input controls
plot_base <- reactive({
  scope_sel  <- if (input$scope == "Combinado") "Combinado" else "Por Cepa"
  strain_sel <- if (scope_sel == "Por Cepa")    input$strain else NULL

  if (input$tipo == "Apiladas") {
    build_plotly_stack(scope_sel, strain_sel)
  } else if (input$tipo == "Curvas") {
    build_plot_curves(scope_sel, strain_sel)
  } else if (input$tipo == "Correlación") {
    build_plot_correlation(scope_sel, strain_sel)
  } else {
    build_plot_boxbar(scope_sel, strain_sel, input$tipo)
  }
})

# Combine selected plots using patchwork
combo_plot <- eventReactive(list(input$makeCombo, ov_trigger()), {
  req(input$plots_chosen)

  theme_ppt <- function(bs = 18, ax = 1.2){
    theme_minimal(base_size = bs) +
      theme(axis.line = element_line(linewidth = ax),
            axis.ticks = element_line(linewidth = ax),
            panel.grid = element_blank())
  }

  plots <- lapply(plot_bank$all[input$plots_chosen], function(info){
    p  <- info$plot
    ov <- info$overrides
    p <- p + theme_ppt(input$base_size_combo)
    if (!is.null(ov$title))
      p <- p + ggtitle(ov$title)
    if (!is.null(ov$fs_title))
      p <- p + theme(plot.title = element_text(size = ov$fs_title, face = "bold"))
    if (!is.null(ov$fs_axis))
      p <- p + theme(axis.title = element_text(size = ov$fs_axis, face = "bold"),
                     axis.text  = element_text(size = ov$fs_axis))
    if (!is.null(ov$fs_legend))
      p <- p + theme(legend.text = element_text(size = ov$fs_legend))
    if (isFALSE(input$show_legend_combo))
      p <- p + theme(legend.position = "none")
    p
  })

  patchwork::wrap_plots(plots,
             nrow = input$nrow_combo,
             ncol = input$ncol_combo) &
    theme(plot.title  = element_text(size = input$fs_title_all, face = "bold"),
          axis.title  = element_text(size = input$fs_axis_title_all, face = "bold"),
          axis.text   = element_text(size = input$fs_axis_text_all),
          legend.text = element_text(size = input$fs_legend_all))
})
