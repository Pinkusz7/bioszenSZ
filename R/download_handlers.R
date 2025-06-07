# Functions setting up downloadHandler outputs from the original app

setup_download_handlers <- function(output, session) {
  output$downloadPlot <- downloadHandler(
    filename = function(){
      paste0(if (input$scope == "Combinado") "Combinado" else sanitize(input$strain),
             "_", input$tipo, ".png")
    },
    content = function(file){
      scope_sel  <- if (input$scope=="Combinado") "Combinado" else "Por Cepa"
      strain_sel <- if (scope_sel=="Por Cepa") input$strain else NULL
      if (input$tipo == "Apiladas") {
        plt <- build_plotly_stack(scope_sel, strain_sel)
        export_plotly_png(p = plt, file = file,
                          width = input$plot_w, height = input$plot_h)
      } else {
        p <- plot_base()
        if (inherits(p, "ggplot")) {
          ggplot2::ggsave(filename = file, plot = p,
                          width = input$plot_w/100, height = input$plot_h/100,
                          dpi = 300, bg = "transparent")
        } else {
          p <- p %>% layout(paper_bgcolor = "rgba(0,0,0,0)",
                            plot_bgcolor  = "rgba(0,0,0,0)")
          export_plotly_png(p = p, file = file,
                            width = input$plot_w, height = input$plot_h)
        }
      }
    }
  )

  output$downloadExcel <- downloadHandler(
    filename    = function() "Parametros_por_grupo.xlsx",
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    content     = function(file) {
      datos  <- datos_combinados()
      params <- plot_settings()$Parameter
      wb_sum <- generate_summary_wb(datos, params)
      saveWorkbook(wb_sum, file, overwrite = TRUE)
    }
  )

  output$downloadStats <- downloadHandler(
    filename = function() "Tests_estadisticos.xlsx",
    content  = function(file){

      req(input$dataFile)

      params <- plot_settings()$Parameter
      datos  <- datos_combinados()

      scope_sel    <- isolate(input$scope)          %||% "Por Cepa"
      strain_sel   <- isolate(input$strain)
      sigTest_sel  <- isolate(input$sigTest)        %||% "ANOVA"
      postHoc_sel  <- isolate(input$postHoc)        %||% "Tukey"
      compMode_sel <- isolate(input$compMode)       %||% "all"
      controlGroup_sel <- isolate(input$controlGroup) %||% ""
      group1_sel   <- isolate(input$group1)         %||% ""
      group2_sel   <- isolate(input$group2)         %||% ""

      wb_tests <- createWorkbook()

      split_comparison <- function(x) stringr::str_split_fixed(x, "-", 2)

      for (param in params){
        sheet <- safe_sheet(param)
        addWorksheet(wb_tests, sheet)

        if (scope_sel == "Por Cepa"){
          df_param <- datos_agrupados() |>
            dplyr::filter(Strain == strain_sel) |>
            order_filter_strain()              |>
            filter_reps_strain()               |>
            dplyr::transmute(Label = Media,
                             Valor = .data[[param]])
        } else {
          df_param <- datos_agrupados() |>
            order_filter_group() |>
            dplyr::transmute(Label,
                             Valor = .data[[param]])
        }

        if (nrow(df_param) < 3 || dplyr::n_distinct(df_param$Label) < 2){
          removeWorksheet(wb_tests, sheet)
          next
        }

        norm_tbl <- df_param |>
          dplyr::group_by(Label) |>
          dplyr::summarise(
            Shapiro.stat = stats::shapiro.test(Valor)$statistic,
            Shapiro.p    = stats::shapiro.test(Valor)$p.value,
            Normal       = dplyr::if_else(Shapiro.p > 0.05, "Sí", "No"),
            .groups      = "drop"
          )

        writeData(wb_tests, sheet, "Normalidad",
                  startRow = 1, startCol = 1,
                  headerStyle = createStyle(textDecoration = "bold"))
        writeData(wb_tests, sheet, norm_tbl,
                  startRow = 2, startCol = 1,
                  headerStyle = createStyle(textDecoration = "bold"))

        do_anova <- function(df){
          aovm <- aov(Valor ~ Label, data = df)
          switch(postHoc_sel,
                 "Tukey"      = broom::tidy(TukeyHSD(aovm)),
                 "Bonferroni" = rstatix::pairwise_t_test(df, Valor ~ Label,
                                                         p.adjust.method = "bonferroni"),
                 "Sidak"      = rstatix::pairwise_t_test(df, Valor ~ Label,
                                                         p.adjust.method = "sidak"),
                 "Dunnett"    = dunnett_to_tibble(
                   DescTools::DunnettTest(Valor ~ Label,
                                          data = set_control(df, controlGroup_sel))),
                 "Scheffe"    = pmcmr_to_tibble(PMCMRplus::scheffeTest(aovm, "Label")),
                 "GamesHowell"= rstatix::games_howell_test(df, Valor ~ Label)
          )
        }

        do_kw <- function(df){
          switch(postHoc_sel,
                 "Dunn"    = rstatix::dunn_test(df, Valor ~ Label, p.adjust.method = "bonferroni"),
                 "Conover" = pmcmr_to_tibble(PMCMRplus::kwAllPairsConoverTest(df$Valor, df$Label)),
                 "Nemenyi" = pmcmr_to_tibble(PMCMRplus::kwAllPairsNemenyiTest(df$Valor, df$Label)),
                 "DSCF"    = {
                   f <- if (exists("kwAllPairsDSCFTest", asNamespace("PMCMRplus"), FALSE))
                     PMCMRplus::kwAllPairsDSCFTest
                   else
                     PMCMRplus::kwAllPairsDscfTest
                   pmcmr_to_tibble(f(df$Valor, df$Label))
                 }
          )
        }

        sig_raw <- switch(sigTest_sel,
                          "ANOVA"          = do_anova(df_param),
                          "Kruskal–Wallis" = do_kw(df_param),
                          "ttest" = {
                            if (compMode_sel == "all"){
                              rstatix::pairwise_t_test(df_param, Valor ~ Label, p.adjust.method = "holm")
                            } else if (compMode_sel == "control"){
                              rstatix::t_test(df_param, Valor ~ Label, ref.group = controlGroup_sel)
                            } else {
                              rstatix::t_test(
                                df_param |> dplyr::filter(Label %in% c(group1_sel, group2_sel)),
                                Valor ~ Label, paired = TRUE)
                            }
                          },
                          "wilcox" = {
                            if (compMode_sel == "all"){
                              rstatix::pairwise_wilcox_test(df_param, Valor ~ Label, p.adjust.method = "holm")
                            } else if (compMode_sel == "control"){
                              rstatix::wilcox_test(df_param, Valor ~ Label, ref.group = controlGroup_sel)
                            } else {
                              rstatix::wilcox_test(
                                df_param |> dplyr::filter(Label %in% c(group1_sel, group2_sel)),
                                Valor ~ Label, paired = TRUE)
                            }
                          }
        )

        if ("comparison" %in% names(sig_raw)){
          cmp <- split_comparison(sig_raw$comparison)
          sig_raw$group1 <- cmp[,1]; sig_raw$group2 <- cmp[,2]
        }
        if (all(c("grupo1","grupo2") %in% names(sig_raw))){
          sig_raw$group1 <- sig_raw$grupo1
          sig_raw$group2 <- sig_raw$grupo2
        }

        p_candidates <- intersect(
          c("p","p.value","p.adj","adj.p.value","p_val","p.value.adj"),
          names(sig_raw)
        )
        pcol <- p_candidates[1]

        sig_tbl <- sig_raw |>
          dplyr::mutate(
            P_valor       = .data[[pcol]],
            Significativo = dplyr::if_else(P_valor < 0.05, "Sí", "No"),
            Estrellas     = dplyr::case_when(
              P_valor < 0.001 ~ "***",
              P_valor < 0.01  ~ "**",
              P_valor < 0.05  ~ "*",
              TRUE            ~ ""
            )
          )

        start <- nrow(norm_tbl) + 4

        writeData(wb_tests, sheet,
                  paste("Test usado:", sigTest_sel),
                  startRow = start - 1, startCol = 1,
                  headerStyle = createStyle(textDecoration = "bold"))

        if (sigTest_sel %in% c("ANOVA","Kruskal–Wallis")){
          writeData(wb_tests, sheet,
                    paste("Post-hoc:", postHoc_sel),
                    startRow = start, startCol = 1,
                    headerStyle = createStyle(textDecoration = "bold"))
          sig_header_row <- start + 1
        } else {
          sig_header_row <- start
        }

        writeData(wb_tests, sheet, "Significancia",
                  startRow = sig_header_row, startCol = 1,
                  headerStyle = createStyle(textDecoration = "bold"))
        writeData(wb_tests, sheet, sig_tbl,
                  startRow = sig_header_row + 1, startCol = 1,
                  headerStyle = createStyle(textDecoration = "bold"))
      }

      saveWorkbook(wb_tests, file, overwrite = TRUE)
    }
  )

  output$downloadMetadata <- downloadHandler(
    filename = function(){
      paste0("metadata_", input$tipo, ".xlsx")
    },
    content  = function(file){
      wb <- createWorkbook()
      addWorksheet(wb, "Metadata")

      meta <- tibble::tibble(
        Campo  = c("scope", "tipo", "colorMode",
                   "plot_w", "plot_h", "fs_title", "fs_axis", "fs_legend"),
        Valor  = c(input$scope, input$tipo, input$colorMode,
                   input$plot_w, input$plot_h,
                   input$fs_title, input$fs_axis, input$fs_legend)
      )

      if (input$tipo %in% c("Boxplot", "Barras")){
        meta <- add_row(
          meta,
          Campo = c("param", "doNorm", "ctrlMedium",
                    "ymax", "ybreak"),
          Valor = c(
            safe_param(),
            as.character(input$doNorm),
            if (is.null(input$ctrlMedium)) "NULL" else input$ctrlMedium,
            get_ylim(safe_param())$ymax,
            get_ylim(safe_param())$ybreak
          )

        )
      } else if (input$tipo == "Apiladas"){
        meta <- add_row(
          meta,
          Campo = c("stackParams",
                    "orderStack",
                    "showErrBars",
                    "errbar_size",
                    "ymax", "ybreak"),
          Valor = c(
            paste(input$stackParams, collapse = ","),
            input$orderStack %||% "",
            as.character(input$showErrBars),
            input$errbar_size,
            input$ymax,
            input$ybreak
          )
        )
      } else if (input$tipo == "Curvas"){
        meta <- add_row(
          meta,
          Campo = c("xmax_cur","xbreak_cur",
                    "ymax_cur","ybreak_cur"),
          Valor = c(input$xmax_cur, input$xbreak_cur,
                    input$ymax_cur, input$ybreak_cur)
        )
        if (!is.null(curve_settings())){
          addWorksheet(wb, "CurvasSettings")
          writeData(wb, "CurvasSettings", curve_settings())
        }
      }

      writeData(wb, "Metadata", meta,
                headerStyle = createStyle(textDecoration = "bold"))
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )

  output$dl_combo_png <- downloadHandler(
    filename = function() "combo.png",
    content  = function(file){
      ggsave(
        file, combo_plot(),
        width  = input$combo_width  / 100,
        height = input$combo_height / 100,
        dpi    = 300, bg = "white"
      )
    }
  )

  output$dl_combo_pptx <- downloadHandler(
    filename = function() "combo.pptx",
    content  = function(file){
      library(officer); library(rvg)
      doc <- read_pptx()
      doc <- add_slide(doc, layout = "Title and Content",
                       master = "Office Theme") |>
        ph_with(dml(ggobj = combo_plot()),
                location = ph_location_fullsize())
      print(doc, target = file)
    }
  )
}
