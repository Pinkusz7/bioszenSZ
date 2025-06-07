# Functions for statistical tests extracted from the original app.R

# Data frame used for either normality or significance tests
make_test_df <- function() {
  p   <- safe_param()  # may contain *_Norm
  src <- if (isTRUE(input$doNorm)) datos_agrupados_norm() else datos_agrupados()

  if (input$scope == "Por Cepa") {
    src %>%
      filter(Strain == input$strain) %>%
      order_filter_strain() %>%
      filter_reps_strain() %>%
      transmute(Label = Media,
                Valor = .data[[p]])
  } else {
    src %>%
      order_filter_group() %>%
      transmute(Label,
                Valor = .data[[p]])
  }
}

# Normality results for the selected data frame
norm_res <- function() {
  df <- make_test_df()
  if (nrow(df) == 0 || dplyr::n_distinct(df$Label) < 2)
    return(tibble::tibble())

  sw <- df %>% group_by(Label) %>%
    summarise(
      shapiro.stat = stats::shapiro.test(Valor)$statistic,
      shapiro.p    = stats::shapiro.test(Valor)$p.value,
      .groups = "drop"
    )
  res <- sw

  if ("ks" %in% input$normTests) {
    ksdf <- df %>% group_by(Label) %>%
      summarise(
        ks.stat = stats::ks.test(Valor, "pnorm",
                                 mean(Valor), sd(Valor))$statistic,
        ks.p    = stats::ks.test(Valor, "pnorm",
                                 mean(Valor), sd(Valor))$p.value,
        .groups = "drop"
      )
    res <- left_join(res, ksdf, by = "Label")
  } else {
    res <- mutate(res, ks.stat = NA_real_, ks.p = NA_real_)
  }

  if ("ad" %in% input$normTests) {
    addf <- df %>%
      dplyr::group_by(Label) %>%
      dplyr::summarise(
        ad.stat = if (dplyr::n() >= 8) nortest::ad.test(Valor)$statistic else NA_real_,
        ad.p    = if (dplyr::n() >= 8) nortest::ad.test(Valor)$p.value     else NA_real_,
        .groups = "drop"
      )
    res <- dplyr::left_join(res, addf, by = "Label")
  } else {
    res <- dplyr::mutate(res, ad.stat = NA_real_, ad.p = NA_real_)
  }
  res
}

# Significance tests according to selected options
sig_res <- function() {
  df <- make_test_df()
  if (dplyr::n_distinct(df$Label) < 2)
    return(tibble())

  do_anova <- function() {
    aovm <- aov(Valor ~ Label, data = df)
    switch(input$postHoc,
           "Tukey"      = broom::tidy(TukeyHSD(aovm)) |> rename(p.adj = adj.p.value),
           "Bonferroni" = rstatix::pairwise_t_test(df, Valor ~ Label, p.adjust.method = "bonferroni"),
           "Sidak"      = safe_pairwise_t(df, "sidak"),
           "Dunnett"    = dunnett_to_tibble(
             DescTools::DunnettTest(Valor ~ Label,
                                    data = set_control(df, input$controlGroup))),
           "Scheffe"    = pmcmr_to_tibble(PMCMRplus::scheffeTest(aovm, "Label")),
           "GamesHowell"= rstatix::games_howell_test(df, Valor ~ Label)
    )
  }

  do_kw <- function() {
    switch(input$postHoc,
           "Dunn"    = rstatix::dunn_test(df, Valor ~ Label, p.adjust.method = "bonferroni"),
           "Conover" = pmcmr_to_tibble(PMCMRplus::kwAllPairsConoverTest(df$Valor, df$Label)),
           "Nemenyi" = pmcmr_to_tibble(PMCMRplus::kwAllPairsNemenyiTest(df$Valor, df$Label)),
           "DSCF"    = {
             f <- if (exists("kwAllPairsDSCFTest", asNamespace("PMCMRplus"), FALSE))
               PMCMRplus::kwAllPairsDSCFTest else PMCMRplus::kwAllPairsDscfTest
             pmcmr_to_tibble(f(df$Valor, df$Label))
           }
    )
  }

  res <- switch(input$sigTest,
                "ANOVA"          = do_anova(),
                "Kruskal–Wallis" = do_kw(),
                "ttest" = {
                  if (input$compMode == "all") {
                    rstatix::pairwise_t_test(df, Valor ~ Label, p.adjust.method = "holm")
                  } else if (input$compMode == "control") {
                    rstatix::t_test(df, Valor ~ Label, ref.group = input$controlGroup)
                  } else {
                    sub <- df %>% filter(Label %in% c(input$group1, input$group2))
                    rstatix::t_test(sub, Valor ~ Label, paired = TRUE)
                  }
                },
                "wilcox" = {
                  if (input$compMode == "all") {
                    rstatix::pairwise_wilcox_test(df, Valor ~ Label, p.adjust.method = "holm")
                  } else if (input$compMode == "control") {
                    rstatix::wilcox_test(df, Valor ~ Label, ref.group = input$controlGroup)
                  } else {
                    sub <- df %>% filter(Label %in% c(input$group1, input$group2))
                    rstatix::wilcox_test(sub, Valor ~ Label, paired = TRUE)
                  }
                }
  )
  res
}
