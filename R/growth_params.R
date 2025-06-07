# 1. Fase exponencial - ROBUSTA
identify_exponential_phase_robust <- function(df, time_col, measure_col,
                                              umax_lower_bound = 0.05,
                                              umax_upper_bound = 0.25,
                                              max_iterations = 10,
                                              initial_r_squared_threshold = 0.95) {
  
  best_model <- NULL
  best_r2    <- -Inf
  best_start <- best_end <- NULL
  
  # limpia NAs y filtrado del 5-95 %
  df <- df[!is.na(df[[time_col]]) & !is.na(df[[measure_col]]), ]
  df <- dplyr::filter(df,
                      dplyr::between(df[[measure_col]],
                                     stats::quantile(df[[measure_col]], 0.05),
                                     stats::quantile(df[[measure_col]], 0.95)))
  
  min_pts            <- 10
  r2_threshold       <- initial_r_squared_threshold
  
  for (i in seq_len(max_iterations)) {
    for (start in seq_len(nrow(df) - min_pts)) {
      for (end in seq(start + min_pts, nrow(df))) {
        
        if ((end - start + 1) < min_pts) next
        
        model <- tryCatch(
          lm(log(df[[measure_col]][start:end]) ~
               df[[time_col]][start:end]),
          error = function(e) NULL)
        
        if (is.null(model)) next
        
        r2   <- summary(model)$r.squared
        umax <- coef(model)[2]
        
        if (!is.na(r2) &&
            umax > umax_lower_bound &&
            umax < umax_upper_bound &&
            r2   > r2_threshold &&
            r2   > best_r2) {
          
          best_r2    <- r2
          best_model <- model
          best_start <- start
          best_end   <- end
        }
      }
    }
    
    # heurística de ajuste de límites
    if (!is.null(best_model)) {
      umax <- coef(best_model)[2]
      if (umax < umax_lower_bound) {
        min_pts          <- max(min_pts - 1, 5)
        umax_lower_bound <- umax_lower_bound - 0.01
        r2_threshold     <- max(r2_threshold - 0.01, 0.90)
      } else if (umax > umax_upper_bound) {
        min_pts          <- min(min_pts + 1, nrow(df) - 5)
        umax_upper_bound <- umax_upper_bound + 0.01
        r2_threshold     <- min(r2_threshold + 0.01, 0.99)
      } else {
        break
      }
    }
  }
  
  list(start = best_start, end = best_end, model = best_model)
}

# 2. Fase exponencial - PERMISIVA
identify_exponential_phase_permissive <- function(df, time_col, measure_col,
                                                  umax_lower_bound = 0.01,
                                                  umax_upper_bound = 0.50,
                                                  max_iterations   = 10) {
  
  best_model <- NULL
  best_r2    <- -Inf
  best_start <- best_end <- NULL
  
  df <- df[!is.na(df[[time_col]]) & !is.na(df[[measure_col]]), ]
  df <- dplyr::filter(df,
                      dplyr::between(df[[measure_col]],
                                     stats::quantile(df[[measure_col]], 0.05),
                                     stats::quantile(df[[measure_col]], 0.95)))
  
  min_pts <- 10
  
  for (i in seq_len(max_iterations)) {
    for (start in seq_len(nrow(df) - min_pts)) {
      for (end in seq(start + min_pts, nrow(df))) {
        
        if ((end - start + 1) < min_pts) next
        
        model <- tryCatch(
          lm(log(df[[measure_col]][start:end]) ~
               df[[time_col]][start:end]),
          error = function(e) NULL)
        
        if (is.null(model)) next
        
        r2 <- summary(model)$r.squared
        if (!is.na(r2) && r2 > best_r2) {
          best_r2    <- r2
          best_model <- model
          best_start <- start
          best_end   <- end
        }
      }
    }
    break        # en permisiva basta la 1.ª iteración
  }
  
  list(start = best_start, end = best_end, model = best_model)
}

# 3. Calcular parámetros – ROBUSTO
calculate_growth_rates_robust <- function(df) {
  
  df %>%
    dplyr::group_by(Well) %>%
    dplyr::do({
      
      phase <- identify_exponential_phase_robust(.,
                                                 time_col = "Time",
                                                 measure_col = "Measurements")
      
      model <- phase$model
      start <- phase$start
      end   <- phase$end
      
      if (!is.null(model)) {
        
        lag_time <- .$Time[which.max(.$Time[1:start])]
        
        dplyr::tibble(
          µMax            = coef(model)[2],
          max_percap_time = mean(.$Time[start:end]),
          doub_time       = log(2) / coef(model)[2],
          lag_time        = lag_time,
          ODmax           = max(.$Measurements),
          max_time        = .$Time[which.max(.$Measurements)],
          AUC             = gcplyr::auc(x = .$Time, y = .$Measurements)
        )
      } else {
        dplyr::tibble(
          µMax            = NA_real_,
          max_percap_time = NA_real_,
          doub_time       = NA_real_,
          lag_time        = NA_real_,
          ODmax           = max(.$Measurements),
          max_time        = NA_real_,
          AUC             = NA_real_
        )
      }
    })
}

# 4. Calcular parámetros – PERMISIVO
calculate_growth_rates_permissive <- function(df) {
  
  df %>%
    dplyr::group_by(Well) %>%
    dplyr::do({
      
      phase <- identify_exponential_phase_permissive(.,
                                                     time_col = "Time",
                                                     measure_col = "Measurements")
      
      model <- phase$model
      start <- phase$start
      end   <- phase$end
      
      if (!is.null(model)) {
        
        lag_time <- .$Time[which.max(.$Time[1:start])]
        
        dplyr::tibble(
          Well           = .$Well[1],
          µMax            = coef(model)[2],
          max_percap_time = mean(.$Time[start:end]),
          doub_time       = log(2) / coef(model)[2],
          lag_time        = lag_time,
          ODmax           = max(.$Measurements),
          max_time        = .$Time[which.max(.$Measurements)],
          AUC             = gcplyr::auc(x = .$Time, y = .$Measurements)
        )
      } else {
        dplyr::tibble(
          Well           = .$Well[1],
          µMax            = NA_real_,
          max_percap_time = NA_real_,
          doub_time       = NA_real_,
          lag_time        = NA_real_,
          ODmax           = max(.$Measurements),
          max_time        = NA_real_,
          AUC             = NA_real_
        )
      }
    })
}

# 5. Detectar valores vacíos
is_empty_value <- function(x) {
  is.na(x) | x == "" | is.nan(x)
}
