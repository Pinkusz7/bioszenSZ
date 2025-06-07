#──────── helpers genéricos para objetos sin método broom::tidy() ────────  
matrix_to_tibble <- function(mat, colname = "p.adj") {  
  tibble::as_tibble(mat, rownames = "grupo1") |>  
    tidyr::pivot_longer(-grupo1,  
                        names_to  = "grupo2",  
                        values_to = colname) |>  
    dplyr::filter(!is.na(.data[[colname]]))  
}  

# ── helper: convierte matriz de p-values de PMCMRplus a tibble ──────────────  
pmcmr_to_tibble <- function(obj) {  
  mat <- obj$p.value                   # extrae la matriz interna de p-values  
  tibble::as_tibble(mat, rownames = "grupo1") |>  
    tidyr::pivot_longer(  
      -grupo1,  
      names_to  = "grupo2",  
      values_to = "p.adj"  
    ) |>  
    dplyr::filter(!is.na(p.adj))  
}  


#──────────────────────────────────────────────────────────────────────────  
# Helpers estadísticos ─ NUEVOS  
split_comparison <- function(x) {  
  stringr::str_split_fixed(x, "-", 2)  
}  

dunnett_to_tibble <- function(obj) {  
  # obj es la lista que devuelve DescTools::DunnettTest()  
  mat <- obj[[1]][ , 4, drop = FALSE]        # 4ª columna = p ajustado  
  cmp <- split_comparison(rownames(mat))  
  tibble(  
    grupo1 = cmp[, 1],  
    grupo2 = cmp[, 2],  
    p.adj  = mat[, 1]  
  )  
}  

set_control <- function(df, control_lbl) {  
  if (!is.null(control_lbl) && control_lbl %in% df$Label)  
    df$Label <- forcats::fct_relevel(df$Label, control_lbl)  
  df  
}  

# Si ya existían matrix_to_tibble() y pmcmr_to_tibble() NO los dupliques.  
#──────────────────────────────────────────────────────────────────────────  
safe_pairwise_t <- function(df, method = "sidak") {  
  res <- rstatix::pairwise_t_test(df, Valor ~ Label, p.adjust.method = method)  
  if (nrow(res) == 0) tibble() else res  
}  


# ── Nombre seguro para archivos (mantiene la extensión) ────────────  
safe_file <- function(x) {  
  # separa nombre y extensión  
  ext  <- tools::file_ext(x)           # "png"  
  name <- tools::file_path_sans_ext(x) # "NAD+_Boxplot"  
  
  # sanea solo el nombre (letras, números, _ y -)  
  name <- gsub("[^A-Za-z0-9_\\-]", "_", name)  
  
  paste0(name, ".", ext)               # vuelve a unir con .  
}  

# ── Nombre seguro para hojas de Excel ──────────────────────────────  
safe_sheet <- function(x) {  
  ## solo letras, números o "_" (lo demás → "_")  
  gsub("[^A-Za-z0-9_]", "_", x)  
}  
# ── Nombre seguro para etiquetas simples ────────────────────  
sanitize <- function(x) {  
  gsub("[/\\\\:*?\"<>|]", "_", x)  
}  

theme_light <- bs_theme(version = 5)
theme_dark  <- bs_theme(version = 5, bootswatch = "cyborg")
