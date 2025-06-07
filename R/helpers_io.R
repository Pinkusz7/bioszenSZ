# Helper robusto: lee una hoja Excel aunque el nombre temporal NO tenga extensión  
read_excel_tmp <- function(path, sheet = NULL) {  
  sig <- readBin(path, "raw", n = 8)  
  
  # 0xD0CF11E0 = formato OLE (xls 97-2003)  
  is_xls  <- identical(sig[1:4], as.raw(c(0xD0,0xCF,0x11,0xE0)))  
  # 0x504B0304 = inicio de archivo ZIP (xlsx, xlsm, ods…)  
  is_zip  <- identical(sig[1:4], as.raw(c(0x50,0x4B,0x03,0x04)))  
  
  if (is_xls)       return(readxl::read_xls (path, sheet = sheet))  
  if (is_zip)       return(readxl::read_xlsx(path, sheet = sheet))  
  
  stop("El archivo subido no es un Excel válido (.xls o .xlsx)")  
}  
# ──────────────────────────────────────────────────────────────────────────────  

# ── Función para generar el Excel de resumen por parámetro ────────────  
generate_summary_wb <- function(datos, params) {  
  wb <- createWorkbook()  
  for (param in params) {  
    sheet <- safe_sheet(param)  
    addWorksheet(wb, sheet)  
    
    # 1) Detalle técnico  
    det <- datos %>%  
      dplyr::filter(!is.na(Strain), !is.na(Media)) %>%  
      dplyr::select(  
        Strain, Media, Orden,  
        BiologicalReplicate, TechnicalReplicate,  
        Valor = !!rlang::sym(param)  
      ) %>%  
      dplyr::arrange(Strain, BiologicalReplicate, TechnicalReplicate)  
    
    # 2) Orden de medios según 'Orden'  
    medias_order <- det %>%  
      dplyr::distinct(Media, Orden) %>%  
      dplyr::arrange(Orden) %>%  
      dplyr::pull(Media)  
    
    # 3) Escribir por cepa  
    fila <- 1  
    for (s in unique(det$Strain)) {  
      writeData(wb, sheet, paste("Strain:", s),  
                startRow = fila, startCol = 1)  
      fila <- fila + 1  
      
      tab_cepa <- det %>%  
        dplyr::filter(Strain == s) %>%  
        dplyr::select(BiologicalReplicate, TechnicalReplicate, Media, Valor) %>%  
        tidyr::pivot_wider(  
          id_cols    = c(BiologicalReplicate, TechnicalReplicate),  
          names_from = Media,  
          values_from = Valor,  
          values_fill = NA  
        ) %>%  
        dplyr::select(BiologicalReplicate, dplyr::all_of(medias_order)) %>%  
        dplyr::rename(RepBiol = BiologicalReplicate)  
      
      writeData(wb, sheet, tab_cepa,  
                startRow    = fila,  
                startCol    = 1,  
                headerStyle = createStyle(textDecoration = "bold"))  
      fila <- fila + nrow(tab_cepa) + 2  
    }  
    
    # 4) Resumen por réplica biológica  
    writeData(wb, sheet, "Resumen por réplica biológica",  
              startRow    = fila,  
              startCol    = 1,  
              headerStyle = createStyle(fontSize = 12, textDecoration = "bold"))  
    fila <- fila + 1  
    
    resumen <- det %>%  
      dplyr::group_by(Strain, BiologicalReplicate, Media) %>%  
      dplyr::summarise(Promedio = mean(Valor, na.rm = TRUE), .groups = "drop") %>%  
      tidyr::pivot_wider(  
        id_cols    = c(Strain, BiologicalReplicate),  
        names_from = Media,  
        values_from = Promedio,  
        values_fill = NA  
      ) %>%  
      dplyr::arrange(Strain, BiologicalReplicate) %>%  
      dplyr::rename(RepBiol = BiologicalReplicate)  
    
    writeData(wb, sheet, resumen,  
              startRow    = fila,  
              startCol    = 1,  
              headerStyle = createStyle(textDecoration = "bold"))  
  }  
  wb  
}  

  export_plotly_png <- function(p, file,
                                width, height,
                                delay = 0.5,   # deja que Plotly acabe de renderizar
                                zoom  = 3) {    # 3 × ⇒ 300 dpi aprox. si usas 100 px = 1 in
    # Fondo transparente
    p <- p %>% layout(
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor  = "rgba(0,0,0,0)"
    )
    
    tmp_html <- tempfile(fileext = ".html")
    on.exit(unlink(tmp_html), add = TRUE)
    
    htmlwidgets::saveWidget(p, tmp_html, selfcontained = TRUE)
    
    webshot2::webshot(
      url      = tmp_html,
      file     = file,
      vwidth   = width,
      vheight  = height,
      delay    = delay,
      zoom     = zoom          # ↑ resolución final  = zoom × vwidth
    )
  }
  
  
