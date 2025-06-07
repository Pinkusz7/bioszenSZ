# UI components extracted from the original app.R

## Themes ---------------------------------------------------------------
theme_light <- bs_theme(version = 5)
theme_dark  <- bs_theme(version = 5, bootswatch = "cyborg")

## Panel de composición -------------------------------------------------
tab_compos <- tabPanel(
  "Panel de Composición",
  sidebarLayout(
    sidebarPanel(
      uiOutput("plotPicker"),
      checkboxInput("show_legend_combo", "Mostrar leyenda", value = FALSE),
      hr(),
      numericInput("nrow_combo", "Filas", 1, min = 1, max = 4),
      numericInput("ncol_combo", "Columnas", 1, min = 1, max = 4),
      numericInput("combo_width",  "Ancho px", 1920, min = 400),
      numericInput("combo_height", "Alto  px", 1080, min = 400),
      numericInput("base_size_combo", "Tamaño base (pts)", 18, min = 8),
      numericInput("fs_title_all",      "Tamaño título (pts)",     20, min = 6),
      numericInput("fs_axis_title_all", "Títulos de ejes",         16, min = 6),
      numericInput("fs_axis_text_all",  "Ticks de ejes",           14, min = 6),
      numericInput("fs_legend_all",     "Texto de leyenda",        16, min = 6),
      conditionalPanel(
        condition = "input.plot_edit != null && input.plot_edit != ''",
        h4("Ajustes Boxplot / Barras"),
        numericInput("ov_box_w",  "Ancho de caja:",     NA, min = 0.1, max = 1.5, step = 0.05),
        numericInput("ov_pt_size","Tamaño de puntos:",  NA, min = 0.5, max = 6,  step = 0.5),
        numericInput("ov_pt_jit", "Dispersión puntos:", NA, min = 0,   max = 0.5, step = 0.01)
      ),
      actionButton("makeCombo", "Actualiza / Previsualiza",
                   class = "btn btn-primary"),
      br(), br(),
      downloadButton("dl_combo_png",  "Descargar PNG"),
      downloadButton("dl_combo_pptx", "Descargar PPTX")
    ),
    mainPanel(
      plotOutput("comboPreview", height = "auto")
    )
  )
)

## UI principal ---------------------------------------------------------
ui <- fluidPage(
  theme = theme_light,
  useShinyjs(),

  # Estado de tema guardado en localStorage
  tags$script(HTML(
    "$(function () {\n      var m = localStorage.getItem('appMode') || 'light';\n      Shiny.setInputValue('mode', m, {priority: 'event'});\n    });\n    Shiny.addCustomMessageHandler('saveMode', function (m) {\n      localStorage.setItem('appMode', m);\n    });"
  )),

  # Botones de idioma y modo claro/oscuro
  absolutePanel(
    top  = 10, right = 10, draggable = FALSE,
    style = 'z-index:1000; display:flex; gap:8px;',
    tags$div(
      class = "dropdown",
      actionButton(
        "lang_btn", "Idioma",
        class = "btn btn-secondary dropdown-toggle",
        `data-bs-toggle` = "dropdown", `aria-expanded` = "false"
      ),
      tags$ul(
        class = "dropdown-menu dropdown-menu-end",
        tags$li(actionLink("lang_es", "Español", class = "dropdown-item")),
        tags$li(actionLink("lang_en", "English", class = "dropdown-item"))
      )
    ),
    actionButton('btn_light', label = 'Tema Claro',
                 class = 'btn btn-light', style = 'color:#000;'),
    actionButton('btn_dark',  label = 'Tema Oscuro',
                 class = 'btn btn-dark',  style = 'color:#fff;')
  ),

  # Google Analytics y Traductor (idénticos al app original)
  tags$head(HTML(
    "<!-- Global site tag (gtag.js) - Google Analytics -->\n    <script async src='https://www.googletagmanager.com/gtag/js?id=G-Q5FYW8FV3Z'></script>\n    <script>\n      window.dataLayer = window.dataLayer || [];\n      function gtag(){dataLayer.push(arguments);}\n      gtag('js', new Date());\n      gtag('config', 'G-Q5FYW8FV3Z');\n    </script>"
  )),
  tags$head(HTML('
  <!-- Traductor de Google -->
  <style>#google_translate_element {display:none;}</style>
  <div id="google_translate_element"></div>

  <script type="text/javascript">
    function googleTranslateElementInit(){
      new google.translate.TranslateElement({pageLanguage:"es", includedLanguages:"es,en", autoDisplay:false}, "google_translate_element");
    }

    function setLang(lang){
      var sel=document.querySelector("#google_translate_element select");
      if(!sel) return;
      for(var i=0;i<sel.options.length;i++){
        if(sel.options[i].value.indexOf(lang)>-1){
          sel.selectedIndex=i;
          sel.dispatchEvent(new Event("change"));
          break;
        }
      }
      localStorage.setItem("appLang",lang);
      var link = document.getElementById("manual_link");
      if(link){
        link.setAttribute("href", lang==="en" ? "MANUAL_EN.docx" : "MANUAL_ES.docx");
      }
    }

    document.addEventListener("DOMContentLoaded",function(){
      var lg=localStorage.getItem("appLang")||"es";
      setTimeout(function(){ setLang(lg); },500);
    });
  </script>

  <script src="//translate.google.com/translate_a/element.js?cb=googleTranslateElementInit"></script>
  ')),

  # Estilos para acordeones y tablas
  tags$head(tags$style(HTML("#statsTabs .checkbox { margin-top: 6px; }"))),
  tags$head(tags$style(HTML(
    "#statsPanel .accordion-header .accordion-button{font-size: 18px;font-weight: 700;}"
  ))),
  tags$head(tags$style(HTML(
    "#statsPanel .accordion-header .accordion-button,\n     #repsPanel  .accordion-header .accordion-button,\n     #repsGrpPanel .accordion-header .accordion-button {font-size: 18px;font-weight: 700;}"
  ))),

  # Handler de descarga plotly
  tags$script(HTML(
    "Shiny.addCustomMessageHandler('downloadPlotlyImage', function(msg){\n      var gd = document.getElementById('plotInteractivo');\n      Plotly.downloadImage(gd, {format: 'png', filename: msg.filename, width: msg.width, height: msg.height, scale: 1});\n    });"
  )),

  titlePanel(tags$div(
    style = 'display:flex; align-items:center; gap:10px;',
    uiOutput('logo_img'),
    tags$span('Centro de Gráficos y Análisis',
              style = 'font-size:30px; font-weight:bold;')
  )),

  tabsetPanel(id = "mainTabs", type = "tabs",
    tabPanel("Gráficos & Stats", value = "graficos"),
    tabPanel("Obtención de Parámetros de Crecimiento")
  )
)
