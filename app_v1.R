#=============================================================
# La presente aplicación es una visualización aproximada de la
# situación de rezago habitacional existente en México hasta
# el año 2020.
# La información fue tratada con el script:
# ENVI_2020_Consultas_v12102016.1.3
#=============================================================

# ============================================================
# DASHBOARD ENVI 2020 
# CARGA DE DATOS, VALIDACIONES, COLORES E INTERFAZ
# ============================================================

# Shiny se carga para las funciones de interfaz y servidor usadas sin prefijo.
library(shiny)

# ------------------------------------------------------------
# 1. UBICACIÓN Y CARGA DE DATOS
# ------------------------------------------------------------

archivo_rds <- file.path("datos", "dashboard_envi.rds")
dashboard <- readRDS(archivo_rds)

componentes <- c(
  "nacional", "entidad", "entidad_tloc",
  "hacinamiento", "materiales", "rezago",
  "demanda", "rezago_entidad"
)

if (!all(componentes %in% names(dashboard))) {
  stop(
    "El archivo RDS no contiene todos los componentes requeridos: ",
    paste(setdiff(componentes, names(dashboard)), collapse = ", ")
  )
}

# ------------------------------------------------------------
# 2. VALIDACIÓN DE COLUMNAS
# ------------------------------------------------------------

columnas_requeridas <- list(
  nacional = c(
    "nivel_intervencion", "viviendas_muestra",
    "viviendas_estimadas", "personas_en_viviendas_estimadas",
    "hogares_estimados", "porcentaje_viviendas"
  ),
  entidad = c(
    "Entidad", "nivel_intervencion", "viviendas_muestra",
    "viviendas_estimadas", "personas_estimadas",
    "porcentaje_viviendas"
  ),
  entidad_tloc = c(
    "Entidad", "Tam_Loc", "etiqueta_tloc", "ambito",
    "ambito_general", "nivel_intervencion",
    "viviendas_muestra", "viviendas_estimadas",
    "personas_estimadas", "porcentaje_viviendas"
  ),
  hacinamiento = c(
    "ENT", "Entidad_nombre", "TLOC_codigo",
    "etiqueta_tloc", "viviendas_muestra",
    "viviendas_clasificables", "viviendas_expand",
    "porcentaje_hacinamiento"
  ),
  materiales = c(
    "clasif_materiales", "viviendas_observadas",
    "viviendas_expand", "porcentaje"
  ),
  rezago = c(
    "ENT", "Entidad_nombre", "viviendas_muestra",
    "viviendas_clasificables", "viviendas_expandibles",
    "viviendas_expand", "viviendas_rezago_expand",
    "viviendas_sin_rezago_expand", "denominador_rezago",
    "porcentaje_rezago", "porcentaje_sin_rezago"
  ),
  demanda = c(
    "nivel_intervencion", "tiene_registro_demanda",
    "viviendas_muestra", "viviendas_estimadas",
    "personas_estimadas", "porcentaje_dentro_nivel",
    "registro_demanda"
  ),
  rezago_entidad = c(
    "Entidad", "estado_rezago", "viviendas_muestra",
    "viviendas_estimadas", "personas_estimadas",
    "porcentaje_viviendas"
  )
)

for (nm in names(columnas_requeridas)) {
  faltantes <- setdiff(
    columnas_requeridas[[nm]],
    names(dashboard[[nm]])
  )
  
  if (length(faltantes) > 0) {
    stop(
      "Faltan columnas en dashboard$", nm, ": ",
      paste(faltantes, collapse = ", ")
    )
  }
}

# ------------------------------------------------------------
# 3. NORMALIZACIÓN DE CÓDIGOS
# ------------------------------------------------------------

normalizar_codigo <- function(x) {
  x <- trimws(as.character(x))
  x <- sub("\\.0$", "", x)
  
  ifelse(
    is.na(x) | x == "",
    NA_character_,
    ifelse(
      grepl("^[0-9]+$", x),
      paste0(strrep("0", pmax(0, 2 - nchar(x))), x),
      x
    )
  )
}

dashboard$entidad$Entidad <-
  normalizar_codigo(dashboard$entidad$Entidad)

dashboard$entidad_tloc$Entidad <-
  normalizar_codigo(dashboard$entidad_tloc$Entidad)

dashboard$rezago$ENT <-
  normalizar_codigo(dashboard$rezago$ENT)

dashboard$hacinamiento$ENT <-
  normalizar_codigo(dashboard$hacinamiento$ENT)

# ------------------------------------------------------------
# 4. CATÁLOGOS
# ------------------------------------------------------------

catalogo_entidades <- dashboard$rezago |>
  dplyr::transmute(
    Entidad = normalizar_codigo(ENT),
    Entidad_nombre = as.character(Entidad_nombre)
  ) |>
  dplyr::filter(!is.na(Entidad)) |>
  dplyr::distinct(Entidad, .keep_all = TRUE) |>
  dplyr::arrange(Entidad_nombre)

opciones_tloc <- sort(unique(
  na.omit(as.character(dashboard$entidad_tloc$ambito))
))


# ------------------------------------------------------------
# 5. COLORES
# ------------------------------------------------------------

colores_necesidad <- c(
  "Sustitución" = "#C0392B",
  "Mejoramiento" = "#F39C12",
  "Ampliación o reemplazo" = "#2878B5",
  "Sin nivel de intervención clasificable" = "#238B57",
  "Sin rezago habitacional" = "#238B57"
)

colores_hacinamiento <- c(
  "Sin hacinamiento" = "#238B57",
  "Con hacinamiento" = "#C0392B"
)

#colores_ingresos <- c(
#  "Con al menos un hogar de bajos ingresos" = "#C0392B",
#  "Sin hogares clasificados con bajos ingresos" = "#238B57"
#)

# ------------------------------------------------------------
# 6. INTERFAZ SHINY
# ------------------------------------------------------------

ui <- bslib::page_sidebar(
  title = shiny::div(
    shiny::span("ENVI 2020", style = "font-weight:800;"),
    shiny::span(
      " | Análisis de rezago habitacional, población potencial y nivel de intervención",
      style = "font-weight:400;font-size:15px;"
    )
  ),
  
  theme = bslib::bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#16324F",
    secondary = "#64748B",
    success = "#188568",
    warning = "#E9A23B",
    danger = "#B91C1C"
  ),
  
  fillable = TRUE,
  
  sidebar = bslib::sidebar(
    width = 290,
    open = "always",
    
    shiny::h5("Filtros de análisis"),
    shiny::p(
      "Los filtros se aplican a las vistas que contienen esas variables. Algunas tablas nacionales no tienen desglose por entidad.",
      class = "text-muted small"
    ),
    
    shiny::selectInput(
      "filtro_entidad",
      "Entidad federativa",
      choices = c(
        "Todas las entidades" = "TODAS",
        stats::setNames(
          catalogo_entidades$Entidad,
          catalogo_entidades$Entidad_nombre
        )
      ),
      selected = "TODAS"
    ),
    
    shiny::selectInput(
      "filtro_intervencion",
      "Nivel de intervención",
      choices = c(
        "Todos los niveles" = "TODOS",
        "Sustitución" = "Sustitución",
        "Mejoramiento" = "Mejoramiento",
        "Ampliación o reemplazo" = "Ampliación o reemplazo",
        "Sin nivel de intervención clasificable" =
          "Sin nivel de intervención clasificable",
        "Sin rezago habitacional" =
          "Sin rezago habitacional"
      ),
      selected = "TODOS"
    ),
    
    shiny::selectInput(
      "filtro_tloc",
      "Tipo de localidad",
      choices = c(
        "Todos los tamaños" = "TODOS",
        stats::setNames(opciones_tloc, opciones_tloc)
      ),
      selected = "TODOS"
    ),
    
    shiny::hr(),
    
    shiny::actionButton(
      "restablecer",
      "Restablecer filtros",
      icon = shiny::icon("rotate-left"),
      class = "btn-outline-primary w-100"
    ),
    
    shiny::br(), shiny::br(),
    
    shiny::downloadButton(
      "descargar_datos",
      "Descargar tabla actual",
      class = "btn-primary w-100"
    ),
    
    shiny::hr(),
    
    shiny::p(
      "Fuente: ENVI 2020, INEGI",
      class = "small text-muted"
    ),
    
    shiny::p(
      paste(
        "Las cifras expandidas son estimaciones",
        "al número total de viviendas contenidas",
        "en la Encuesta Nacional de Vivienda 2020, INEGI."
      ),
      class = "small text-muted"
    )
  ),
  
  shiny::tags$head(
    shiny::tags$style(shiny::HTML("
      body { background-color: #F4F7FB; }
      .bslib-page-sidebar > .main { padding: 20px; }
      .card {
        border: 1px solid #E2E8F0;
        border-radius: 12px;
        box-shadow: 0 2px 8px rgba(15,23,42,.04);
      }
      .card-header {
        background: #FFFFFF;
        font-weight: 700;
        color: #16324F;
      }
      .kpi-label {
        color: #64748B;
        font-size: 13px;
        font-weight: 600;
      }
      .kpi-value {
        color: #16324F;
        font-size: 25px;
        font-weight: 800;
        line-height: 1.3;
      }
      .kpi-note { color: #64748B; font-size: 12px; }
      .section-title {
        color: #16324F;
        font-weight: 800;
        margin: 5px 0 15px 0;
      }
      .nav-link { font-weight: 600; }
    "))
  ),
  
  bslib::navset_tab(
    id = "pestana",
    
    # --------------------------------------------------------
    # PESTAÑA 1. RESUMEN NACIONAL
    # --------------------------------------------------------
    
    bslib::nav_panel(
      "Resumen nacional",
      shiny::br(),
      shiny::h3("Panorama nacional", class = "section-title"),
      shiny::p(
        "Indicadores de viviendas, hogares y población estimada.",
        class = "text-muted"
      ),
      
      bslib::layout_columns(
        col_widths = c(3, 3, 3, 3),
        
        bslib::card(bslib::card_body(
          shiny::div(class = "kpi-label", "Viviendas estimadas"),
          shiny::div(class = "kpi-value", shiny::textOutput("kpi_viviendas")),
          shiny::div(class = "kpi-note", "Estimación expandida")
        )),
        
        bslib::card(bslib::card_body(
          shiny::div(class = "kpi-label", "Personas estimadas"),
          shiny::div(class = "kpi-value", shiny::textOutput("kpi_personas")),
          shiny::div(class = "kpi-note", "Personas en viviendas")
        )),
        
        bslib::card(bslib::card_body(
          shiny::div(class = "kpi-label", "Hogares estimados"),
          shiny::div(class = "kpi-value", shiny::textOutput("kpi_hogares")),
          shiny::div(class = "kpi-note", "Estimación nacional")
        )),
        
        bslib::card(bslib::card_body(
          shiny::div(class = "kpi-label", "Rezago habitacional"),
          shiny::div(class = "kpi-value", shiny::textOutput("kpi_rezago_habitacional")),
          shiny::div(class = "kpi-note", "Porcentaje estimado")
        ))
      ),
      
      bslib::layout_columns(
        col_widths = c(5, 7),
        
        bslib::card(
          bslib::card_header("Rezago por tipo"),
          plotly::plotlyOutput("grafica_rezago_ent", height = "350px")
        ),
        
        bslib::card(
          bslib::card_header("Grado de intervención necesario"),
          plotly::plotlyOutput("grafica_nacional", height = "350px")
        )
      ),
      
      bslib::card(
        bslib::card_header("Clasificación nacional"),
        DT::DTOutput("tabla_nacional")
      )
    ),
    
    # --------------------------------------------------------
    # PESTAÑA 2. REZAGO HABITACIONAL
    # --------------------------------------------------------
    
    bslib::nav_panel(
      "Rezago habitacional",
      shiny::br(),
      shiny::h3("Rezago habitacional por entidad", class = "section-title"),
      
      bslib::layout_columns(
        col_widths = c(7, 5),
        
        bslib::card(
          bslib::card_header("Porcentaje de rezago por entidad"),
          plotly::plotlyOutput("grafica_rezago", height = "500px")
        ),
        
        bslib::card(
          bslib::card_header("Resumen de indicadores"),
          shiny::uiOutput("resumen_rezago")
        )
      ),
      
      bslib::card(
        bslib::card_header("Tabla de rezago habitacional"),
        DT::DTOutput("tabla_rezago")
      )
    ),
    
    # --------------------------------------------------------
    # PESTAÑA 3. REZAGO POR LOCALIDAD
    # --------------------------------------------------------
    
    bslib::nav_panel(
      "Ámbito de localidad",
      shiny::br(),
      shiny::h3("Tipo de intervención", class = "section-title"),
      
      bslib::card(
        bslib::card_header("Distribución por tamaño de localidad"),
        plotly::plotlyOutput("grafica_tloc", height = "430px")
      ),
      
      bslib::card(
        bslib::card_header("Detalle por entidad, tamaño y necesidad"),
        DT::DTOutput("tabla_tloc")
      )
    ),
    
    # --------------------------------------------------------
    # PESTAÑA 4. HACINAMIENTO Y MATERIALES
    # --------------------------------------------------------
    
    bslib::nav_panel(
      "Hacinamiento y materiales",
      shiny::br(),
      shiny::h3("Caracterización de la vivienda", class = "section-title"),
      
      bslib::layout_columns(
        col_widths = c(6, 6),
        
        bslib::card(
          bslib::card_header("Clasificación de hacinamiento"),
          plotly::plotlyOutput(
            "grafica_hacinamiento",
            height = "370px"
          ),
          DT::DTOutput("tabla_hacinamiento")
        ),
        
        bslib::card(
          bslib::card_header("Clasificación de materiales"),
          plotly::plotlyOutput("grafica_materiales", height = "370px"),
          DT::DTOutput("tabla_materiales")
        )
      )
    ),
    
    # --------------------------------------------------------
    # PESTAÑA 5. NECESIDAD EXPLÍCITA DE VIVIENDA
    # --------------------------------------------------------
    
    bslib::nav_panel(
      "Necesidad explícita de vivienda",
      
      shiny::br(),
      
      shiny::h3(
        "Registro de demanda y niveles de intervención",
        class = "section-title"
      ),
      
      shiny::p(
        "Distribución de las viviendas y personas estimadas según ",
        "la existencia de registro en TDEMANDA."
      ),
      
      bslib::layout_columns(
        col_widths = c(6, 6),
        
        bslib::card(
          bslib::card_header(
            "Distribución porcentual con y sin registro"
          ),
          plotly::plotlyOutput(
            "grafica_demanda_porcentaje",
            height = "430px"
          )
        ),
        
        bslib::card(
          bslib::card_header(
            "Personas estimadas con y sin registro"
          ),
          plotly::plotlyOutput(
            "grafica_demanda_personas",
            height = "430px"
          )
        )
      ),
      
      shiny::br(),
      
      bslib::layout_columns(
        col_widths = c(6, 6),
        
        bslib::card(
          bslib::card_header(
            "Resumen por nivel de intervención"
          ),
          DT::DTOutput("tabla_demanda_resumen")
        ),
        
        bslib::card(
          bslib::card_header(
            "Detalle de viviendas y personas estimadas"
          ),
          DT::DTOutput("tabla_demanda_detalle")
        )
      )
    ),
    
    # --------------------------------------------------------
    # PESTAÑA 6. DISTRIBUCIÓN POR ENTIDAD
    # --------------------------------------------------------
    bslib::nav_panel(
      "Distribución por entidad",
      shiny::br(),
      shiny::h3("Distribución por entidad y nivel de intervención",
                class = "section-title"),
      shiny::p(
        "El gráfico Sunburst muestra el total seleccionado, su distribución por entidad y el nivel de intervención.",
        class = "text-muted"
      ),
      bslib::card(
        bslib::card_header("Indicador de visualización"),
        shiny::selectInput(
          "metrica_sunburst",
          "Selecciona el indicador",
          choices = c(
            "Viviendas de la muestra" = "viviendas_muestra",
            "Viviendas estimadas" = "viviendas_estimadas",
            "Personas estimadas" = "personas_estimadas",
            "Porcentaje de viviendas" = "porcentaje_viviendas"
          ),
          selected = "viviendas_estimadas"
        ),
        plotly::plotlyOutput("grafica_sunburst", height = "650px")
      ),
      bslib::card(
        bslib::card_header("Datos por entidad y nivel de intervención"),
        DT::DTOutput("tabla_entidades")
      )
    )
  )
)
# ============================================================
# FUNCIONES AUXILIARES, SERVIDOR Y FILTROS
# ============================================================

# ------------------------------------------------------------
# 1. FUNCIONES AUXILIARES
# ------------------------------------------------------------

fmt_num <- function(x, digits = 0) {
  if (length(x) == 0 || all(is.na(x))) {
    return("Sin datos")
  }
  
  format(
    round(x[1], digits),
    big.mark = ",",
    decimal.mark = ".",
    nsmall = digits,
    scientific = FALSE,
    trim = TRUE
  )
}

fmt_moneda <- function(x, digits = 2) {
  if (
    length(x) == 0 ||
    all(is.na(x)) ||
    !is.finite(x[1])
  ) {
    return("Sin datos")
  }
  
  paste0("$", fmt_num(x, digits))
}

tabla_dt <- function(df, nombres = NULL, pagina = 15) {
  etiquetas <- c(
    "ENT" = "Código de entidad", "Entidad" = "Entidad",
    "Entidad_nombre" = "Entidad", "Ent" = "Entidad",
    "Tam_Loc" = "Tamaño de localidad", "TLOC_codigo" = "Código de localidad",
    "nivel_intervencion" = "Nivel de intervención",
    "viviendas_muestra" = "Viviendas en muestra",
    "viviendas_estimadas" = "Viviendas estimadas",
    "personas_estimadas" = "Personas estimadas",
    "personas_en_viviendas_estimadas" = "Personas estimadas",
    "hogares_estimados" = "Hogares estimados",
    "porcentaje_viviendas" = "Porcentaje de viviendas",
    "viviendas_expand" = "Viviendas expandidas",
    "viviendas_expandibles" = "Viviendas expandibles",
    "viviendas_clasificables" = "Viviendas clasificables",
    "viviendas_rezago_expand" = "Viviendas con rezago",
    "viviendas_sin_rezago_expand" = "Viviendas sin rezago",
    "denominador_rezago" = "Denominador del rezago",
    "porcentaje_rezago" = "Porcentaje de rezago",
    "porcentaje_sin_rezago" = "Porcentaje sin rezago",
    "porcentaje_hacinamiento" = "Porcentaje de hacinamiento",
    "viviendas_observadas" = "Viviendas observadas",
    "clasif_materiales" = "Clasificación de materiales",
    "etiqueta_tloc" = "Tamaño de localidad",
    "ambito" = "Tipo de localidad", "ambito_general" = "Ámbito general",
    "estado_rezago" = "Condición de rezago",
    "tiene_registro_demanda" = "Tiene registro de demanda",
    "registro_demanda" = "Registro de demanda",
    "porcentaje_dentro_nivel" = "Porcentaje dentro del nivel"
  )
  encabezados <- names(df)
  idx <- encabezados %in% names(etiquetas)
  encabezados[idx] <- unname(etiquetas[encabezados[idx]])
  if (!is.null(nombres) && length(nombres) == ncol(df)) encabezados <- nombres
  
  DT::datatable(
    df, rownames = FALSE, colnames = encabezados, filter = "top",
    extensions = "Buttons",
    options = list(
      pageLength = pagina, scrollX = TRUE, autoWidth = TRUE,
      dom = "Bfrtip", buttons = c("copy", "csv", "excel")
    )
  )
}

# ------------------------------------------------------------
# 1.1 EXCLUIR CATEGORÍAS NO CLASIFICABLES
# ------------------------------------------------------------

limpiar_necesidades <- function(df) {
  if (!"nivel_intervencion" %in% names(df)) {
    return(df)
  }
  
  df |>
    dplyr::filter(
      !is.na(nivel_intervencion),
      trimws(as.character(nivel_intervencion)) != "",
      !tolower(trimws(as.character(nivel_intervencion))) %in%
        c(
          "no clasificable",
          "no clasificables",
          "no clasificado",
          "no clasificada",
          "no clasificadas",
          "sin clasificar"
        )
    )
}

# ------------------------------------------------------------
# 2. SERVIDOR
# ------------------------------------------------------------

server <- function(input, output, session) {
  
  # ----------------------------------------------------------
  # 2.1 RESTABLECER FILTROS
  # ----------------------------------------------------------
  
  observeEvent(input$restablecer, {
    updateSelectInput(
      session,
      "filtro_entidad",
      selected = "TODAS"
    )
    
    updateSelectInput(
      session,
      "filtro_intervencion",
      selected = "TODOS"
    )
    
    updateSelectInput(
      session,
      "filtro_tloc",
      selected = "TODOS"
    )
  })
  
  # ----------------------------------------------------------
  # 2.2 DATOS FILTRADOS POR ENTIDAD
  # ----------------------------------------------------------
  
  datos_entidad <- reactive({
    df <- dashboard$entidad |>
      dplyr::mutate(
        Entidad = as.character(Entidad)
      ) |>
      limpiar_necesidades()
    
    if (
      !is.null(input$filtro_entidad) &&
      input$filtro_entidad != "TODAS"
    ) {
      df <- df |>
        dplyr::filter(
          Entidad == input$filtro_entidad
        )
    }
    
    if (
      !is.null(input$filtro_intervencion) &&
      input$filtro_intervencion != "TODOS"
    ) {
      df <- df |>
        dplyr::filter(
          nivel_intervencion == input$filtro_intervencion
        )
    }
    
    df
  })
  
  # ----------------------------------------------------------
  # 2.3 DATOS FILTRADOS POR TAMAÑO DE LOCALIDAD
  # ----------------------------------------------------------
  
  datos_tloc <- reactive({
    df <- dashboard$entidad_tloc |>
      dplyr::mutate(
        Entidad = as.character(Entidad),
        Tam_Loc = as.character(ambito)
      ) |>
      limpiar_necesidades()
    
    if (
      !is.null(input$filtro_entidad) &&
      input$filtro_entidad != "TODAS"
    ) {
      df <- df |>
        dplyr::filter(
          Entidad == input$filtro_entidad
        )
    }
    
    if (
      !is.null(input$filtro_intervencion) &&
      input$filtro_intervencion != "TODOS"
    ) {
      df <- df |>
        dplyr::filter(
          nivel_intervencion == input$filtro_intervencion
        )
    }
    
    if (
      !is.null(input$filtro_tloc) &&
      input$filtro_tloc != "TODOS"
    ) {
      df <- df |>
        dplyr::filter(
          Tam_Loc == input$filtro_tloc
        )
    }
    
    df
  })
  
  # ----------------------------------------------------------
  # 2.6 KPI NACIONALES
  # ----------------------------------------------------------
  
  output$kpi_viviendas <- renderText({
    fmt_num(
      sum(
        dashboard$nacional$viviendas_estimadas,
        na.rm = TRUE
      )
    )
  })
  
  output$kpi_personas <- renderText({
    fmt_num(
      sum(
        dashboard$nacional$personas_en_viviendas_estimadas,
        na.rm = TRUE
      )
    )
  })
  
  output$kpi_hogares <- renderText({
    fmt_num(
      sum(
        dashboard$nacional$hogares_estimados,
        na.rm = TRUE
      )
    )
  })
  
  output$kpi_rezago_habitacional <- renderText({
    df <- limpiar_necesidades(dashboard$nacional) |>
      dplyr::filter(
        nivel_intervencion %in% c(
          "Mejoramiento", 
          "Sustitución", 
          "Ampliación o reemplazo"
        )
      )
    
    if (nrow(df) == 0) {
      return("Sin datos")
    }
    
    paste0(
      fmt_num(sum(df$porcentaje_viviendas, na.rm = TRUE), 1),
      "%"
    )
  })
  
  # ----------------------------------------------------------
  # 2.7 GRÁFICA NACIONAL DE NECESIDADES
  # ----------------------------------------------------------
  
  output$grafica_nacional <- plotly::renderPlotly({
    df <- limpiar_necesidades(dashboard$nacional)
    
    if (!is.null(input$filtro_intervencion) && input$filtro_intervencion != "TODOS") {
      df <- df |> dplyr::filter(nivel_intervencion == input$filtro_intervencion)
    }
    
    req(nrow(df) > 0)
    
    plotly::plot_ly(
      data = df,
      x = ~nivel_intervencion,
      y = ~viviendas_estimadas,
      type = "bar",
      color = ~nivel_intervencion,
      colors = colores_necesidad,
      customdata = ~porcentaje_viviendas,
      hovertemplate = paste0(
        "<b>%{x}</b><br>",
        "Viviendas estimadas: %{y:,.0f}<br>",
        "Porcentaje: %{customdata:.2f}%<extra></extra>"
      )
    ) |>
      plotly::layout(
        xaxis = list(title = ""),
        yaxis = list(title = "Viviendas estimadas"),
        legend = list(title = list(text = "Necesidad")),
        margin = list(b = 100),
        barmode = "group"
      )
  })
  
  # ----------------------------------------------------------
  # 2.8 GRÁFICA DE HACINAMIENTO
  # ----------------------------------------------------------
  
  
  output$grafica_hacinamiento <- plotly::renderPlotly({
    
    df <- dashboard$hacinamiento
    
    req(!is.null(df), nrow(df) > 0)
    
    # Excluir registros sin datos válidos
    df <- df |>
      dplyr::filter(
        !is.na(viviendas_expand),
        !is.na(porcentaje_hacinamiento),
        viviendas_expand > 0
      )
    
    req(nrow(df) > 0)
    
    # Estimar el total de viviendas con hacinamiento
    total_viviendas <- sum(df$viviendas_expand)
    
    viviendas_con <- sum(
      df$viviendas_expand *
        df$porcentaje_hacinamiento / 100
    )
    
    porcentaje_con <- viviendas_con / total_viviendas * 100
    porcentaje_sin <- 100 - porcentaje_con
    
    resumen <- data.frame(
      condicion = c(
        "Con hacinamiento",
        "Sin hacinamiento"
      ),
      porcentaje = c(
        porcentaje_con,
        porcentaje_sin
      )
    )
    
    plotly::plot_ly(
      data = resumen,
      labels = ~condicion,
      values = ~porcentaje,
      type = "pie",
      hole = 0.50,
      marker = list(
        colors = c("#C0392B", "#238B57")
      ),
      textinfo = "label+percent",
      hovertemplate = paste0(
        "<b>%{label}</b><br>",
        "Porcentaje: %{value:.2f}%<br>",
        "<extra></extra>"
      )
    ) |>
      plotly::layout(
        showlegend = TRUE,
        annotations = list(
          list(
            text = "Viviendas",
            x = 0.5,
            y = 0.5,
            showarrow = FALSE
          )
        )
      )
  })
  
  # ----------------------------------------------------------
  # 2.9 GRÁFICA DE MATERIALES
  # ----------------------------------------------------------
  
  output$grafica_materiales <- plotly::renderPlotly({
    df <- dashboard$materiales |>
      dplyr::filter(
        !is.na(clasif_materiales),
        trimws(as.character(clasif_materiales)) != "",
        !tolower(
          trimws(as.character(clasif_materiales))
        ) %in% c(
          "no clasificable",
          "no clasificables",
          "sin clasificar"
        )
      )
    
    req(nrow(df) > 0)
    
    plotly::plot_ly(
      data = df,
      labels = ~clasif_materiales,
      values = ~viviendas_expand,
      type = "pie",
      hole = 0.4,
      textinfo = "label+percent",
      hovertemplate = paste0(
        "<b>%{label}</b><br>",
        "Viviendas expandidas: %{value:,.0f}<br>",
        "<extra></extra>"
      )
    ) |>
      plotly::layout(
        showlegend = TRUE
      )
  })
  
  # ----------------------------------------------------------
  # 2.10 COMPARATIVO POR ENTIDAD
  # Tres gráficas independientes por nivel de intervención
  # GRAFICO SUNBURST PRUEBA
  # ----------------------------------------------------------
  
  datos_sunburst <- shiny::reactive({
    df <- datos_entidad()
    shiny::req(nrow(df) > 0)
    df |>
      dplyr::left_join(catalogo_entidades, by = "Entidad") |>
      dplyr::mutate(
        nombre_entidad = dplyr::coalesce(as.character(Entidad_nombre),
                                         as.character(Entidad))
      )
  })
  
  output$grafica_sunburst <- plotly::renderPlotly({
    df <- datos_sunburst()
    metrica <- input$metrica_sunburst
    shiny::req(metrica %in% c("viviendas_muestra", "viviendas_estimadas",
                              "personas_estimadas", "porcentaje_viviendas"))
    df <- df |>
      dplyr::filter(!is.na(nombre_entidad), !is.na(nivel_intervencion),
                    !is.na(.data[[metrica]]), .data[[metrica]] >= 0) |>
      dplyr::group_by(nombre_entidad, nivel_intervencion) |>
      dplyr::summarise(valor = sum(.data[[metrica]], na.rm = TRUE),
                       .groups = "drop")
    shiny::validate(
      shiny::need(nrow(df) > 0, "No hay datos para mostrar con estos filtros."),
      shiny::need(sum(df$valor, na.rm = TRUE) > 0,
                  "El indicador no tiene valores positivos.")
    )
    hojas <- df |>
      dplyr::transmute(id = paste(nombre_entidad, nivel_intervencion, sep = " | "),
                       label = nivel_intervencion, parent = nombre_entidad,
                       value = valor)
    entidades <- df |>
      dplyr::group_by(nombre_entidad) |>
      dplyr::summarise(value = sum(valor, na.rm = TRUE), .groups = "drop") |>
      dplyr::transmute(id = nombre_entidad, label = nombre_entidad,
                       parent = "raiz_nacional", value = value)
    # El identificador de la raíz debe ser único: el Estado de México
    # también aparece como una entidad y no debe confundirse con el país.
    raiz <- data.frame(id = "raiz_nacional", label = "México", parent = "",
                       value = sum(df$valor, na.rm = TRUE))
    nodos <- dplyr::bind_rows(raiz, entidades, hojas)
    plotly::plot_ly(data = nodos, ids = ~id, labels = ~label, parents = ~parent,
                    values = ~value, type = "sunburst", branchvalues = "total",
                    maxdepth = 3, insidetextorientation = "radial",
                    hovertemplate = paste0("<b>%{label}</b><br>Valor: %{value:,.0f}",
                                           "<br>Porcentaje del total: %{percentRoot:.2%}<extra></extra>")) |>
      plotly::layout(margin = list(t = 20, l = 10, r = 10, b = 10))
  })
  
  # ----------------------------------------------------------
  # 2.11 NECESIDAD POR TAMAÑO DE LOCALIDAD
  # ----------------------------------------------------------
  
  output$grafica_tloc <- plotly::renderPlotly({
    df <- datos_tloc()
    
    req(nrow(df) > 0)
    
    resumen <- df |>
      dplyr::group_by(
        etiqueta_tloc,
        nivel_intervencion
      ) |>
      dplyr::summarise(
        viviendas_estimadas = sum(
          viviendas_estimadas,
          na.rm = TRUE
        ),
        .groups = "drop"
      )
    
    plotly::plot_ly(
      data = resumen,
      x = ~etiqueta_tloc,
      y = ~viviendas_estimadas,
      type = "bar",
      color = ~nivel_intervencion,
      colors = colores_necesidad,
      hovertemplate = paste0(
        "Tamaño: %{x}<br>",
        "Viviendas estimadas: %{y:,.0f}<br>",
        "<extra></extra>"
      )
    ) |>
      plotly::layout(
        xaxis = list(
          title = "Tamaño de localidad"
        ),
        yaxis = list(
          title = "Viviendas estimadas"
        ),
        barmode = "stack"
      )
  })
  
  # ----------------------------------------------------------
  # 2.14 GRÁFICA DE REZAGO HABITACIONAL
  # ----------------------------------------------------------
  
  output$grafica_rezago <- plotly::renderPlotly({
    df <- dashboard$rezago |>
      dplyr::mutate(
        ENT = as.character(ENT)
      )
    
    if (
      !is.null(input$filtro_entidad) &&
      input$filtro_entidad != "TODAS"
    ) {
      df <- df |>
        dplyr::filter(
          ENT == input$filtro_entidad
        )
    }
    
    req(nrow(df) > 0)
    
    df <- df |>
      dplyr::arrange(porcentaje_rezago)
    
    plotly::plot_ly(
      data = df,
      x = ~porcentaje_rezago,
      y = ~reorder(
        Entidad_nombre,
        porcentaje_rezago
      ),
      type = "bar",
      orientation = "h",
      marker = list(
        color = "#C0392B"
      ),
      customdata = ~ENT,
      hovertemplate = paste0(
        "<b>%{y}</b><br>",
        "Código: %{customdata}<br>",
        "Rezago: %{x:.2f}%<br>",
        "<extra></extra>"
      )
    ) |>
      plotly::layout(
        xaxis = list(
          title = "Porcentaje de rezago (%)"
        ),
        yaxis = list(title = ""),
        margin = list(l = 170)
      )
  })
  
  # ----------------------------------------------------------
  # 2.14 GRÁFICA DE REZAGO ENTIDADES (PRUEBA)
  # ----------------------------------------------------------
  
  output$grafica_rezago_ent <- plotly::renderPlotly({
    
    df <- dashboard$rezago_entidad |>
      dplyr::mutate(
        Entidad = normalizar_codigo(Entidad)
      )
    
    if (
      !is.null(input$filtro_entidad) &&
      input$filtro_entidad != "TODAS"
    ) {
      df <- df |>
        dplyr::filter(
          Entidad == input$filtro_entidad
        )
    }
    
    req(nrow(df) > 0)
    
    # Sumar viviendas estimadas por condición de rezago
    df_pie <- df |>
      dplyr::group_by(estado_rezago) |>
      dplyr::summarise(
        viviendas = sum(viviendas_estimadas, na.rm = TRUE),
        .groups = "drop"
      )
    
    plotly::plot_ly(
      data = df_pie,
      labels = ~estado_rezago,
      values = ~viviendas,
      type = "pie",
      textinfo = "percent",
      hovertemplate = paste0(
        "<b>%{label}</b><br>",
        "Viviendas estimadas: %{value:,.0f}<br>",
        "Porcentaje: %{percent}<br>",
        "<extra></extra>"
      ),
      marker = list(
        colors = c("#C0392B", "#27AE60")
      ),
      sort = FALSE
    ) |>
      plotly::layout(
        title = "Viviendas estimadas según rezago habitacional",
        showlegend = TRUE,
        margin = list(l = 30, r = 30, t = 40, b = 30)
      )
  })
  
  # ----------------------------------------------------------
  # 2.12 NECESIDAD EXPLÍCITA DE VIVIENDA
  # ----------------------------------------------------------
  
  output$grafica_demanda_porcentaje <- plotly::renderPlotly({
    df <- dashboard$demanda |>
      dplyr::filter(
        !is.na(nivel_intervencion),
        !is.na(tiene_registro_demanda)
      )
    
    if (!is.null(input$filtro_intervencion) && input$filtro_intervencion != "TODOS") {
      df <- df |> dplyr::filter(nivel_intervencion == input$filtro_intervencion)
    }
    req(nrow(df) > 0)
    
    plotly::plot_ly(
      data = df,
      x = ~nivel_intervencion,
      y = ~porcentaje_dentro_nivel,
      type = "bar",
      color = ~tiene_registro_demanda,
      customdata = ~registro_demanda,
      hovertemplate = paste0(
        "Nivel: %{x}<br>",
        "Porcentaje dentro del nivel: %{y:.2f}%<br>",
        "Registro: %{customdata}<br>",
        "<extra></extra>"
      )
    ) |>
      plotly::layout(
        xaxis = list(title = "", tickangle = -25),
        yaxis = list(
          title = "Porcentaje dentro del nivel (%)",
          ticksuffix = "%"
        ),
        barmode = "group",
        legend = list(title = list(text = "Tiene registro de demanda")),
        margin = list(b = 110)
      )
  })
  
  output$grafica_demanda_personas <- plotly::renderPlotly({
    df <- dashboard$demanda |>
      dplyr::filter(
        !is.na(nivel_intervencion),
        !is.na(tiene_registro_demanda)
      )
    
    if (!is.null(input$filtro_intervencion) && input$filtro_intervencion != "TODOS") {
      df <- df |> dplyr::filter(nivel_intervencion == input$filtro_intervencion)
    }
    req(nrow(df) > 0)
    
    plotly::plot_ly(
      data = df,
      x = ~nivel_intervencion,
      y = ~personas_estimadas,
      type = "bar",
      color = ~tiene_registro_demanda,
      customdata = ~registro_demanda,
      hovertemplate = paste0(
        "Nivel: %{x}<br>",
        "Personas estimadas: %{y:,.0f}<br>",
        "Registro: %{customdata}<br>",
        "<extra></extra>"
      )
    ) |>
      plotly::layout(
        xaxis = list(title = "", tickangle = -25),
        yaxis = list(title = "Personas estimadas"),
        barmode = "group",
        legend = list(title = list(text = "Tiene registro de demanda")),
        margin = list(b = 110)
      )
  })
  
  output$tabla_demanda_resumen <- DT::renderDT({
    df <- dashboard$demanda |>
      dplyr::group_by(nivel_intervencion) |>
      dplyr::summarise(
        viviendas_muestra = sum(viviendas_muestra, na.rm = TRUE),
        viviendas_estimadas = sum(viviendas_estimadas, na.rm = TRUE),
        personas_estimadas = sum(personas_estimadas, na.rm = TRUE),
        .groups = "drop"
      )
    
    if (!is.null(input$filtro_intervencion) && input$filtro_intervencion != "TODOS") {
      df <- df |> dplyr::filter(nivel_intervencion == input$filtro_intervencion)
    }
    tabla_dt(
      df,
      c(
        "Nivel de intervención",
        "Viviendas en muestra",
        "Viviendas estimadas",
        "Personas estimadas"
      ),
      pagina = 10
    ) |>
      DT::formatRound(
        columns = c(
          "viviendas_muestra",
          "viviendas_estimadas",
          "personas_estimadas"
        ),
        digits = 0,
        mark = ","
      )
  })
  
  output$tabla_demanda_detalle <- DT::renderDT({
    df <- dashboard$demanda |>
      dplyr::select(
        nivel_intervencion,
        tiene_registro_demanda,
        registro_demanda,
        viviendas_muestra,
        viviendas_estimadas,
        personas_estimadas,
        porcentaje_dentro_nivel
      )
    
    if (!is.null(input$filtro_intervencion) && input$filtro_intervencion != "TODOS") {
      df <- df |> dplyr::filter(nivel_intervencion == input$filtro_intervencion)
    }
    tabla_dt(
      df,
      c(
        "Nivel de intervención",
        "Tiene registro de demanda",
        "Registro de demanda",
        "Viviendas en muestra",
        "Viviendas estimadas",
        "Personas estimadas",
        "Porcentaje dentro del nivel"
      ),
      pagina = 15
    ) |>
      DT::formatRound(
        columns = c(
          "viviendas_muestra",
          "viviendas_estimadas",
          "personas_estimadas"
        ),
        digits = 0,
        mark = ","
      ) |>
      DT::formatRound(
        columns = "porcentaje_dentro_nivel",
        digits = 2
      )
  })
  
  # ------------------------------------------------------------
  # 3.6 TABLA NACIONAL
  # ------------------------------------------------------------
  
  output$tabla_nacional <- DT::renderDT({
    df <- dashboard$nacional
    
    if (!is.null(input$filtro_intervencion) && input$filtro_intervencion != "TODOS") {
      df <- df |> dplyr::filter(nivel_intervencion == input$filtro_intervencion)
    }
    
    tabla_dt(
      df,
      c(
        "Nivel de necesidad",
        "Viviendas en muestra",
        "Viviendas estimadas",
        "Personas estimadas",
        "Hogares estimados",
        "Porcentaje de viviendas"
      ),
      pagina = 10
    ) |>
      DT::formatRound(
        columns = c(
          "viviendas_muestra",
          "viviendas_estimadas",
          "personas_en_viviendas_estimadas",
          "hogares_estimados"
        ),
        digits = 0,
        mark = ","
      ) |>
      DT::formatRound(
        columns = "porcentaje_viviendas",
        digits = 2
      )
  })
  
  # ------------------------------------------------------------
  # 3.7 TABLA POR ENTIDAD
  # ------------------------------------------------------------
  
  output$tabla_entidades <- DT::renderDT({
    df <- datos_entidad()
    
    tabla_dt(
      df,
      c(
        "Código de entidad",
        "Nivel de necesidad",
        "Viviendas en muestra",
        "Viviendas estimadas",
        "Personas estimadas",
        "Porcentaje de viviendas"
      )
    ) |>
      DT::formatRound(
        columns = c(
          "viviendas_muestra",
          "viviendas_estimadas",
          "personas_estimadas"
        ),
        digits = 0,
        mark = ","
      ) |>
      DT::formatRound(
        columns = "porcentaje_viviendas",
        digits = 2
      )
  })
  
  # ------------------------------------------------------------
  # 3.8 TABLA POR ENTIDAD Y TAMAÑO DE LOCALIDAD
  # ------------------------------------------------------------
  
  output$tabla_tloc <- DT::renderDT({
    df <- datos_tloc()
    
    tabla_dt(
      df,
      c(
        "Código de entidad",
        "Tamaño de localidad",
        "Nivel de necesidad",
        "Viviendas en muestra",
        "Viviendas estimadas",
        "Personas estimadas",
        "Porcentaje de viviendas"
      )
    ) |>
      DT::formatRound(
        columns = c(
          "viviendas_muestra",
          "viviendas_estimadas",
          "personas_estimadas"
        ),
        digits = 0,
        mark = ","
      ) |>
      DT::formatRound(
        columns = "porcentaje_viviendas",
        digits = 2
      )
  })
  
  # ------------------------------------------------------------
  # 3.9 TABLA DE HACINAMIENTO
  # ------------------------------------------------------------
  
  output$tabla_hacinamiento <- DT::renderDT({
    df <- dashboard$hacinamiento |>
      dplyr::mutate(ENT = normalizar_codigo(ENT))
    if (!is.null(input$filtro_entidad) && input$filtro_entidad != "TODAS") {
      df <- df |> dplyr::filter(ENT == input$filtro_entidad)
    }
    df <- df |>
      dplyr::select(ENT, Entidad_nombre, TLOC_codigo, etiqueta_tloc,
                    viviendas_muestra, viviendas_clasificables,
                    viviendas_expand, porcentaje_hacinamiento)
    tabla_dt(df, c("Código de entidad", "Entidad", "Código de localidad",
                   "Tamaño de localidad", "Viviendas en muestra",
                   "Viviendas clasificables", "Viviendas expandidas",
                   "Porcentaje de hacinamiento"), pagina = 10) |>
      DT::formatRound(columns = c("viviendas_muestra", "viviendas_clasificables",
                                  "viviendas_expand"), digits = 0, mark = ",") |>
      DT::formatRound(columns = "porcentaje_hacinamiento", digits = 2)
  })
  # ------------------------------------------------------------
  # 3.10 TABLA DE MATERIALES
  # ------------------------------------------------------------
  
  output$tabla_materiales <- DT::renderDT({
    tabla_dt(
      dashboard$materiales,
      c(
        "Clasificación de materiales",
        "Viviendas observadas",
        "Viviendas expandidas",
        "Porcentaje"
      ),
      pagina = 10
    ) |>
      DT::formatRound(
        columns = c(
          "viviendas_observadas",
          "viviendas_expand",
          "porcentaje"
        ),
        digits = 2,
        mark = ","
      )
  })
  
  # ------------------------------------------------------------
  # 3.13 TABLA DE REZAGO HABITACIONAL
  # ------------------------------------------------------------
  
  output$tabla_rezago <- DT::renderDT({
    df <- dashboard$rezago |>
      dplyr::mutate(ENT = as.character(ENT))
    
    if (!is.null(input$filtro_entidad) &&
        input$filtro_entidad != "TODAS") {
      df <- df |>
        dplyr::filter(ENT == input$filtro_entidad)
    }
    
    tabla_dt(
      df,
      c(
        "Código de entidad",
        "Entidad",
        "Viviendas en muestra",
        "Viviendas clasificables",
        "Viviendas expandibles",
        "Viviendas expandidas",
        "Viviendas con rezago expandidas",
        "Viviendas sin rezago expandidas",
        "Denominador del rezago",
        "Porcentaje de rezago",
        "Porcentaje sin rezago"
      )
    ) |>
      DT::formatRound(
        columns = c(
          "viviendas_muestra",
          "viviendas_clasificables",
          "viviendas_expandibles",
          "viviendas_expand",
          "viviendas_rezago_expand",
          "viviendas_sin_rezago_expand",
          "denominador_rezago"
        ),
        digits = 0,
        mark = ","
      ) |>
      DT::formatRound(
        columns = c(
          "porcentaje_rezago",
          "porcentaje_sin_rezago"
        ),
        digits = 2
      )
  })
  
  # ============================================================
  # DASHBOARD ENVI 2020 - PARTE 4 DE 4
  # RESUMEN, DESCARGAS, VALIDACIONES Y ARRANQUE
  # ============================================================
  
  # ------------------------------------------------------------
  # 4.2 RESUMEN DE REZAGO HABITACIONAL
  # ------------------------------------------------------------
  
  output$resumen_rezago <- renderUI({
    df <- dashboard$rezago |>
      dplyr::mutate(ENT = as.character(ENT))
    
    if (!is.null(input$filtro_entidad) &&
        input$filtro_entidad != "TODAS") {
      df <- df |>
        dplyr::filter(ENT == input$filtro_entidad)
    }
    
    req(nrow(df) > 0)
    
    total_rezago <- sum(
      df$viviendas_rezago_expand,
      na.rm = TRUE
    )
    
    total_sin_rezago <- sum(
      df$viviendas_sin_rezago_expand,
      na.rm = TRUE
    )
    
    total_denominador <- sum(
      df$denominador_rezago,
      na.rm = TRUE
    )
    
    porcentaje <- if (total_denominador > 0) {
      100 * total_rezago / total_denominador
    } else {
      NA_real_
    }
    
    div(
      h5("Viviendas con rezago habitacional"),
      h3(fmt_num(total_rezago)),
      tags$hr(),
      
      h5("Viviendas sin rezago habitacional"),
      h3(fmt_num(total_sin_rezago)),
      tags$hr(),
      
      h5("Porcentaje de rezago calculado"),
      h3(
        if (is.finite(porcentaje)) {
          paste0(fmt_num(porcentaje, 2), "%")
        } else {
          "Sin datos"
        }
      ),
      
      tags$hr(),
      
      p(
        paste(
          "El porcentaje se calcula a partir de las sumas",
          "de los componentes disponibles. Puede diferir",
          "del porcentaje oficial si los denominadores o",
          "factores de expansión requieren otro tratamiento."
        ),
        class = "small text-muted"
      )
    )
  })
  
  # ------------------------------------------------------------
  # 4.3 DESCARGA SEGÚN LA PESTAÑA ACTIVA
  # ------------------------------------------------------------
  # Se permite descargar los datos correspondientes a la vista
  # seleccionada. Los datos originales no se modifican.
  
  output$descargar_datos <- downloadHandler(
    filename = function() {
      nombre_pestana <- switch(
        input$pestana,
        "Resumen nacional" = "resumen_nacional",
        "Distribución por entidad" = "entidades",
        "Ámbito de localidad" = "tamano_localidad",
        "Hacinamiento y materiales" = "hacinamiento_materiales",
        "Necesidad explícita de vivienda" = "necesidad_explicita_vivienda",
        "Rezago habitacional" = "rezago_habitacional",
        "datos"
      )
      
      paste0(
        "ENVI_", nombre_pestana, "_",
        format(Sys.Date(), "%Y%m%d"),
        ".csv"
      )
    },
    
    content = function(file) {
      pestana <- input$pestana
      
      df <- switch(
        pestana,
        
        "Resumen nacional" = dashboard$nacional,
        
        "Distribución por entidad" = datos_entidad(),
        
        "Ámbito de localidad" = datos_tloc(),
        
        "Necesidad explícita de vivienda" = dashboard$demanda,
        
        "Hacinamiento y materiales" = dplyr::bind_rows(
          dashboard$hacinamiento |>
            dplyr::mutate(
              tipo_dato = "Hacinamiento"
            ) |>
            dplyr::rename(
              categoria = Nivel_hacinamiento
            ) |>
            dplyr::select(
              tipo_dato,
              categoria,
              dplyr::everything()
            ),
          
          dashboard$materiales |>
            dplyr::mutate(
              tipo_dato = "Materiales"
            ) |>
            dplyr::rename(
              categoria = clasif_materiales
            ) |>
            dplyr::select(
              tipo_dato,
              categoria,
              dplyr::everything()
            ),
          .id = "origen"
        ),
        
        "Rezago habitacional" = {
          df_rezago <- dashboard$rezago |>
            dplyr::mutate(ENT = as.character(ENT))
          
          if (!is.null(input$filtro_entidad) &&
              input$filtro_entidad != "TODAS") {
            df_rezago <- df_rezago |>
              dplyr::filter(ENT == input$filtro_entidad)
          }
          
          df_rezago
        },
        
        datos_entidad()
      )
      
      utils::write.csv(
        df,
        file,
        row.names = FALSE,
        na = "",
        fileEncoding = "UTF-8"
      )
    },
    
    contentType = "text/csv"
  )
  
  # ------------------------------------------------------------
  # 4.5 VALIDACIÓN GENERAL
  # ------------------------------------------------------------
  
  observe({
    req(dashboard$nacional)
    req(dashboard$entidad)
    req(dashboard$entidad_tloc)
    req(dashboard$hacinamiento)
    req(dashboard$materiales)
    req(dashboard$rezago)
  })
  
  # ------------------------------------------------------------
  # 4.6 INICIAR APLICACIÓN
  # ------------------------------------------------------------
  
}  # Fin de server()

shiny::shinyApp(
  ui = ui,
  server = server
)

