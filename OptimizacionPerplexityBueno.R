# -------------------------------------------------------------------------
# Cargar librerías necesarias
# -------------------------------------------------------------------------
library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(readxl)
library(data.table)
library(plotly)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(tidytext)
library(stringr)
library(tibble)
library(lubridate)
library(survey)

# -------------------------------------------------------------------------
# Utilidades: normalización y factores
# -------------------------------------------------------------------------
parse_datetime_smart <- function(x) {
  if (inherits(x, "POSIXct") || inherits(x, "POSIXt")) return(as.POSIXct(x, origin = "1970-01-01", tz = "UTC"))
  if (inherits(x, "Date")) return(as.POSIXct(x))
  if (is.numeric(x)) {
    # Rango razonable para serial de Excel
    if (mean(x, na.rm = TRUE) > 10 & mean(x, na.rm = TRUE) < 100000) {
      return(as.POSIXct(x * 86400, origin = "1899-12-30", tz = "UTC"))
    }
  }
  if (is.character(x) || is.factor(x)) {
    xc <- as.character(x)
    parsed <- suppressWarnings(parse_date_time(
      xc,
      orders = c("Ymd HMS","Ymd HM","Ymd","dmy HMS","dmy HM","dmy","mdy HMS","mdy HM","mdy","HMS","HM"),
      tz = "UTC",
      quiet = TRUE
    ))
    if (mean(!is.na(parsed)) >= 0.7) return(parsed)
  }
  x
}

normalize_datetime_columns <- function(dt) {
  stopifnot(is.data.table(dt))
  cols <- names(dt)
  patterns <- c("hora","fecha","inicio","fin","time","date","start","end")
  targets <- cols[grepl(paste(patterns, collapse = "|"), cols, ignore.case = TRUE)]
  if (length(targets)) {
    dt[, (targets) := lapply(.SD, parse_datetime_smart), .SDcols = targets]
  }
  invisible(dt)
}

coerce_categoricals <- function(dt, max_levels = 50L) {
  stopifnot(is.data.table(dt))
  char_cols <- names(dt)[vapply(dt, is.character, logical(1))]
  if (!length(char_cols)) return(dt)
  dt[, (char_cols) := lapply(.SD, function(x) {
    if (length(unique(x)) <= max_levels) factor(x) else x
  }), .SDcols = char_cols]
  invisible(dt)
}

# -------------------------------------------------------------------------
# Interfaz de Usuario (UI)
# -------------------------------------------------------------------------
ui <- dashboardPage(
  dashboardHeader(title = "Análisis de Encuestas"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Inicio", tabName = "inicio", icon = icon("home")),
      menuItem("Cargar Datos", tabName = "cargar", icon = icon("upload")),
      menuItem("Control de Calidad", tabName = "calidad", icon = icon("check-circle")),
      menuItem("Ponderación y Ajuste", tabName = "ponderacion", icon = icon("balance-scale")),
      menuItem("Análisis de Consistencia", tabName = "consistencia", icon = icon("sitemap")),
      menuItem("Análisis Univariado", tabName = "univariado", icon = icon("chart-bar")),
      menuItem("Análisis Bivariado", tabName = "contingencia", icon = icon("table")),
      menuItem("Análisis de Texto", tabName = "texto", icon = icon("comment-alt")),
      menuItem("Vista de Datos", tabName = "datos", icon = icon("database"))
    )
  ),
  dashboardBody(
    tabItems(
      # Inicio
      tabItem("inicio",
              fluidRow(
                box(title = "Bienvenido al Analizador de Encuestas", status = "primary", solidHeader = TRUE, width = 12,
                    p("Esta aplicación te permite cargar un archivo de encuesta (.xlsx o .csv) para realizar análisis de calidad, ajustes de muestra y análisis exploratorios."),
                    tags$ul(
                      tags$li("Usa la pestaña 'Cargar Datos' para subir tu archivo."),
                      tags$li("El 'Control de Calidad' te ayuda a identificar duplicados y patrones en los tiempos de respuesta."),
                      tags$li("El módulo de 'Ponderación y Ajuste' te permite calibrar muestras no probabilísticas contra datos poblacionales de referencia."),
                      tags$li("El 'Análisis de Consistencia' permite verificar si los patrones de respuesta se mantienen en submuestras aleatorias."),
                      tags$li("En 'Análisis Univariado' y 'Análisis Bivariado', podrás explorar tus variables categóricas."),
                      tags$li("El módulo 'Análisis de Texto' permite estudiar preguntas abiertas."),
                      tags$li("La pestaña 'Vista de Datos' te permite ver y filtrar tus datos originales.")
                    )
                )
              )
      ),
      
      # Cargar archivo
      tabItem("cargar",
              fluidRow(
                box(title = "Sube tu archivo (.xlsx o .csv)", status = "primary", solidHeader = TRUE, width = 6,
                    fileInput("archivo_excel", "Archivo", accept = c(".xlsx", ".xls", ".csv")),
                    helpText("Asegúrate de que la primera fila contenga los nombres de las variables.")
                )
              )
      ),
      
      # Módulo de Control de Calidad
      tabItem("calidad",
              fluidRow(
                box(title = "Controles de Calidad", status = "primary", solidHeader = TRUE, width = 12,
                    p("Selecciona las columnas adecuadas para realizar el análisis de calidad de los datos."),
                    column(3, uiOutput("selector_id_calidad")),
                    column(3, uiOutput("selector_hora_inicio_calidad")),
                    column(3, uiOutput("selector_hora_fin_calidad")),
                    column(3, uiOutput("selector_fecha_calidad_ui"))
                )
              ),
              fluidRow(
                column(12, uiOutput("filtro_dia_calidad_ui"))
              ),
              conditionalPanel(
                condition = "input.activar_analisis_por_dia == false",
                fluidRow(
                  box(title = "1. Detección de Duplicados por Identificador (General)", status = "warning", solidHeader = TRUE, width = 12,
                      p("Esta tabla muestra los identificadores que aparecen más de una vez en los datos, lo que podría indicar que una persona respondió varias veces."),
                      DTOutput("tabla_duplicados")
                  )
                ),
                fluidRow(
                  box(title = "2. Análisis de Tiempos de Respuesta (General)", status = "info", solidHeader = TRUE, width = 6,
                      p("El resumen muestra la distribución del tiempo (en minutos) que tardaron en completar la encuesta. Tiempos muy cortos o muy largos pueden ser sospechosos."),
                      verbatimTextOutput("resumen_tiempos"),
                      plotlyOutput("grafico_tiempos", height = "300px")
                  ),
                  box(title = "3. Respuestas por Bloque Horario (General)", status = "info", solidHeader = TRUE, width = 6,
                      p("Este gráfico muestra en qué momento del día se recibieron más respuestas, lo que puede ayudar a detectar patrones inusuales (ej. muchas respuestas en la madrugada)."),
                      plotlyOutput("grafico_bloques_horarios", height = "300px"),
                      DTOutput("tabla_bloques_horarios")
                  )
                )
              ),
              conditionalPanel(
                condition = "input.activar_analisis_por_dia == true",
                uiOutput("resultados_por_dia_ui")
              )
      ),
      
      # Módulo de Ponderación y Ajuste
      tabItem("ponderacion",
              fluidRow(
                box(title = "Marco Conceptual: Ajuste de Muestras No Probabilísticas", status = "primary", solidHeader = TRUE, width = 12, collapsible = TRUE,
                    p("Este módulo aborda los errores de cobertura y autoselección, inherentes a los sondeos en redes sociales (muestras no probabilísticas u 'opt-in'), siguiendo el paradigma del Error Total de la Encuesta (TSE)."),
                    p("El objetivo es ponderar los datos de la encuesta para que la distribución de variables demográficas clave (ej. sexo, edad, región) se asemeje a la de una población de referencia conocida (ej. un censo). Esta técnica, conocida como post-estratificación o 'raking', reduce el sesgo y mejora la capacidad de generalización de los resultados."),
                    p("Para usar este módulo, necesita un segundo archivo (Excel o CSV) que contenga los totales poblacionales de referencia.")
                )
              ),
              fluidRow(
                box(title = "Paso 1: Configuración de Variables y Datos de Referencia", status = "warning", solidHeader = TRUE, width = 4,
                    h4("Variables de su Encuesta"),
                    uiOutput("selector_vars_ponderacion"),
                    hr(),
                    h4("Archivo de Referencia Poblacional"),
                    fileInput("archivo_referencia", "Subir archivo (.xlsx o .csv)", accept = c(".xlsx", ".xls", ".csv")),
                    helpText("El archivo debe tener 3 columnas: una con el nombre de la variable (ej. 'sexo'), otra con la categoría (ej. 'Hombre') y otra con el total poblacional (ej. 1500000)."),
                    uiOutput("map_cols_referencia_ui"),
                    hr(),
                    actionButton("run_raking", "Calcular Pesos de Ponderación", icon = icon("cogs"), class = "btn-success")
                ),
                box(title = "Paso 2: Resultados del Ajuste", status = "info", solidHeader = TRUE, width = 8,
                    uiOutput("resultados_ponderacion_tabs")
                )
              )
      ),
      
      # Módulo de Análisis de Consistencia
      tabItem("consistencia",
              fluidRow(
                box(title = "Controles de Consistencia", status = "primary", solidHeader = TRUE, width = 12,
                    p("Este módulo divide la muestra total en varios subconjuntos aleatorios para comparar los resultados. Si los porcentajes son muy similares entre submuestras, los datos son consistentes."),
                    column(4, uiOutput("selector_respuesta_consistencia")),
                    column(4, uiOutput("selector_control_consistencia")),
                    column(3, sliderInput("num_submuestras", "Número de submuestras:", min = 2, max = 5, value = 2, step = 1)),
                    column(1, actionButton("analizar_consistencia", "Analizar", icon = icon("play"), style="margin-top: 25px;"))
                )
              ),
              fluidRow(
                uiOutput("analisis_estadistico_consistencia_ui")
              ),
              fluidRow(
                uiOutput("resultados_consistencia")
              )
      ),
      
      # Análisis Univariado
      tabItem("univariado",
              fluidRow(
                box(title = "Controles", status = "primary", solidHeader = TRUE, width = 3,
                    uiOutput("selector_variable_uni"),
                    selectInput("tipo_grafico", "Tipo de gráfico:", choices = c("Barras" = "barras", "Circular" = "pie")),
                    checkboxInput("mostrar_n", "Mostrar N", TRUE),
                    checkboxInput("mostrar_porcentaje", "Mostrar Porcentaje", TRUE)
                ),
                box(title = "Gráfico", status = "info", solidHeader = TRUE, width = 9,
                    plotlyOutput("grafico_univariado", height = "400px"))
              ),
              fluidRow(
                box(title = "Tabla de Frecuencias", status = "success", solidHeader = TRUE, width = 12,
                    DTOutput("tabla_univariada"))
              )
      ),
      
      # Análisis Bivariado
      tabItem("contingencia",
              fluidRow(
                box(title = "Controles", status = "primary", solidHeader = TRUE, width = 3,
                    uiOutput("selector_variable_x"),
                    uiOutput("selector_variable_y")
                ),
                box(title = "Tabla de Frecuencias Absolutas (N)", status = "info", solidHeader = TRUE, width = 9,
                    DTOutput("tabla_contingencia_n"))
              ),
              fluidRow(
                box(title = "Porcentaje por columna", status = "warning", solidHeader = TRUE, width = 4,
                    DTOutput("tabla_contingencia_col")),
                box(title = "Porcentaje por fila", status = "warning", solidHeader = TRUE, width = 4,
                    DTOutput("tabla_contingencia_fila")),
                box(title = "Porcentaje total", status = "warning", solidHeader = TRUE, width = 4,
                    DTOutput("tabla_contingencia_total"))
              )
      ),
      
      # Módulo de Análisis de Texto
      tabItem("texto",
              fluidRow(
                box(title = "Controles de Texto", status = "primary", solidHeader = TRUE, width = 3,
                    uiOutput("selector_variable_texto"),
                    uiOutput("selector_variable_cruce"),
                    uiOutput("selector_categoria_cruce"),
                    actionButton("actualizar_nube", "Generar Nube de Palabras"),
                    numericInput("max_palabras", "Máximo de palabras:", value = 100, min = 10),
                    hr(),
                    h4("Ajuste de Sentimiento"),
                    numericInput("sesgo_sentimiento", "Ajustar Sesgo (valor entre -5 y 5):", value = 0, min = -5, max = 5, step = 0.5),
                    helpText("Añade un peso a la puntuación. Un valor negativo (ej: -2) es útil para preguntas sobre problemas.")
                ),
                box(title = "Nube de Palabras", status = "info", solidHeader = TRUE, width = 9,
                    plotOutput("nube_palabras", height = "400px")),
                box(title = "Análisis de Sentimiento", status = "info", solidHeader = TRUE, width = 6,
                    verbatimTextOutput("resumen_sentimiento")),
                box(title = "Tabla de Frecuencia de Palabras", status = "success", solidHeader = TRUE, width = 6,
                    DTOutput("tabla_frecuencia_palabras"))
              )
      ),
      
      # Vista de Datos
      tabItem("datos",
              fluidRow(
                box(title = "Vista de Datos", status = "primary", solidHeader = TRUE, width = 12,
                    DTOutput("tabla_datos"))
              ),
              fluidRow(
                box(title = "Resumen de Datos", status = "info", solidHeader = TRUE, width = 12,
                    verbatimTextOutput("resumen_datos"))
              )
      )
    )
  )
)

# -------------------------------------------------------------------------
# Lógica del Servidor (Server)
# -------------------------------------------------------------------------
server <- function(input, output, session) {
  
  # ----------------- Carga y limpieza de datos -----------------
  datos_raw <- shiny::bindCache(
    reactive({
      req(input$archivo_excel)
      ext <- tolower(tools::file_ext(input$archivo_excel$name))
      
      dt <- tryCatch({
        if (ext == "csv") {
          data.table::fread(input$archivo_excel$datapath, encoding = "UTF-8")
        } else if (ext %in% c("xls", "xlsx")) {
          as.data.table(readxl::read_excel(input$archivo_excel$datapath))
        } else {
          showModal(modalDialog(
            title = "Error de archivo",
            "Formato de archivo no soportado. Por favor, usa un archivo .csv, .xls o .xlsx.",
            easyClose = TRUE,
            footer = NULL
          ))
          return(NULL)
        }
      }, error = function(e) {
        showModal(modalDialog(
          title = "Error de archivo",
          paste("No se pudo leer el archivo:", e$message),
          easyClose = TRUE
        ))
        NULL
      })
      
      validate(need(!is.null(dt), "No se pudo cargar el archivo."))
      
      # Normalización
      normalize_datetime_columns(dt)
      coerce_categoricals(dt, max_levels = 50L)
      dt
    }),
    input$archivo_excel$datapath
  )
  
  # --- Selectores dinámicos ---
  output$selector_variable_uni <- renderUI({
    validate(need(datos_raw(), "Por favor, carga un archivo para empezar."))
    selectInput("variable_uni", "Selecciona una variable:", choices = names(datos_raw()))
  })
  
  output$selector_variable_x <- renderUI({
    validate(need(datos_raw(), "Por favor, carga un archivo para empezar."))
    selectInput("variable_x", "Variable X:", choices = names(datos_raw()))
  })
  
  output$selector_variable_y <- renderUI({
    validate(need(datos_raw(), "Por favor, carga un archivo para empezar."))
    selectInput("variable_y", "Variable Y:", choices = names(datos_raw()))
  })
  
  output$selector_variable_texto <- renderUI({
    validate(need(datos_raw(), "Por favor, carga un archivo para empezar."))
    vars <- names(datos_raw())
    text_vars <- vars[sapply(datos_raw(), is.character)]
    selectInput("variable_texto", "Selecciona una pregunta abierta:", choices = text_vars)
  })
  
  output$selector_variable_cruce <- renderUI({
    req(input$variable_texto)
    df <- datos_raw()
    vars <- names(df)
    vars_cat <- vars[sapply(df, is.factor)]
    selectInput("variable_cruce", "Cruce por variable categórica (opcional):",
                choices = c("Sin cruce" = "", vars_cat))
  })
  
  output$selector_categoria_cruce <- renderUI({
    req(input$variable_cruce != "")
    df <- datos_raw()
    choices_list <- sort(unique(df[[input$variable_cruce]]))
    selectInput("categoria_cruce", "Selecciona la categoría:", choices = choices_list)
  })
  
  # --- Controles dinámicos para el módulo de calidad ---
  output$selector_id_calidad <- renderUI({
    validate(need(datos_raw(), "Carga un archivo."))
    selectInput("id_calidad", "Columna de Identificador:", choices = names(datos_raw()))
  })
  
  output$selector_hora_inicio_calidad <- renderUI({
    validate(need(datos_raw(), "Carga un archivo."))
    choices <- names(datos_raw())
    pre_selected <- choices[grepl("inicio|start", choices, ignore.case = TRUE)][1]
    selectInput("hora_inicio_calidad", "Columna Hora Inicio:", choices = choices, selected = pre_selected)
  })
  
  output$selector_hora_fin_calidad <- renderUI({
    validate(need(datos_raw(), "Carga un archivo."))
    choices <- names(datos_raw())
    pre_selected <- choices[grepl("fin|end", choices, ignore.case = TRUE)][1]
    selectInput("hora_fin_calidad", "Columna Hora Fin:", choices = choices, selected = pre_selected)
  })
  
  output$selector_fecha_calidad_ui <- renderUI({
    validate(need(datos_raw(), "Carga un archivo."))
    choices <- names(datos_raw())
    date_cols <- choices[sapply(datos_raw(), function(c) inherits(c, "Date") || inherits(c, "POSIXt"))]
    pre_selected <- date_cols[grepl("fecha|date", date_cols, ignore.case = TRUE)][1]
    selectInput("fecha_calidad", "Columna de Fecha:", choices = choices, selected = pre_selected)
  })
  
  output$filtro_dia_calidad_ui <- renderUI({
    req(input$fecha_calidad)
    box(title = "Análisis Comparativo por Día", status = "primary", solidHeader = FALSE, width = 12, collapsible = TRUE, collapsed = TRUE,
        checkboxInput("activar_analisis_por_dia", "Generar análisis por cada día de aplicación", value = FALSE),
        helpText("Al activar esta opción, se generarán los análisis de calidad para cada fecha única encontrada en la columna seleccionada. Esto puede tardar unos segundos si hay muchos días diferentes.")
    )
  })
  
  # --- Lógica del Servidor para Control de Calidad ---
  datos_filtrados_calidad <- reactive({
    validate(need(datos_raw(), "Carga un archivo."))
    datos_raw()
  })
  
  output$tabla_duplicados <- renderDT({
    req(input$id_calidad)
    df <- datos_filtrados_calidad()
    id_col <- input$id_calidad
    validate(need(id_col %in% names(df), "La columna de identificador no existe."))
    duplicados <- df %>%
      count(!!sym(id_col), name = "Frecuencia") %>%
      filter(Frecuencia > 1) %>%
      arrange(desc(Frecuencia))
    if (nrow(duplicados) == 0) {
      return(datatable(data.frame(Resultado = "No se encontraron identificadores duplicados. ¡Buenas noticias!"),
                       options = list(dom = 't'), rownames = FALSE))
    }
    datatable(duplicados, caption = "Tabla de Identificadores Duplicados",
              options = list(pageLength = 5, searching = TRUE, ordering = TRUE), rownames = FALSE)
  })
  
  tiempos_data <- shiny::bindCache(
    reactive({
      req(input$hora_inicio_calidad, input$hora_fin_calidad, input$fecha_calidad)
      df <- datos_filtrados_calidad()
      
      start_col_name <- input$hora_inicio_calidad
      end_col_name   <- input$hora_fin_calidad
      date_col_name  <- input$fecha_calidad
      
      validate(
        need(all(c(start_col_name, end_col_name, date_col_name) %in% names(df)), "Una o más columnas de fecha/hora seleccionadas no existen."),
        need(inherits(df[[start_col_name]], "POSIXt"), "La columna de Hora Inicio no es un formato de hora válido."),
        need(inherits(df[[end_col_name]], "POSIXt"), "La columna de Hora Fin no es un formato de hora válido."),
        need(inherits(df[[date_col_name]], "POSIXt") || inherits(df[[date_col_name]], "Date"), "La columna de Fecha no es un formato de fecha válido.")
      )
      
      fecha_base <- as.Date(df[[date_col_name]])
      hora_inicio_str <- format(df[[start_col_name]], "%H:%M:%S")
      hora_fin_str    <- format(df[[end_col_name]], "%H:%M:%S")
      
      start_datetime <- ymd_hms(paste(fecha_base, hora_inicio_str), quiet = TRUE)
      end_datetime   <- ymd_hms(paste(fecha_base, hora_fin_str), quiet = TRUE)
      
      indices_medianoche <- which(end_datetime < start_datetime)
      if (length(indices_medianoche) > 0) {
        end_datetime[indices_medianoche] <- end_datetime[indices_medianoche] + days(1)
      }
      
      duracion_min <- as.numeric(difftime(end_datetime, start_datetime, units = "mins"))
      duracion_final <- duracion_min[is.finite(duracion_min)]
      validate(need(length(duracion_final) > 0, "No se pudo calcular la duración. Verifique que las columnas de fecha y hora sean correctas."))
      data.frame(duracion = duracion_final)
    }),
    input$hora_inicio_calidad, input$hora_fin_calidad, input$fecha_calidad, datos_filtrados_calidad()
  )
  
  output$resumen_tiempos <- renderPrint({
    df_tiempos <- tiempos_data()
    cat("Resumen de Tiempos de Respuesta (en minutos)\n\n")
    summary(df_tiempos$duracion)
  })
  
  output$grafico_tiempos <- renderPlotly({
    df_tiempos <- tiempos_data()
    plot_ly(df_tiempos, x = ~duracion, type = "histogram") %>%
      layout(title = "Distribución de Tiempos de Respuesta",
             xaxis = list(title = "Duración (minutos)"),
             yaxis = list(title = "Cantidad de Encuestas"))
  })
  
  bloques_data <- shiny::bindCache(
    reactive({
      req(input$hora_inicio_calidad)
      df <- datos_filtrados_calidad()
      hora_col_name <- input$hora_inicio_calidad
      validate(
        need(hora_col_name %in% names(df), "La columna de hora de inicio no existe."),
        need(is.POSIXct(df[[hora_col_name]]), "La columna de hora de inicio no tiene un formato de fecha/hora válido.")
      )
      horas_num <- hour(df[[hora_col_name]])
      df_bloques <- data.frame(hora = horas_num) %>%
        mutate(
          bloque = case_when(
            hora >= 6  & hora < 12 ~ "1. Mañana (6am - 12pm)",
            hora >= 12 & hora < 19 ~ "2. Tarde (12pm - 7pm)",
            hora >= 19 & hora < 23 ~ "3. Noche (7pm - 11pm)",
            TRUE ~ "4. Madrugada (11pm - 6am)"
          )
        ) %>%
        count(bloque, name = "Total") %>%
        arrange(bloque)
      df_bloques
    }),
    input$hora_inicio_calidad, datos_filtrados_calidad()
  )
  
  output$grafico_bloques_horarios <- renderPlotly({
    df_bloques <- bloques_data()
    validate(need(nrow(df_bloques) > 0, "No hay datos de bloques horarios para graficar."))
    plot_ly(df_bloques, x = ~bloque, y = ~Total, type = "bar", color = ~bloque) %>%
      layout(title = "Total de Respuestas por Bloque Horario",
             xaxis = list(title = "Bloque del Día"),
             yaxis = list(title = "Cantidad de Encuestas"),
             showlegend = FALSE)
  })
  
  output$tabla_bloques_horarios <- renderDT({
    df_bloques <- bloques_data()
    datatable(df_bloques, caption = "Frecuencia por Bloque Horario",
              options = list(dom = 't', searching = FALSE, ordering = FALSE), rownames = FALSE)
  })
  
  observeEvent(input$activar_analisis_por_dia, {
    if (input$activar_analisis_por_dia == TRUE) {
      
      output$resultados_por_dia_ui <- renderUI({
        req(input$fecha_calidad)
        df <- datos_raw()
        fecha_col_name <- input$fecha_calidad
        validate(need(inherits(df[[fecha_col_name]], "Date") || inherits(df[[fecha_col_name]], "POSIXt"), "Columna de fecha no válida."))
        
        dias_unicos <- sort(unique(as.Date(df[[fecha_col_name]])))
        
        lapply(dias_unicos, function(dia) {
          dia_str <- as.character(dia)
          fluidRow(
            box(title = paste("Análisis para el día:", dia_str), status = "primary", solidHeader = TRUE, width = 12, collapsible = TRUE,
                fluidRow(
                  box(title = "1. Detección de Duplicados", status = "warning", width = 12,
                      DTOutput(outputId = paste0("duplicados_", dia_str))
                  )
                ),
                fluidRow(
                  box(title = "2. Tiempos de Respuesta", status = "info", width = 6,
                      verbatimTextOutput(outputId = paste0("resumen_tiempos_", dia_str)),
                      plotlyOutput(outputId = paste0("grafico_tiempos_", dia_str), height = "300px")
                  ),
                  box(title = "3. Bloques Horarios", status = "info", width = 6,
                      plotlyOutput(outputId = paste0("grafico_bloques_", dia_str), height = "300px"),
                      DTOutput(outputId = paste0("tabla_bloques_", dia_str))
                  )
                )
            )
          )
        })
      })
      
      df <- datos_raw()
      fecha_col_name <- input$fecha_calidad
      dias_unicos <- sort(unique(as.Date(df[[fecha_col_name]])))
      
      for (dia_actual in dias_unicos) {
        local({
          dia_loop <- as.Date(dia_actual, origin = "1970-01-01")
          dia_str_loop <- as.character(dia_loop)
          
          datos_dia <- df[as.Date(get(fecha_col_name)) == dia_loop, ]
          
          output[[paste0("duplicados_", dia_str_loop)]] <- renderDT({
            id_col <- input$id_calidad
            duplicados_dia <- datos_dia %>% count(!!sym(id_col), name = "Frecuencia") %>% filter(Frecuencia > 1)
            if (nrow(duplicados_dia) > 0) {
              datatable(duplicados_dia, options = list(pageLength = 3, searching = FALSE), rownames = FALSE)
            } else {
              datatable(data.frame(Resultado = "No se encontraron duplicados para este día."), rownames = FALSE, options = list(dom = 't'))
            }
          })
          
          output[[paste0("resumen_tiempos_", dia_str_loop)]] <- renderPrint({
            start_dt <- ymd_hms(paste(as.Date(datos_dia[[input$fecha_calidad]]), format(datos_dia[[input$hora_inicio_calidad]], "%H:%M:%S")))
            end_dt   <- ymd_hms(paste(as.Date(datos_dia[[input$fecha_calidad]]), format(datos_dia[[input$hora_fin_calidad]], "%H:%M:%S")))
            indices_medianoche <- which(end_dt < start_dt)
            if (length(indices_medianoche) > 0) { end_dt[indices_medianoche] <- end_dt[indices_medianoche] + days(1) }
            duracion <- as.numeric(difftime(end_dt, start_dt, units = "mins"))
            summary(na.omit(duracion))
          })
          
          output[[paste0("grafico_tiempos_", dia_str_loop)]] <- renderPlotly({
            start_dt <- ymd_hms(paste(as.Date(datos_dia[[input$fecha_calidad]]), format(datos_dia[[input$hora_inicio_calidad]], "%H:%M:%S")))
            end_dt   <- ymd_hms(paste(as.Date(datos_dia[[input$fecha_calidad]]), format(datos_dia[[input$hora_fin_calidad]], "%H:%M:%S")))
            indices_medianoche <- which(end_dt < start_dt)
            if (length(indices_medianoche) > 0) { end_dt[indices_medianoche] <- end_dt[indices_medianoche] + days(1) }
            duracion <- as.numeric(difftime(end_dt, start_dt, units = "mins"))
            plot_ly(data.frame(duracion = na.omit(duracion)), x = ~duracion, type = "histogram") %>% layout(xaxis=list(title="Minutos"), yaxis=list(title="Frecuencia"))
          })
          
          bloques_dia_data <- datos_dia %>%
            mutate(hora = hour(get(input$hora_inicio_calidad))) %>%
            mutate(bloque = case_when(
              hora >= 6  & hora < 12 ~ "1. Mañana",
              hora >= 12 & hora < 19 ~ "2. Tarde",
              hora >= 19 & hora < 23 ~ "3. Noche",
              TRUE ~ "4. Madrugada"
            )) %>% count(bloque, name = "Total") %>% arrange(bloque)
          
          output[[paste0("grafico_bloques_", dia_str_loop)]] <- renderPlotly({
            plot_ly(bloques_dia_data, x = ~bloque, y = ~Total, type = "bar") %>% layout(xaxis=list(title=""), yaxis=list(title="Frecuencia"))
          })
          
          output[[paste0("tabla_bloques_", dia_str_loop)]] <- renderDT({
            datatable(bloques_dia_data, options = list(dom = 't', searching = FALSE, ordering = FALSE), rownames = FALSE)
          })
        })
      }
    }
  })
  
  # --- Lógica del Módulo de Ponderación y Ajuste ---
  output$selector_vars_ponderacion <- renderUI({
    validate(need(datos_raw(), "Cargue los datos de la encuesta primero."))
    vars_cat <- names(datos_raw())[sapply(datos_raw(), is.factor)]
    selectInput("vars_raking", "Seleccione las variables para ajustar:", choices = vars_cat, multiple = TRUE)
  })
  
  datos_referencia <- reactive({
    req(input$archivo_referencia)
    ext <- tools::file_ext(input$archivo_referencia$name)
    if (tolower(ext) == "csv") {
      data.table::fread(input$archivo_referencia$datapath, encoding = "UTF-8")
    } else {
      as.data.table(readxl::read_excel(input$archivo_referencia$datapath))
    }
  })
  
  output$map_cols_referencia_ui <- renderUI({
    req(datos_referencia())
    nombres_cols <- names(datos_referencia())
    tagList(
      selectInput("col_var_ref", "Columna de 'Variable':", choices = nombres_cols),
      selectInput("col_cat_ref", "Columna de 'Categoría':", choices = nombres_cols),
      selectInput("col_total_ref", "Columna de 'Total Poblacional':", choices = nombres_cols, selected = tail(nombres_cols, 1))
    )
  })
  
  resultados_raking <- eventReactive(input$run_raking, {
    req(datos_raw(), datos_referencia(), input$vars_raking, input$col_var_ref, input$col_cat_ref, input$col_total_ref)
    
    df_encuesta <- datos_raw()
    df_ref <- datos_referencia()
    vars_a_ajustar <- input$vars_raking
    
    if (!".id" %in% names(df_encuesta)) {
      df_encuesta[, .id := .I]
    }
    design_unweighted <- svydesign(ids = ~.id, data = df_encuesta)
    
    population_marginals <- lapply(vars_a_ajustar, function(var) {
      ref_subset <- df_ref[get(input$col_var_ref) == var]
      setnames(ref_subset, old = c(input$col_cat_ref, input$col_total_ref), new = c(var, "Freq"))
      return(ref_subset[, c(var, "Freq"), with = FALSE])
    })
    names(population_marginals) <- vars_a_ajustar
    
    rake_formula <- as.formula(paste("~", paste(vars_a_ajustar, collapse = " + ")))
    
    design_raked <- tryCatch({
      rake(design = design_unweighted,
           sample.margins = list(rake_formula),
           population.margins = population_marginals)
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error en el Raking",
        paste("No se pudo completar el ajuste. Verifique que las categorías en su encuesta y en el archivo de referencia coincidan para las variables seleccionadas. Error original:", e$message),
        easyClose = TRUE
      ))
      return(NULL)
    })
    
    if (is.null(design_raked)) return(NULL)
    
    pesos <- weights(design_raked)
    df_ponderado <- copy(df_encuesta)
    df_ponderado[, peso_ajustado := pesos]
    
    tablas_comparacion <- lapply(vars_a_ajustar, function(var) {
      muestra_orig <- df_encuesta[, .N, by = var]
      muestra_orig[, pct_muestra := round(N / sum(N) * 100, 2)]
      
      poblacion_ref <- population_marginals[[var]]
      setnames(poblacion_ref, "Freq", "Total_Pob")
      poblacion_ref[, pct_poblacion := round(Total_Pob / sum(Total_Pob) * 100, 2)]
      
      comp_table <- merge(muestra_orig, poblacion_ref, by = var, all = TRUE)
      comp_table[, `pct_muestra` := paste0(pct_muestra, "%")]
      comp_table[, `pct_poblacion` := paste0(pct_poblacion, "%")]
      setnames(comp_table, c(var, "N", "pct_muestra", "Total_Pob", "pct_poblacion"),
               c("Categoría", "N Muestra", "% Muestra", "N Población", "% Población"))
      return(comp_table)
    })
    names(tablas_comparacion) <- vars_a_ajustar
    
    list(
      datos_ponderados = df_ponderado,
      resumen_pesos = summary(pesos),
      pesos_vector = pesos,
      tablas_comparacion = tablas_comparacion
    )
  })
  
  output$resultados_ponderacion_tabs <- renderUI({
    req(resultados_raking())
    tabBox(
      id = "tabset_ponderacion", width = 12,
      tabPanel("Resumen de Pesos",
               p("Los pesos indican cuánto 'vale' cada respuesta para que la muestra total represente a la población. Un peso de 2.5 significa que esa persona 'representa' a 2.5 personas de la población de referencia."),
               verbatimTextOutput("resumen_pesos_out"),
               plotlyOutput("hist_pesos_out"),
               helpText("Nota: Pesos muy altos o muy dispares pueden incrementar la varianza de las estimaciones. Es una buena práctica revisar los casos con pesos extremos.")
      ),
      tabPanel("Comparación de Distribuciones",
               p("Esta pestaña muestra cómo el proceso de ponderación ha ajustado la distribución de su muestra para que coincida con la de la población de referencia."),
               uiOutput("tablas_comparacion_ui")
      ),
      tabPanel("Datos Ponderados",
               p("Aquí puede previsualizar y descargar su conjunto de datos original con una nueva columna ('peso_ajustado') añadida. Use esta columna en análisis posteriores."),
               downloadButton("download_datos_ponderados", "Descargar Datos Ponderados (.csv)"),
               hr(),
               DTOutput("preview_datos_ponderados")
      )
    )
  })
  
  output$resumen_pesos_out <- renderPrint({
    req(resultados_raking()$resumen_pesos)
    cat("Resumen Estadístico de los Pesos Calculados\n\n")
    print(resultados_raking()$resumen_pesos)
  })
  
  output$hist_pesos_out <- renderPlotly({
    req(resultados_raking()$pesos_vector)
    df_pesos <- data.frame(pesos = resultados_raking()$pesos_vector)
    plot_ly(df_pesos, x = ~pesos, type = "histogram") %>%
      layout(title = "Distribución de los Pesos de Ponderación",
             xaxis = list(title = "Valor del Peso"),
             yaxis = list(title = "Frecuencia"))
  })
  
  output$tablas_comparacion_ui <- renderUI({
    req(resultados_raking()$tablas_comparacion)
    tablas <- resultados_raking()$tablas_comparacion
    lapply(names(tablas), function(var_name) {
      tagList(
        h4(paste("Comparación para la variable:", var_name)),
        renderDT({
          datatable(tablas[[var_name]], options = list(dom = 't', ordering=F), rownames = FALSE)
        })
      )
    })
  })
  
  output$preview_datos_ponderados <- renderDT({
    req(resultados_raking()$datos_ponderados)
    datatable(resultados_raking()$datos_ponderados, options = list(pageLength = 5, scrollX = TRUE), rownames = FALSE)
  })
  
  output$download_datos_ponderados <- downloadHandler(
    filename = function() {
      paste0("datos_ponderados_", Sys.Date(), ".csv")
    },
    content = function(file) {
      fwrite(resultados_raking()$datos_ponderados, file)
    }
  )
  
  # --- Lógica del Servidor para Análisis de Consistencia ---
  output$selector_control_consistencia <- renderUI({
    validate(need(datos_raw(), "Carga un archivo."))
    vars_cat <- names(datos_raw())[sapply(datos_raw(), is.factor)]
    selectInput("control_consistencia", "Variable de Control (Columnas):", choices = vars_cat)
  })
  
  output$selector_respuesta_consistencia <- renderUI({
    validate(need(datos_raw(), "Carga un archivo."))
    vars_cat <- names(datos_raw())[sapply(datos_raw(), is.factor)]
    selectInput("respuesta_consistencia", "Variable de Respuesta (Filas):", choices = vars_cat)
  })
  
  analysis_results <- eventReactive(input$analizar_consistencia, {
    req(input$control_consistencia, input$respuesta_consistencia, input$num_submuestras)
    df <- datos_raw()
    k <- input$num_submuestras
    control_var <- sym(input$control_consistencia)
    respuesta_var <- sym(input$respuesta_consistencia)
    
    df_sampled <- df %>%
      mutate(submuestra = sample(1:k, nrow(.), replace = TRUE))
    
    resultados_tabla <- df_sampled %>%
      group_by(submuestra, !!control_var) %>%
      count(!!respuesta_var, name = "n") %>%
      mutate(porcentaje = round(n / sum(n) * 100, 2)) %>%
      ungroup()
    
    resultado_estadistico <- tryCatch({
      tabla_chi <- table(df_sampled[[input$respuesta_consistencia]], df_sampled$submuestra)
      test_chi <- suppressWarnings(chisq.test(tabla_chi))
      
      p_valor <- test_chi$p.value
      interpretacion <- if (p_valor < 0.05) {
        "El p-valor es menor que 0.05. Esto sugiere que existen diferencias estadísticamente SIGNIFICATIVAS entre las submuestras. Las variaciones en los resultados probablemente no se deben al azar, lo que podría indicar una INCONSISTENCIA en los datos o un patrón real que depende de cuándo se tomó la muestra."
      } else {
        "El p-valor es mayor o igual a 0.05. Esto sugiere que NO existen diferencias estadísticamente significativas entre las submuestras. Las variaciones observadas en los resultados son probablemente producto del azar, lo que indica que los datos son CONSISTENTES."
      }
      
      list(test = test_chi, interpretacion = interpretacion)
    }, error = function(e) {
      list(test = NULL, interpretacion = paste("No se pudo realizar la prueba de Chi-cuadrado. Error:", e$message))
    })
    
    list(
      tablas = resultados_tabla,
      estadisticas = resultado_estadistico
    )
  })
  
  output$analisis_estadistico_consistencia_ui <- renderUI({
    req(analysis_results())
    box(title = "Análisis Estadístico de Consistencia (Prueba Chi-cuadrado)", status = "primary", solidHeader = TRUE, width = 12, collapsible = TRUE,
        verbatimTextOutput("texto_analisis_estadistico")
    )
  })
  
  output$texto_analisis_estadistico <- renderPrint({
    resultados <- analysis_results()
    req(resultados$estadisticas)
    
    cat("--- Interpretación de la Prueba ---\n\n")
    cat(resultados$estadisticas$interpretacion)
    cat("\n\n--- Detalles de la Prueba ---\n\n")
    if (!is.null(resultados$estadisticas$test)) {
      print(resultados$estadisticas$test)
    }
  })
  
  output$resultados_consistencia <- renderUI({
    req(analysis_results())
    k <- isolate(input$num_submuestras)
    box_width <- if (k %in% c(2, 4)) 6 else 4
    
    box_list <- lapply(1:k, function(i) {
      box(
        title = paste("Submuestra", i),
        status = "success", solidHeader = TRUE, width = box_width,
        DTOutput(outputId = paste0("submuestra_tabla_n_", i)),
        hr(),
        DTOutput(outputId = paste0("submuestra_tabla_pct_", i))
      )
    })
    tagList(box_list)
  })
  
  observeEvent(input$analizar_consistencia, {
    resultados <- analysis_results()$tablas
    k <- isolate(input$num_submuestras)
    
    for (i in 1:k) {
      local({
        my_i <- i
        sub_data <- resultados %>% filter(submuestra == my_i) %>% select(-submuestra)
        
        output[[paste0("submuestra_tabla_n_", my_i)]] <- renderDT({
          tabla_n <- sub_data %>%
            select(-porcentaje) %>%
            tidyr::pivot_wider(names_from = !!sym(input$control_consistencia), values_from = n, values_fill = 0)
          
          datatable(tabla_n,
                    caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center;', 'Tabla de Frecuencias (N)'),
                    options = list(dom = 't', searching = FALSE, ordering = FALSE), rownames = FALSE)
        })
        
        output[[paste0("submuestra_tabla_pct_", my_i)]] <- renderDT({
          tabla_pct <- sub_data %>%
            select(-n) %>%
            tidyr::pivot_wider(names_from = !!sym(input$control_consistencia), values_from = porcentaje, values_fill = 0)
          
          datatable(tabla_pct,
                    caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center;', 'Tabla de Porcentajes (%)'),
                    options = list(dom = 't', searching = FALSE, ordering = FALSE), rownames = FALSE)
        })
      })
    }
  })
  
  # --- Análisis Univariado ---
  tabla_uni <- shiny::bindCache(
    reactive({
      req(input$variable_uni, datos_raw())
      variable_seleccionada <- input$variable_uni
      df <- datos_raw()
      if (!is.factor(df[[variable_seleccionada]]) && !is.character(df[[variable_seleccionada]])) return(NULL)
      df_clean <- df[!is.na(get(variable_seleccionada))]
      frecuencias <- df_clean[, .N, by = get(variable_seleccionada)]
      data.table::setnames(frecuencias, old = names(frecuencias)[1], new = "Categoría")
      frecuencias[, porcentaje := round(N / sum(N) * 100, 2)]
      frecuencias
    }),
    input$variable_uni, datos_raw()
  )
  
  output$grafico_univariado <- renderPlotly({
    df <- tabla_uni()
    validate(need(!is.null(df), "La variable seleccionada no es categórica. Por favor, elige una variable de tipo factor o carácter."))
    if (identical(input$tipo_grafico, "barras")) {
      plot_ly(df, x = ~Categoría, y = ~N, type = "bar", color = ~Categoría) %>%
        layout(title = paste("Distribución de", input$variable_uni),
               xaxis = list(title = input$variable_uni, showgrid = FALSE),
               yaxis = list(title = "Frecuencia", showgrid = TRUE))
    } else {
      plot_ly(df, labels = ~Categoría, values = ~N, type = "pie") %>%
        layout(title = paste("Distribución de", input$variable_uni))
    }
  })
  
  output$tabla_univariada <- renderDT({
    df <- tabla_uni()
    validate(need(!is.null(df), "La variable seleccionada no es categórica."))
    cols <- "Categoría"
    if (input$mostrar_n) cols <- c(cols, "N")
    if (input$mostrar_porcentaje) cols <- c(cols, "porcentaje")
    datatable(df[, ..cols], options = list(pageLength = 10, dom = 't', searching = FALSE, ordering = FALSE), rownames = FALSE,
              caption = paste("Tabla de Frecuencias de:", input$variable_uni))
  })
  
  # --- Análisis Bivariado (Contingencia) ---
  tabla_cont <- shiny::bindCache(
    reactive({
      req(input$variable_x, input$variable_y, datos_raw())
      df <- datos_raw()
      if (!is.factor(df[[input$variable_x]])) return(NULL)
      if (!is.factor(df[[input$variable_y]])) return(NULL)
      xt <- table(df[[input$variable_x]], df[[input$variable_y]])
      dt_cont <- as.data.table(xt)
      data.table::setnames(dt_cont, c("V1", "V2", "N"), c("Fila", "Columna", "N"))
      dt_cont
    }),
    input$variable_x, input$variable_y, datos_raw()
  )
  
  output$tabla_contingencia_n <- renderDT({
    dt_cont <- tabla_cont()
    validate(need(!is.null(dt_cont), "Por favor, selecciona dos variables categóricas."))
    datatable(dcast(dt_cont, Fila ~ Columna, value.var = "N", fill = 0),
              options = list(scrollX = TRUE, pageLength = 10, searching = FALSE, ordering = FALSE),
              rownames = FALSE,
              caption = paste("Frecuencias (N) para", input$variable_x, "y", input$variable_y))
  })
  
  output$tabla_contingencia_col <- renderDT({
    dt_cont <- tabla_cont()
    validate(need(!is.null(dt_cont), "Por favor, selecciona dos variables categóricas."))
    wide <- dcast(dt_cont, Fila ~ Columna, value.var = "N", fill = 0)
    matriz <- as.matrix(wide[, -1])
    porcentajes <- prop.table(matriz, 2) * 100
    df_result <- cbind(Fila = wide$Fila, as.data.frame(round(porcentajes, 2)))
    datatable(df_result,
              options = list(scrollX = TRUE, pageLength = 10, searching = FALSE, ordering = FALSE),
              rownames = FALSE,
              caption = paste("Porcentajes por Columna para", input$variable_x, "y", input$variable_y))
  })
  
  output$tabla_contingencia_fila <- renderDT({
    dt_cont <- tabla_cont()
    validate(need(!is.null(dt_cont), "Por favor, selecciona dos variables categóricas."))
    wide <- dcast(dt_cont, Fila ~ Columna, value.var = "N", fill = 0)
    matriz <- as.matrix(wide[, -1])
    porcentajes <- prop.table(matriz, 1) * 100
    df_result <- cbind(Fila = wide$Fila, as.data.frame(round(porcentajes, 2)))
    datatable(df_result,
              options = list(scrollX = TRUE, pageLength = 10, searching = FALSE, ordering = FALSE),
              rownames = FALSE,
              caption = paste("Porcentajes por Fila para", input$variable_x, "y", input$variable_y))
  })
  
  output$tabla_contingencia_total <- renderDT({
    dt_cont <- tabla_cont()
    validate(need(!is.null(dt_cont), "Por favor, selecciona dos variables categóricas."))
    wide <- dcast(dt_cont, Fila ~ Columna, value.var = "N", fill = 0)
    matriz <- as.matrix(wide[, -1])
    porcentajes <- prop.table(matriz) * 100
    df_result <- cbind(Fila = wide$Fila, as.data.frame(round(porcentajes, 2)))
    datatable(df_result,
              options = list(scrollX = TRUE, pageLength = 10, searching = FALSE, ordering = FALSE),
              rownames = FALSE,
              caption = paste("Porcentajes Totales para", input$variable_x, "y", input$variable_y))
  })
  
  # --- Módulo de Análisis de Texto ---
  texto_filtrado <- shiny::bindCache(
    reactive({
      req(input$variable_texto, datos_raw())
      df <- datos_raw()
      
      if (input$variable_cruce != "" && !is.null(input$categoria_cruce)) {
        df <- df[get(input$variable_cruce) == input$categoria_cruce]
      }
      
      text_data <- df[[input$variable_texto]]
      text_data <- tolower(text_data)
      text_data <- str_remove_all(text_data, "[[:punct:]]")
      text_data <- str_remove_all(text_data, "[[:digit:]]")
      text_data <- removeWords(text_data, stopwords("es"))
      text_data <- trimws(text_data)
      text_data <- text_data[text_data != ""]
      
      text_data
    }),
    input$variable_texto, input$variable_cruce, input$categoria_cruce, datos_raw()
  )
  
  output$nube_palabras <- renderPlot({
    req(input$variable_texto)
    input$actualizar_nube
    
    isolate({
      text_data <- texto_filtrado()
      if (length(text_data) == 0) return(NULL)
      corpus <- VCorpus(VectorSource(text_data))
      tdm <- TermDocumentMatrix(corpus)
      m <- as.matrix(tdm)
      v <- sort(rowSums(m), decreasing = TRUE)
      d <- data.frame(word = names(v), freq = v)
      wordcloud(words = d$word, freq = d$freq, min.freq = 1, max.words = input$max_palabras,
                random.order = FALSE, rot.per = 0.35, colors = brewer.pal(8, "Dark2"))
    })
  })
  
  output$tabla_frecuencia_palabras <- renderDT({
    req(input$variable_texto)
    text_data <- texto_filtrado()
    if (length(text_data) == 0) {
      return(datatable(data.frame(Palabra = "No hay datos de texto para mostrar."), rownames = FALSE))
    }
    corpus <- VCorpus(VectorSource(text_data))
    tdm <- TermDocumentMatrix(corpus)
    m <- as.matrix(tdm)
    v <- sort(rowSums(m), decreasing = TRUE)
    df <- data.frame(Palabra = names(v), Frecuencia = v, row.names = NULL)
    datatable(df, options = list(pageLength = 10, dom = 'tip', searching = TRUE, ordering = TRUE))
  })
  
  output$resumen_sentimiento <- renderPrint({
    req(input$variable_texto)
    text_data <- texto_filtrado()
    if (length(text_data) == 0) {
      cat("No hay datos de texto para realizar el análisis de sentimiento.")
      return(NULL)
    }
    sentimientos_espanol <- data.frame(
      word = c("excelente", "bueno", "agradable", "genial", "satisfecho", "positivo", "increíble", "magnífico",
               "malo", "pésimo", "terrible", "decepcionado", "negativo", "triste", "horror", "desastre", "insatisfecho"),
      sentimiento = c(rep("positivo", 8), rep("negativo", 9)),
      score = c(3, 2, 2, 3, 2, 1, 3, 3, -3, -3, -2, -2, -1, -2, -3, -2, -2)
    )
    df_text <- tibble(respuesta_id = seq_along(text_data), text = text_data) %>% unnest_tokens(word, text)
    df_sentimiento <- df_text %>% left_join(sentimientos_espanol, by = "word") %>% filter(!is.na(sentimiento))
    puntajes <- df_sentimiento %>% group_by(respuesta_id) %>% summarise(puntaje_original = sum(score, na.rm = TRUE))
    puntajes_con_sesgo <- puntajes %>% mutate(puntaje_ajustado = puntaje_original + input$sesgo_sentimiento)
    df_final <- df_text %>% group_by(respuesta_id) %>% slice(1) %>% ungroup() %>%
      left_join(puntajes_con_sesgo, by = "respuesta_id") %>%
      mutate(puntaje_ajustado = ifelse(is.na(puntaje_ajustado), 0 + input$sesgo_sentimiento, puntaje_ajustado))
    
    promedio_sentimiento <- mean(df_final$puntaje_ajustado, na.rm = TRUE)
    positivos <- sum(df_final$puntaje_ajustado > 0)
    negativos <- sum(df_final$puntaje_ajustado < 0)
    neutros <- sum(df_final$puntaje_ajustado == 0)
    
    cat("Resumen de Análisis de Sentimiento Ponderado:\n\n")
    cat("Respuestas analizadas:", nrow(df_final), "\n")
    cat("Puntaje promedio del sentimiento:", round(promedio_sentimiento, 2), "\n\n")
    cat("Clasificación de respuestas (con sesgo de", input$sesgo_sentimiento, "):\n")
    cat("- Positivas:", positivos, "(", round(positivos/nrow(df_final)*100, 2), "%)\n")
    cat("- Negativas:", negativos, "(", round(negativos/nrow(df_final)*100, 2), "%)\n")
    cat("- Neutras:", neutros, "(", round(neutros/nrow(df_final)*100, 2), "%)\n")
    cat("\n----------------------------------\n")
    if (promedio_sentimiento > 0.5) {
      cat("Conclusión: El sentimiento general es POSITIVO. ✅\n")
    } else if (promedio_sentimiento < -0.5) {
      cat("Conclusión: El sentimiento general es NEGATIVO. ❌\n")
    } else {
      cat("Conclusión: El sentimiento general es NEUTRO. 😐\n")
    }
  })
  
  # --- Vista de Datos ---
  output$tabla_datos <- renderDT({
    validate(need(datos_raw(), "Por favor, carga un archivo para ver los datos."))
    datatable(datos_raw(), options = list(pageLength = 25, scrollX = TRUE), filter = "top", rownames = FALSE)
  })
  
  output$resumen_datos <- renderPrint({
    validate(need(datos_raw(), "Por favor, carga un archivo para ver el resumen."))
    df <- datos_raw()
    cat("Resumen de Datos\n\n")
    cat("Observaciones:", nrow(df), "\n")
    cat("Variables:", ncol(df), "\n")
    cat("Nombres de variables:", paste(names(df), collapse = ", "), "\n")
    cat("\nEstructura de los datos (str()):\n")
    str(df)
  })
}

# -------------------------------------------------------------------------
# Iniciar la aplicación
# -------------------------------------------------------------------------
shinyApp(ui, server)
