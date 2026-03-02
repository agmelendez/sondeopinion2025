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
                    p("Esta aplicación te permite cargar un archivo de encuesta (.xlsx o .csv) para realizar análisis univariados, bivariados y de texto."),
                    tags$ul(
                      tags$li("Usa la pestaña 'Cargar Datos' para subir tu archivo."),
                      tags$li("El 'Control de Calidad' te ayuda a identificar duplicados y patrones en los tiempos de respuesta."),
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
              # --- INICIO DE LA MODIFICACIÓN: Paneles condicionales ---
              # Panel para el análisis general (se muestra por defecto)
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
              # Panel para el análisis por día (se muestra al activar el checkbox)
              conditionalPanel(
                condition = "input.activar_analisis_por_dia == true",
                uiOutput("resultados_por_dia_ui")
              )
              # --- FIN DE LA MODIFICACIÓN ---
      ),
      
      # --- Módulo de Análisis de Consistencia ---
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
                # --- INICIO DE LA MODIFICACIÓN: Añadir UI para el análisis estadístico ---
                uiOutput("analisis_estadistico_consistencia_ui")
                # --- FIN DE LA MODIFICACIÓN ---
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
                    numericInput("sesgo_sentimiento", "Ajustar Sesgo (valor entre -5 y 5):", 
                                 value = 0, min = -5, max = 5, step = 0.5),
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
  
  # Carga y limpieza de datos (se ejecuta una sola vez al subir el archivo)
  datos_raw <- reactive({
    req(input$archivo_excel)
    ext <- tools::file_ext(input$archivo_excel$name)
    
    dt <- if (tolower(ext) == "csv") {
      data.table::fread(input$archivo_excel$datapath, encoding = "UTF-8")
    } else if (tolower(ext) %in% c("xls", "xlsx")) {
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
    
    # Pre-procesamiento para corregir formatos de fecha/hora
    arreglar_formato_hora <- function(columna) {
      if (is.POSIXct(columna) || is.Date(columna)) return(columna)
      if (is.numeric(columna)) {
        if (all(na.omit(columna) > 30000 & na.omit(columna) < 60000)) {
          return(as.POSIXct(columna * 86400, origin = "1899-12-30", tz = "UTC"))
        }
      }
      if (is.character(columna) || is.factor(columna)) {
        columna_char <- as.character(columna)
        parsed_dates <- parse_date_time(columna_char, 
                                        orders = c("Ymd HMS", "dmy HMS", "mdy HMS", "HMS", "HM", "Ymd", "dmy", "mdy"),
                                        quiet = TRUE)
        if (sum(!is.na(parsed_dates)) / length(parsed_dates) > 0.7) {
          return(parsed_dates)
        }
      }
      return(columna)
    }
    nombres_cols <- names(dt)
    patrones_hora <- c("hora", "fecha", "inicio", "fin", "time", "date", "start", "end")
    cols_a_revisar <- nombres_cols[grepl(paste(patrones_hora, collapse = "|"), nombres_cols, ignore.case = TRUE)]
    if (length(cols_a_revisar) > 0) {
      dt[, (cols_a_revisar) := lapply(.SD, arreglar_formato_hora), .SDcols = cols_a_revisar]
    }
    
    char_cols <- names(dt)[sapply(dt, is.character)]
    if (length(char_cols) > 0) {
      dt[, (char_cols) := lapply(.SD, function(x) {
        if (length(unique(x)) < 50) {
          factor(x)
        } else {
          as.character(x)
        }
      }), .SDcols = char_cols]
    }
    return(dt)
  })
  
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
  
  # --- INICIO DE LA MODIFICACIÓN: UI para activar el análisis por día ---
  output$filtro_dia_calidad_ui <- renderUI({
    req(input$fecha_calidad)
    box(title = "Análisis Comparativo por Día", status = "primary", solidHeader = FALSE, width = 12, collapsible = TRUE, collapsed = TRUE,
        checkboxInput("activar_analisis_por_dia", "Generar análisis por cada día de aplicación", value = FALSE),
        helpText("Al activar esta opción, se generarán los análisis de calidad para cada fecha única encontrada en la columna seleccionada. Esto puede tardar unos segundos si hay muchos días diferentes.")
    )
  })
  # --- FIN DE LA MODIFICACIÓN ---
  
  # --- Lógica del Servidor para Control de Calidad ---
  
  # Datos para el análisis general (cuando el checkbox no está activo)
  datos_filtrados_calidad <- reactive({
    validate(need(datos_raw(), "Carga un archivo."))
    return(datos_raw())
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
    validate(need(nrow(duplicados) > 0, "No se encontraron identificadores duplicados. ¡Buenas noticias!"))
    datatable(duplicados, caption = "Tabla de Identificadores Duplicados",
              options = list(pageLength = 5, searching = TRUE, ordering = TRUE), rownames = FALSE)
  })
  
  tiempos_data <- reactive({
    req(input$hora_inicio_calidad, input$hora_fin_calidad, input$fecha_calidad)
    df <- datos_filtrados_calidad()
    
    start_col_name <- input$hora_inicio_calidad
    end_col_name <- input$hora_fin_calidad
    date_col_name <- input$fecha_calidad
    
    validate(
      need(all(c(start_col_name, end_col_name, date_col_name) %in% names(df)), "Una o más columnas de fecha/hora seleccionadas no existen."),
      need(inherits(df[[start_col_name]], "POSIXt"), "La columna de Hora Inicio no es un formato de hora válido."),
      need(inherits(df[[end_col_name]], "POSIXt"), "La columna de Hora Fin no es un formato de hora válido."),
      need(inherits(df[[date_col_name]], "POSIXt") || inherits(df[[date_col_name]], "Date"), "La columna de Fecha no es un formato de fecha válido.")
    )
    
    fecha_base <- as.Date(df[[date_col_name]])
    hora_inicio_str <- format(df[[start_col_name]], "%H:%M:%S")
    hora_fin_str <- format(df[[end_col_name]], "%H:%M:%S")
    
    start_datetime <- ymd_hms(paste(fecha_base, hora_inicio_str), quiet = TRUE)
    end_datetime <- ymd_hms(paste(fecha_base, hora_fin_str), quiet = TRUE)
    
    indices_medianoche <- which(end_datetime < start_datetime)
    if (length(indices_medianoche) > 0) {
      end_datetime[indices_medianoche] <- end_datetime[indices_medianoche] + days(1)
    }
    
    duracion_min <- as.numeric(difftime(end_datetime, start_datetime, units = "mins"))
    duracion_final <- na.omit(duracion_min)
    
    validate(need(length(duracion_final) > 0, "No se pudo calcular la duración. Verifique que las columnas de fecha y hora sean correctas."))
    return(data.frame(duracion = duracion_final))
  })
  
  output$resumen_tiempos <- renderPrint({
    df_tiempos <- tiempos_data()
    validate(need(nrow(df_tiempos) > 0, "No se pudo calcular la duración."))
    cat("Resumen de Tiempos de Respuesta (en minutos)\n\n")
    summary(df_tiempos$duracion)
  })
  
  output$grafico_tiempos <- renderPlotly({
    df_tiempos <- tiempos_data()
    validate(need(nrow(df_tiempos) > 0, "No hay datos de tiempo para graficar."))
    plot_ly(df_tiempos, x = ~duracion, type = "histogram") %>%
      layout(title = "Distribución de Tiempos de Respuesta",
             xaxis = list(title = "Duración (minutos)"),
             yaxis = list(title = "Cantidad de Encuestas"))
  })
  
  bloques_data <- reactive({
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
          hora >= 6 & hora < 12 ~ "1. Mañana (6am - 12pm)",
          hora >= 12 & hora < 19 ~ "2. Tarde (12pm - 7pm)",
          hora >= 19 & hora < 23 ~ "3. Noche (7pm - 11pm)",
          TRUE ~ "4. Madrugada (11pm - 6am)"
        )
      ) %>%
      count(bloque, name = "Total") %>%
      arrange(bloque)
    return(df_bloques)
  })
  
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
  
  # --- INICIO DE LA MODIFICACIÓN: Lógica para generar análisis por día ---
  observeEvent(input$activar_analisis_por_dia, {
    if (input$activar_analisis_por_dia == TRUE) {
      
      # Generar la UI dinámica
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
      
      # Renderizar los outputs para cada día
      df <- datos_raw()
      fecha_col_name <- input$fecha_calidad
      dias_unicos <- sort(unique(as.Date(df[[fecha_col_name]])))
      
      for (dia_actual in dias_unicos) {
        local({
          dia_loop <- as.Date(dia_actual, origin = "1970-01-01")
          dia_str_loop <- as.character(dia_loop)
          
          datos_dia <- df[as.Date(get(fecha_col_name)) == dia_loop, ]
          
          # 1. Duplicados por día
          output[[paste0("duplicados_", dia_str_loop)]] <- renderDT({
            id_col <- input$id_calidad
            duplicados_dia <- datos_dia %>% count(!!sym(id_col), name = "Frecuencia") %>% filter(Frecuencia > 1)
            if (nrow(duplicados_dia) > 0) {
              datatable(duplicados_dia, options = list(pageLength = 3, searching = FALSE), rownames = FALSE)
            } else {
              datatable(data.frame(Resultado = "No se encontraron duplicados para este día."), rownames = FALSE, options = list(dom = 't'))
            }
          })
          
          # 2. Tiempos por día
          output[[paste0("resumen_tiempos_", dia_str_loop)]] <- renderPrint({
            # Reutiliza la lógica de cálculo de tiempo
            start_dt <- ymd_hms(paste(as.Date(datos_dia[[input$fecha_calidad]]), format(datos_dia[[input$hora_inicio_calidad]], "%H:%M:%S")))
            end_dt <- ymd_hms(paste(as.Date(datos_dia[[input$fecha_calidad]]), format(datos_dia[[input$hora_fin_calidad]], "%H:%M:%S")))
            indices_medianoche <- which(end_dt < start_dt)
            if (length(indices_medianoche) > 0) { end_dt[indices_medianoche] <- end_dt[indices_medianoche] + days(1) }
            duracion <- as.numeric(difftime(end_dt, start_dt, units = "mins"))
            summary(na.omit(duracion))
          })
          output[[paste0("grafico_tiempos_", dia_str_loop)]] <- renderPlotly({
            start_dt <- ymd_hms(paste(as.Date(datos_dia[[input$fecha_calidad]]), format(datos_dia[[input$hora_inicio_calidad]], "%H:%M:%S")))
            end_dt <- ymd_hms(paste(as.Date(datos_dia[[input$fecha_calidad]]), format(datos_dia[[input$hora_fin_calidad]], "%H:%M:%S")))
            indices_medianoche <- which(end_dt < start_dt)
            if (length(indices_medianoche) > 0) { end_dt[indices_medianoche] <- end_dt[indices_medianoche] + days(1) }
            duracion <- as.numeric(difftime(end_dt, start_dt, units = "mins"))
            plot_ly(data.frame(duracion = na.omit(duracion)), x = ~duracion, type = "histogram") %>% layout(xaxis=list(title="Minutos"), yaxis=list(title="Frecuencia"))
          })
          
          # 3. Bloques horarios por día
          bloques_dia_data <- datos_dia %>%
            mutate(hora = hour(get(input$hora_inicio_calidad))) %>%
            mutate(bloque = case_when(
              hora >= 6 & hora < 12 ~ "1. Mañana", hora >= 12 & hora < 19 ~ "2. Tarde",
              hora >= 19 & hora < 23 ~ "3. Noche", TRUE ~ "4. Madrugada"
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
  # --- FIN DE LA MODIFICACIÓN ---
  
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
    
    # --- INICIO DE LA MODIFICACIÓN: Cálculo de Chi-cuadrado ---
    tryCatch({
      tabla_chi <- table(df_sampled[[input$respuesta_consistencia]], df_sampled$submuestra)
      test_chi <- chisq.test(tabla_chi)
      
      p_valor <- test_chi$p.value
      interpretacion <- if (p_valor < 0.05) {
        "El p-valor es menor que 0.05. Esto sugiere que existen diferencias estadísticamente SIGNIFICATIVAS entre las submuestras. Las variaciones en los resultados probablemente no se deben al azar, lo que podría indicar una INCONSISTENCIA en los datos o un patrón real que depende de cuándo se tomó la muestra."
      } else {
        "El p-valor es mayor o igual a 0.05. Esto sugiere que NO existen diferencias estadísticamente significativas entre las submuestras. Las variaciones observadas en los resultados son probablemente producto del azar, lo que indica que los datos son CONSISTENTES."
      }
      
      resultado_estadistico <- list(
        test = test_chi,
        interpretacion = interpretacion
      )
    }, error = function(e) {
      resultado_estadistico <- list(
        test = NULL,
        interpretacion = paste("No se pudo realizar la prueba de Chi-cuadrado. Error:", e$message)
      )
    })
    # --- FIN DE LA MODIFICACIÓN ---
    
    return(list(
      tablas = resultados_tabla,
      estadisticas = resultado_estadistico,
      df_sampled = df_sampled # Devolver también los datos muestreados
    ))
  })
  
  # --- INICIO DE LA MODIFICACIÓN: UI para el análisis estadístico ---
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
  # --- FIN DE LA MODIFICACIÓN ---
  
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
  tabla_uni <- reactive({
    req(input$variable_uni, datos_raw())
    variable_seleccionada <- input$variable_uni
    df <- datos_raw()
    if (!is.factor(df[[variable_seleccionada]]) && !is.character(df[[variable_seleccionada]])) return(NULL)
    df_clean <- df[!is.na(get(variable_seleccionada))]
    frecuencias <- df_clean[, .N, by = get(variable_seleccionada)]
    setnames(frecuencias, old = names(frecuencias)[1], new = "Categoría")
    frecuencias[, porcentaje := round(N / sum(N) * 100, 2)]
    return(frecuencias)
  })
  
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
    datatable(df[, ..cols], options = list(pageLength = 10, dom = 't', searching = FALSE, ordering = FALSE), rownames = FALSE, caption = paste("Tabla de Frecuencias de:", input$variable_uni))
  })
  
  # --- Análisis Bivariado (Contingencia) ---
  tabla_cont <- reactive({
    req(input$variable_x, input$variable_y, datos_raw())
    df <- datos_raw()
    if (!is.factor(df[[input$variable_x]])) return(NULL)
    if (!is.factor(df[[input$variable_y]])) return(NULL)
    xt <- table(df[[input$variable_x]], df[[input$variable_y]])
    dt_cont <- as.data.table(xt)
    data.table::setnames(dt_cont, c("V1", "V2", "N"), c("Fila", "Columna", "N"))
    return(dt_cont)
  })
  
  output$tabla_contingencia_n <- renderDT({
    dt_cont <- tabla_cont()
    validate(need(!is.null(dt_cont), "Por favor, selecciona dos variables categóricas."))
    datatable(dcast(dt_cont, Fila ~ Columna, value.var = "N", fill = 0), options = list(scrollX = TRUE, pageLength = 10, searching = FALSE, ordering = FALSE), rownames = FALSE, caption = paste("Frecuencias (N) para", input$variable_x, "y", input$variable_y))
  })
  
  output$tabla_contingencia_col <- renderDT({
    dt_cont <- tabla_cont()
    validate(need(!is.null(dt_cont), "Por favor, selecciona dos variables categóricas."))
    matriz <- dcast(dt_cont, Fila ~ Columna, value.var = "N", fill = 0)[, -1]
    porcentajes <- prop.table(as.matrix(matriz), 2) * 100
    df_result <- cbind(Fila = dcast(dt_cont, Fila ~ Columna, value.var = "N", fill = 0)$Fila, as.data.frame(round(porcentajes, 2)))
    datatable(df_result, options = list(scrollX = TRUE, pageLength = 10, searching = FALSE, ordering = FALSE), rownames = FALSE, caption = paste("Porcentajes por Columna para", input$variable_x, "y", input$variable_y))
  })
  
  output$tabla_contingencia_fila <- renderDT({
    dt_cont <- tabla_cont()
    validate(need(!is.null(dt_cont), "Por favor, selecciona dos variables categóricas."))
    matriz <- dcast(dt_cont, Fila ~ Columna, value.var = "N", fill = 0)[, -1]
    porcentajes <- prop.table(as.matrix(matriz), 1) * 100
    df_result <- cbind(Fila = dcast(dt_cont, Fila ~ Columna, value.var = "N", fill = 0)$Fila, as.data.frame(round(porcentajes, 2)))
    datatable(df_result, options = list(scrollX = TRUE, pageLength = 10, searching = FALSE, ordering = FALSE), rownames = FALSE, caption = paste("Porcentajes por Fila para", input$variable_x, "y", input$variable_y))
  })
  
  output$tabla_contingencia_total <- renderDT({
    dt_cont <- tabla_cont()
    validate(need(!is.null(dt_cont), "Por favor, selecciona dos variables categóricas."))
    matriz <- dcast(dt_cont, Fila ~ Columna, value.var = "N", fill = 0)[, -1]
    porcentajes <- prop.table(as.matrix(matriz)) * 100
    df_result <- cbind(Fila = dcast(dt_cont, Fila ~ Columna, value.var = "N", fill = 0)$Fila, as.data.frame(round(porcentajes, 2)))
    datatable(df_result, options = list(scrollX = TRUE, pageLength = 10, searching = FALSE, ordering = FALSE), rownames = FALSE, caption = paste("Porcentajes Totales para", input$variable_x, "y", input$variable_y))
  })
  
  # --- Módulo de Análisis de Texto ---
  texto_filtrado <- reactive({
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
    
    return(text_data)
  })
  
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
