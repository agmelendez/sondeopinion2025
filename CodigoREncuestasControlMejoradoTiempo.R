# app.R

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
library(lubridate) # <--- LIBRERÍA AÑADIDA

# -------------------------------------------------------------------------
# Interfaz de Usuario (UI)
# -------------------------------------------------------------------------
ui <- dashboardPage(
  dashboardHeader(title = "Análisis de Encuestas"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Inicio", tabName = "inicio", icon = icon("home")),
      menuItem("Cargar Datos", tabName = "cargar", icon = icon("upload")),
      menuItem("Control de Calidad", tabName = "calidad", icon = icon("check-circle")), # <-- NUEVO MÓDULO
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
      
      # -------------------------------------------------------------------
      # --- Módulo de Control de Calidad ---
      # -------------------------------------------------------------------
      tabItem("calidad",
              fluidRow(
                box(title = "Controles de Calidad", status = "primary", solidHeader = TRUE, width = 12,
                    p("Selecciona las columnas adecuadas para realizar el análisis de calidad de los datos."),
                    # --- AJUSTE: Se eliminó el cuarto selector ---
                    column(4, uiOutput("selector_id_calidad")),
                    column(4, uiOutput("selector_hora_inicio_calidad")),
                    column(4, uiOutput("selector_hora_fin_calidad"))
                )
              ),
              fluidRow(
                box(title = "1. Detección de Duplicados por Identificador", status = "warning", solidHeader = TRUE, width = 12,
                    p("Esta tabla muestra los identificadores que aparecen más de una vez en los datos, lo que podría indicar que una persona respondió varias veces."),
                    DTOutput("tabla_duplicados")
                )
              ),
              fluidRow(
                box(title = "2. Análisis de Tiempos de Respuesta", status = "info", solidHeader = TRUE, width = 6,
                    p("El resumen muestra la distribución del tiempo (en minutos) que tardaron en completar la encuesta. Tiempos muy cortos o muy largos pueden ser sospechosos."),
                    verbatimTextOutput("resumen_tiempos"),
                    plotlyOutput("grafico_tiempos", height = "300px")
                ),
                box(title = "3. Respuestas por Bloque Horario", status = "info", solidHeader = TRUE, width = 6,
                    p("Este gráfico muestra en qué momento del día se recibieron más respuestas, lo que puede ayudar a detectar patrones inusuales (ej. muchas respuestas en la madrugada)."),
                    plotlyOutput("grafico_bloques_horarios", height = "300px"),
                    DTOutput("tabla_bloques_horarios")
                )
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
    
    # --- INICIO DE LA MODIFICACIÓN: Pre-procesamiento para corregir formatos de hora ---
    # Esta función intenta convertir una columna a formato POSIXct desde varios formatos comunes.
    arreglar_formato_hora <- function(columna) {
      # Si ya es un formato de fecha/hora, no hacer nada.
      if (is.POSIXct(columna) || is.Date(columna)) {
        return(columna)
      }
      
      # Intento 1: Formato numérico de Excel.
      # Excel cuenta los días desde 1899-12-30. Si es numérico y parece una fecha de Excel, se convierte.
      if (is.numeric(columna)) {
        # Se asume que son fechas si los números están en un rango plausible (ej: entre 1990 y 2050).
        # 32874 es 1990-01-01. 54787 es 2050-01-01.
        if (all(na.omit(columna) > 30000 & na.omit(columna) < 60000)) {
          return(as.POSIXct(columna * 86400, origin = "1899-12-30", tz = "UTC"))
        }
      }
      
      # Intento 2: Formatos de texto.
      if (is.character(columna) || is.factor(columna)) {
        columna_char <- as.character(columna)
        # Se usa `parse_date_time` con una lista de formatos comunes.
        # `quiet = TRUE` evita que se impriman warnings en la consola por cada fallo.
        parsed_dates <- parse_date_time(columna_char, 
                                        orders = c("Ymd HMS", "dmy HMS", "mdy HMS", "HMS", "HM"),
                                        quiet = TRUE)
        # Solo se devuelve el resultado si la mayoría de las conversiones fueron exitosas.
        # Esto evita convertir por error columnas de texto que no son fechas.
        if (sum(!is.na(parsed_dates)) / length(parsed_dates) > 0.7) {
          return(parsed_dates)
        }
      }
      
      # Si ninguna conversión funcionó, devolver la columna original.
      return(columna)
    }
    
    # Identificar columnas que probablemente contengan fechas u horas por su nombre.
    nombres_cols <- names(dt)
    patrones_hora <- c("hora", "fecha", "inicio", "fin", "time", "date", "start", "end")
    cols_a_revisar <- nombres_cols[grepl(paste(patrones_hora, collapse = "|"), nombres_cols, ignore.case = TRUE)]
    
    # Aplicar la función de corrección a las columnas identificadas.
    if (length(cols_a_revisar) > 0) {
      dt[, (cols_a_revisar) := lapply(.SD, arreglar_formato_hora), .SDcols = cols_a_revisar]
    }
    # --- FIN DE LA MODIFICACIÓN ---
    
    # Convertir columnas de texto a factores (si tienen pocas categorías)
    char_cols <- names(dt)[sapply(dt, is.character)]
    if (length(char_cols) > 0) {
      dt[, (char_cols) := lapply(.SD, function(x) {
        if (length(unique(x)) < 50) { # Umbral para variables categóricas
          factor(x)
        } else {
          as.character(x)
        }
      }), .SDcols = char_cols]
    }
    return(dt)
  })
  
  # Controles dinámicos para los selectores
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
    # Intenta pre-seleccionar columnas que contengan "inicio" o "start"
    choices <- names(datos_raw())
    pre_selected <- choices[grepl("inicio|start", choices, ignore.case = TRUE)][1]
    selectInput("hora_inicio_calidad", "Columna Hora Inicio:", choices = choices, selected = pre_selected)
  })
  
  output$selector_hora_fin_calidad <- renderUI({
    validate(need(datos_raw(), "Carga un archivo."))
    # Intenta pre-seleccionar columnas que contengan "fin" o "end"
    choices <- names(datos_raw())
    pre_selected <- choices[grepl("fin|end", choices, ignore.case = TRUE)][1]
    selectInput("hora_fin_calidad", "Columna Hora Fin:", choices = choices, selected = pre_selected)
  })
  
  # -------------------------------------------------------------------
  # --- Lógica del Servidor para Control de Calidad ---
  # -------------------------------------------------------------------
  
  # 1. Detección de duplicados
  output$tabla_duplicados <- renderDT({
    req(input$id_calidad)
    df <- datos_raw()
    id_col <- input$id_calidad
    
    validate(need(id_col %in% names(df), "La columna de identificador no existe."))
    
    duplicados <- df %>%
      count(!!sym(id_col), name = "Frecuencia") %>%
      filter(Frecuencia > 1) %>%
      arrange(desc(Frecuencia))
    
    validate(need(nrow(duplicados) > 0, "No se encontraron identificadores duplicados. ¡Buenas noticias!"))
    
    datatable(duplicados,
              caption = "Tabla de Identificadores Duplicados",
              options = list(pageLength = 5, searching = TRUE, ordering = TRUE),
              rownames = FALSE)
  })
  
  # 2. Análisis de tiempos de respuesta
  tiempos_data <- reactive({
    req(input$hora_inicio_calidad, input$hora_fin_calidad)
    df <- datos_raw()
    start_col <- input$hora_inicio_calidad
    end_col <- input$hora_fin_calidad
    
    validate(
      need(start_col %in% names(df) && end_col %in% names(df), "Las columnas de hora de inicio o fin no existen."),
      need(is.POSIXct(df[[start_col]]) && is.POSIXct(df[[end_col]]), "Las columnas de hora seleccionadas no tienen un formato de fecha/hora válido. La conversión automática falló.")
    )
    
    start_time <- df[[start_col]]
    end_time <- df[[end_col]]
    
    # Calcular duración en minutos
    duracion_min <- as.numeric(difftime(end_time, start_time, units = "mins"))
    
    # --- INICIO DE LA CORRECCIÓN: Ajuste para encuestas que cruzan la medianoche ---
    # Si la duración es negativa, significa que la encuesta terminó al día siguiente.
    # Se suman 24 horas (1440 minutos) para corregir el cálculo.
    duracion_corregida <- ifelse(duracion_min < 0, duracion_min + 1440, duracion_min)
    # --- FIN DE LA CORRECCIÓN ---
    
    return(data.frame(duracion = duracion_corregida))
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
             # --- INICIO DE LA MODIFICACIÓN: Ajuste del eje X ---
             xaxis = list(title = "Duración (minutos)",
                          tickmode = "linear", # Asegura que los ticks sean lineales
                          tick0 = 0,          # Empieza los ticks en 0
                          dtick = 60),        # Pone una marca cada 60 minutos
             # --- FIN DE LA MODIFICACIÓN ---
             yaxis = list(title = "Cantidad de Encuestas"))
  })
  
  # 3. Análisis de bloques horarios
  bloques_data <- reactive({
    req(input$hora_inicio_calidad)
    df <- datos_raw()
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
    datatable(df_bloques,
              caption = "Frecuencia por Bloque Horario",
              options = list(dom = 't', searching = FALSE, ordering = FALSE),
              rownames = FALSE)
  })
  
  # --- Análisis Univariado ---
  tabla_uni <- reactive({
    req(input$variable_uni, datos_raw())
    variable_seleccionada <- input$variable_uni
    df <- datos_raw()
    
    if (!is.factor(df[[variable_seleccionada]]) && !is.character(df[[variable_seleccionada]])) {
      return(NULL)
    }
    
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
    validate(need(!is.null(df), "La variable seleccionada no es categórica. Por favor, elige una variable de tipo factor o carácter."))
    cols <- "Categoría"
    if (input$mostrar_n) cols <- c(cols, "N")
    if (input$mostrar_porcentaje) cols <- c(cols, "porcentaje")
    datatable(df[, ..cols], options = list(pageLength = 10, dom = 't', searching = FALSE, ordering = FALSE), rownames = FALSE, caption = paste("Tabla de Frecuencias de:", input$variable_uni))
  })
  
  # --- Análisis Bivariado (Contingencia) ---
  tabla_cont <- reactive({
    req(input$variable_x, input$variable_y, datos_raw())
    df <- datos_raw()
    if (!is.factor(df[[input$variable_x]])) {
      return(NULL)
    }
    if (!is.factor(df[[input$variable_y]])) {
      return(NULL)
    }
    xt <- table(df[[input$variable_x]], df[[input$variable_y]])
    dt_cont <- as.data.table(xt)
    data.table::setnames(dt_cont, c("V1", "V2", "N"), c("Fila", "Columna", "N"))
    return(dt_cont)
  })
  
  output$tabla_contingencia_n <- renderDT({
    dt_cont <- tabla_cont()
    validate(need(!is.null(dt_cont), "Por favor, selecciona dos variables categóricas para el análisis bivariado."))
    datatable(dcast(dt_cont, Fila ~ Columna, value.var = "N", fill = 0), options = list(scrollX = TRUE, pageLength = 10, searching = FALSE, ordering = FALSE), rownames = FALSE, caption = paste("Tabla de Frecuencias (N) para", input$variable_x, "y", input$variable_y))
  })
  
  output$tabla_contingencia_col <- renderDT({
    dt_cont <- tabla_cont()
    validate(need(!is.null(dt_cont), "Por favor, selecciona dos variables categóricas para el análisis bivariado."))
    matriz <- dcast(dt_cont, Fila ~ Columna, value.var = "N", fill = 0)[, -1]
    porcentajes <- prop.table(as.matrix(matriz), 2) * 100
    df_result <- cbind(Fila = dcast(dt_cont, Fila ~ Columna, value.var = "N", fill = 0)$Fila, as.data.frame(round(porcentajes, 2)))
    datatable(df_result, options = list(scrollX = TRUE, pageLength = 10, searching = FALSE, ordering = FALSE), rownames = FALSE, caption = paste("Porcentajes por Columna para", input$variable_x, "y", input$variable_y))
  })
  
  output$tabla_contingencia_fila <- renderDT({
    dt_cont <- tabla_cont()
    validate(need(!is.null(dt_cont), "Por favor, selecciona dos variables categóricas para el análisis bivariado."))
    matriz <- dcast(dt_cont, Fila ~ Columna, value.var = "N", fill = 0)[, -1]
    porcentajes <- prop.table(as.matrix(matriz), 1) * 100
    df_result <- cbind(Fila = dcast(dt_cont, Fila ~ Columna, value.var = "N", fill = 0)$Fila, as.data.frame(round(porcentajes, 2)))
    datatable(df_result, options = list(scrollX = TRUE, pageLength = 10, searching = FALSE, ordering = FALSE), rownames = FALSE, caption = paste("Porcentajes por Fila para", input$variable_x, "y", input$variable_y))
  })
  
  output$tabla_contingencia_total <- renderDT({
    dt_cont <- tabla_cont()
    validate(need(!is.null(dt_cont), "Por favor, selecciona dos variables categóricas para el análisis bivariado."))
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
      if (length(text_data) == 0) {
        return(NULL)
      }
      
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
  
  # Análisis de sentimiento avanzado con sesgo
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
      score = c(3, 2, 2, 3, 2, 1, 3, 3,
                -3, -3, -2, -2, -1, -2, -3, -2, -2)
    )
    
    df_text <- tibble(
      respuesta_id = seq_along(text_data),
      text = text_data
    ) %>%
      unnest_tokens(word, text)
    
    df_sentimiento <- df_text %>%
      left_join(sentimientos_espanol, by = "word") %>%
      filter(!is.na(sentimiento))
    
    puntajes <- df_sentimiento %>%
      group_by(respuesta_id) %>%
      summarise(puntaje_original = sum(score, na.rm = TRUE))
    
    puntajes_con_sesgo <- puntajes %>%
      mutate(puntaje_ajustado = puntaje_original + input$sesgo_sentimiento)
    
    df_final <- df_text %>%
      group_by(respuesta_id) %>%
      slice(1) %>%
      ungroup() %>%
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
    datatable(datos_raw(),
              options = list(pageLength = 25, scrollX = TRUE),
              filter = "top",
              rownames = FALSE)
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
