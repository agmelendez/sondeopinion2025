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

# -------------------------------------------------------------------------
# Interfaz de Usuario (UI)
# -------------------------------------------------------------------------
ui <- dashboardPage(
  dashboardHeader(title = "Análisis de Encuestas"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Inicio", tabName = "inicio", icon = icon("home")),
      menuItem("Cargar Datos", tabName = "cargar", icon = icon("upload")),
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
                      tags$li("En 'Análisis Univariado' y 'Análisis Bivariado', podrás explorar tus variables categóricas."),
                      tags$li("El módulo 'Análisis de Texto' permite estudiar preguntas abiertas, incluyendo nube de palabras y análisis de sentimiento."),
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
    
    if (tolower(ext) == "csv") {
      dt <- data.table::fread(input$archivo_excel$datapath, encoding = "UTF-8")
    } else if (tolower(ext) %in% c("xls", "xlsx")) {
      dt <- as.data.table(readxl::read_excel(input$archivo_excel$datapath))
    } else {
      showModal(modalDialog(
        title = "Error de archivo",
        "Formato de archivo no soportado. Por favor, usa un archivo .csv, .xls o .xlsx.",
        easyClose = TRUE,
        footer = NULL
      ))
      return(NULL)
    }
    
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