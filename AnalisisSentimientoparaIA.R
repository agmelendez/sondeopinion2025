library(shiny)
library(readxl)
library(writexl)
library(syuzhet)
library(dplyr)
library(bslib)
library(stringr)
library(tidytext)
library(ggplot2)

ui <- page_fluid(
  theme = bs_theme(bootswatch = "cerulean"),
  
  # Título de la aplicación
  card(
    card_header("Análisis de Sentimiento para Preguntas Abiertas"),
    card_body(
      p("Esta herramienta analiza respuestas abiertas y clasifica el sentimiento como positivo, negativo o neutro."),
      p("Sube un archivo Excel que contenga las 6 preguntas abiertas por cada observación y un ID único.")
    )
  ),
  
  layout_sidebar(
    sidebar = sidebar(
      fileInput("file", "Cargar archivo Excel", 
                accept = c(".xlsx", ".xls"),
                placeholder = "Seleccionar archivo Excel..."),
      
      hr(),
      
      selectInput("id_column", "Seleccionar columna de ID único:", 
                  choices = NULL),
      
      checkboxGroupInput("question_columns", "Seleccionar columnas de preguntas abiertas:",
                         choices = NULL),
      
      hr(),
      
      h4("Definición de categorías de sentimiento"),
      textAreaInput("positive_def", "¿Qué se considera positivo?:",
                    value = "bueno, excelente, fantástico, maravilloso, útil, recomendable, satisfecho, contento, feliz, genial, perfecto, tuanis, pura vida, muy bien, súper, increíble, espectacular, magnífico, extraordinario, agradable, positivo, favorable, gran, mejor, óptimo, ideal, eficiente, efectivo, rápido, fácil, cómodo, conveniente, adecuado, correcto, acertado, brillante, estupendo, hermoso, precioso, lindo, bonito, agradecido, encantado, complacido, alegre, optimista, confiado, seguro, tranquilo, relajado, calmado, pacífico",
                    height = "80px"),
      
      textAreaInput("negative_def", "¿Qué se considera negativo?:",
                    value = "malo, pésimo, terrible, inútil, deficiente, incompleto, problemático, molesto, enojado, frustrado, decepcionado, horrible, desagradable, feo, sucio, lento, difícil, complicado, confuso, aburrido, cansado, estresado, preocupado, nervioso, ansioso, triste, deprimido, desanimado, desalentado, desesperado, furioso, irritado, disgustado, insatisfecho, descontento, inconforme, crítico, negativo, desfavorable, inadecuado, incorrecto, erróneo, fallido, fracasado, imposible, inaceptable, intolerable, insoportable, fastidioso, pesado, agobiante, angustiante, dañino, perjudicial, malo, peor, pésimo, deplorable, lamentable, vergonzoso",
                    height = "80px"),
      
      textAreaInput("neutral_def", "¿Qué se considera neutro?:",
                    value = "normal, regular, promedio, estándar, común, típico, adecuado, aceptable, bien, está bien, no mal, más o menos, así así, ok, okay, igual, similar, parecido, habitual, corriente, ordinario, convencional, neutral, objetivo, descriptivo, informativo, explicativo, claro, simple, básico, general, principal, importante, necesario, posible, disponible, presente, actual, nuevo, viejo, grande, pequeño, alto, bajo, largo, corto, rápido, lento",
                    height = "80px"),
      
      hr(),
      
      sliderInput("sentiment_threshold", "Umbral de clasificación:", 
                  min = 0, max = 0.3, value = 0.05, step = 0.01),
      
      h4("Contexto para análisis"),
      textAreaInput("context", "Proporcionar contexto para el análisis de sentimiento:",
                    value = "Este análisis es para respuestas de Costa Rica. Considera las expresiones locales y el contexto cultural costarricense. Toma en cuenta modismos como 'tuanis', 'pura vida', 'mae', etc.",
                    height = "100px"),
      
      hr(),
      
      actionButton("analyze", "Analizar Sentimiento", class = "btn-primary"),
      
      hr(),
      
      downloadButton("download", "Descargar Resultados", class = "btn-info")
    ),
    
    card(
      card_header("Resultados"),
      card_body(
        tabsetPanel(
          tabPanel("Vista previa datos originales", div(style = "overflow-x: scroll;", tableOutput("original_data"))),
          tabPanel("Vista previa resultados", div(style = "overflow-x: scroll;", tableOutput("results_preview"))),
          tabPanel("Resumen de Análisis", 
                   plotOutput("sentiment_summary"),
                   hr(),
                   verbatimTextOutput("analysis_summary")),
          tabPanel("Palabras clave", verbatimTextOutput("keywords_info"))
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # Variables reactivas
  data_raw <- reactiveVal(NULL)
  results <- reactiveVal(NULL)
  sentiment_words <- reactiveVal(list(positive = NULL, negative = NULL, neutral = NULL))
  
  # Extraer palabras clave de las definiciones
  observe({
    if(input$positive_def != "" && input$negative_def != "" && input$neutral_def != "") {
      # Procesar definiciones para extraer palabras clave
      extract_keywords <- function(text) {
        words <- unlist(strsplit(tolower(text), "\\W+"))
        words <- words[nchar(words) > 2] # Filtrar palabras muy cortas
        return(unique(words[words != ""]))
      }
      
      pos_words <- extract_keywords(input$positive_def)
      neg_words <- extract_keywords(input$negative_def)
      neu_words <- extract_keywords(input$neutral_def)
      
      sentiment_words(list(
        positive = pos_words,
        negative = neg_words,
        neutral = neu_words
      ))
    }
  })
  
  # Mostrar palabras clave extraídas
  output$keywords_info <- renderPrint({
    req(sentiment_words())
    sw <- sentiment_words()
    
    cat("Palabras clave positivas:\n")
    cat(paste(sw$positive, collapse = ", "), "\n\n")
    
    cat("Palabras clave negativas:\n")
    cat(paste(sw$negative, collapse = ", "), "\n\n")
    
    cat("Palabras clave neutras:\n")
    cat(paste(sw$neutral, collapse = ", "), "\n\n")
  })
  
  # Cuando se carga el archivo
  observeEvent(input$file, {
    req(input$file)
    
    # Leer archivo Excel
    tryCatch({
      data <- read_excel(input$file$datapath)
      data_raw(data)
      
      # Actualizar opciones de columnas
      updateSelectInput(session, "id_column", choices = names(data))
      updateCheckboxGroupInput(session, "question_columns", choices = names(data))
      
      showNotification("Archivo cargado exitosamente", type = "message")
    }, error = function(e) {
      showNotification(paste("Error al cargar el archivo:", e$message), type = "error")
    })
  })
  
  # Función mejorada para analizar sentimiento
  analyze_sentiment <- function(text, context, pos_words, neg_words, neu_words, threshold) {
    # Verificar si el texto es válido
    if(is.null(text) || is.na(text) || text == "" || nchar(trimws(text)) == 0 || 
       trimws(toupper(text)) == "SIN RESPUESTA") {
      return(NA_character_)
    }
    
    # Limpiar y preparar texto
    text_clean <- tolower(trimws(text))
    
    # Método 1: Usar syuzhet para obtener sentimiento base
    sentiment_score <- tryCatch({
      # Intentar diferentes métodos de syuzhet
      score1 <- get_sentiment(text_clean, method = "syuzhet")
      score2 <- get_sentiment(text_clean, method = "bing")
      score3 <- get_sentiment(text_clean, method = "afinn")
      
      # Promediar los scores disponibles
      scores <- c(score1, score2, score3)
      scores <- scores[!is.na(scores) & is.finite(scores)]
      
      if(length(scores) > 0) {
        mean(scores)
      } else {
        0
      }
    }, error = function(e) {
      0 # En caso de error, asignar sentimiento neutro
    })
    
    # Método 2: Análisis de palabras clave (más peso para español)
    text_words <- unlist(strsplit(text_clean, "\\W+"))
    text_words <- text_words[nchar(text_words) > 2] # Filtrar palabras muy cortas
    text_words <- unique(text_words[text_words != ""])
    
    # Contar palabras de cada categoría
    pos_count <- sum(text_words %in% pos_words)
    neg_count <- sum(text_words %in% neg_words)
    neu_count <- sum(text_words %in% neu_words)
    
    total_keywords <- pos_count + neg_count + neu_count
    
    # Lógica de clasificación simplificada y más efectiva
    
    # Si hay palabras clave detectadas, darles prioridad
    if(total_keywords > 0) {
      if(pos_count > neg_count && pos_count > neu_count) {
        return("Positivo")
      } else if(neg_count > pos_count && neg_count > neu_count) {
        return("Negativo")
      } else if(pos_count == neg_count && pos_count > 0) {
        # En caso de empate entre positivo y negativo, usar syuzhet como desempate
        if(sentiment_score > threshold) {
          return("Positivo")
        } else if(sentiment_score < -threshold) {
          return("Negativo")
        } else {
          return("Neutro")
        }
      } else {
        return("Neutro")
      }
    } else {
      # Si no hay palabras clave, usar solo syuzhet con umbral más bajo
      if(sentiment_score > threshold) {
        return("Positivo")
      } else if(sentiment_score < -threshold) {
        return("Negativo")
      } else {
        return("Neutro")
      }
    }
  }
  
  # Cuando se hace clic en el botón de análisis
  observeEvent(input$analyze, {
    req(data_raw(), input$id_column, input$question_columns, sentiment_words())
    
    # Validar selecciones
    if(length(input$question_columns) == 0) {
      showNotification("Por favor, seleccione al menos una columna de pregunta", type = "error")
      return()
    }
    
    # Obtener datos
    data <- data_raw()
    sw <- sentiment_words()
    
    # Crear copia para resultados
    result_data <- data
    
    # Analizar cada pregunta seleccionada
    withProgress(message = 'Analizando sentimiento...', value = 0, {
      for(i in seq_along(input$question_columns)) {
        col <- input$question_columns[i]
        
        # Actualizar progreso
        incProgress(1/length(input$question_columns), 
                    detail = paste("Procesando pregunta:", col))
        
        # Nombre de la nueva columna
        new_col_name <- paste0(col, "_sentimiento")
        
        # Analizar sentimiento para cada respuesta
        result_data[[new_col_name]] <- vapply(seq_len(nrow(data)), function(row_idx) {
          text <- data[[col]][row_idx]
          
          # Verificar si es NA, NULL, cadena vacía o "SIN RESPUESTA"
          if(is.null(text) || is.na(text) || text == "" || 
             nchar(trimws(text)) == 0 || trimws(toupper(text)) == "SIN RESPUESTA") {
            return(NA_character_)
          }
          
          # Analizar sentimiento con la función mejorada
          result <- analyze_sentiment(
            text, 
            input$context, 
            sw$positive, 
            sw$negative, 
            sw$neutral,
            input$sentiment_threshold
          )
          
          # Si el resultado es NA, devolver NA_character_
          if(is.na(result)) {
            return(NA_character_)
          }
          
          return(result)
        }, character(1))
      }
    })
    
    # Guardar resultados
    results(result_data)
    
    showNotification("Análisis de sentimiento completado", type = "message")
  })
  
  # Vista previa de datos originales (primeras 10 filas)
  output$original_data <- renderTable({
    req(data_raw())
    data <- data_raw()
    
    # Mostrar máximo 10 filas y truncar texto largo
    preview_data <- data[1:min(10, nrow(data)), ]
    
    # Truncar texto en columnas de caracteres para mejor visualización
    preview_data[] <- lapply(preview_data, function(x) {
      if(is.character(x)) {
        ifelse(nchar(as.character(x)) > 50, 
               paste0(substr(as.character(x), 1, 47), "..."), 
               as.character(x))
      } else {
        x
      }
    })
    
    preview_data
  }, bordered = TRUE, striped = TRUE)
  
  # Vista previa de resultados (primeras 10 filas)
  output$results_preview <- renderTable({
    req(results())
    result_data <- results()
    
    # Mostrar máximo 10 filas
    preview_data <- result_data[1:min(10, nrow(result_data)), ]
    
    # Truncar texto en columnas de caracteres para mejor visualización
    preview_data[] <- lapply(preview_data, function(x) {
      if(is.character(x) && !all(x %in% c("Positivo", "Negativo", "Neutro", NA))) {
        ifelse(nchar(as.character(x)) > 50, 
               paste0(substr(as.character(x), 1, 47), "..."), 
               as.character(x))
      } else {
        x
      }
    })
    
    preview_data
  }, bordered = TRUE, striped = TRUE)
  
  # Resumen gráfico del análisis
  output$sentiment_summary <- renderPlot({
    req(results(), input$question_columns)
    
    result_data <- results()
    sentiment_cols <- paste0(input$question_columns, "_sentimiento")
    
    # Crear datos para el gráfico
    sentiment_summary <- data.frame()
    
    for(col in sentiment_cols) {
      if(col %in% names(result_data)) {
        temp_data <- data.frame(
          Pregunta = gsub("_sentimiento", "", col),
          Sentimiento = result_data[[col]],
          stringsAsFactors = FALSE
        )
        sentiment_summary <- rbind(sentiment_summary, temp_data)
      }
    }
    
    # Remover NAs para el gráfico
    sentiment_summary <- sentiment_summary[!is.na(sentiment_summary$Sentimiento), ]
    
    if(nrow(sentiment_summary) > 0) {
      # Crear gráfico de barras
      ggplot(sentiment_summary, aes(x = Sentimiento, fill = Sentimiento)) +
        geom_bar() +
        facet_wrap(~Pregunta, scales = "free_y") +
        scale_fill_manual(values = c("Positivo" = "#2ecc71", 
                                     "Negativo" = "#e74c3c", 
                                     "Neutro" = "#95a5a6")) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(title = "Distribución de Sentimientos por Pregunta",
             x = "Sentimiento",
             y = "Frecuencia")
    }
  })
  
  # Resumen estadístico del análisis
  output$analysis_summary <- renderPrint({
    req(results(), input$question_columns)
    
    result_data <- results()
    sentiment_cols <- paste0(input$question_columns, "_sentimiento")
    
    cat("=== RESUMEN DE ANÁLISIS DE SENTIMIENTO ===\n\n")
    cat("Total de observaciones:", nrow(result_data), "\n")
    cat("Preguntas analizadas:", length(input$question_columns), "\n\n")
    
    for(i in seq_along(input$question_columns)) {
      original_col <- input$question_columns[i]
      sentiment_col <- sentiment_cols[i]
      
      if(sentiment_col %in% names(result_data)) {
        cat("--- Pregunta:", original_col, "---\n")
        
        # Contar respuestas válidas vs sin respuesta
        valid_responses <- sum(!is.na(result_data[[original_col]]) & 
                                 result_data[[original_col]] != "" &
                                 trimws(toupper(result_data[[original_col]])) != "SIN RESPUESTA")
        
        missing_responses <- nrow(result_data) - valid_responses
        
        cat("Respuestas válidas:", valid_responses, "\n")
        cat("Sin respuesta/vacías:", missing_responses, "\n")
        
        # Distribución de sentimientos
        sentiment_table <- table(result_data[[sentiment_col]], useNA = "always")
        cat("Distribución de sentimientos:\n")
        print(sentiment_table)
        
        # Porcentajes (excluyendo NAs)
        valid_sentiments <- result_data[[sentiment_col]][!is.na(result_data[[sentiment_col]])]
        if(length(valid_sentiments) > 0) {
          percentages <- round(prop.table(table(valid_sentiments)) * 100, 1)
          cat("Porcentajes (de respuestas válidas):\n")
          print(percentages)
        }
        
        cat("\n")
      }
    }
  })
  
  # Descarga de resultados
  output$download <- downloadHandler(
    filename = function() {
      paste0("analisis_sentimiento_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      req(results())
      
      tryCatch({
        # Escribir archivo Excel
        write_xlsx(results(), file)
        
        showNotification("Archivo descargado exitosamente", type = "message")
      }, error = function(e) {
        showNotification(paste("Error al descargar archivo:", e$message), type = "error")
      })
    }
  )
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)