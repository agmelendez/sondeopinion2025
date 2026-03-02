# app.R — Duplicados optimizado (blocking + candidatos + poda + paralelización opcional)

# >>> MEJORA: Se recomienda usar un gestor de dependencias como 'renv' para proyectos
# complejos. Esto crea un entorno aislado y garantiza que las versiones de los paquetes
# sean consistentes, mejorando la reproducibilidad. Para iniciar: renv::init()
pkgs <- c(
  "shiny","shinycssloaders","DT","readxl","dplyr","stringr","stringi",
  "lubridate","RecordLinkage","tibble","readr","bsplus","data.table","future","future.apply",
  "writexl" # >>> NUEVO: Paquete para exportar a Excel
)
new <- pkgs[!pkgs %in% installed.packages()[,1]]
if(length(new)) install.packages(new, repos = "https://cloud.r-project.org")
invisible(lapply(pkgs, library, character.only = TRUE))

# ---------------- Utilidades ----------------
clean_text <- function(z){
  z <- ifelse(is.na(z), "", z)
  z <- stringr::str_to_lower(z)
  z <- stringi::stri_trans_general(z, "Latin-ASCII") # Eficiente para remover tildes
  z <- stringr::str_replace_all(z, "[^a-z0-9 ]", " ") # Remueve caracteres no alfanuméricos
  z <- stringr::str_squish(z) # Remueve espacios extra
  z
}

d_cat  <- function(a, b) as.numeric(a != b) # Distancia binaria: 0 si son iguales, 1 si no

d_text_jw <- function(a, b){
  a <- ifelse(is.na(a),"",a); b <- ifelse(is.na(b),"",b)
  if(identical(a,b)) return(0.0) # Atajo: si son idénticos, la distancia es 0
  la <- nchar(a); lb <- nchar(b)
  
  # >>> MEJORA: Comentario explicativo sobre los "números mágicos"
  # Atajos: diferencias de longitud muy grandes suelen ser casos distintos.
  # El valor 60 es un umbral heurístico para evitar cálculos costosos en pares muy disímiles.
  if(abs(la - lb) > 60) return(1.0)
  
  # Truncar para acelerar. Jaro-Winkler es sensible a la longitud.
  # 160 caracteres es un buen compromiso: captura la mayor parte del contenido
  # semántico sin penalizar demasiado el rendimiento.
  if(la > 160) a <- substr(a, 1, 160)
  if(lb > 160) b <- substr(b, 1, 160)
  
  1 - RecordLinkage::jarowinkler(a, b) # Convertir similitud (0-1) a distancia (0-1)
}

d_edad <- function(a, b){
  # >>> MEJORA: Comentario explicativo
  # Distancia normalizada. El divisor '5' significa que una diferencia de 5 años o más
  # ya se considera distancia máxima (1). Esto da un margen de flexibilidad.
  # Si un dato falta, se imputa una distancia intermedia (0.5) para no sesgar el resultado.
  ifelse(is.na(a) | is.na(b), 0.5, pmin(abs(a - b)/5, 1))
}

d_tiempo <- function(t1, t2){
  if(any(is.na(c(t1,t2)))) return(0.5) # Distancia intermedia si falta un dato
  mins <- abs(as.numeric(difftime(t1, t2, units = "mins")))
  # Distancia por tramos, asumiendo que entrevistas muy cercanas en tiempo son más sospechosas
  if(mins < 30)   return(0.0)
  if(mins < 120)  return(0.25)
  if(mins < 1440) return(0.5) # Menos de 1 día
  if(mins < 4320) return(0.75) # Menos de 3 días
  1.0
}

parse_time_any <- function(x){
  if(is.null(x)) return(NA)
  # >>> MEJORA: Añadido tz = "UTC" para evitar problemas de zona horaria y asegurar consistencia
  lubridate::parse_date_time(
    x,
    orders = c("H:M:S","H:M",
               "d/m/Y H:M:S","d/m/Y H:M","d/m/Y",
               "Y-m-d H:M:S","Y-m-d H:M","Y-m-d",
               "m/d/Y H:M:S","m/d/Y H:M","m/d/Y"),
    quiet = TRUE,
    tz = "UTC" 
  )
}

# ---------------- UI ----------------
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      /* Estilos para una UI más limpia y profesional */
      .control-label { font-weight: 600; }
      .shiny-input-container { margin-bottom: 10px; }
      .input-wide .selectize-control { width: 100% !important; }
      .wrap-text .selectize-input { white-space: normal !important; line-height: 1.2; min-height: 38px; }
      .section-title { margin-top: 15px; margin-bottom: 6px; border-bottom: 1px solid #eee; padding-bottom: 4px; }
      .small-note { color: #666; font-size: 12px; }
      .subgroup-title { margin-top: 8px; font-weight: 600; color: #333; }
      table.dataTable tbody td { white-space: nowrap; }
      .dt-center { text-align: center; }
      /* >>> MEJORA: Estilo para mensajes de validación */
      .shiny-output-error-validation { color: #d9534f; font-weight: bold; }
      /* >>> NUEVA SECCIÓN: Estilos para la pestaña de interpretación */
      .summary-box { border: 1px solid #ddd; border-left: 5px solid #337ab7; padding: 15px; margin-bottom: 20px; background-color: #f9f9f9; border-radius: 4px; }
      .summary-box h4 { margin-top: 0; color: #337ab7; }
      .summary-box .stat { font-size: 24px; font-weight: bold; }
    "))
  ),
  titlePanel("Detección de posibles respuestas duplicadas — Optimizado"),
  sidebarLayout(
    sidebarPanel(
      width = 3, # >>> MEJORA: Panel lateral un poco más angosto para dar más espacio al contenido
      div(class = "section-title", h4("Archivo")),
      fileInput("file", NULL, accept = ".xlsx", buttonLabel = "Elegir…", placeholder = "Datos.xlsx"),
      uiOutput("sheet_ui"),
      
      div(class = "section-title", h4("Mapeo de variables")),
      helpText("Seleccione la columna del archivo. Pase el mouse para ver ayuda."),
      
      div(class = "subgroup-title", "Sociodemográficos"),
      uiOutput("map_soc"),
      
      div(class = "subgroup-title", "Variables de contenido"),
      uiOutput("map_content"),
      
      div(class = "subgroup-title", "Variables de tiempo"),
      uiOutput("map_time"),
      
      div(class = "section-title", h4("Parámetros de similitud")),
      sliderInput("w_sc1","Peso SC1 (texto)", 0, 3, 1.3, 0.1),
      sliderInput("w_pe3","Peso PE3", 0, 3, 1.6, 0.1),
      sliderInput("w_pe2","Peso PE2", 0, 3, 1.4, 0.1),
      sliderInput("w_pe1","Peso PE1", 0, 2, 1.2, 0.1),
      sliderInput("w_am3","Peso AM3", 0, 2, 1.0, 0.1),
      sliderInput("w_am1","Peso AM1", 0, 2, 1.2, 0.1),
      sliderInput("w_canton","Peso cantón", 0, 2, 1.3, 0.1),
      sliderInput("w_ingreso","Peso ingreso", 0, 2, 1.2, 0.1),
      sliderInput("w_empleo","Peso empleo", 0, 2, 1.0, 0.1),
      sliderInput("w_educ","Peso educación", 0, 2, 1.0, 0.1),
      sliderInput("w_edad","Peso edad", 0, 3, 1.5, 0.1),
      sliderInput("w_sexo","Peso sexo", 0, 2, 1.0, 0.1),
      sliderInput("w_tiempo","Peso tiempo", 0, 2, 1.0, 0.1),
      sliderInput("alpha","Curvatura del score (alpha)", 0.5, 3, 1.3, 0.1),
      
      div(class = "section-title", h4("Optimización")),
      checkboxInput("use_block", "Usar blocking (recomendado)", TRUE),
      checkboxInput("edad_neighbor", "Permitir edad ±1 banda en blocking", TRUE),
      numericInput("band_size", "Tamaño de banda de edad (años)", 5, min = 2, max = 10, step = 1),
      checkboxInput("use_parallel", "Paralelizar (usa todos menos 1 núcleo)", FALSE),
      numericInput("chunks", "Cantidad de chunks", 100, min = 10, max = 1000, step = 10),
      
      div(class = "section-title", h4("Umbrales y clustering")),
      sliderInput("umbral_alto","Umbral sospecha alta", 0.5, 0.99, 0.85, 0.01),
      sliderInput("umbral_medio","Umbral sospecha media", 0.5, 0.99, 0.75, 0.01),
      sliderInput("h_stricto","Altura cluster estricto", 0.05, 1, 0.25, 0.01),
      sliderInput("h_medio","Altura cluster medio", 0.05, 1, 0.35, 0.01),
      sliderInput("h_laxo","Altura cluster laxo", 0.05, 1, 0.45, 0.01),
      
      div(class = "section-title"),
      actionButton("run", "Ejecutar análisis", class = "btn btn-primary btn-block", icon = icon("play")),
      
      div(class = "section-title", h4("Descargas")),
      # >>> MEJORA: Se desactivan los botones de descarga hasta que haya resultados.
      uiOutput("download_buttons_ui"),
      tags$p(class="small-note", "Use filtros y ordenamiento en las tablas para explorar.")
    ),
    mainPanel(
      width = 9, # >>> MEJORA: Panel principal un poco más ancho
      tabsetPanel(
        id = "main_tabs", # >>> MEJORA: Asignar un ID para poder controlarlo desde el server
        tabPanel("1) Carga",
                 h4("Introducción"),
                 p("Se importa el archivo y se inspecciona su estructura para mapear variables."),
                 withSpinner(DTOutput("preview"), type = 6)
        ),
        tabPanel("2) Normalización",
                 h4("Método"),
                 p("Estandarización de textos, consolidación de 'otro', tipificación de edad y parseo de tiempos."),
                 # >>> MEJORA: Añadido un output para mensajes de validación
                 uiOutput("norm_validation_ui"),
                 withSpinner(DTOutput("norm_preview"), type = 6)
        ),
        tabPanel("3) Pares (score)",
                 h4("Método"),
                 tags$blockquote("p_dup = 1 - (d_compuesta) ^ alpha"),
                 p("d_compuesta es un promedio ponderado de sub-distancias por variable."),
                 withSpinner(DTOutput("pairs_top"), type = 6)
        ),
        tabPanel("4) Clustering",
                 h4("Método"),
                 p("Clustering jerárquico (enlace promedio) sobre la matriz de distancias normalizadas."),
                 withSpinner(DTOutput("clusters_tbl"), type = 6)
        ),
        tabPanel("5) Inspección",
                 uiOutput("inspect_controls"),
                 withSpinner(DTOutput("inspect_table"), type = 6)
        ),
        # >>> NUEVA SECCIÓN: Pestaña de Interpretación
        tabPanel("6) Interpretación", value = "interpret_panel", icon = icon("comment-dots"),
                 withSpinner(uiOutput("interpretation_ui"), type = 6)
        ),
        # >>> NUEVO: Pestaña para la base de datos final
        tabPanel("7) Base Final", value = "final_db_panel", icon = icon("database"),
                 h4("Base de datos final (sin duplicados de sospecha alta)"),
                 p("Esta tabla muestra la base de datos original después de eliminar todos los registros que formaron parte de al menos un 'Par de sospecha Alta'. Puede descargar esta base de datos filtrada en formato Excel."),
                 withSpinner(DTOutput("final_database_table"), type = 6)
        ),
        tabPanel("Metodología",
                 h3("Resumen"),
                 uiOutput("metodo_html")
        )
      )
    )
  )
)

# ---------------- SERVER ----------------
# (El resto del servidor es igual, solo se añade la lógica para la nueva pestaña al final)
server <- function(input, output, session){
  
  # ---------- Hojas ----------
  output$sheet_ui <- renderUI({
    req(input$file)
    sh <- tryCatch(readxl::excel_sheets(input$file$datapath), error=function(e) character(0))
    selectInput("sheet","Hoja", choices = sh, selected = if(length(sh)) sh[1] else NULL, width = "100%")
  })
  
  # ---------- Lectura ----------
  dat_raw <- reactive({
    req(input$file, input$sheet)
    readxl::read_excel(input$file$datapath, sheet = input$sheet)
  })
  
  output$preview <- renderDT({
    req(dat_raw())
    DT::datatable(head(dat_raw(), 20),
                  options = list(scrollX=TRUE, pageLength=12, dom='ftip'),
                  rownames = FALSE)
  })
  
  # ---------- UI mapeo ----------
  short_label <- function(id, label, tip, choices, selected){
    bsplus::bs_embed_tooltip(
      selectInput(id, label, choices = c("", choices),
                  selected = selected, width="100%"),
      title = tip, placement = "right", trigger = "hover"
    )
  }
  
  output$map_soc <- renderUI({
    req(dat_raw())
    cols <- names(dat_raw())
    guess <- function(pattern){
      x <- grep(pattern, cols, value = TRUE, ignore.case = TRUE)
      if(length(x)) x[1] else ""
    }
    tagList(
      div(class="input-wide wrap-text",
          short_label("v_sexo","Sexo","CS1", cols, guess("^CS1\\.|sexo"))),
      div(class="input-wide wrap-text",
          short_label("v_edad","Edad","CS2 (numérica) o texto", cols, guess("^CS2\\.|edad"))),
      div(class="input-wide wrap-text",
          short_label("v_grup_edad","Grupo de edad","Categoría si no hay número", cols, guess("grupo.*edad|GruposEdad"))),
      div(class="input-wide wrap-text",
          short_label("v_educ","Educación","CS3", cols, guess("^CS3\\.|nivel"))),
      div(class="input-wide wrap-text",
          short_label("v_empleo","Condición de empleo","CS4", cols, guess("^CS4\\.|condicion|empleo"))),
      div(class="input-wide wrap-text",
          short_label("v_empleo_otro","Empleo (otro)","Campo 'Otro' de CS4", cols, guess("^Otro_-_CS4|otro.*CS4"))),
      div(class="input-wide wrap-text",
          short_label("v_ingreso","Ingreso percibido","CS5", cols, guess("^CS5\\.|alcanza"))),
      div(class="input-wide wrap-text",
          short_label("v_canton","Cantón","Lugar de residencia", cols, guess("canton|Cant[oó]n")))
    )
  })
  
  output$map_content <- renderUI({
    req(dat_raw())
    cols <- names(dat_raw())
    guess <- function(pattern){
      x <- grep(pattern, cols, value = TRUE, ignore.case = TRUE)
      if(length(x)) x[1] else ""
    }
    tagList(
      div(class="input-wide wrap-text",
          short_label("v_sc1","SC1 (texto)","Problemas prioritarios", cols, guess("^SC1\\."))),
      div(class="input-wide wrap-text",
          short_label("v_am1","AM1","Evaluación de gobierno", cols, guess("^AM\\.?1|^AM1"))),
      div(class="input-wide wrap-text",
          short_label("v_am3","AM3 (medios)","Medios/mecanismos de información", cols, guess("^AM3(_|\\.)|^AM3"))),
      div(class="input-wide wrap-text",
          short_label("v_pe1","PE1","Identificación electoral", cols, guess("^PE\\.?1|^PE_?1"))),
      div(class="input-wide wrap-text",
          short_label("v_pe2","PE2","Intención por agrupación/partido", cols, guess("^PE\\.?2|^PE_?2"))),
      div(class="input-wide wrap-text",
          short_label("v_pe2_otro","PE2 (otro)","Texto libre del partido", cols, guess("^Otro_-_PE\\.?2|^Otro_-_PE_?2|otro.*PE ?2"))),
      div(class="input-wide wrap-text",
          short_label("v_pe3","PE3","Intención por persona", cols, guess("^PE\\.?3|^PE_?3")))
    )
  })
  
  output$map_time <- renderUI({
    req(dat_raw())
    cols <- names(dat_raw())
    guess <- function(pattern){
      x <- grep(pattern, cols, value = TRUE, ignore.case = TRUE)
      if(length(x)) x[1] else ""
    }
    tagList(
      div(class="input-wide wrap-text",
          short_label("v_inicio","Hora inicio","Hora_de_inicio", cols, guess("Hora_de_inicio|inicio"))),
      div(class="input-wide wrap-text",
          short_label("v_envio","Hora envío","Hora_de_envío/envio", cols, guess("Hora_de_env[íi]o|envio"))),
      div(class="input-wide wrap-text",
          short_label("v_fecha_aj","Fecha ajustada","FechaAjustada", cols, guess("^FechaAjustada"))),
      div(class="input-wide wrap-text",
          short_label("v_fecha","Fecha original","Fecha", cols, guess("^Fecha$|fecha$")))
    )
  })
  
  # ---------- Normalización ----------
  dat_norm <- eventReactive(input$run, {
    req(dat_raw())
    
    # >>> MEJORA: Validación de inputs clave. Evita que la app se rompa.
    validate(
      need(input$v_sexo, "Debe seleccionar la variable 'Sexo'."),
      need(input$v_edad, "Debe seleccionar la variable 'Edad'."),
      need(input$v_canton, "Debe seleccionar la variable 'Cantón'."),
      need(input$v_sc1, "Debe seleccionar la variable 'SC1 (texto)'.")
    )
    
    # >>> MEJORA: Mover el cambio de pestaña aquí para una mejor experiencia de usuario.
    updateTabsetPanel(session, "main_tabs", selected = "2) Normalización")
    
    df <- dat_raw()
    nm <- function(v) if(!is.null(v) && nzchar(v)) v else NULL
    
    # Función segura para extraer columnas
    get_col <- function(col_name) if (!is.null(nm(col_name))) df[[nm(col_name)]] else NULL
    
    sexo        <- get_col(input$v_sexo)
    edad        <- get_col(input$v_edad)
    educ        <- get_col(input$v_educ)
    empleo      <- get_col(input$v_empleo)
    empleo_otro <- get_col(input$v_empleo_otro)
    ingreso     <- get_col(input$v_ingreso)
    canton      <- get_col(input$v_canton)
    sc1         <- get_col(input$v_sc1)
    am1         <- get_col(input$v_am1)
    am3         <- get_col(input$v_am3)
    pe1         <- get_col(input$v_pe1)
    pe2         <- get_col(input$v_pe2)
    pe2_otro    <- get_col(input$v_pe2_otro)
    pe3         <- get_col(input$v_pe3)
    inicio      <- as.character(get_col(input$v_inicio))
    envio       <- as.character(get_col(input$v_envio))
    fecha       <- as.character(get_col(input$v_fecha))
    fecha_aj    <- as.character(get_col(input$v_fecha_aj))
    grupedad    <- get_col(input$v_grup_edad)
    
    out <- tibble::tibble(
      id_row = seq_len(nrow(df)),
      sexo, edad, educ, empleo, empleo_otro, ingreso, canton,
      sc1, am1, am3, pe1, pe2, pe2_otro, pe3,
      inicio, envio, fecha, fecha_aj, grupedad
    )
    
    # Unir campos 'otro' de forma segura
    out$empleo_n  <- clean_text(ifelse(!is.na(out$empleo_otro) & out$empleo_otro != "", paste(out$empleo, out$empleo_otro), out$empleo))
    out$pe2_n     <- clean_text(ifelse(!is.na(out$pe2_otro) & out$pe2_otro != "", paste(out$pe2, out$pe2_otro), out$pe2))
    
    # Limpieza de texto estándar
    out$sexo_n    <- clean_text(out$sexo)
    out$educ_n    <- clean_text(out$educ)
    out$ingreso_n <- clean_text(out$ingreso)
    out$canton_n  <- clean_text(out$canton)
    out$sc1_n     <- clean_text(out$sc1)
    out$am1_n     <- clean_text(out$am1)
    out$am3_n     <- clean_text(out$am3)
    out$pe1_n     <- clean_text(out$pe1)
    out$pe3_n     <- clean_text(out$pe3)
    
    suppressWarnings(out$edad_num <- as.numeric(out$edad))
    
    # banda de edad para blocking
    b <- input$band_size
    out$edad_band <- ifelse(is.na(out$edad_num), NA_real_, floor(out$edad_num / b))
    
    out$edad_grp <- ifelse(is.na(out$edad_num),
                           clean_text(out$grupedad),
                           as.character(cut(out$edad_num,
                                            breaks = c(18,25,30,40,50,60,70,100),
                                            include.lowest = TRUE, right = TRUE)))
    
    out$t_inicio    <- parse_time_any(out$inicio)
    out$t_envio     <- parse_time_any(out$envio)
    out$t_fecha     <- parse_time_any(out$fecha)
    out$t_fecha_aj  <- parse_time_any(out$fecha_aj)
    
    out
  })
  
  # >>> MEJORA: UI para mostrar advertencias de validación en la pestaña de normalización
  output$norm_validation_ui <- renderUI({
    df_norm <- dat_norm()
    if (is.null(df_norm)) return()
    
    na_edad_pct <- round(100 * mean(is.na(df_norm$edad_num)), 1)
    if (na_edad_pct > 20) {
      return(tags$div(class = "alert alert-warning",
                      icon("exclamation-triangle"),
                      paste("Advertencia:", na_edad_pct, "% de los valores de edad no pudieron convertirse a número. La calidad del blocking y la distancia de edad puede verse afectada.")))
    }
  })
  
  
  output$norm_preview <- renderDT({
    req(dat_norm())
    show <- dat_norm()[, c("id_row","sexo_n","edad","edad_num","edad_band","edad_grp","educ_n","empleo_n",
                           "ingreso_n","canton_n","sc1_n","am1_n","am3_n","pe1_n","pe2_n","pe3_n",
                           "t_envio","t_inicio","t_fecha_aj")]
    DT::datatable(
      head(show, 20),
      options = list(scrollX=TRUE, pageLength=12, dom='ftip',
                     columnDefs=list(list(className='dt-center', targets=c(0)))),
      rownames = FALSE
    )
  })
  
  # ---------- Generación de candidatos (blocking) ----------
  candidates_build <- function(df2, allow_neighbor = TRUE){
    # Clave de blocking: canton | sexo | banda edad. Esta es la parte más importante
    # de la optimización, ya que reduce drásticamente el número de pares a comparar.
    DT <- data.table(df2[, c("id_row","canton_n","sexo_n","edad_band")])
    setkey(DT, canton_n, sexo_n, edad_band)
    
    if(allow_neighbor){
      # El argumento `roll=1` es una forma muy eficiente en data.table para encontrar
      # coincidencias exactas Y la más cercana (en la última variable de la clave).
      # Esto implementa la lógica de "banda de edad vecina" de forma muy rápida.
      cand <- DT[DT, allow.cartesian=TRUE, roll=1,
                 on = .(canton_n, sexo_n, edad_band)]
    } else {
      # Coincidencia estricta en todas las claves de blocking.
      cand <- DT[DT, allow.cartesian=TRUE,
                 on = .(canton_n, sexo_n, edad_band)]
    }
    # dejar únicamente pares únicos (i < j) para evitar duplicados y auto-comparaciones.
    cand <- cand[id_row < i.id_row, .(i = id_row, j = i.id_row)]
    as.data.frame(cand)
  }
  
  # ---------- Pares (optimizado) ----------
  compute_pairs_chunk <- function(chunk, df2, w, alpha, umbral_medio){
    if(nrow(chunk) == 0) return(tibble::tibble(i=integer(0), j=integer(0), dist=numeric(0), p_dup=numeric(0)))
    
    # >>> MEJORA: Pre-alocación de vectores para almacenar resultados. Esto es mucho más
    # rápido que hacer `rbind` o `c()` dentro de un bucle en R.
    nC <- nrow(chunk)
    res_i <- integer(nC); res_j <- integer(nC)
    res_d <- numeric(nC); res_p <- numeric(nC)
    
    for(idx in seq_len(nC)){
      i <- chunk$i[idx]; j <- chunk$j[idx]
      
      # --- Poda temprana (Pruning) ---
      # Estas son comprobaciones muy baratas. Si un par falla aquí, nos ahorramos
      # todos los cálculos costosos de abajo.
      if(df2$sexo_n[i] != df2$sexo_n[j] && w$sexo >= 1) next
      if(df2$canton_n[i] != df2$canton_n[j] && w$canton >= 1) next
      if(!is.na(df2$edad_num[i]) && !is.na(df2$edad_num[j]) &&
         abs(df2$edad_num[i]-df2$edad_num[j]) > 15 && w$edad >= 1) next
      
      di <- 0; wt <- 0
      
      # --- Cálculo de distancia ---
      # Se calculan primero las variables más baratas (categóricas)
      d <- d_cat(df2$sexo_n[i], df2$sexo_n[j]); di <- di + w$sexo*d; wt <- wt + w$sexo
      d <- d_edad(df2$edad_num[i], df2$edad_num[j]); di <- di + w$edad*d; wt <- wt + w$edad
      d <- d_cat(df2$educ_n[i], df2$educ_n[j]); di <- di + w$educ*d; wt <- wt + w$educ
      d <- d_cat(df2$empleo_n[i], df2$empleo_n[j]); di <- di + w$empleo*d; wt <- wt + w$empleo
      d <- d_cat(df2$ingreso_n[i], df2$ingreso_n[j]); di <- di + w$ingreso*d; wt <- wt + w$ingreso
      d <- d_cat(df2$canton_n[i], df2$canton_n[j]); di <- di + w$canton*d; wt <- wt + w$canton
      
      # --- Poda por Upper Bound ---
      # Si la probabilidad máxima posible (asumiendo distancia 0 para todas las
      # variables restantes) no supera el umbral, no tiene sentido seguir calculando.
      # Esta es una optimización muy potente.
      d_parcial <- if(wt > 0) di/wt else 1
      p_upper <- 1 - (d_parcial^alpha)
      if(p_upper < umbral_medio) next
      
      # Si el par sobrevive a la poda, se calculan las variables más costosas (texto)
      d <- d_cat(df2$am1_n[i], df2$am1_n[j]); di <- di + w$am1*d; wt <- wt + w$am1
      d <- d_text_jw(df2$sc1_n[i], df2$sc1_n[j]); di <- di + w$sc1*d; wt <- wt + w$sc1 # <--- La más cara
      d <- d_cat(df2$am3_n[i], df2$am3_n[j]); di <- di + w$am3*d; wt <- wt + w$am3
      d <- d_cat(df2$pe1_n[i], df2$pe1_n[j]); di <- di + w$pe1*d; wt <- wt + w$pe1
      d <- d_cat(df2$pe2_n[i], df2$pe2_n[j]); di <- di + w$pe2*d; wt <- wt + w$pe2
      d <- d_cat(df2$pe3_n[i], df2$pe3_n[j]); di <- di + w$pe3*d; wt <- wt + w$pe3
      
      # Coalesce para encontrar la mejor marca de tiempo disponible para cada registro
      ti_i <- dplyr::coalesce(df2$t_envio[i], df2$t_inicio[i], df2$t_fecha_aj[i], df2$t_fecha[i])
      ti_j <- dplyr::coalesce(df2$t_envio[j], df2$t_inicio[j], df2$t_fecha_aj[j], df2$t_fecha[j])
      d <- d_tiempo(ti_i, ti_j); di <- di + w$tiempo*d; wt <- wt + w$tiempo
      
      d_norm <- if(wt > 0) di/wt else 1
      p_dup  <- 1 - (d_norm^alpha)
      
      # Almacenar resultados en los vectores pre-alocados
      res_i[idx] <- df2$id_row[i]
      res_j[idx] <- df2$id_row[j]
      res_d[idx] <- d_norm
      res_p[idx] <- p_dup
    }
    
    # Remover filas vacías (donde se usó 'next') de una sola vez
    keep <- res_i > 0
    tibble::tibble(i=res_i[keep], j=res_j[keep], dist=res_d[keep], p_dup=res_p[keep])
  }
  
  # Wrapper reactivo principal
  pairs_compute_opt <- eventReactive(input$run, {
    req(dat_norm())
    df2 <- dat_norm(); n <- nrow(df2)
    if(n < 2) return(tibble::tibble())
    
    # >>> MEJORA: Mostrar notificación de progreso al usuario
    showNotification("Iniciando análisis. Esto puede tardar varios minutos...", type="message", duration = 5)
    
    if(isTRUE(input$use_block)){
      showNotification("Paso 1/3: Generando pares candidatos con 'blocking'...", id="progress_note", type="message", duration = NULL)
      cand <- candidates_build(df2, allow_neighbor = isTRUE(input$edad_neighbor))
    } else {
      showNotification("Paso 1/3: Generando todos los pares (sin blocking, más lento)...", id="progress_note", type="message", duration = NULL)
      idx <- utils::combn(n, 2)
      cand <- data.frame(i = idx[1,], j = idx[2,], row.names = NULL)
    }
    
    if(nrow(cand) == 0) {
      removeNotification("progress_note")
      showNotification("Análisis completado: No se encontraron pares candidatos con los criterios de blocking.", type="warning")
      return(tibble::tibble())
    }
    
    updateTabsetPanel(session, "main_tabs", selected = "3) Pares (score)")
    showNotification(paste("Paso 2/3: Calculando similitud para", nrow(cand), "pares..."), id="progress_note", type="message", duration = NULL)
    
    w <- list(
      sexo = input$w_sexo, edad = input$w_edad, educ = input$w_educ,
      empleo = input$w_empleo, ingreso = input$w_ingreso, canton = input$w_canton,
      am1 = input$w_am1, sc1 = input$w_sc1, am3 = input$w_am3,
      pe1 = input$w_pe1, pe2 = input$w_pe2, pe3 = input$w_pe3,
      tiempo = input$w_tiempo
    )
    
    k <- max(1, min(input$chunks, nrow(cand)))
    part <- split(cand, cut(seq_len(nrow(cand)), breaks = k, labels = FALSE))
    
    alpha <- input$alpha
    umb_m <- input$umbral_medio
    
    if(isTRUE(input$use_parallel)){
      workers <- max(1, future::availableCores() - 1)
      future::plan(future::multisession, workers = workers)
      on.exit(future::plan(future::sequential), add = TRUE)
      
      res_list <- future.apply::future_lapply(
        part,
        compute_pairs_chunk,
        df2 = df2, w = w, alpha = alpha, umbral_medio = umb_m,
        future.seed = TRUE
      )
    } else {
      res_list <- lapply(part, compute_pairs_chunk, df2 = df2, w = w, alpha = alpha, umbral_medio = umb_m)
    }
    
    removeNotification("progress_note")
    showNotification("Paso 3/3: Consolidando resultados...", type="message", duration=2)
    
    pr <- dplyr::bind_rows(res_list)
    pr
  })
  
  # >>> MEJORA: Corrección de error tipográfico (by_y -> by.y) y eliminación de reactive duplicado
  pairs_ranked <- reactive({
    req(pairs_compute_opt(), dat_norm())
    df2 <- dat_norm()
    pr <- pairs_compute_opt()
    if(!nrow(pr)) return(pr)
    
    pr <- pr[order(-pr$p_dup), , drop = FALSE]
    pr$flag_alto  <- pr$p_dup >= input$umbral_alto
    pr$flag_medio <- pr$p_dup >= input$umbral_medio & pr$p_dup < input$umbral_alto
    
    add <- df2[, c("id_row","sexo_n","edad_num","educ_n","canton_n","pe2_n","pe3_n")]
    
    # Unir para obtener información contextual del registro 'i'
    pr <- merge(pr, add, by.x = "i", by.y = "id_row", all.x = TRUE, sort = FALSE)
    names(pr)[names(pr) %in% c("sexo_n","edad_num","educ_n","canton_n","pe2_n","pe3_n")] <-
      paste0(c("sexo_n","edad_num","educ_n","canton_n","pe2_n","pe3_n"), "_i")
    
    # Unir para obtener información contextual del registro 'j'
    pr <- merge(pr, add, by.x = "j", by.y = "id_row", all.x = TRUE, sort = FALSE)
    names(pr)[names(pr) %in% c("sexo_n","edad_num","educ_n","canton_n","pe2_n","pe3_n")] <-
      paste0(c("sexo_n","edad_num","educ_n","canton_n","pe2_n","pe3_n"), "_j")
    
    pr[order(-pr$p_dup), , drop = FALSE]
  })
  
  output$pairs_top <- renderDT({
    req(pairs_ranked())
    dt <- pairs_ranked()
    DT::datatable(
      head(dt[, c("i","j","p_dup","dist","flag_alto","flag_medio",
                  grep("_i$|_j$", names(dt), value = TRUE))], 200),
      options = list(
        scrollX=TRUE, pageLength=25, dom='ftip',
        columnDefs = list(
          list(className='dt-center', targets = 0:5),
          list(render = JS(
            "function(data,type,row,meta){",
            " if(type==='display'){return (typeof data==='number')? data.toFixed(3): data}",
            " return data; }"
          ), targets = c(2,3))
        )
      ),
      rownames = FALSE
    )
  })
  
  # ---------- Clustering ----------
  clusters_tbl <- reactive({
    req(pairs_compute_opt(), dat_norm())
    
    updateTabsetPanel(session, "main_tabs", selected = "4) Clustering")
    
    df2 <- dat_norm()
    pr  <- pairs_compute_opt()
    n <- nrow(df2); if(n < 2) return(tibble::tibble())
    
    # Matriz dispersa a partir de pares.
    D <- matrix(1, n, n)
    diag(D) <- 0
    if(nrow(pr) > 0){
      # >>> MEJORA DE RENDIMIENTO: Vectorización.
      # En lugar de un bucle for, se usa indexación matricial, que es órdenes de
      # magnitud más rápida en R para esta tarea.
      idx_upper <- cbind(pr$i, pr$j)
      idx_lower <- cbind(pr$j, pr$i)
      D[idx_upper] <- pr$dist
      D[idx_lower] <- pr$dist
    }
    rownames(D) <- df2$id_row
    colnames(D) <- df2$id_row
    
    hc <- hclust(as.dist(D), method="average")
    cl_str <- cutree(hc, h = input$h_stricto)
    cl_med <- cutree(hc, h = input$h_medio)
    cl_lax <- cutree(hc, h = input$h_laxo)
    
    dct <- data.frame(
      id_row = df2$id_row,
      cl_stricto = cl_str,
      cl_medio   = cl_med,
      cl_laxo    = cl_lax,
      stringsAsFactors = FALSE
    )
    
    resumir <- function(vector_clusters, nivel){
      # >>> MEJORA: Usar dplyr para una sintaxis más limpia y potencialmente más rápida
      tibble(id_row = dct$id_row, cluster_id = vector_clusters) %>%
        group_by(cluster_id) %>%
        filter(n() > 1) %>%
        summarise(
          n = n(),
          miembros = paste(sort(id_row), collapse = ","),
          .groups = 'drop'
        ) %>%
        mutate(nivel = nivel) %>%
        select(n, miembros, nivel) %>%
        arrange(desc(n))
    }
    
    out <- dplyr::bind_rows(
      resumir(dct$cl_stricto, "estricto"),
      resumir(dct$cl_medio,   "medio"),
      resumir(dct$cl_laxo,    "laxo")
    )
    
    showNotification("Análisis completo.", type="message", duration=4)
    # >>> MEJORA: Mover el cambio a la nueva pestaña de interpretación al final del todo.
    updateTabsetPanel(session, "main_tabs", selected = "interpret_panel")
    tibble::as_tibble(out)
  })
  
  output$clusters_tbl <- renderDT({
    req(clusters_tbl())
    DT::datatable(
      clusters_tbl(),
      options = list(scrollX=TRUE, pageLength=20, dom='ftip',
                     columnDefs=list(list(className='dt-center', targets=0))),
      rownames = FALSE
    )
  })
  
  # ---------- Inspección ----------
  output$inspect_controls <- renderUI({
    req(pairs_ranked(), clusters_tbl())
    pr <- pairs_ranked()
    # >>> MEJORA: Limitar el número de opciones para que la UI no se congele
    pr_sample <- head(pr, 1000)
    pr_sample$lbl <- paste0(pr_sample$i," - ",pr_sample$j," (p=", round(pr_sample$p_dup,3),")")
    cl <- clusters_tbl()
    
    tagList(
      h4("Inspección de casos"),
      fluidRow(
        column(6, selectInput("pair_sel", "Par (top 1000)",
                              choices = setNames(paste0(pr_sample$i,"|",pr_sample$j), pr_sample$lbl), width="100%")),
        column(6, selectInput("cluster_sel", "Cluster",
                              choices = if(nrow(cl)) setNames(cl$miembros,
                                                              paste0(cl$nivel," | ",cl$miembros," (n=",cl$n,")")) else character(0),
                              width="100%"))
      )
    )
  })
  
  # >>> MEJORA: Crear un reactive para la selección de IDs para no repetir código
  selected_ids <- reactive({
    # Priorizar la selección de cluster sobre la de par
    if (!is.null(input$cluster_sel) && nzchar(input$cluster_sel)) {
      as.integer(strsplit(input$cluster_sel, ",")[[1]])
    } else if (!is.null(input$pair_sel) && nzchar(input$pair_sel)) {
      as.integer(strsplit(input$pair_sel, "\\|")[[1]])
    } else {
      NULL
    }
  })
  
  output$inspect_table <- renderDT({
    req(dat_norm(), selected_ids())
    df2 <- dat_norm()
    ids <- selected_ids()
    
    view_cols <- c("id_row","sexo","edad","educ","empleo","ingreso","canton",
                   "sc1","am1","am3","pe1","pe2","pe3","inicio","envio","fecha","fecha_aj")
    
    sub_df <- df2[df2$id_row %in% ids, intersect(view_cols, names(df2)), drop=FALSE]
    sub_df <- sub_df[order(sub_df$id_row), , drop = FALSE]
    DT::datatable(sub_df, options = list(scrollX=TRUE, pageLength=10, dom='t'), rownames = FALSE)
  })
  
  # >>> NUEVO: Reactive para la base de datos final
  final_data <- eventReactive(input$run, {
    req(dat_raw(), pairs_ranked())
    
    pr <- pairs_ranked()
    # Identificar los IDs de las filas que participan en un par de alta sospecha
    ids_to_remove <- unique(c(pr$i[pr$flag_alto], pr$j[pr$flag_alto]))
    
    if (length(ids_to_remove) > 0) {
      # Las IDs corresponden al número de fila original
      dat_raw()[-ids_to_remove, , drop = FALSE]
    } else {
      dat_raw() # Si no hay duplicados altos, devolver la tabla original
    }
  })
  
  # >>> NUEVO: Render de la tabla para la base final
  output$final_database_table <- renderDT({
    req(final_data())
    DT::datatable(
      final_data(),
      options = list(scrollX = TRUE, pageLength = 25, dom = 'Bfrtip'),
      rownames = FALSE,
      filter = 'top'
    )
  })
  
  # ---------- Descargas ----------
  # >>> MODIFICADO: Añadido el botón de descarga para la base final
  output$download_buttons_ui <- renderUI({
    req(pairs_ranked())
    tagList(
      downloadButton("dl_pairs", "Pares (CSV)"),
      downloadButton("dl_clusters", "Clusters (CSV)"),
      downloadButton("dl_final_db", "Base Final (Excel)")
    )
  })
  
  output$dl_pairs <- downloadHandler(
    filename = function(){ paste0("posibles_duplicados_pares_", Sys.Date(), ".csv") },
    content  = function(file){ readr::write_csv(pairs_ranked(), file) }
  )
  output$dl_clusters <- downloadHandler(
    filename = function(){ paste0("posibles_duplicados_clusters_", Sys.Date(), ".csv") },
    content  = function(file){ readr::write_csv(clusters_tbl(), file) }
  )
  
  # >>> NUEVO: Handler para descargar la base de datos final en Excel
  output$dl_final_db <- downloadHandler(
    filename = function() { paste0("base_datos_final_sin_altos_", Sys.Date(), ".xlsx") },
    content = function(file) {
      req(final_data())
      writexl::write_xlsx(final_data(), file)
    }
  )
  
  # >>> NUEVA SECCIÓN: Lógica para la pestaña de Interpretación
  output$interpretation_ui <- renderUI({
    req(pairs_ranked(), clusters_tbl(), dat_norm())
    
    pr <- pairs_ranked()
    cl <- clusters_tbl()
    df_norm <- dat_norm()
    
    # Cálculos para el resumen
    n_total <- nrow(df_norm)
    n_alta <- sum(pr$flag_alto)
    n_media <- sum(pr$flag_medio)
    
    ids_implicados_pares <- unique(c(
      pr$i[pr$flag_alto | pr$flag_medio],
      pr$j[pr$flag_alto | pr$flag_medio]
    ))
    n_implicados_pares <- length(ids_implicados_pares)
    
    n_clusters_total <- nrow(cl)
    
    ids_implicados_clusters <- if (nrow(cl) > 0) {
      length(unique(as.integer(unlist(strsplit(cl$miembros, ",")))))
    } else {
      0
    }
    
    tagList(
      h3("Interpretación de Resultados"),
      p("Esta sección resume los hallazgos del análisis, explicando cuántos posibles duplicados se han identificado y qué significa cada categoría."),
      
      # Análisis basado en Pares
      div(class="summary-box",
          h4("Análisis por Pares de Registros"),
          p(paste0("Se compararon registros de dos en dos. Un 'par' consiste en dos filas de datos (ej. Fila 5 y Fila 87) que se parecen entre sí por encima de un umbral.")),
          fluidRow(
            column(4, strong("Pares de sospecha ALTA:"), tags$span(class="stat", n_alta)),
            column(4, strong("Pares de sospecha MEDIA:"), tags$span(class="stat", n_media)),
            column(4, strong("Registros únicos implicados:"), tags$span(class="stat", n_implicados_pares))
          ),
          br(),
          tags$ul(
            tags$li(strong("Sospecha ALTA:"), paste0("Se encontraron ", n_alta, " pares de registros cuyo score de duplicado fue mayor o igual a ", strong(input$umbral_alto), ". Estos son los candidatos más fuertes a ser duplicados y requieren revisión prioritaria.")),
            tags$li(strong("Sospecha MEDIA:"), paste0("Se encontraron ", n_media, " pares cuyo score estuvo entre ", strong(input$umbral_medio), " y ", input$umbral_alto, ". Estos casos son sospechosos pero menos evidentes que los de categoría alta.")),
            tags$li(strong("Registros Únicos Implicados:"), paste0("En total, ", n_implicados_pares, " de los ", n_total, " registros originales (un ", round(100 * n_implicados_pares/n_total, 1), "%) están involucrados en al menos un par sospechoso."))
          )
      ),
      
      # Análisis basado en Clusters
      div(class="summary-box", style = "border-left-color: #5cb85c;",
          h4("Análisis por Grupos (Clusters)"),
          p("El clustering agrupa registros que son similares entre sí, permitiendo encontrar duplicados de 3 o más respuestas (ej. Filas 10, 55 y 120 son un mismo grupo). Se definen según la 'altura' o distancia máxima permitida entre miembros del grupo."),
          fluidRow(
            column(4, strong("Clusters totales encontrados:"), tags$span(class="stat", n_clusters_total)),
            column(4, strong("Registros únicos en clusters:"), tags$span(class="stat", ids_implicados_clusters))
          ),
          br(),
          tags$ul(
            tags$li(strong("Estricto (altura ≤ ", input$h_stricto, "):"), paste0("Se encontraron ", sum(cl$nivel == "estricto"), " grupos. Estos son los más cohesivos y con mayor probabilidad de ser duplicados reales.")),
            tags$li(strong("Medio (altura ≤ ", input$h_medio, "):"), paste0("Se encontraron ", sum(cl$nivel == "medio"), " grupos, permitiendo un poco más de variabilidad interna.")),
            tags$li(strong("Laxo (altura ≤ ", input$h_laxo, "):"), paste0("Se encontraron ", sum(cl$nivel == "laxo"), " grupos, con los criterios de similitud más flexibles."))
          )
      ),
      
      # Conclusión y Siguientes Pasos
      h4("Conclusión y Siguientes Pasos"),
      p("Los resultados sugieren la presencia de respuestas potencialmente duplicadas. Se recomienda proceder con los siguientes pasos:"),
      tags$ol(
        tags$li("Utilice la pestaña ", strong("5) Inspección"), " para revisar manualmente los pares y clusters con la puntuación más alta. Seleccione un par o cluster de la lista para ver los datos originales uno al lado del otro."),
        tags$li("Descargue los resultados desde los botones en el panel lateral para un análisis más profundo en Excel u otro software."),
        tags$li("Considere ajustar los ", strong("pesos de las variables"), " y los ", strong("umbrales"), " en el panel lateral y volver a ejecutar el análisis si los resultados no se ajustan a las expectativas.")
      )
    )
  })
  
  # ---------- Metodología ----------
  output$metodo_html <- renderUI({
    HTML(paste0(
      "<h4>Objetivo</h4>",
      "<p>Estimar la probabilidad de duplicado entre registros usando una distancia compuesta y optimizaciones de rendimiento.</p>",
      "<h4>Normalización</h4>",
      "<ul>",
      "<li>Textos: Minúsculas, sin tildes, limpieza de signos no alfanuméricos.</li>",
      "<li>Consolidación: Fusión de campos categóricos con sus respectivos campos 'otro'.</li>",
      "<li>Edad: Extracción de valor numérico y creación de una 'banda de edad' para blocking de ", input$band_size, " años.</li>",
      "<li>Tiempos: Parseo flexible de múltiples formatos de fecha/hora.</li>",
      "</ul>",
      "<h4>Métrica de Distancia por Variable (d_k)</h4>",
      "<ul>",
      "<li><b>Categóricas:</b> d = 1 si son distintos, 0 si son iguales (distancia de Hamming).</li>",
      "<li><b>Texto (SC1):</b> d = 1 − Similitud de Jaro–Winkler. Se usan atajos (truncado a 160 caracteres, diferencia de longitud) para acelerar.</li>",
      "<li><b>Edad:</b> d = min(|Δedad| / 5, 1). Una diferencia de 5 años o más es distancia máxima. Si falta un dato, se imputa 0.5.</li>",
      "<li><b>Tiempo:</b> Distancia por tramos (0, 0.25, 0.5, 0.75, 1) basada en la diferencia en minutos. Si falta, se imputa 0.5.</li>",
      "</ul>",
      "<h4>Cálculo del Score de Duplicado</h4>",
      "<p>La distancia compuesta es un promedio ponderado de las distancias individuales: <code>d_compuesta = (Σ w_k * d_k) / (Σ w_k)</code>.</p>",
      "<p>El score final (probabilidad de duplicado) se calcula como: <code>p_dup = 1 − (d_compuesta) ^ α</code>. El parámetro <code>α</code> (alpha) ajusta la curvatura, permitiendo penalizar más o menos las distancias pequeñas.</p>",
      "<h4>Optimización</h4>",
      "<ul>",
      "<li><b>Blocking:</b> Se reduce el universo de comparación de N*(N-1)/2 pares a solo aquellos que coinciden en cantón, sexo y banda de edad. Se puede permitir una tolerancia de ±1 en la banda de edad.</li>",
      "<li><b>Poda (Pruning):</b> Durante el cálculo, si un par ya es muy diferente con base en variables 'baratas' (categóricas), se descarta antes de calcular la distancia de texto (Jaro-Winkler), que es la más costosa computacionalmente.</li>",
      "<li><b>Paralelización:</b> El cálculo de los chunks de pares se puede distribuir entre múltiples núcleos de la CPU para acelerar el proceso.</li>",
      "</ul>",
      "<h4>Umbrales y Clustering</h4>",
      "<p>Los pares se clasifican en sospecha 'alta' (<code>p_dup ≥ umbral_alto</code>) y 'media'. Adicionalmente, se realiza un clustering jerárquico (método de enlace promedio) sobre la matriz de distancias para identificar grupos de duplicados (más de 2 registros).</p>"
    ))
  })
}

shinyApp(ui, server)