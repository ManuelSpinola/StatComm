# =============================================================================
# mod_upload.R — Carga de datos para StatComm
# StatComm · StatSuite · Manuel Spínola · ICOMVIS · UNA
# =============================================================================
# Exporta:
#   mod_upload_ui(id)
#   mod_upload_server(id)  →  reactivo con $Y, $X, $traits, $meta, $source
# =============================================================================

# ── UI ────────────────────────────────────────────────────────────────────────
mod_upload_ui <- function(id) {
  ns <- NS(id)

  tagList(

    div(
      class = "py-3 px-2",
      h4(
        bs_icon("upload", class = "me-2"),
        "Datos de comunidad",
        style = paste0("color:", colores$primario, "; font-weight:700;")
      ),
      p(
        class = "text-muted mb-0",
        "Cargá un conjunto de datos de ejemplo o subí tus propios datos. ",
        "Los análisis de GLLVM y mrIML compartirán estos datos."
      )
    ),

    navset_card_tab(

      # ══════════════════════════════════════════════════════════════════
      # PESTAÑA 1: Datos de ejemplo
      # ══════════════════════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("collection", class = "me-1"), "Datos de ejemplo"),
        card_body(

          layout_columns(
            col_widths = c(4, 8),

            # ── Selector ──────────────────────────────────────────────
            div(
              selectInput(
                inputId  = ns("example_choice"),
                label    = "Seleccionar conjunto de datos",
                choices  = list(
                  "Arañas cazadoras (mvabund)"         = "spider",
                  "Hongos — presencia/ausencia (gllvm)" = "fungi",
                  "Comunidad microbiana (gllvm)"        = "microbiome",
                  "Ácaros oribátidos — predictores mixtos (vegan)" = "mites",
                  "Peces río Doubs (ade4)"              = "doubs",
                  "Escarabajos + traits (gllvm)"        = "beetle"
                ),
                selected = "spider"
              ),

              # Tarjeta informativa del dataset seleccionado
              uiOutput(ns("example_info"))
            ),

            # ── Vista previa ──────────────────────────────────────────
            div(
              uiOutput(ns("preview_ejemplo"))
            )
          )
        )
      ),

      # ══════════════════════════════════════════════════════════════════
      # PESTAÑA 2: Subir mis datos
      # ══════════════════════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("folder2-open", class = "me-1"), "Subir mis datos"),
        card_body(

          p(class = "text-muted small mb-3",
            bs_icon("info-circle", class = "me-1"),
            "Los datos se suben en partes separadas. Los nombres de sitios deben ",
            "coincidir en nombre y orden entre la matriz de especies (Y) y los ",
            "predictores (X). Los traits deben tener los nombres de especies como filas."
          ),

          layout_columns(
            col_widths = c(4, 4, 4),

            # ── Matriz de especies (Y) ─────────────────────────────
            card(
              card_header(
                tagList(
                  tags$span(
                    class = "badge me-2",
                    style = paste0("background:", colores$primario),
                    "Requerido"
                  ),
                  bs_icon("table", class = "me-1"),
                  "Matriz de especies (Y)"
                )
              ),
              card_body(
                p(class = "small text-muted",
                  "Filas = sitios, columnas = especies. ",
                  "La primera columna debe contener los nombres de sitio."),
                fileInput(
                  inputId     = ns("file_Y"),
                  label       = NULL,
                  accept      = c(".csv", ".xlsx", ".xls"),
                  placeholder = "Sin archivo"
                ),
                uiOutput(ns("status_Y"))
              )
            ),

            # ── Predictores ambientales (X) ────────────────────────
            card(
              card_header(
                tagList(
                  tags$span(
                    class = "badge me-2",
                    style = paste0("background:", colores$secundario),
                    "Opcional"
                  ),
                  bs_icon("sliders", class = "me-1"),
                  "Predictores (X)"
                )
              ),
              card_body(
                p(class = "small text-muted",
                  "Mismo orden y nombres de sitio que Y. ",
                  "Los factores se detectan automáticamente."),
                fileInput(
                  inputId     = ns("file_X"),
                  label       = NULL,
                  accept      = c(".csv", ".xlsx", ".xls"),
                  placeholder = "Sin archivo"
                ),
                uiOutput(ns("status_X")),
                uiOutput(ns("factor_selector"))
              )
            ),

            # ── Traits de especies ─────────────────────────────────
            card(
              card_header(
                tagList(
                  tags$span(
                    class = "badge me-2",
                    style = paste0("background:", colores$secundario),
                    "Opcional"
                  ),
                  bs_icon("diagram-3", class = "me-1"),
                  "Traits de especies"
                )
              ),
              card_body(
                p(class = "small text-muted",
                  "Filas = especies (deben coincidir con columnas de Y). ",
                  "Columnas = rasgos funcionales o morfológicos."),
                fileInput(
                  inputId     = ns("file_traits"),
                  label       = NULL,
                  accept      = c(".csv", ".xlsx", ".xls"),
                  placeholder = "Sin archivo"
                ),
                uiOutput(ns("status_traits")),
                uiOutput(ns("factor_selector_traits"))
              )
            )
          ),

          # ── Validación ─────────────────────────────────────────────
          uiOutput(ns("validation_summary")),

          # ── Vista previa datos usuario ──────────────────────────────
          uiOutput(ns("preview_usuario"))
        )
      )
    )
  )
}


# ── Server ────────────────────────────────────────────────────────────────────
mod_upload_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Ruta a datos de ejemplo ──────────────────────────────────────────────
    data_path <- system.file("data", package = "StatComm")
    if (data_path == "") data_path <- "inst/data"

    # ════════════════════════════════════════════════════════════════════════
    # DATOS DE EJEMPLO
    # ════════════════════════════════════════════════════════════════════════

    example_data <- reactive({
      req(input$example_choice)
      path <- file.path(data_path, paste0(input$example_choice, ".rds"))
      if (!file.exists(path)) {
        showNotification(
          paste("Conjunto de datos no encontrado:", basename(path)),
          type = "error"
        )
        return(NULL)
      }
      obj <- readRDS(path)
      obj$source <- "example"
      obj
    })

    output$example_info <- renderUI({
      obj <- example_data()
      req(obj)
      m <- obj$meta

      div(
        class = "mt-3 p-3 rounded",
        style = paste0("background:", colores$fondo,
                       "; border-left: 4px solid ", colores$primario, ";"),
        tags$p(class = "fw-bold mb-1",
               style = paste0("color:", colores$primario), m$name),
        tags$p(class = "small text-muted mb-2", m$description),
        tags$p(
          class = "small mb-1",
          tags$b("Sitios: "),    m$n_sites,    " · ",
          tags$b("Especies: "),  m$n_species,  " · ",
          tags$b("Predictores: "), m$n_predictors
        ),
        tags$p(
          class = "small mb-1",
          tags$b("Respuesta: "), m$response, " · ",
          tags$b("Familia sugerida: "), m$family_suggestion
        ),
        tags$p(
          class = "small mb-1",
          tags$b("Traits: "),
          if (isTRUE(m$has_traits)) paste0(m$n_traits, " variables") else "No"
        ),
        if (!is.null(m$reference)) {
          tags$p(class = "small text-muted fst-italic mb-0", m$reference)
        }
      )
    })

    output$preview_ejemplo <- renderUI({
      obj <- example_data()
      req(obj)
      tagList(
        tags$p(class = "fw-semibold small mb-1",
               style = paste0("color:", colores$primario),
               bs_icon("table", class = "me-1"), "Vista previa — Matriz Y"),
        div(style = "overflow-x: auto; font-size: 0.82rem;",
            tableOutput(ns("prev_ej_Y"))
        ),
        if (!is.null(obj$X)) {
          tagList(
            tags$p(class = "fw-semibold small mb-1 mt-3",
                   style = paste0("color:", colores$primario),
                   bs_icon("sliders", class = "me-1"), "Vista previa — Predictores X"),
            div(style = "overflow-x: auto; font-size: 0.82rem;",
                tableOutput(ns("prev_ej_X"))
            )
          )
        },
        if (!is.null(obj$traits)) {
          tagList(
            tags$p(class = "fw-semibold small mb-1 mt-3",
                   style = paste0("color:", colores$primario),
                   bs_icon("diagram-3", class = "me-1"), "Vista previa — Traits"),
            div(style = "overflow-x: auto; font-size: 0.82rem;",
                tableOutput(ns("prev_ej_traits"))
            )
          )
        }
      )
    })

    output$prev_ej_Y <- renderTable({
      obj <- example_data()
      req(obj)
      obj$Y[seq_len(min(6, nrow(obj$Y))),
            seq_len(min(8, ncol(obj$Y))), drop = FALSE]
    }, rownames = TRUE, digits = 0)

    output$prev_ej_X <- renderTable({
      obj <- example_data()
      req(obj, obj$X)
      obj$X[seq_len(min(6, nrow(obj$X))), , drop = FALSE]
    }, rownames = TRUE)

    output$prev_ej_traits <- renderTable({
      obj <- example_data()
      req(obj, obj$traits)
      obj$traits[seq_len(min(6, nrow(obj$traits))),
                 seq_len(min(6, ncol(obj$traits))), drop = FALSE]
    }, rownames = TRUE)


    # ════════════════════════════════════════════════════════════════════════
    # DATOS DEL USUARIO — helpers
    # ════════════════════════════════════════════════════════════════════════

    read_uploaded_file <- function(file_info) {
      req(file_info)
      ext <- tolower(tools::file_ext(file_info$name))
      tryCatch({
        switch(ext,
          csv  = read.csv(file_info$datapath,
                          row.names        = 1,
                          check.names      = FALSE,
                          stringsAsFactors = FALSE),
          xlsx = ,
          xls  = readxl::read_excel(file_info$datapath) |>
                   as.data.frame() |>
                   (\(d) { rownames(d) <- d[[1]]; d[, -1, drop = FALSE] })(),
          stop("Formato no soportado: .", ext)
        )
      }, error = function(e) {
        showNotification(
          paste("Error leyendo", file_info$name, ":", e$message),
          type = "error", duration = 8
        )
        NULL
      })
    }

    auto_detect_factors <- function(df) {
      sapply(df, function(col) {
        is.character(col) || is.factor(col) ||
        (is.numeric(col) && length(unique(col)) <= 6 &&
           all(col == as.integer(col), na.rm = TRUE))
      })
    }

    raw_Y      <- reactive(read_uploaded_file(input$file_Y))
    raw_X      <- reactive(read_uploaded_file(input$file_X))
    raw_traits <- reactive(read_uploaded_file(input$file_traits))

    # ── Factor selector ───────────────────────────────────────────────────────
    output$factor_selector <- renderUI({
      X <- raw_X()
      req(X)
      auto     <- names(which(auto_detect_factors(X)))
      non_auto <- names(X)[!names(X) %in% auto]
      if (length(non_auto) == 0) return(NULL)
      tagList(
        tags$p(class = "small text-muted mt-2 mb-1",
               tags$b("Factores auto-detectados: "),
               if (length(auto) > 0) paste(auto, collapse = ", ") else "ninguno"),
        checkboxGroupInput(
          inputId  = ns("extra_factors"),
          label    = "Convertir a factor:",
          choices  = non_auto,
          selected = NULL,
          inline   = TRUE
        )
      )
    })

    processed_X <- reactive({
      X <- raw_X()
      req(X)
      auto   <- names(which(auto_detect_factors(X)))
      manual <- input$extra_factors %||% character(0)
      for (col in unique(c(auto, manual))) {
        if (col %in% names(X)) X[[col]] <- factor(X[[col]])
      }
      X
    })

    # ── Factor selector traits ───────────────────────────────────────────────
    output$factor_selector_traits <- renderUI({
      tr <- raw_traits()
      req(tr)
      auto     <- names(which(auto_detect_factors(tr)))
      non_auto <- names(tr)[!names(tr) %in% auto]
      if (length(non_auto) == 0) return(NULL)
      tagList(
        tags$p(class = "small text-muted mt-2 mb-1",
               tags$b("Factores auto-detectados: "),
               if (length(auto) > 0) paste(auto, collapse = ", ") else "ninguno"),
        checkboxGroupInput(
          inputId  = ns("extra_factors_traits"),
          label    = "Convertir a factor:",
          choices  = non_auto,
          selected = NULL,
          inline   = TRUE
        )
      )
    })

    processed_traits <- reactive({
      tr <- raw_traits()
      req(tr)
      auto   <- names(which(auto_detect_factors(tr)))
      manual <- input$extra_factors_traits %||% character(0)
      for (col in unique(c(auto, manual))) {
        if (col %in% names(tr)) tr[[col]] <- factor(tr[[col]])
      }
      tr
    })

    # ── Status widgets ────────────────────────────────────────────────────────
    status_ok <- function(df, label) {
      if (is.null(df)) return(NULL)
      div(
        class = "small mt-1",
        style = paste0("color:", colores$primario),
        bs_icon("check-circle", class = "me-1"),
        sprintf("%s: %d filas × %d columnas", label, nrow(df), ncol(df))
      )
    }

    output$status_Y <- renderUI(status_ok(raw_Y(), "Y"))

    output$status_traits <- renderUI({
      tr <- processed_traits()
      if (is.null(tr)) return(NULL)
      tagList(
        status_ok(tr, "Traits"),
        div(
          class = "d-flex flex-wrap gap-1 mt-2",
          lapply(names(tr), function(nm) {
            es_factor <- is.factor(tr[[nm]]) || is.character(tr[[nm]])
            tags$span(
              class = "badge",
              style = paste0("background:",
                if (es_factor) colores$acento else colores$primario,
                "; font-size:0.72rem;"),
              paste0(nm, if (es_factor) " (F)" else " (N)")
            )
          })
        )
      )
    })

    output$status_X <- renderUI({
      X <- processed_X()
      if (is.null(X)) return(NULL)
      tagList(
        status_ok(X, "X"),
        div(
          class = "d-flex flex-wrap gap-1 mt-2",
          lapply(names(X), function(nm) {
            es_factor <- is.factor(X[[nm]]) || is.character(X[[nm]])
            tags$span(
              class = "badge",
              style = paste0("background:",
                if (es_factor) colores$acento else colores$primario,
                "; font-size:0.72rem;"),
              paste0(nm, if (es_factor) " (F)" else " (N)")
            )
          })
        )
      )
    })

    # ── Validación ────────────────────────────────────────────────────────────
    validation <- reactive({
      Y <- raw_Y()
      if (is.null(Y)) return(list(ok = FALSE, errors = NULL, warnings = NULL))

      errors <- warnings <- character(0)

      non_num <- names(Y)[!sapply(Y, is.numeric)]
      if (length(non_num) > 0)
        errors <- c(errors, paste("Y contiene columnas no numéricas:",
                                  paste(non_num, collapse = ", ")))

      X <- processed_X()
      if (!is.null(X)) {
        if (!identical(rownames(Y), rownames(X))) {
          if (setequal(rownames(Y), rownames(X)))
            errors <- c(errors,
              "Los nombres de sitio coinciden pero están en diferente orden entre Y y X.")
          else {
            mis_x <- setdiff(rownames(Y), rownames(X))
            mis_y <- setdiff(rownames(X), rownames(Y))
            if (length(mis_x) > 0)
              errors <- c(errors, paste("Sitios en Y no encontrados en X:",
                                        paste(head(mis_x, 5), collapse = ", ")))
            if (length(mis_y) > 0)
              errors <- c(errors, paste("Sitios en X no encontrados en Y:",
                                        paste(head(mis_y, 5), collapse = ", ")))
          }
        }
      }

      tr <- raw_traits()
      if (!is.null(tr)) {
        if (!identical(rownames(tr), colnames(Y))) {
          if (setequal(rownames(tr), colnames(Y)))
            errors <- c(errors,
              "Los nombres de especie en traits coinciden pero están en diferente orden.")
          else {
            mis_tr <- setdiff(colnames(Y), rownames(tr))
            ext_tr <- setdiff(rownames(tr), colnames(Y))
            if (length(mis_tr) > 0)
              errors <- c(errors, paste("Especies sin traits:",
                                        paste(head(mis_tr, 5), collapse = ", ")))
            if (length(ext_tr) > 0)
              warnings <- c(warnings, paste("Traits de especies no en Y (ignorados):",
                                            paste(head(ext_tr, 5), collapse = ", ")))
          }
        }
      }

      zero_sites <- rownames(Y)[rowSums(Y, na.rm = TRUE) == 0]
      if (length(zero_sites) > 0)
        warnings <- c(warnings, paste("Sitios con abundancia total cero:",
                                      paste(head(zero_sites, 5), collapse = ", ")))

      zero_spp <- colnames(Y)[colSums(Y, na.rm = TRUE) == 0]
      if (length(zero_spp) > 0)
        warnings <- c(warnings, paste("Especies sin registros:",
                                      paste(head(zero_spp, 5), collapse = ", ")))

      list(ok = length(errors) == 0, errors = errors, warnings = warnings)
    })

    output$validation_summary <- renderUI({
      req(raw_Y())
      v <- validation()
      tagList(
        if (length(v$errors) > 0)
          div(class = "alert alert-danger small mt-3",
              tags$b("Errores:"),
              tags$ul(class = "mb-0", lapply(v$errors, tags$li))),
        if (length(v$warnings) > 0)
          div(class = "alert alert-warning small mt-3",
              tags$b("Advertencias:"),
              tags$ul(class = "mb-0", lapply(v$warnings, tags$li))),
        if (v$ok)
          div(class = "alert alert-success small mt-3",
              bs_icon("check-circle", class = "me-1"),
              "Datos validados correctamente. Listos para analizar.")
      )
    })

    output$preview_usuario <- renderUI({
      v <- validation()
      req(v$ok, raw_Y())
      tagList(
        tags$hr(),
        tags$p(class = "fw-semibold small mb-1",
               style = paste0("color:", colores$primario),
               bs_icon("table", class = "me-1"), "Vista previa — Matriz Y"),
        div(style = "overflow-x: auto; font-size: 0.82rem;",
            tableOutput(ns("prev_usr_Y"))
        ),
        if (!is.null(processed_X())) {
          tagList(
            tags$p(class = "fw-semibold small mb-1 mt-3",
                   style = paste0("color:", colores$primario),
                   bs_icon("sliders", class = "me-1"), "Vista previa — Predictores X"),
            div(style = "overflow-x: auto; font-size: 0.82rem;",
                tableOutput(ns("prev_usr_X"))
            )
          )
        }
      )
    })

    output$prev_usr_Y <- renderTable({
      v <- validation()
      req(v$ok, raw_Y())
      Y <- raw_Y()
      Y[seq_len(min(6, nrow(Y))), seq_len(min(8, ncol(Y))), drop = FALSE]
    }, rownames = TRUE, digits = 0)

    output$prev_usr_X <- renderTable({
      v <- validation()
      req(v$ok, processed_X())
      X <- processed_X()
      X[seq_len(min(6, nrow(X))), , drop = FALSE]
    }, rownames = TRUE)

    # ════════════════════════════════════════════════════════════════════════
    # DATOS ACTIVOS — compartidos con otros módulos
    # ════════════════════════════════════════════════════════════════════════

    active_data <- reactive({
      # Determinar pestaña activa por si hay file_Y cargado
      if (!is.null(input$file_Y)) {
        v <- validation()
        if (!v$ok || is.null(raw_Y())) return(NULL)
        list(
          Y      = raw_Y(),
          X      = processed_X(),
          traits = processed_traits(),
          meta   = list(
            name         = "Datos propios",
            response     = "desconocido",
            n_sites      = nrow(raw_Y()),
            n_species    = ncol(raw_Y()),
            n_predictors = if (is.null(processed_X())) 0L else ncol(processed_X()),
            has_traits   = !is.null(raw_traits())
          ),
          source = "user"
        )
      } else {
        example_data()
      }
    })

    return(reactive(active_data()))
  })
}

`%||%` <- function(x, y) if (is.null(x)) y else x
