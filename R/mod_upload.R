# =============================================================================
# StatCoom — mod_upload.R
# =============================================================================
# Data input module: example datasets + user upload (Y, X, traits)
#
# Exports:
#   mod_upload_ui(id)
#   mod_upload_server(id)  →  returns reactive list: data()
#                              with $Y, $X, $traits, $meta, $source
# =============================================================================

# --------------------------------------------------------------------------- #
#  UI                                                                          #
# --------------------------------------------------------------------------- #

mod_upload_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # ---- Source selector ----
    div(
      class = "upload-source-selector",
      radioButtons(
        inputId  = ns("data_source"),
        label    = "Data source",
        choices  = c(
          "Use an example dataset" = "example",
          "Upload my own data"     = "user"
        ),
        selected = "example",
        inline   = TRUE
      )
    ),

    hr(),

    # ---- Example datasets panel ----
    conditionalPanel(
      condition = sprintf("input['%s'] == 'example'", ns("data_source")),

      selectInput(
        inputId  = ns("example_choice"),
        label    = "Select example dataset",
        choices  = list(
          "Spider communities (mvabund)"     = "spider",
          "Fungi — presence/absence (gllvm)" = "fungi",
          "Microbiome counts (gllvm)"        = "microbiome",
          "Oribatid mites — mixed predictors (vegan)" = "mites",
          "Doubs river fish (ade4)"          = "doubs"
        ),
        selected = "spider"
      ),

      # Dataset info card
      uiOutput(ns("example_info"))
    ),

    # ---- User upload panel ----
    conditionalPanel(
      condition = sprintf("input['%s'] == 'user'", ns("data_source")),

      # --- Species matrix (required) ---
      div(
        class = "upload-block required",
        tags$h5(
          tags$span(class = "badge-required", "Required"),
          " Species / Response matrix (Y)"
        ),
        tags$p(
          class = "upload-hint",
          "Rows = sites, columns = species. First column must be site names."
        ),
        fileInput(
          inputId  = ns("file_Y"),
          label    = NULL,
          accept   = c(".csv", ".xlsx", ".xls"),
          placeholder = "No file selected"
        ),
        uiOutput(ns("status_Y"))
      ),

      # --- Environmental variables (optional) ---
      div(
        class = "upload-block optional",
        tags$h5(
          tags$span(class = "badge-optional", "Optional"),
          " Environmental predictors (X)"
        ),
        tags$p(
          class = "upload-hint",
          "Same row order and site names as Y. Factors will be detected automatically."
        ),
        fileInput(
          inputId  = ns("file_X"),
          label    = NULL,
          accept   = c(".csv", ".xlsx", ".xls"),
          placeholder = "No file selected"
        ),
        uiOutput(ns("status_X"))
      ),

      # --- Traits (optional) ---
      div(
        class = "upload-block optional",
        tags$h5(
          tags$span(class = "badge-optional", "Optional"),
          " Species traits"
        ),
        tags$p(
          class = "upload-hint",
          "Rows = species (must match column names of Y). Columns = trait variables."
        ),
        fileInput(
          inputId  = ns("file_traits"),
          label    = NULL,
          accept   = c(".csv", ".xlsx", ".xls"),
          placeholder = "No file selected"
        ),
        uiOutput(ns("status_traits"))
      ),

      # --- Factor column selector (shown after X is loaded) ---
      uiOutput(ns("factor_selector")),

      # --- Validation summary ---
      uiOutput(ns("validation_summary"))
    ),

    hr(),

    # ---- Preview (both modes) ----
    uiOutput(ns("data_preview"))
  )
}


# --------------------------------------------------------------------------- #
#  Server                                                                      #
# --------------------------------------------------------------------------- #

mod_upload_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---- Path to example data ----
    extdata_path <- system.file("data", package = "StatCoom")
    # Fallback for development without installed package
    if (extdata_path == "") extdata_path <- "inst/data"

    # ================================================================
    # EXAMPLE DATASETS
    # ================================================================

    example_data <- reactive({
      req(input$data_source == "example", input$example_choice)
      path <- file.path(extdata_path, paste0(input$example_choice, ".rds"))
      if (!file.exists(path)) {
        showNotification(
          paste("Example dataset not found:", basename(path)),
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
        class = "dataset-info-card",
        tags$h6(m$name),
        tags$p(m$description),
        tags$small(
          class = "text-muted",
          tags$b("Sites: "),     m$n_sites,   " · ",
          tags$b("Species: "),   m$n_species, " · ",
          tags$b("Predictors: "), m$n_predictors, " · ",
          tags$b("Response: "),  m$response,  " · ",
          tags$b("Source: "),    m$source
        ),
        if (!is.null(m$reference)) {
          tags$small(class = "text-muted d-block mt-1",
                     tags$i(m$reference))
        }
      )
    })


    # ================================================================
    # USER UPLOAD — helpers
    # ================================================================

    read_uploaded_file <- function(file_info) {
      req(file_info)
      ext <- tolower(tools::file_ext(file_info$name))
      tryCatch({
        df <- switch(ext,
          csv  = read.csv(file_info$datapath,
                          row.names  = 1,
                          check.names = FALSE,
                          stringsAsFactors = FALSE),
          xlsx = ,
          xls  = readxl::read_excel(file_info$datapath) |>
                   as.data.frame() |>
                   (\(d) { rownames(d) <- d[[1]]; d[, -1, drop = FALSE] })(),
          stop("Unsupported format: .", ext)
        )
        df
      }, error = function(e) {
        showNotification(paste("Error reading", file_info$name, ":", e$message),
                         type = "error", duration = 8)
        NULL
      })
    }

    # Auto-detect columns that should be factors in X
    auto_detect_factors <- function(df) {
      sapply(df, function(col) {
        is.character(col) ||
        is.factor(col)    ||
        (is.numeric(col) && length(unique(col)) <= 6 &&
           all(col == as.integer(col), na.rm = TRUE))
      })
    }

    # ---- Reactive: raw uploaded tables ----
    raw_Y      <- reactive(read_uploaded_file(input$file_Y))
    raw_X      <- reactive(read_uploaded_file(input$file_X))
    raw_traits <- reactive(read_uploaded_file(input$file_traits))


    # ================================================================
    # USER UPLOAD — factor column selector
    # ================================================================

    output$factor_selector <- renderUI({
      X <- raw_X()
      req(X)
      auto <- names(which(auto_detect_factors(X)))
      non_auto <- names(X)[!names(X) %in% auto]

      if (length(non_auto) == 0) return(NULL)

      tagList(
        tags$h6("Convert additional columns to factors?"),
        tags$p(class = "upload-hint",
               "Auto-detected factors: ",
               if (length(auto) > 0) paste(auto, collapse = ", ") else "none"),
        checkboxGroupInput(
          inputId  = ns("extra_factors"),
          label    = "Mark as factor:",
          choices  = non_auto,
          selected = NULL,
          inline   = TRUE
        )
      )
    })


    # ================================================================
    # USER UPLOAD — processed X (with factor coercion)
    # ================================================================

    processed_X <- reactive({
      X <- raw_X()
      req(X)
      auto    <- names(which(auto_detect_factors(X)))
      manual  <- input$extra_factors %||% character(0)
      to_fac  <- unique(c(auto, manual))
      for (col in to_fac) {
        if (col %in% names(X)) X[[col]] <- factor(X[[col]])
      }
      X
    })


    # ================================================================
    # USER UPLOAD — validation
    # ================================================================

    validation <- reactive({
      Y <- raw_Y()
      if (is.null(Y)) return(list(ok = FALSE, errors = NULL, warnings = NULL))

      errors   <- character(0)
      warnings <- character(0)

      # 1. Y must be all numeric
      non_num <- names(Y)[!sapply(Y, is.numeric)]
      if (length(non_num) > 0) {
        errors <- c(errors,
          paste("Y contains non-numeric columns:", paste(non_num, collapse = ", ")))
      }

      # 2. Y vs X: row names must match
      X <- processed_X()
      if (!is.null(X)) {
        if (!identical(rownames(Y), rownames(X))) {
          if (setequal(rownames(Y), rownames(X))) {
            errors <- c(errors,
              "Site names in Y and X match but are in different order. Please sort before uploading.")
          } else {
            missing_in_X  <- setdiff(rownames(Y), rownames(X))
            missing_in_Y  <- setdiff(rownames(X), rownames(Y))
            if (length(missing_in_X) > 0)
              errors <- c(errors,
                paste("Sites in Y not found in X:", paste(head(missing_in_X, 5), collapse = ", ")))
            if (length(missing_in_Y) > 0)
              errors <- c(errors,
                paste("Sites in X not found in Y:", paste(head(missing_in_Y, 5), collapse = ", ")))
          }
        }
      }

      # 3. traits: row names must match species (col names of Y)
      traits <- raw_traits()
      if (!is.null(traits)) {
        if (!identical(rownames(traits), colnames(Y))) {
          if (setequal(rownames(traits), colnames(Y))) {
            errors <- c(errors,
              "Species names in traits match Y columns but are in different order.")
          } else {
            missing_traits <- setdiff(colnames(Y), rownames(traits))
            extra_traits   <- setdiff(rownames(traits), colnames(Y))
            if (length(missing_traits) > 0)
              errors <- c(errors,
                paste("Species in Y without traits:", paste(head(missing_traits, 5), collapse = ", ")))
            if (length(extra_traits) > 0)
              warnings <- c(warnings,
                paste("Traits for species not in Y (will be ignored):", paste(head(extra_traits, 5), collapse = ", ")))
          }
        }
      }

      # 4. Warn about zero-sum rows in Y
      zero_sites <- rownames(Y)[rowSums(Y, na.rm = TRUE) == 0]
      if (length(zero_sites) > 0) {
        warnings <- c(warnings,
          paste("Sites with all-zero abundances:", paste(head(zero_sites, 5), collapse = ", ")))
      }

      # 5. Warn about zero-sum columns in Y
      zero_spp <- colnames(Y)[colSums(Y, na.rm = TRUE) == 0]
      if (length(zero_spp) > 0) {
        warnings <- c(warnings,
          paste("Species with all-zero records:", paste(head(zero_spp, 5), collapse = ", ")))
      }

      list(
        ok       = length(errors) == 0,
        errors   = errors,
        warnings = warnings
      )
    })


    # ================================================================
    # USER UPLOAD — status outputs
    # ================================================================

    status_widget <- function(df, label) {
      if (is.null(df)) return(NULL)
      div(class = "upload-status ok",
          tags$i(class = "fa fa-check-circle"),
          sprintf(" %s loaded: %d rows × %d columns", label, nrow(df), ncol(df)))
    }

    output$status_Y      <- renderUI(status_widget(raw_Y(),      "Y"))
    output$status_X      <- renderUI(status_widget(processed_X(), "X"))
    output$status_traits <- renderUI(status_widget(raw_traits(),  "Traits"))

    output$validation_summary <- renderUI({
      req(input$data_source == "user", raw_Y())
      v <- validation()
      tagList(
        if (length(v$errors) > 0) {
          div(class = "alert alert-danger",
              tags$b("Errors:"),
              tags$ul(lapply(v$errors, tags$li)))
        },
        if (length(v$warnings) > 0) {
          div(class = "alert alert-warning",
              tags$b("Warnings:"),
              tags$ul(lapply(v$warnings, tags$li)))
        },
        if (v$ok && is.null(raw_Y()) == FALSE) {
          div(class = "alert alert-success",
              tags$i(class = "fa fa-check"),
              " Data validated successfully. Ready to use.")
        }
      )
    })


    # ================================================================
    # DATA PREVIEW (both modes)
    # ================================================================

    active_data <- reactive({
      if (input$data_source == "example") {
        example_data()
      } else {
        v <- validation()
        if (!v$ok || is.null(raw_Y())) return(NULL)
        list(
          Y      = raw_Y(),
          X      = processed_X(),
          traits = raw_traits(),
          meta   = list(
            name     = "User data",
            response = "unknown",
            n_sites  = nrow(raw_Y()),
            n_species = ncol(raw_Y()),
            n_predictors = if (is.null(processed_X())) 0 else ncol(processed_X())
          ),
          source = "user"
        )
      }
    })

    output$data_preview <- renderUI({
      d <- active_data()
      req(d)

      # Show first 6 rows / 8 cols of Y
      Y_show <- d$Y[seq_len(min(6, nrow(d$Y))),
                    seq_len(min(8, ncol(d$Y))),
                    drop = FALSE]

      tagList(
        tags$h6("Preview: species matrix (Y)"),
        div(class = "table-responsive",
            renderTable(Y_show, rownames = TRUE, digits = 0)
        ),
        if (!is.null(d$X)) {
          X_show <- d$X[seq_len(min(6, nrow(d$X))), , drop = FALSE]
          tagList(
            tags$h6("Preview: predictors (X)"),
            div(class = "table-responsive",
                renderTable(X_show, rownames = TRUE))
          )
        }
      )
    })


    # ================================================================
    # RETURN — reactive data accessible by other modules
    # ================================================================

    return(reactive(active_data()))
  })
}


# ---- Utility ----
`%||%` <- function(x, y) if (is.null(x)) y else x
