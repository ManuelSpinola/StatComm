# ============================================================
# utils_cita.R — Metadatos y tarjeta "Cómo citar" de la app
# StatSuite · Manuel Spínola · ICOMVIS · UNA
#
# Fuente única: DESCRIPTION del paquete (Title, Version, Date,
# Authors@R, URL). Ningún dato de citación se escribe a mano.
# Archivo idéntico en todas las apps de StatSuite.
# ============================================================

institucion_statsuite <- paste0(
  "Instituto Internacional en Conservación y Manejo de Vida Silvestre ",
  "(ICOMVIS), Universidad Nacional, Costa Rica"
)

# Lee los metadatos de la app desde DESCRIPTION.
# Date es obligatorio (define el año de la cita); URL es opcional.
metadatos_app <- function(pkg) {
  d <- utils::packageDescription(pkg)
  campo <- function(x) {
    if (is.null(x) || is.na(x)) NA_character_ else gsub("\\s+", " ", trimws(x))
  }

  fecha <- campo(d[["Date"]])
  if (is.na(fecha)) {
    stop("El DESCRIPTION de ", pkg, " no tiene campo Date. ",
         "Defínalo con desc::desc_set(\"Date\", \"AAAA-MM-DD\").",
         call. = FALSE)
  }

  personas <- eval(parse(text = d[["Authors@R"]]))
  es_autor <- vapply(seq_along(personas),
                     function(i) "aut" %in% personas[[i]]$role,
                     logical(1))

  url <- campo(d[["URL"]])
  if (!is.na(url)) url <- trimws(strsplit(url, ",")[[1]][1])

  list(
    nombre  = pkg,
    titulo  = campo(d[["Title"]]),
    version = campo(d[["Version"]]),
    anio    = substr(fecha, 1, 4),
    url     = url,
    autores = personas[es_autor]
  )
}

# Tarjeta "Cómo citar" (APA 7 para software + BibTeX).
# Si DESCRIPTION está incompleto, muestra un aviso en lugar de la
# cita: el resto de la app sigue funcionando.
tarjeta_cita <- function(pkg, ns) {
  m <- tryCatch(metadatos_app(pkg), error = function(e) e)
  if (inherits(m, "error")) {
    return(card(
      class = "mt-3",
      card_header(bs_icon("quote", class = "me-1"),
                  paste("Cómo citar", pkg)),
      card_body(div(class = "alert alert-warning small mb-0",
                    bs_icon("exclamation-triangle", class = "me-1"),
                    "Cita no disponible: ", conditionMessage(m)))
    ))
  }
  n <- length(m$autores)

  autores_apa <- vapply(seq_len(n), function(i) {
    p <- m$autores[[i]]
    paste0(p$family, ", ", paste0(substr(p$given, 1, 1), ".", collapse = " "))
  }, character(1))
  autores_apa <- if (n == 1) {
    autores_apa
  } else {
    paste0(paste(autores_apa[-n], collapse = ", "), ", & ", autores_apa[n])
  }

  autores_bib <- paste(vapply(seq_len(n), function(i) {
    p <- m$autores[[i]]
    paste0(p$family, ", ", paste(p$given, collapse = " "))
  }, character(1)), collapse = " and ")

  titulo_completo <- paste0(m$nombre, ": ", m$titulo)

  bibtex <- paste0(
    "@software{", tolower(m$nombre), m$anio, ",\n",
    "  author    = {", autores_bib, "},\n",
    "  title     = {", titulo_completo, "},\n",
    "  year      = {", m$anio, "},\n",
    "  version   = {", m$version, "},\n",
    "  publisher = {", institucion_statsuite, "}",
    if (!is.na(m$url)) paste0(",\n  url       = {", m$url, "}"),
    "\n}"
  )

  card(
    class = "mt-3",
    card_header(bs_icon("quote", class = "me-1"),
                paste("Cómo citar", m$nombre)),
    card_body(
      p(
        id = ns("cita_texto"),
        class = "small mb-2",
        autores_apa, " (", m$anio, "). ",
        tags$em(titulo_completo),
        " (Versión ", m$version, ") [Aplicación web]. ",
        institucion_statsuite, ".",
        if (!is.na(m$url)) paste0(" ", m$url)
      ),
      div(
        class = "d-flex flex-wrap gap-2",
        tags$button(
          type    = "button",
          class   = "btn btn-sm btn-outline-primary",
          onclick = paste0(
            "navigator.clipboard.writeText(",
            "document.getElementById('", ns("cita_texto"), "').innerText);",
            "this.innerText = '✓ Cita copiada';"
          ),
          bs_icon("clipboard", class = "me-1"), "Copiar cita"
        )
      ),
      tags$details(
        class = "mt-3 small",
        tags$summary("BibTeX"),
        tags$pre(class = "codigo-bloque mt-2", bibtex)
      )
    )
  )
}
