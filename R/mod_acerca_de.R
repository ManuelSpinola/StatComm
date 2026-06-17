# ============================================================
# mod_acerca_de.R — Información sobre StatComm
# StatComm · StatSuite · Manuel Spínola · ICOMVIS · UNA
# ============================================================

mod_acerca_de_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "py-4 px-3",
      style = "max-width: 780px; margin: 0 auto;",

      h4(
        bs_icon("info-circle", class = "me-2"),
        "Acerca de StatComm",
        style = paste0("color:", colores$primario, "; font-weight:700;")
      ),
      p(class = "text-muted mb-4",
        "StatComm es el m\u00f3dulo de an\u00e1lisis multivariado en ecolog\u00eda de ",
        "StatSuite, desarrollado en el ICOMVIS de la Universidad Nacional, ",
        "Costa Rica. Integra modelos lineales generalizados latentes (GLLVM) ",
        "y aprendizaje autom\u00e1tico multirrespuesta (mrIML) para el an\u00e1lisis ",
        "de comunidades biol\u00f3gicas, microbiomas y otros datos composicionales."
      ),

      layout_columns(
        col_widths = c(6, 6),

        card(
          card_header(bs_icon("collection", class = "me-1"),
                      "StatSuite \u2014 Ecosistema completo"),
          card_body(
            tags$ul(
              class = "small",
              tags$li(strong("StatDesign"),  " \u2014 Dise\u00f1o de estudios y muestreo"),
              tags$li(strong("StatFlow"),    " \u2014 Primeros an\u00e1lisis y visualizaci\u00f3n"),
              tags$li(strong("StatGeo"),     " \u2014 An\u00e1lisis espacial y mapas"),
              tags$li(strong("StatMonitor"), " \u2014 Monitoreo poblacional"),
              tags$li(strong("StatModels"),  " \u2014 Modelos estad\u00edsticos"),
              tags$li(strong("StatH3sdm"),   " \u2014 SDM con grillas H3"),
              tags$li(strong("StatComm"),    " \u2014 An\u00e1lisis multivariado \u2190 aqu\u00ed")
            )
          )
        ),

        card(
          card_header(bs_icon("box-seam", class = "me-1"),
                      "Ecosistema R utilizado"),
          card_body(
            tags$ul(
              class = "small",
              tags$li(strong("gllvm"),
                      " \u2014 Modelos lineales generalizados latentes"),
              tags$li(strong("mrIML"),
                      " \u2014 Machine learning multirrespuesta"),
              tags$li(strong("mvabund"),
                      " \u2014 M\u00e9todos multivariados basados en modelos"),
              tags$li(strong("betapart"),
                      " \u2014 Diversidad beta y partici\u00f3n"),
              tags$li(strong("vegan"),
                      " \u2014 Distancias y datos de comunidades"),
              tags$li(strong("ggplot2"),
                      " \u2014 Visualizaci\u00f3n"),
              tags$li(strong("tidymodels"),
                      " \u2014 Flujo de modelado ML")
            )
          )
        )
      ),

      card(
        class = "mt-3",
        card_header(bs_icon("database", class = "me-1"),
                    "Datos de ejemplo incluidos"),
        card_body(
          tags$ul(
            class = "small",
            tags$li(strong("Spider"), " \u2014 Arañas cazadoras, 28 sitios \u00d7 12 especies (mvabund)"),
            tags$li(strong("Fungi"),  " \u2014 Presencia/ausencia de hongos (gllvm)"),
            tags$li(strong("Microbiome"), " \u2014 Conteos microbianos de alta dimensi\u00f3n (gllvm)"),
            tags$li(strong("Mites"),  " \u2014 \u00c1caros oribátidos, predictores mixtos, 70 sitios (vegan)"),
            tags$li(strong("Doubs"),  " \u2014 Peces del r\u00edo Doubs, gradiente ambiental (ade4)")
          )
        )
      ),

      card(
        class = "mt-3",
        card_header(bs_icon("code-slash", class = "me-1"),
                    "Desarrollo"),
        card_body(
          p(class = "small mb-2",
            bs_icon("person-fill", class = "me-1"),
            strong("Autor:"), " Manuel Sp\u00ednola \u2014 ICOMVIS, ",
            "Universidad Nacional, Costa Rica."),
          p(class = "small mb-2",
            bs_icon("robot", class = "me-1"),
            strong("Asistencia en desarrollo:"), " StatComm fue desarrollado ",
            "con asistencia de ", strong("Claude (Anthropic)"),
            " para la estructura de m\u00f3dulos, interfaz de usuario, ",
            "l\u00f3gica del servidor y contenido did\u00e1ctico."),
          p(class = "small mb-0",
            bs_icon("building", class = "me-1"),
            strong("Instituci\u00f3n:"), " Instituto Internacional en ",
            "Conservaci\u00f3n y Manejo de Vida Silvestre (ICOMVIS), ",
            "Universidad Nacional de Costa Rica.")
        )
      ),

      div(
        class = "alert alert-info small mt-3 mb-0",
        bs_icon("envelope", class = "me-1"),
        "Contacto: ",
        tags$a(href = "mailto:manuel.spinola@una.ac.cr",
               "manuel.spinola@una.ac.cr")
      )
    )
  )
}

mod_acerca_de_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # sin lógica reactiva
  })
}
