# =============================================================================
# mod_gllvm.R — Modelos Lineales Generalizados con Variables Latentes (GLLVM)
# StatComm · StatSuite · Manuel Spínola · ICOMVIS · UNA
#
# Paquete principal: gllvm (Niku et al.)
# Datos: compartidos desde mod_upload (app_data reactivo)
# Flujo: familia → factores latentes → ajuste → ordenación →
#        especies → correlaciones → diagnóstico → código R
# =============================================================================

# ── UI ────────────────────────────────────────────────────────────────────────
mod_gllvm_ui <- function(id) {
  ns <- NS(id)

  tagList(

    div(
      class = "px-1 pt-2 pb-2",
      layout_columns(
        col_widths = c(9, 3),
        div(
          h4(style = paste0("color:", colores$primario, "; font-weight:700; margin-bottom:4px;"),
             bs_icon("diagram-3", class = "me-2"),
             "Modelos Lineales Generalizados con Variables Latentes (GLLVM)"),
          p(class = "text-muted small mb-0",
            "Modela comunidades multivariadas bajo un marco probabilístico. ",
            "Estima ", strong("gradientes latentes"), " que capturan la covariación entre especies, ",
            "y permite incorporar ", strong("predictores ambientales"), " y ",
            strong("traits de especies"), " (modelos fourth-corner). ",
            "Paquete: ", strong("gllvm"), " · Niku et al. (2019, 2021).")
        ),
        div(
          class = "text-end pt-1",
          tags$span(
            class = "badge",
            style = paste0("background:", colores$primario, "; font-size:0.8rem; padding:6px 12px;"),
            bs_icon("diagram-3", class = "me-1"), "gllvm"
          )
        )
      )
    ),

    navset_card_tab(
      id = ns("pestanas"),

      # ══════════════════════════════════════════════════════════════════════
      # PESTAÑA 1: ¿Qué es?
      # ══════════════════════════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("book", class = "me-1"), "¿Qué es?"),
        card_body(

          h4(style = paste0("color:", colores$primario, "; font-weight:700;"),
             "GLLVM — Modelos Lineales Generalizados con Variables Latentes"),
          p(class = "text-muted small mb-3",
            "gllvm · Niku et al. (2019) · Journal of Ecology"),

          layout_columns(
            col_widths = c(4, 4, 4),
            fill = FALSE,

            card(
              card_header(bs_icon("question-circle", class = "me-1"),
                          "¿Cuándo usar GLLVM?"),
              card_body(
                tags$ul(class = "small mb-0",
                  tags$li("Datos de ", strong("comunidades biológicas"),
                          ": matrices de especies × sitios"),
                  tags$li("Respuesta puede ser ", strong("conteos, presencia/ausencia, biomasa")),
                  tags$li("Querés estimar ", strong("gradientes ecológicos latentes"),
                          " sin definirlos a priori"),
                  tags$li("Querés modelar el efecto de ",
                          strong("variables ambientales"), " sobre múltiples especies"),
                  tags$li("Datos de ", strong("microbiomas, hongos, invertebrados"),
                          " con alta dimensionalidad")
                )
              )
            ),

            card(
              card_header(bs_icon("arrow-left-right", class = "me-1"),
                          "GLLVM vs métodos clásicos"),
              card_body(
                tags$table(class = "table table-sm small mb-0",
                  tags$thead(tags$tr(
                    tags$th("Método"),
                    tags$th("Marco"),
                    tags$th("Distribución")
                  )),
                  tags$tbody(
                    tags$tr(tags$td(strong("NMDS")),
                            tags$td("Distancias"),
                            tags$td("Ninguna")),
                    tags$tr(tags$td(strong("RDA/CCA")),
                            tags$td("Distancias"),
                            tags$td("Normal/Poisson")),
                    tags$tr(
                      style = paste0("background:", colores$fondo),
                      tags$td(strong("GLLVM")),
                      tags$td("Probabilístico"),
                      tags$td("Flexible ✓"))
                  )
                ),
                div(class = "alert alert-info small py-2 px-3 mt-2 mb-0",
                    bs_icon("info-circle", class = "me-1"),
                    "GLLVM trata la incertidumbre estadística explícitamente ",
                    "y permite inferencia formal sobre parámetros.")
              )
            ),

            card(
              card_header(bs_icon("layers", class = "me-1"),
                          "Tipos de modelos GLLVM"),
              card_body(
                tags$ul(class = "small mb-0",
                  tags$li(strong("Sin covariables"),
                          " — ordenación latente pura (como NMDS probabilístico)"),
                  tags$li(strong("Con covariables (X)"),
                          " — efecto de predictores ambientales sobre especies"),
                  tags$li(strong("Con traits (fourth-corner)"),
                          " — los traits median la respuesta al ambiente"),
                  tags$li(strong("Mixto"),
                          " — covariables + factores latentes residuales")
                )
              )
            )
          ),

          tags$hr(),

          layout_columns(
            col_widths = c(6, 6),
            fill = FALSE,

            card(
              card_header(bs_icon("code-slash", class = "me-1"),
                          "El modelo en términos matemáticos"),
              card_body(
                p(class = "small text-muted mb-2",
                  "Para la especie ", em("j"), " en el sitio ", em("i"), ":"),
                div(class = "codigo-bloque mb-2",
                    "g(μᵢⱼ) = α_i + β₀ⱼ + xᵢᵀβⱼ + uᵢᵀγⱼ"),
                tags$table(class = "table table-sm small mb-0",
                  tags$tbody(
                    tags$tr(tags$td(code("g(·)")),
                            tags$td("Función de enlace (log, logit, etc.)")),
                    tags$tr(tags$td(code("α_i")),
                            tags$td("Efecto de sitio (row effect)")),
                    tags$tr(tags$td(code("β₀ⱼ")),
                            tags$td("Intercepto de la especie j")),
                    tags$tr(tags$td(code("xᵢᵀβⱼ")),
                            tags$td("Efecto de covariables sobre especie j")),
                    tags$tr(tags$td(code("uᵢᵀγⱼ")),
                            tags$td("Variables latentes × loadings"))
                  )
                )
              )
            ),

            card(
              card_header(bs_icon("lightbulb", class = "me-1"),
                          "Variables latentes — intuición"),
              card_body(
                p(class = "small text-muted mb-2",
                  "Las variables latentes ", strong("uᵢ"), " son gradientes no observados ",
                  "que capturan la covariación entre especies después de controlar por X."),
                tags$ul(class = "small mb-2",
                  tags$li("Son análogas a los ", strong("ejes de ordenación"),
                          " en NMDS o PCA"),
                  tags$li("Su número (", em("d"), ") es un parámetro a seleccionar"),
                  tags$li("Capturan ", strong("covariación residual"),
                          " entre especies — correlaciones que no explica X"),
                  tags$li("Permiten hacer ", strong("biplots"), " interpretables")
                ),
                div(class = "alert alert-success small py-2 px-3 mb-0",
                    bs_icon("check-circle", class = "me-1"),
                    "Con d = 0 el modelo asume independencia entre especies dado X. ",
                    "Con d > 0 se modela la dependencia residual.")
              )
            )
          )
        )
      ), # /PESTAÑA 1


      # ══════════════════════════════════════════════════════════════════════
      # PESTAÑA 2: Fundamentos
      # ══════════════════════════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("mortarboard", class = "me-1"), "Fundamentos"),
        card_body(

          h4(style = paste0("color:", colores$primario, "; font-weight:700;"),
             "Fundamentos estadísticos de GLLVM"),
          p(class = "text-muted small mb-3",
            "Estimación, inferencia y selección de modelos"),

          layout_columns(
            col_widths = c(4, 4, 4),
            fill = FALSE,

            card(
              card_header(bs_icon("calculator", class = "me-1"),
                          "Estimación: método VA"),
              card_body(
                p(class = "small text-muted mb-2",
                  "GLLVM estima los parámetros maximizando la ",
                  strong("verosimilitud marginal"), ", integrando sobre las ",
                  "variables latentes no observadas. Como esta integral no tiene ",
                  "solución analítica, se usa una aproximación:"),
                div(class = "alert alert-secondary small py-2 px-3 mb-2",
                    style = "font-family: monospace;",
                    "L(θ) = ∫ p(y | u, θ) p(u) du"),
                tags$ul(class = "small mb-0",
                  tags$li(strong("VA (Variational Approximation)"),
                          " — aproxima la distribución posterior de u con una gaussiana. ",
                          "Rápido y preciso para la mayoría de los datos ecológicos."),
                  tags$li(strong("Laplace"),
                          " — aproximación de segundo orden. Más lento pero más preciso ",
                          "para datos con pocos ceros o familias complejas.")
                )
              )
            ),

            card(
              card_header(bs_icon("graph-up", class = "me-1"),
                          "Selección de modelos"),
              card_body(
                p(class = "small text-muted mb-2",
                  "Se usan criterios de información basados en la verosimilitud:"),
                tags$table(class = "table table-sm small mb-2",
                  tags$tbody(
                    tags$tr(
                      tags$td(strong("AIC")),
                      tags$td("-2 log L + 2k"),
                      tags$td("Menor = mejor ajuste penalizado por complejidad")
                    ),
                    tags$tr(
                      tags$td(strong("BIC")),
                      tags$td("-2 log L + k log(n)"),
                      tags$td("Penaliza más la complejidad que AIC")
                    ),
                    tags$tr(
                      tags$td(strong("AICc")),
                      tags$td("AIC corregido"),
                      tags$td("Preferido con muestras pequeñas")
                    )
                  )
                ),
                div(class = "alert alert-info small py-2 px-3 mb-0",
                    bs_icon("info-circle", class = "me-1"),
                    "Para seleccionar ", strong("d"), " (factores latentes), ",
                    "ajustar modelos con d = 0, 1, 2, 3 y elegir el menor AIC/BIC.")
              )
            ),

            card(
              card_header(bs_icon("shield-check", class = "me-1"),
                          "Inferencia sobre parámetros"),
              card_body(
                p(class = "small text-muted mb-2",
                  "Los coeficientes de especie (β) se estiman con sus ",
                  strong("errores estándar"), " e ", strong("intervalos de confianza"),
                  " al 95%:"),
                tags$ul(class = "small mb-2",
                  tags$li("IC basados en la ", strong("matriz de información de Fisher")),
                  tags$li("Para familias ZIP/ZINB, los IC son aproximados"),
                  tags$li(strong("p-valores"), " disponibles para coeficientes de covariables")
                ),
                div(class = "alert alert-warning small py-2 px-3 mb-0",
                    bs_icon("exclamation-diamond", class = "me-1"),
                    "Los loadings (γ) no tienen p-valores directos — ",
                    "interpretar el biplot cualitativamente.")
              )
            )
          ),

          tags$hr(),

          layout_columns(
            col_widths = c(6, 6),
            fill = FALSE,

            card(
              card_header(bs_icon("arrows-collapse", class = "me-1"),
                          "Indeterminación de signo y rotación"),
              card_body(
                p(class = "small text-muted mb-2",
                  "Las variables latentes tienen dos propiedades que pueden confundir:"),
                tags$ul(class = "small mb-0",
                  tags$li(strong("Indeterminación de signo"),
                          " — el eje latente puede estar 'al revés' entre corridas. ",
                          "Esto es normal: lo que importa es la ", em("relación relativa"),
                          " entre sitios y especies, no el signo absoluto."),
                  tags$li(strong("No hay rotación única"),
                          " — a diferencia del PCA, los ejes latentes no tienen ",
                          "una orientación privilegiada. La interpretación es relativa."),
                  tags$li("Solución: correr el modelo varias veces y comparar biplots. ",
                          "Si la estructura es robusta, las conclusiones son estables.")
                )
              )
            ),

            card(
              card_header(bs_icon("book", class = "me-1"),
                          "Referencias clave"),
              card_body(
                tags$ul(class = "small mb-0",
                  tags$li(
                    tags$b("Niku et al. (2019)"),
                    " — gllvm: Fast analysis of multivariate abundance data with ",
                    "generalized linear latent variable models in R. ",
                    em("Methods in Ecology and Evolution.")
                  ),
                  tags$li(
                    tags$b("Warton et al. (2015)"),
                    " — So many variables: joint modeling in community ecology. ",
                    em("Trends in Ecology & Evolution.")
                  ),
                  tags$li(
                    tags$b("Ovaskainen et al. (2017)"),
                    " — How to make more out of community data? ",
                    em("Ecology Letters.")
                  ),
                  tags$li(
                    tags$b("Niku et al. (2021)"),
                    " — Analyzing environmental associations of species with ",
                    "GLLVM. ", em("Journal of Animal Ecology.")
                  )
                )
              )
            )
          )
        )
      ), # /PESTAÑA 2

      # ══════════════════════════════════════════════════════════════════════
      # PESTAÑA 3: Familia
      # ══════════════════════════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("bar-chart-fill", class = "me-1"), "Familia"),
        card_body(

          layout_columns(
            col_widths = c(4, 8),
            fill = FALSE,

            # ── Panel de selección ──────────────────────────────────
            div(
              card(
                card_header(bs_icon("sliders", class = "me-1"),
                            "Seleccionar familia"),
                card_body(
                  selectInput(
                    ns("familia"),
                    label = "Distribución de la respuesta",
                    choices = list(
                      "Conteos" = list(
                        "Poisson"                    = "poisson",
                        "Binomial negativa"          = "negative.binomial",
                        "ZIP (Poisson con inflación de ceros)" = "ZIP",
                        "ZINB (BN con inflación de ceros)"     = "ZINB"
                      ),
                      "Presencia/ausencia" = list(
                        "Binomial (logit)"  = "binomial",
                        "Binomial (probit)" = "probit"
                      ),
                      "Continua" = list(
                        "Normal (gaussiana)" = "gaussian",
                        "Tweedie"            = "tweedie",
                        "Gamma"              = "gamma",
                        "Beta"               = "beta"
                      ),
                      "Ordinal" = list(
                        "Ordinal (cumulative)" = "ordinal"
                      )
                    ),
                    selected = "negative.binomial"
                  ),
                  uiOutput(ns("familia_sugerida")),
                  tags$hr(),
                  actionButton(
                    ns("btn_diagnostico_ceros"),
                    label = tagList(bs_icon("search", class = "me-1"),
                                    "Diagnóstico de ceros"),
                    class = "btn btn-outline-primary btn-sm w-100"
                  )
                )
              )
            ),

            # ── Panel informativo ────────────────────────────────────
            div(
              uiOutput(ns("info_familia")),
              tags$hr(),
              uiOutput(ns("diagnostico_ceros_output"))
            )
          )
        )
      ), # /PESTAÑA 2

      # ══════════════════════════════════════════════════════════════════════
      # PESTAÑA 4: Factores latentes
      # ══════════════════════════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("grid-3x3", class = "me-1"), "Factores latentes"),
        card_body(

          layout_columns(
            col_widths = c(4, 8),
            fill = FALSE,

            div(
              card(
                card_header(bs_icon("sliders", class = "me-1"),
                            "Selección de d"),
                card_body(
                  p(class = "small text-muted mb-3",
                    "El número de factores latentes (", em("d"), ") controla ",
                    "cuántos gradientes no observados se estiman. ",
                    "Más factores = más flexible pero más lento."),
                  numericInput(
                    ns("n_latentes_max"),
                    label = "Máximo d a evaluar",
                    value = 3, min = 1, max = 5, step = 1
                  ),
                  radioButtons(
                    ns("criterio_seleccion"),
                    label = "Criterio de selección",
                    choices = c("AIC" = "AIC", "BIC" = "BIC", "AICc" = "AICc"),
                    selected = "AIC",
                    inline = TRUE
                  ),
                  actionButton(
                    ns("btn_seleccion_d"),
                    label = tagList(bs_icon("play-circle", class = "me-1"),
                                    "Evaluar modelos"),
                    class = "btn btn-primary btn-sm w-100 mt-2"
                  ),
                  uiOutput(ns("d_seleccionado_badge"))
                )
              )
            ),

            div(
              uiOutput(ns("tabla_seleccion_d")),
              plotOutput(ns("plot_seleccion_d"), height = "250px")
            )
          ),

          tags$hr(),

          layout_columns(
            col_widths = c(6, 6),
            fill = FALSE,

            card(
              card_header(bs_icon("lightbulb", class = "me-1"),
                          "¿Cuántos factores usar?"),
              card_body(
                tags$ul(class = "small mb-0",
                  tags$li(strong("d = 0"),
                          " — sin estructura latente; especies independientes dado X"),
                  tags$li(strong("d = 1"),
                          " — un gradiente principal (p. ej. humedad)"),
                  tags$li(strong("d = 2"),
                          " — biplot en 2D, más interpretable"),
                  tags$li(strong("d ≥ 3"),
                          " — útil para datos complejos pero difícil de visualizar"),
                  tags$li("Elegir el ", strong("menor d"), " con AIC/BIC mínimo")
                )
              )
            ),

            card(
              card_header(bs_icon("info-circle", class = "me-1"),
                          "Nota computacional"),
              card_body(
                p(class = "small text-muted mb-0",
                  "Cada modelo se ajusta con ", code("gllvm()"),
                  " usando el método de aproximación de Laplace (VA). ",
                  "Para matrices grandes (", em("> 200 especies"),
                  ") puede tardar varios minutos. ",
                  "Se recomienda empezar con d = 1 a 3.")
              )
            )
          )
        )
      ), # /PESTAÑA 3

      # ══════════════════════════════════════════════════════════════════════
      # PESTAÑA 5: Ajustar modelo
      # ══════════════════════════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("gear", class = "me-1"), "Ajustar modelo"),
        card_body(

          layout_columns(
            col_widths = c(4, 8),
            fill = FALSE,

            div(
              card(
                card_header(bs_icon("sliders", class = "me-1"),
                            "Configuración del modelo"),
                card_body(
                  numericInput(
                    ns("n_latentes"),
                    label = "Factores latentes (d)",
                    value = 2, min = 0, max = 5, step = 1
                  ),
                  radioButtons(
                    ns("tipo_modelo"),
                    label = "Tipo de modelo",
                    choices = c(
                      "Sin covariables (ordenación)"    = "unconstrained",
                      "Con covariables (X)"             = "constrained",
                      "Con covariables + traits (4th corner)" = "fourth_corner"
                    ),
                    selected = "unconstrained"
                  ),
                  uiOutput(ns("sel_covariables")),
                  uiOutput(ns("sel_traits")),
                  radioButtons(
                    ns("row_effect"),
                    label = "Efecto de sitio",
                    choices = c(
                      "Ninguno"       = "none",
                      "Fijo"          = "fixed",
                      "Aleatorio"     = "random"
                    ),
                    selected = "none",
                    inline = TRUE
                  ),
                  tags$hr(),
                  actionButton(
                    ns("btn_ajustar"),
                    label = tagList(bs_icon("play-circle", class = "me-1"),
                                    "Ajustar modelo"),
                    class = "btn btn-primary w-100"
                  ),
                  uiOutput(ns("modelo_status"))
                )
              )
            ),

            div(
              uiOutput(ns("resumen_modelo")),
              tags$hr(),
              card(
                card_header(bs_icon("pie-chart", class = "me-1"),
                            "Partición de varianza"),
                card_body(
                  p(class = "small text-muted mb-2",
                    "Proporción de varianza explicada por cada componente del modelo: ",
                    "predictores ambientales (X), factores latentes y efecto de sitio."),
                  plotly::plotlyOutput(ns("plot_var_part"), height = "480px"),
                  uiOutput(ns("nota_var_part"))
                )
              )
            )
          )
        )
      ), # /PESTAÑA 4

      # ══════════════════════════════════════════════════════════════════════
      # PESTAÑA 6: Diagnóstico
      # ══════════════════════════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("clipboard-check", class = "me-1"), "Diagnóstico"),
        card_body(

          layout_columns(
            col_widths = c(6, 6),
            fill = FALSE,

            card(
              card_header(bs_icon("graph-up", class = "me-1"),
                          "Residuos de Dunn-Smyth (randomizados)"),
              card_body(
                plotOutput(ns("plot_residuos_qq"), height = "280px"),
                p(class = "small text-muted mt-2 mb-0",
                  "Los puntos deben seguir la línea diagonal. ",
                  "Desviaciones sistemáticas sugieren que la familia elegida ",
                  "no es adecuada para los datos.")
              )
            ),

            card(
              card_header(bs_icon("graph-up-arrow", class = "me-1"),
                          "Residuos vs valores predichos"),
              card_body(
                plotOutput(ns("plot_residuos_fitted"), height = "280px"),
                p(class = "small text-muted mt-2 mb-0",
                  "No debe haber patrones sistemáticos. ",
                  "Una tendencia en forma de arco puede indicar ",
                  "que faltan factores latentes.")
              )
            )
          ),

          tags$hr(),

          layout_columns(
            col_widths = c(4, 4, 4),
            fill = FALSE,

            card(
              card_header(bs_icon("info-circle", class = "me-1"),
                          "Residuos de Dunn-Smyth"),
              card_body(
                p(class = "small text-muted mb-0",
                  "Son residuos ", strong("aleatorizados cuantílicos"),
                  " que tienen distribución normal bajo el modelo correcto, ",
                  "independientemente de la familia. Son el estándar para ",
                  "diagnóstico en modelos de conteo y presencia/ausencia.")
              )
            ),

            card(
              card_header(bs_icon("exclamation-diamond", class = "me-1"),
                          "Señales de alerta"),
              card_body(
                tags$ul(class = "small mb-0",
                  tags$li(strong("QQ-plot en S"), " → cambiar familia"),
                  tags$li(strong("Patrón en residuos"), " → añadir covariables o factores"),
                  tags$li(strong("Varianza no constante"), " → considerar Tweedie o NB"),
                  tags$li(strong("Exceso de ceros"), " → probar ZIP o ZINB")
                )
              )
            ),

            card(
              card_header(bs_icon("table", class = "me-1"),
                          "Bondad de ajuste"),
              card_body(
                uiOutput(ns("tabla_gof"))
              )
            )
          )
        )
      ), # /PESTAÑA 8

      # ══════════════════════════════════════════════════════════════════════
      # PESTAÑA 7: Performance
      # ══════════════════════════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("trophy", class = "me-1"), "Performance"),
        card_body(

          layout_columns(
            col_widths = c(4, 8),
            fill = FALSE,

            div(
              card(
                card_header(bs_icon("sliders", class = "me-1"), "Opciones"),
                card_body(
                  p(class = "small text-muted mb-3",
                    "Se calculan todas las métricas disponibles. ",
                    "Las métricas que no aplican para la familia del modelo ",
                    "se muestran como ", code("NA"), "."),
                  actionButton(
                    ns("btn_gof"),
                    label = tagList(bs_icon("play-circle", class = "me-1"),
                                    "Calcular métricas"),
                    class = "btn btn-primary btn-sm w-100"
                  )
                )
              ),

              card(
                class = "mt-3",
                card_header(bs_icon("info-circle", class = "me-1"),
                            "¿Qué métrica usar?"),
                card_body(
                  tags$ul(class = "small mb-0",
                    tags$li(strong("R² / cor"), " — datos continuos o conteos"),
                    tags$li(strong("RMSE / MAE"), " — error en las mismas unidades que Y"),
                    tags$li(strong("TjurR2 / AUC"), " — solo presencia/ausencia (binomial)"),
                    tags$li(strong("Nagelkerke R²"), " — bondad de ajuste global del modelo"),
                    tags$li(strong("MARNE"), " — error normalizado, comparable entre especies")
                  )
                )
              )
            ),

            div(
              uiOutput(ns("gof_global")),
              tags$hr(),
              card(
                card_header(bs_icon("table", class = "me-1"),
                            "Métricas por especie"),
                card_body(
                  DT::DTOutput(ns("tabla_gof_especies"))
                )
              ),
              card(
                class = "mt-3",
                card_header(bs_icon("bar-chart-steps", class = "me-1"),
                            "Gráfico de performance por especie"),
                card_body(
                  plotly::plotlyOutput(ns("plot_gof_especies"), height = "380px")
                )
              )
            )
          )
        )
      ), # /PESTAÑA 8

      # ══════════════════════════════════════════════════════════════════════
      # PESTAÑA 8: Ordenación
      # ══════════════════════════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("graph-up", class = "me-1"), "Ordenación"),
        card_body(

          layout_columns(
            col_widths = c(3, 9),
            fill = FALSE,

            div(
              card(
                card_header(bs_icon("sliders", class = "me-1"), "Opciones"),
                card_body(
                  uiOutput(ns("sel_tipo_biplot")),
                  uiOutput(ns("nota_biplot_arrows")),
                  checkboxInput(
                    ns("mostrar_etiquetas"),
                    label = "Mostrar etiquetas",
                    value = TRUE
                  ),
                  uiOutput(ns("sel_color_sitios")),
                  numericInput(
                    ns("ejes_latentes"),
                    label = "Ejes a mostrar",
                    value = 1, min = 1, max = 2, step = 1
                  )
                )
              )
            ),

            div(
              card(
                card_header(bs_icon("graph-up", class = "me-1"),
                            "Biplot de ordenación latente"),
                card_body(
                  plotOutput(ns("plot_biplot"), height = "480px")
                )
              )
            )
          ),

          tags$hr(),

          layout_columns(
            col_widths = c(6, 6),
            fill = FALSE,

            card(
              card_header(bs_icon("lightbulb", class = "me-1"),
                          "Cómo interpretar el biplot"),
              card_body(
                tags$ul(class = "small mb-0",
                  tags$li(strong("Puntos de sitio"),
                          " — sitios similares en composición aparecen cercanos"),
                  tags$li(strong("Flechas/puntos de especie"),
                          " — especies con loadings similares covarían entre sitios"),
                  tags$li(strong("Ángulo entre flechas"),
                          " — ángulo agudo = correlación positiva entre especies"),
                  tags$li(strong("Longitud de flecha"),
                          " — indica la importancia de la especie en el gradiente"),
                  tags$li("Sitios en el extremo de una flecha tienen ",
                          strong("alta abundancia"), " de esa especie")
                )
              )
            ),

            card(
              card_header(bs_icon("info-circle", class = "me-1"),
                          "Diferencia con NMDS"),
              card_body(
                tags$ul(class = "small mb-0",
                  tags$li("GLLVM usa un ", strong("modelo probabilístico"),
                          " — la ordenación tiene incertidumbre cuantificada"),
                  tags$li("Los ejes son ", strong("variables latentes estimadas"),
                          ", no transformaciones de distancias"),
                  tags$li("Permite añadir ", strong("intervalos de confianza"),
                          " a la posición de los sitios"),
                  tags$li("La orientación de los ejes puede variar entre corridas ",
                          "(indeterminación de signo — normal en modelos latentes)")
                )
              )
            )
          )
        )
      ), # /PESTAÑA 5

      # ══════════════════════════════════════════════════════════════════════
      # PESTAÑA 9: Especies
      # ══════════════════════════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("bug", class = "me-1"), "Especies"),
        card_body(

          layout_columns(
            col_widths = c(3, 9),
            fill = FALSE,

            div(
              card(
                card_header(bs_icon("sliders", class = "me-1"), "Opciones"),
                card_body(
                  radioButtons(
                    ns("tipo_coef_spp"),
                    label = "Mostrar",
                    choices = c(
                      "Coeficientes (β)"  = "coef",
                      "Loadings (γ)"      = "loadings",
                      "Interceptos (β₀)"  = "intercepts"
                    ),
                    selected = "coef"
                  ),
                  uiOutput(ns("sel_especie_detalle")),
                  p(class = "small text-muted mb-0",
                    bs_icon("info-circle", class = "me-1"),
                    "Filtra la tabla de coeficientes. El gráfico siempre muestra todas las especies.")
                )
              )
            ),

            div(
              layout_columns(
                col_widths = c(12),
                card(
                  card_header(bs_icon("bar-chart-steps", class = "me-1"),
                              "Coeficientes por especie"),
                  card_body(
                    plotOutput(ns("plot_coef_spp"), height = "450px")
                  )
                )
              )
            )
          ),

          tags$hr(),

          card(
            card_header(bs_icon("table", class = "me-1"),
                        "Tabla de coeficientes"),
            card_body(
              DT::DTOutput(ns("tabla_coef_spp"))
            )
          )
        )
      ), # /PESTAÑA 6

      # ══════════════════════════════════════════════════════════════════════
      # PESTAÑA 10: Correlaciones residuales
      # ══════════════════════════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("grid", class = "me-1"), "Correlaciones"),
        card_body(

          layout_columns(
            col_widths = c(3, 9),
            fill = FALSE,

            div(
              card(
                card_header(bs_icon("sliders", class = "me-1"), "Opciones"),
                card_body(
                  numericInput(
                    ns("n_spp_corr"),
                    label = "N° especies a mostrar",
                    value = 20, min = 5, max = 50, step = 5
                  ),
                  radioButtons(
                    ns("orden_corr"),
                    label = "Ordenar por",
                    choices = c(
                      "Correlación"  = "corr",
                      "Nombre"       = "nombre"
                    ),
                    selected = "corr"
                  ),
                  numericInput(
                    ns("umbral_corr"),
                    label = "Umbral |r| mínimo",
                    value = 0.3, min = 0, max = 1, step = 0.05
                  )
                )
              ),

              card(
                card_header(bs_icon("lightbulb", class = "me-1"),
                            "¿Qué son?"),
                card_body(
                  p(class = "small text-muted mb-0",
                    "Las correlaciones residuales entre especies miden ",
                    "la covariación que ", strong("no explican"), " los predictores X. ",
                    "Pueden reflejar interacciones bióticas, ",
                    "respuesta a gradientes no medidos, o filtraje ambiental.")
                )
              )
            ),

            div(
              card(
                card_header(bs_icon("grid", class = "me-1"),
                            "Matriz de correlaciones residuales entre especies"),
                card_body(
                  plotOutput(ns("plot_correlaciones"), height = "520px")
                )
              )
            )
          )
        )
      ), # /PESTAÑA 7

      # ══════════════════════════════════════════════════════════════════════
      # PESTAÑA 11: Código R
      # ══════════════════════════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("code-slash", class = "me-1"), "Código R"),
        card_body(

          layout_columns(
            col_widths = c(3, 9),
            fill = FALSE,

            div(
              card(
                card_header(bs_icon("download", class = "me-1"), "Descargar"),
                card_body(
                  p(class = "small text-muted mb-3",
                    "Script R reproducible con el análisis completo. ",
                    "Incluye carga de datos, ajuste del modelo, ",
                    "gráficos y extracción de resultados."),
                  downloadButton(
                    ns("descargar_script"),
                    label = "Descargar script .R",
                    class = "btn btn-primary btn-sm w-100"
                  )
                )
              )
            ),

            div(
              card(
                card_header(bs_icon("code", class = "me-1"),
                            "Script reproducible"),
                card_body(
                  verbatimTextOutput(ns("codigo_r"))
                )
              )
            )
          )
        )
      ) # /PESTAÑA 9

    ) # /navset_card_tab
  )
}


# ── Server ────────────────────────────────────────────────────────────────────
mod_gllvm_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Datos desde mod_upload ──────────────────────────────────────────────
    Y      <- reactive({ req(data()); data()$Y })
    X      <- reactive({ data()$X })
    traits <- reactive({ data()$traits })
    meta   <- reactive({ data()$meta })

    # ── Familia sugerida según dataset ─────────────────────────────────────
    output$familia_sugerida <- renderUI({
      m <- meta()
      req(m)
      sugerida <- m$family_suggestion %||% "negative.binomial"
      div(
        class = "alert alert-info small py-2 px-3 mt-2 mb-0",
        bs_icon("lightbulb", class = "me-1"),
        "Familia sugerida para este dataset: ",
        strong(sugerida)
      )
    })

    # ── Info familia ────────────────────────────────────────────────────────
    output$info_familia <- renderUI({
      fam <- input$familia %||% "negative.binomial"

      info <- list(
        poisson = list(
          titulo = "Poisson",
          cuando = "Conteos sin sobredispersión. La varianza es igual a la media.",
          enlace = "log",
          ejemplo = "Número de individuos en parcelas cuando la media y varianza son similares.",
          alerta = "Si la varianza >> media, usar Binomial Negativa."
        ),
        negative.binomial = list(
          titulo = "Binomial Negativa",
          cuando = "Conteos con sobredispersión (varianza > media). Muy común en ecología.",
          enlace = "log",
          ejemplo = "Abundancias de especies en comunidades naturales.",
          alerta = "La opción por defecto más robusta para conteos ecológicos."
        ),
        binomial = list(
          titulo = "Binomial (logit)",
          cuando = "Datos de presencia/ausencia (0/1).",
          enlace = "logit",
          ejemplo = "Detección de especies en cámaras trampa, redes de mist.",
          alerta = "Requiere que Y sea estrictamente 0 o 1."
        ),
        probit = list(
          titulo = "Binomial (probit)",
          cuando = "Presencia/ausencia cuando se prefiere función probit. Similar a logit.",
          enlace = "probit",
          ejemplo = "Alternativa a binomial-logit, común en ecología de comunidades.",
          alerta = "Produce resultados muy similares a logit en la práctica."
        ),
        gaussian = list(
          titulo = "Gaussiana (Normal)",
          cuando = "Datos continuos normalmente distribuidos.",
          enlace = "identity",
          ejemplo = "Índices de diversidad, biomasa log-transformada.",
          alerta = "Verificar normalidad antes de usar."
        ),
        tweedie = list(
          titulo = "Tweedie",
          cuando = "Datos continuos con muchos ceros exactos (p. ej. biomasa).",
          enlace = "log",
          ejemplo = "Biomasa de especies que tienen ceros reales (ausencias).",
          alerta = "Flexible: incluye Poisson y Gamma como casos especiales."
        ),
        ZIP = list(
          titulo = "ZIP (Zero-Inflated Poisson)",
          cuando = "Conteos con exceso de ceros estructurales.",
          enlace = "log",
          ejemplo = "Datos donde los ceros reflejan ausencia verdadera y Poisson para el resto.",
          alerta = "Más lento de ajustar. Verificar que el exceso de ceros sea real."
        ),
        ZINB = list(
          titulo = "ZINB (Zero-Inflated Binomial Negativa)",
          cuando = "Conteos sobredispersos con exceso de ceros.",
          enlace = "log",
          ejemplo = "Comunidades de macroinvertebrados con muchos ceros y sobredispersión.",
          alerta = "La más flexible para conteos ecológicos complejos."
        )
      )

      i <- info[[fam]]
      if (is.null(i)) return(NULL)

      tagList(
        card(
          card_header(bs_icon("bar-chart-fill", class = "me-1"),
                      paste("Familia:", i$titulo)),
          card_body(
            layout_columns(
              col_widths = c(6, 6),
              fill = FALSE,
              div(
                tags$p(class = "small mb-1", tags$b("¿Cuándo usar?")),
                tags$p(class = "small text-muted", i$cuando),
                tags$p(class = "small mb-1", tags$b("Función de enlace:")),
                tags$p(class = "small text-muted", code(i$enlace)),
                tags$p(class = "small mb-1", tags$b("Ejemplo:")),
                tags$p(class = "small text-muted mb-0", i$ejemplo)
              ),
              div(
                div(
                  class = "alert alert-warning small py-2 px-3",
                  bs_icon("exclamation-diamond", class = "me-1"),
                  i$alerta
                )
              )
            )
          )
        )
      )
    })

    # ── Diagnóstico de ceros ────────────────────────────────────────────────
    output$diagnostico_ceros_output <- renderUI({
      req(input$btn_diagnostico_ceros)
      isolate({
        Y_mat <- Y()
        req(Y_mat)
        prop_ceros <- mean(as.matrix(Y_mat) == 0)
        ceros_por_sp <- colMeans(as.matrix(Y_mat) == 0)
        n_sp_muchos_ceros <- sum(ceros_por_sp > 0.8)

        clase <- if (prop_ceros > 0.7) "alert-danger"
                 else if (prop_ceros > 0.4) "alert-warning"
                 else "alert-success"

        tagList(
          div(
            class = paste("alert", clase, "small"),
            tags$b("Proporción de ceros en Y: "),
            sprintf("%.1f%%", prop_ceros * 100), tags$br(),
            tags$b("Especies con >80% ceros: "),
            n_sp_muchos_ceros, " de ", ncol(Y_mat), tags$br(),
            tags$b("Recomendación: "),
            if (prop_ceros > 0.7) "Considerar ZIP, ZINB o Binomial (si son datos P/A)"
            else if (prop_ceros > 0.4) "Binomial negativa o ZINB"
            else "Poisson o Binomial negativa"
          )
        )
      })
    }) |> bindEvent(input$btn_diagnostico_ceros)

    # ── Selectores de covariables y traits ──────────────────────────────────
    output$sel_covariables <- renderUI({
      req(input$tipo_modelo %in% c("constrained", "fourth_corner"))
      X_dat <- X()
      req(X_dat)
      checkboxGroupInput(
        ns("covariables"),
        label = "Predictores (X)",
        choices  = names(X_dat),
        selected = names(X_dat)[1:min(3, ncol(X_dat))],
        inline   = FALSE
      )
    })

    output$sel_traits <- renderUI({
      req(input$tipo_modelo == "fourth_corner")
      tr <- traits()
      if (is.null(tr)) {
        return(div(class = "alert alert-warning small py-2 px-3",
                   bs_icon("exclamation-diamond", class = "me-1"),
                   "Este dataset no tiene traits de especies. ",
                   "Usa el dataset ", strong("Escarabajos"), " para modelos fourth-corner."))
      }
      checkboxGroupInput(
        ns("traits_sel"),
        label = "Traits de especies",
        choices  = names(tr),
        selected = names(tr)[1:min(3, ncol(tr))],
        inline   = FALSE
      )
    })

    # ══════════════════════════════════════════════════════════════════════════
    # SELECCIÓN DE D
    # ══════════════════════════════════════════════════════════════════════════
    resultados_seleccion_d <- reactiveVal(NULL)

    observeEvent(input$btn_seleccion_d, {
      req(Y())
      withProgress(message = "Evaluando modelos...", value = 0, {
        Y_mat <- as.matrix(Y())
        fam   <- input$familia %||% "negative.binomial"
        d_max <- input$n_latentes_max %||% 3
        resultados <- list()

        for (d in 0:d_max) {
          incProgress(1 / (d_max + 1),
                      detail = paste("Ajustando d =", d))
          tryCatch({
            fit <- gllvm::gllvm(
              y      = Y_mat,
              family = fam,
              num.lv = d,
              trace  = FALSE,
              silent = TRUE
            )
            resultados[[as.character(d)]] <- list(
              d   = d,
              AIC = AIC(fit),
              BIC = BIC(fit),
              logL = logLik(fit)[1]
            )
          }, error = function(e) {
            resultados[[as.character(d)]] <<- list(
              d = d, AIC = NA, BIC = NA, logL = NA
            )
          })
        }
        resultados_seleccion_d(resultados)
      })
    })

    output$tabla_seleccion_d <- renderUI({
      res <- resultados_seleccion_d()
      req(res)
      df <- do.call(rbind, lapply(res, as.data.frame))
      criterio <- input$criterio_seleccion %||% "AIC"
      df$seleccionado <- df[[criterio]] == min(df[[criterio]], na.rm = TRUE)

      tagList(
        card(
          card_header(bs_icon("table", class = "me-1"),
                      "Comparación de modelos"),
          card_body(
            div(style = "overflow-x: auto;",
              tags$table(
                class = "table table-sm small mb-0",
                tags$thead(
                  style = paste0("background:", colores$primario, "; color:#fff;"),
                  tags$tr(
                    tags$th("d"), tags$th("log-L"),
                    tags$th("AIC"), tags$th("BIC"), tags$th("")
                  )
                ),
                tags$tbody(
                  lapply(seq_len(nrow(df)), function(i) {
                    r <- df[i, ]
                    tags$tr(
                      style = if (isTRUE(r$seleccionado))
                        paste0("background:", colores$fondo,
                               "; font-weight:600;") else "",
                      tags$td(r$d),
                      tags$td(sprintf("%.1f", r$logL)),
                      tags$td(sprintf("%.1f", r$AIC)),
                      tags$td(sprintf("%.1f", r$BIC)),
                      tags$td(if (isTRUE(r$seleccionado))
                        tags$span(class = "badge",
                                  style = paste0("background:", colores$acento),
                                  "✓ mejor") else "")
                    )
                  })
                )
              )
            )
          )
        )
      )
    })

    output$plot_seleccion_d <- renderPlot({
      res <- resultados_seleccion_d()
      req(res)
      df <- do.call(rbind, lapply(res, as.data.frame))
      criterio <- input$criterio_seleccion %||% "AIC"
      df_plot <- df[!is.na(df[[criterio]]), ]

      ggplot2::ggplot(df_plot,
             ggplot2::aes(x = d, y = .data[[criterio]])) +
        ggplot2::geom_line(color = colores$primario, linewidth = 1) +
        ggplot2::geom_point(size = 3.5,
               color = ifelse(df_plot[[criterio]] == min(df_plot[[criterio]]),
                              colores$acento, colores$primario)) +
        ggplot2::labs(x = "Número de factores latentes (d)",
             y = criterio,
             subtitle = paste("Menor", criterio, "→ mejor ajuste")) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
              plot.subtitle = ggplot2::element_text(color = colores$texto, size = 9))
    }, res = 96)

    output$d_seleccionado_badge <- renderUI({
      res <- resultados_seleccion_d()
      req(res)
      df <- do.call(rbind, lapply(res, as.data.frame))
      criterio <- input$criterio_seleccion %||% "AIC"
      d_opt <- df$d[which.min(df[[criterio]])]
      div(
        class = "mt-3",
        div(class = "alert alert-success small py-2 px-3 mb-0",
            bs_icon("check-circle", class = "me-1"),
            "d óptimo (", criterio, "): ", strong(d_opt),
            tags$br(),
            tags$small("Actualizá el valor en 'Ajustar modelo'.")
        )
      )
    })

    # ══════════════════════════════════════════════════════════════════════════
    # AJUSTE DEL MODELO
    # ══════════════════════════════════════════════════════════════════════════
    modelo_ajustado <- reactiveVal(NULL)
    warnings_modelo  <- reactiveVal(character(0))
    error_modelo     <- reactiveVal(NULL)

    observeEvent(input$btn_ajustar, {
      req(Y())
      withProgress(message = "Ajustando GLLVM...", value = 0.3, {

        # Resetear estado
        warnings_modelo(character(0))
        error_modelo(NULL)
        warnings_capturados <- character(0)

        tryCatch({
          Y_mat  <- as.matrix(Y())
          fam    <- input$familia %||% "negative.binomial"
          n_lv   <- input$n_latentes %||% 2
          tipo   <- input$tipo_modelo %||% "unconstrained"
          r_eff  <- input$row_effect
          if (r_eff == "none") r_eff <- FALSE

          fit <- withCallingHandlers(

            if (tipo == "unconstrained") {
              gllvm::gllvm(
                y          = Y_mat,
                family     = fam,
                num.lv     = n_lv,
                row.eff    = r_eff,
                trace      = FALSE,
                silent     = FALSE
              )
            } else if (tipo == "constrained") {
              req(input$covariables)
              X_sel <- X()[, input$covariables, drop = FALSE]
              gllvm::gllvm(
                y          = Y_mat,
                X          = X_sel,
                family     = fam,
                num.lv     = n_lv,
                row.eff    = r_eff,
                trace      = FALSE,
                silent     = FALSE
              )
            } else {
              req(input$covariables, input$traits_sel)
              X_sel  <- X()[, input$covariables, drop = FALSE]
              TR_sel <- traits()[, input$traits_sel, drop = FALSE]
              gllvm::gllvm(
                y          = Y_mat,
                X          = X_sel,
                TR         = TR_sel,
                family     = fam,
                num.lv     = n_lv,
                row.eff    = r_eff,
                trace      = FALSE,
                silent     = FALSE
              )
            },

            warning = function(w) {
              warnings_capturados <<- c(warnings_capturados, conditionMessage(w))
              invokeRestart("muffleWarning")
            }
          )

          modelo_ajustado(fit)

          # Capturar warnings del withCallingHandlers + verificar convergencia en fit
          warns <- unique(warnings_capturados)

          # Verificar convergencia directamente en el objeto fit
          if (!is.null(fit$sd)) {
            # Detectar varianzas negativas en factores latentes
            if (!is.null(fit$Hess$cov.mat.mod) && any(diag(fit$Hess$cov.mat.mod) < 0, na.rm = TRUE)) {
              warns <- c(warns,
                "Algunas varianzas tienen estimados negativos. El modelo probablemente no convergió — considerar re-ajustar con diferentes condiciones iniciales.")
            }
            # Verificar si Hessiano es definido positivo
            if (!is.null(fit$Hess$cov.mat.mod) && !is.finite(determinant(fit$Hess$cov.mat.mod)$modulus)) {
              warns <- c(warns,
                "La matriz de varianza-covarianza es singular. Posible sobreparametrización o falta de convergencia.")
            }
          }
          # Verificar convergencia del optimizador
          # En gllvm: TRUE = convergió, FALSE = no convergió
          if (!is.null(fit$convergence) && !isTRUE(fit$convergence)) {
            warns <- c(warns,
              "El optimizador no convergió. Considerar re-ajustar con menos factores latentes, diferente familia, o método LA.")
          }

          warnings_modelo(unique(warns))
          incProgress(0.7)

        }, error = function(e) {
          error_modelo(conditionMessage(e))
        })
      })
    })

    output$modelo_status <- renderUI({
      # Error state
      err <- error_modelo()
      if (!is.null(err)) {
        return(div(class = "alert alert-danger small py-2 px-3 mt-2 mb-0",
                   bs_icon("x-circle", class = "me-1"),
                   "Error al ajustar: ", err))
      }
      # Success state
      fit <- modelo_ajustado()
      if (is.null(fit)) return(NULL)
      warns <- warnings_modelo()
      tagList(
        div(class = "alert alert-success small py-2 px-3 mt-2 mb-0",
            bs_icon("check-circle", class = "me-1"),
            "Modelo ajustado. AIC: ",
            strong(round(AIC(fit), 1)),
            " · log-L: ",
            strong(round(logLik(fit)[1], 1))),
        # Texto didáctico siempre visible
        tagList(
          if (length(warns) > 0) {
            div(
              class = "alert alert-warning small py-2 px-3 mt-2 mb-1",
              bs_icon("exclamation-diamond", class = "me-1"),
              strong(paste0(length(warns), " advertencia(s) de convergencia:")),
              tags$ul(class = "mb-0 mt-1", lapply(warns, tags$li))
            )
          },
          div(
            class = if (length(warns) > 0)
              "alert alert-danger small py-2 px-3 mt-0 mb-0"
            else
              "alert alert-secondary small py-2 px-3 mt-2 mb-0",
            bs_icon(
              if (length(warns) > 0) "exclamation-triangle" else "lightbulb",
              class = "me-1"
            ),
            if (length(warns) > 0)
              strong("El modelo puede tener problemas — considerar:")
            else
              strong("Buenas prácticas para GLLVM:"),

            tags$p(class = "mb-1 mt-1 text-muted",
                   tags$i("Siempre recomendable:")),
            tags$ul(
              class = "mb-2 mt-0",
              tags$li(
                strong("Re-ajustar varias veces"),
                " — gllvm usa valores iniciales aleatorios; resultados pueden variar entre corridas"
              ),
              tags$li(
                strong("Comparar d con AIC/BIC"),
                " — usar la pestaña ", em("Factores latentes"), " para seleccionar el número óptimo"
              )
            ),
            tags$p(class = "mb-1 text-muted",
                   tags$i("Si el modelo no converge:")),
            tags$ul(
              class = "mb-0 mt-0",
              tags$li(
                strong("Reducir d"),
                " — menos factores latentes reduce la complejidad"
              ),
              tags$li("Quitar predictores con poca varianza o muy correlacionados entre sí"),
              tags$li(
                "Cambiar ", strong("familia"),
                " — Poisson puede converger mejor que NB como punto de partida"
              ),
              tags$li(
                "Cambiar ", strong("método"),
                " — probar Laplace (", code("method = 'LA'"),
                ") en lugar de VA (más lento pero más preciso)"
              )
            )
          )
        )
      )
    })

    # ── Partición de varianza ───────────────────────────────────────────────
    output$plot_var_part <- plotly::renderPlotly({
      fit <- modelo_ajustado()
      req(fit)
      tryCatch({
        # varPartitioning requiere formula explícita
        X_df  <- as.data.frame(fit$X)
        X_num <- names(X_df)[sapply(X_df, is.numeric)]
        if (length(X_num) == 0)
          stop("No hay predictores numéricos para partición de varianza.")
        form <- as.formula(paste("~", paste(X_num, collapse = " + ")))
        fit_vp <- gllvm::gllvm(
          y       = fit$y,
          X       = X_df,
          formula = form,
          family  = fit$family[1],
          num.lv  = fit$num.lv,
          trace   = FALSE,
          silent  = TRUE
        )
        VP <- gllvm::varPartitioning(fit_vp)

        # prop_mat: filas = especies, columnas = componentes
        prop_mat    <- VP$PropExplainedVarSp
        especies    <- rownames(prop_mat)
        componentes <- colnames(prop_mat)

        # Medias por componente (promedio sobre especies)
        medias_comp <- round(colMeans(prop_mat) * 100, 1)

        # Data.frame largo: una fila por (especie, componente)
        df_vp <- do.call(rbind, lapply(seq_along(componentes), function(j) {
          data.frame(
            especie    = especies,
            componente = paste0(componentes[j], " (media: ", medias_comp[j], "%)"),
            proporcion = as.numeric(prop_mat[, j]),
            comp_raw   = componentes[j],
            stringsAsFactors = FALSE
          )
        }))

        # Paleta de colores StatSuite para componentes
        n_comp <- length(componentes)
        pal <- c(colores$primario, colores$acento, colores$secundario,
                 colores$advertencia, colores$peligro,
                 grDevices::gray.colors(max(0, n_comp - 5), start = 0.3, end = 0.7))
        pal <- pal[seq_len(n_comp)]
        etiquetas <- unique(df_vp$componente)
        names(pal) <- etiquetas

        p <- ggplot2::ggplot(df_vp,
               ggplot2::aes(
                 x    = especie,
                 y    = proporcion,
                 fill = componente,
                 text = paste0("<b>", especie, "</b><br>",
                               comp_raw, ": ",
                               round(proporcion * 100, 1), "%")
               )) +
          ggplot2::geom_col(position = "stack", width = 0.75) +
          ggplot2::scale_fill_manual(values = pal, name = NULL) +
          ggplot2::scale_y_continuous(
            labels = scales::percent_format(accuracy = 1),
            limits = c(0, 1),
            expand = c(0, 0)
          ) +
          ggplot2::labs(
            title = "Partición de varianza",
            x     = "Especie",
            y     = "Proporción de varianza"
          ) +
          ggplot2::theme_minimal(base_size = 11) +
          ggplot2::theme(
            axis.text.x     = ggplot2::element_text(angle = 45, hjust = 1, size = 9),
            legend.position = "right",
            legend.text     = ggplot2::element_text(size = 9),
            plot.title      = ggplot2::element_text(
              color = colores$primario, face = "bold")
          )

        plotly::ggplotly(p, tooltip = "text") |>
          plotly::layout(legend = list(orientation = "v", font = list(size = 10)))

      }, error = function(e) {
        plotly::plot_ly() |>
          plotly::add_annotations(
            x    = 0.5, y = 0.5,
            text = paste0("Partición de varianza disponible<br>",
                          "para modelos con covariables (X).<br><br>",
                          e$message),
            showarrow = FALSE,
            font = list(size = 13, color = colores$texto)
          )
      })
    })

    output$nota_var_part <- renderUI({
      fit <- modelo_ajustado()
      req(fit)
      if (is.null(fit$params$Xcoef)) {
        div(class = "alert alert-info small py-2 px-3 mt-2 mb-0",
            bs_icon("info-circle", class = "me-1"),
            "La partición de varianza requiere un modelo con covariables (X). ",
            "Ajustá el modelo en modo ", strong("Con covariables"), ".")
      } else {
        div(class = "alert alert-success small py-2 px-3 mt-2 mb-0",
            bs_icon("lightbulb", class = "me-1"),
            "Cada barra representa una especie. ",
            "Las secciones muestran qué proporción de varianza explica X (azul), ",
            "los factores latentes (naranja) y el efecto de sitio si se incluyó.")
      }
    })

    # ── Resumen del modelo ──────────────────────────────────────────────────
    output$resumen_modelo <- renderUI({
      fit <- modelo_ajustado()
      req(fit)
      s <- summary(fit)

      card(
        card_header(bs_icon("info-circle", class = "me-1"),
                    "Resumen del modelo"),
        card_body(
          layout_columns(
            col_widths = c(6, 6),
            fill = FALSE,
            div(
              tags$p(class = "small mb-1", tags$b("Familia: "),
                     fit$family[1]),
              tags$p(class = "small mb-1", tags$b("Factores latentes: "),
                     as.character(fit$num.lv)),
              tags$p(class = "small mb-1", tags$b("Sitios: "),
                     as.character(nrow(fit$y))),
              tags$p(class = "small mb-1", tags$b("Especies: "),
                     as.character(ncol(fit$y))),
              tags$p(class = "small mb-0", tags$b("AIC: "),
                     as.character(round(AIC(fit), 2)))
            ),
            div(
              tags$p(class = "small mb-1", tags$b("log-L: "),
                     as.character(round(logLik(fit)[1], 2))),
              tags$p(class = "small mb-1", tags$b("Parámetros: "),
                     as.character(attr(logLik(fit), "df"))),
              tags$p(class = "small mb-0", tags$b("Método: "),
                     paste0(fit$method %||% "VA", " (aproximación de Laplace)"))
            )
          )
        )
      )
    })

    # ══════════════════════════════════════════════════════════════════════════
    # ORDENACIÓN — BIPLOT
    # ══════════════════════════════════════════════════════════════════════════
    output$sel_tipo_biplot <- renderUI({
      fit <- modelo_ajustado()
      tiene_covariables <- !is.null(fit) && !is.null(fit$params$Xcoef)
      choices <- c(
        "Sitios"          = "sites",
        "Especies"        = "species",
        "Biplot"          = "biplot"
      )
      if (tiene_covariables) {
        choices <- c(choices, "Coeficientes X por especie" = "biplot_arrows")
      }
      radioButtons(
        ns("tipo_biplot"),
        label = "Tipo de gráfico",
        choices  = choices,
        selected = "biplot"
      )
    })

    output$nota_biplot_arrows <- renderUI({
      fit <- modelo_ajustado()
      tiene_covariables <- !is.null(fit) && !is.null(fit$params$Xcoef)
      if (tiene_covariables) return(NULL)
      div(
        class = "alert alert-info small py-2 px-3 mt-1 mb-0",
        bs_icon("info-circle", class = "me-1"),
        strong("Coeficientes X por especie"), " disponible solo con covariables. ",
        "Con covariables podés ver el biplot de sitios/especies ", em("y"),
        " los coeficientes β de cada predictor sobre cada especie."
      )
    })

    output$sel_color_sitios <- renderUI({
      X_dat <- X()
      if (is.null(X_dat)) return(NULL)
      cats <- names(X_dat)[sapply(X_dat, function(x) is.factor(x) || is.character(x))]
      if (length(cats) == 0) return(NULL)
      selectInput(
        ns("color_sitios"),
        label = "Colorear sitios por",
        choices = c("Ninguno" = "none", setNames(cats, cats)),
        selected = "none"
      )
    })

    output$plot_biplot <- renderPlot({
      fit <- modelo_ajustado()
      req(fit, fit$num.lv >= 1)

      tipo    <- input$tipo_biplot %||% "biplot"
      labels  <- input$mostrar_etiquetas %||% TRUE
      col_var <- input$color_sitios %||% "none"

      col_sitios <- if (col_var != "none" && !is.null(X())) {
        var_col <- X()[[col_var]]
        if (is.factor(var_col)) {
          colores$tableau[as.integer(var_col)]
        } else {
          grDevices::colorRampPalette(c(colores$secundario, colores$acento))(100)[
            cut(var_col, 100, labels = FALSE)]
        }
      } else colores$primario

      tryCatch({
        if (tipo == "sites") {
          gllvm::ordiplot(fit, biplot = FALSE,
                          col.sites = col_sitios,
                          symbols   = TRUE,
                          main      = "Sitios — variables latentes")
        } else if (tipo == "species") {
          gllvm::ordiplot(fit, biplot = FALSE, display = "species",
                          main = "Especies — loadings")
        } else if (tipo == "biplot_arrows") {
          # Coeficientes de covariables por especie (equivalente a arrows en ordenación restringida)
          if (is.null(fit$params$Xcoef)) {
            plot.new()
            text(0.5, 0.5,
                 "Ajustá el modelo con covariables (X)\npara ver los coeficientes de predictores.",
                 col = colores$texto, cex = 0.9, adj = 0.5)
          } else {
            gllvm::coefplot(fit,
                            cex.ylab = 0.7,
                            main     = "Coeficientes β por especie y predictor")
          }
        } else {
          # Biplot estándar sitios + especies
          gllvm::ordiplot(fit, biplot = TRUE,
                          col.sites   = col_sitios,
                          symbols     = TRUE,
                          arrow.scale = 0.8,
                          main        = "Biplot GLLVM")
        }
      }, error = function(e) {
        plot.new()
        text(0.5, 0.5, paste("Error en biplot:", e$message),
             col = colores$peligro, cex = 0.9)
      })
    }, res = 96)

    # ══════════════════════════════════════════════════════════════════════════
    # ESPECIES — COEFICIENTES
    # ══════════════════════════════════════════════════════════════════════════
    output$sel_especie_detalle <- renderUI({
      fit <- modelo_ajustado()
      req(fit)
      selectInput(
        ns("especie_detalle"),
        label = "Filtrar tabla por especie",
        choices = c("Todas las especies" = "all", colnames(fit$y)),
        selected = "all"
      )
    })

    output$plot_coef_spp <- renderPlot({
      fit  <- modelo_ajustado()
      req(fit)
      tipo <- input$tipo_coef_spp %||% "coef"

      tryCatch({
        if (tipo == "loadings") {
          gllvm::plot.gllvm(fit, which = 3,
                      main = "Loadings de especies en variables latentes")
        } else if (tipo == "intercepts") {
          b0 <- fit$params$beta0
          df_b0 <- data.frame(
            especie = names(b0),
            intercepto = as.numeric(b0)
          )
          df_b0 <- df_b0[order(df_b0$intercepto), ]
          df_b0$especie <- factor(df_b0$especie, levels = df_b0$especie)

          ggplot2::ggplot(df_b0, ggplot2::aes(x = intercepto, y = especie)) +
            ggplot2::geom_col(fill = colores$primario, alpha = 0.8) +
            ggplot2::labs(x = "Intercepto (β₀)", y = NULL,
                 title = "Interceptos por especie",
                 subtitle = "Refleja la abundancia/detectabilidad media de cada especie") +
            ggplot2::theme_minimal(base_size = 11) +
            ggplot2::theme(axis.text.y = ggplot2::element_text(size = 8))
        } else {
          # Coeficientes de covariables
          if (is.null(fit$params$Xcoef)) {
            plot.new()
            text(0.5, 0.5,
                 "Ajustá el modelo con covariables (X)\npara ver los coeficientes.",
                 col = colores$texto, cex = 0.9, adj = 0.5)
          } else {
            gllvm::coefplot(fit,
                            which.Xcoef = seq_len(ncol(fit$params$Xcoef)),
                            cex.ylab    = 0.7,
                            main        = "Coeficientes β por especie y predictor")
          }
        }
      }, error = function(e) {
        plot.new()
        text(0.5, 0.5, paste("Error:", e$message),
             col = colores$peligro, cex = 0.9)
      })
    }, res = 96, height = 450)

    output$tabla_coef_spp <- DT::renderDT({
      fit <- modelo_ajustado()
      req(fit)
      tryCatch({
        especie_sel <- input$especie_detalle %||% "all"

        if (!is.null(fit$params$Xcoef)) {
          df <- as.data.frame(round(fit$params$Xcoef, 4))
          df <- cbind(Especie = rownames(df), df)
          if (especie_sel != "all") {
            df <- df[df$Especie == especie_sel, , drop = FALSE]
          }
          DT::datatable(df,
                        rownames = FALSE,
                        options  = list(pageLength = 15, scrollX = TRUE),
                        class    = "compact")
        } else {
          df <- data.frame(
            Especie    = colnames(fit$y),
            Intercepto = round(as.numeric(fit$params$beta0), 4)
          )
          if (especie_sel != "all") {
            df <- df[df$Especie == especie_sel, , drop = FALSE]
          }
          DT::datatable(df,
                        rownames = FALSE,
                        options  = list(pageLength = 15),
                        class    = "compact")
        }
      }, error = function(e) {
        DT::datatable(data.frame(Error = conditionMessage(e)))
      })
    })

    # ══════════════════════════════════════════════════════════════════════════
    # CORRELACIONES RESIDUALES
    # ══════════════════════════════════════════════════════════════════════════
    output$plot_correlaciones <- renderPlot({
      fit <- modelo_ajustado()
      req(fit, fit$num.lv >= 1)

      n_spp  <- input$n_spp_corr %||% 20
      umbral <- input$umbral_corr %||% 0.3

      tryCatch({
        cr <- gllvm::getResidualCor(fit)
        n_show <- min(n_spp, nrow(cr))

        # Ordenar por correlación media absoluta
        if (input$orden_corr == "corr") {
          ord <- order(rowMeans(abs(cr)), decreasing = TRUE)
        } else {
          ord <- order(rownames(cr))
        }
        cr_show <- cr[ord[1:n_show], ord[1:n_show]]

        # Visualizar con corrplot si disponible, sino heatmap base
        if (requireNamespace("corrplot", quietly = TRUE)) {
          corrplot::corrplot(
            cr_show,
            method  = "color",
            type    = "upper",
            tl.cex  = 0.7,
            tl.col  = colores$texto,
            col     = grDevices::colorRampPalette(
              c(colores$peligro, "white", colores$primario))(200),
            title   = paste("Correlaciones residuales — top", n_show, "especies"),
            mar     = c(0, 0, 2, 0)
          )
        } else {
          cr_show[abs(cr_show) < umbral] <- 0
          image(
            x    = seq_len(nrow(cr_show)),
            y    = seq_len(ncol(cr_show)),
            z    = cr_show,
            col  = grDevices::colorRampPalette(
              c(colores$peligro, "white", colores$primario))(100),
            xaxt = "n", yaxt = "n",
            xlab = "", ylab = "",
            main = paste("Correlaciones residuales — top", n_show, "especies")
          )
          axis(1, at = seq_len(nrow(cr_show)),
               labels = rownames(cr_show), las = 2, cex.axis = 0.65)
          axis(2, at = seq_len(ncol(cr_show)),
               labels = colnames(cr_show), las = 2, cex.axis = 0.65)
        }
      }, error = function(e) {
        plot.new()
        text(0.5, 0.5,
             paste("Para ver correlaciones residuales\najustá un modelo con d ≥ 1.\n\n",
                   e$message),
             col = colores$texto, cex = 0.9, adj = 0.5)
      })
    }, res = 96)

    # ══════════════════════════════════════════════════════════════════════════
    # DIAGNÓSTICO
    # ══════════════════════════════════════════════════════════════════════════
    # ══════════════════════════════════════════════════════════════════════════
    # PERFORMANCE — goodnessOfFit
    # ══════════════════════════════════════════════════════════════════════════
    gof_resultados <- reactiveVal(NULL)

    observeEvent(input$btn_gof, {
      fit <- modelo_ajustado()
      req(fit)

      # Todas las métricas posibles
      metricas_todas  <- c("cor", "R2", "scor", "sR2", "RMSE", "MAE", "MARNE")
      metricas_pa     <- c("TjurR2", "AUC")
      metricas_pseudo <- c("NagelkerkeR2", "McFaddenR2", "CoxSnellR2")
      es_binomial     <- fit$family[1] %in% c("binomial", "probit")

      withProgress(message = "Calculando métricas...", value = 0.2, {
        tryCatch({
          # Calcular métricas por especie
          resultados <- list()

          for (m in metricas_todas) {
            tryCatch({
              res <- gllvm::goodnessOfFit(object = fit, measure = m, species = TRUE)
              resultados[[m]] <- as.numeric(res[[m]])
            }, error = function(e) {
              resultados[[m]] <<- rep(NA, ncol(fit$y))
            })
          }

          # Métricas solo P/A
          for (m in metricas_pa) {
            if (es_binomial) {
              tryCatch({
                res <- gllvm::goodnessOfFit(object = fit, measure = m, species = TRUE)
                resultados[[m]] <- as.numeric(res[[m]])
              }, error = function(e) {
                resultados[[m]] <<- rep(NA, ncol(fit$y))
              })
            } else {
              resultados[[m]] <- rep(NA, ncol(fit$y))
            }
          }

          # Pseudo-R² — actualmente con limitaciones en gllvm, se muestran como NA
          for (m in metricas_pseudo) {
            resultados[[m]] <- rep(NA_real_, ncol(fit$y))
          }

          incProgress(0.8)

          # Construir tabla
          df <- data.frame(
            Especie = colnames(fit$y),
            stringsAsFactors = FALSE
          )
          for (m in names(resultados)) {
            df[[m]] <- round(resultados[[m]], 4)
          }

          # Fila de medias
          medias <- c("MEDIA", sapply(names(resultados), function(m) {
            round(mean(resultados[[m]], na.rm = TRUE), 4)
          }))
          df_final <- rbind(df, setNames(as.list(medias), names(df)))

          gof_resultados(list(tabla = df_final, especies = colnames(fit$y)))

        }, error = function(e) {
          showNotification(
            paste("Error al calcular métricas:", e$message),
            type = "error", duration = 8
          )
        })
      })
    })

    output$gof_global <- renderUI({
      res <- gof_resultados()
      req(res)
      tagList(
        div(
          class = "alert alert-info small py-2 px-3 mb-1",
          bs_icon("info-circle", class = "me-1"),
          "Métricas calculadas sobre los ", strong("datos de entrenamiento"),
          " — no es validación cruzada. ",
          "La ", strong("correlación (cor)"), " mide qué tan bien el modelo reproduce ",
          "el patrón de cada especie entre sitios (observado vs predicho). ",
          "La última fila muestra el promedio global."
        ),
        div(
          class = "alert alert-warning small py-2 px-3 mb-0",
          bs_icon("exclamation-diamond", class = "me-1"),
          strong("TjurR2 y AUC"), " — solo aplican para modelos binomiales (P/A), NA para otros. ",
          strong("NagelkerkeR2, McFaddenR2, CoxSnellR2"),
          " — NA por limitación actual en ",
          code("gllvm::goodnessOfFit()"), "."
        )
      )
    })

    output$tabla_gof_especies <- DT::renderDT({
      res <- gof_resultados()
      req(res)
      df <- res$tabla

      DT::datatable(
        df,
        rownames = FALSE,
        options  = list(pageLength = 20, scrollX = TRUE,
                        dom = "tip"),
        class    = "compact"
      ) |>
        DT::formatStyle(
          "Especie",
          target    = "row",
          fontWeight = DT::styleEqual("MEDIA", "bold")
        )
    })

    output$plot_gof_especies <- plotly::renderPlotly({
      res <- gof_resultados()
      req(res)
      df <- res$tabla
      # Plot cor by default, excluding MEDIA row
      df_plot <- df[df$Especie != "MEDIA", ]

      if (!"cor" %in% names(df_plot)) return(plotly::plot_ly())

      # Ensure numeric columns after rbind with "MEDIA" row
      for (col in setdiff(names(df_plot), "Especie")) {
        df_plot[[col]] <- suppressWarnings(as.numeric(df_plot[[col]]))
      }

      df_plot$especie <- factor(df_plot$Especie,
                                 levels = df_plot$Especie[order(df_plot$cor)])

      p <- ggplot2::ggplot(df_plot,
             ggplot2::aes(x    = especie,
                          y    = cor,
                          fill = cor,
                          text = paste0("<b>", .data$Especie, "</b><br>",
                                        "cor: ", .data$cor, "<br>",
                                        "R²: ", .data$R2, "<br>",
                                        "RMSE: ", .data$RMSE))) +
        ggplot2::geom_col(width = 0.75) +
        ggplot2::scale_fill_gradient(
          low   = colores$secundario,
          high  = colores$primario,
          guide = "none"
        ) +
        ggplot2::coord_flip() +
        ggplot2::labs(
          title = "Correlación (Pearson) por especie",
          x     = NULL,
          y     = "Correlación"
        ) +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(
            color = colores$primario, face = "bold", size = 11)
        )

      plotly::ggplotly(p, tooltip = "text")
    })

    output$plot_residuos_qq <- renderPlot({
      fit <- modelo_ajustado()
      req(fit)
      tryCatch({
        res <- as.vector(as.matrix(residuals(fit)$residuals))
        res <- res[is.finite(res)]
        qqnorm(res,
               main = "QQ-plot — residuos de Dunn-Smyth",
               col  = adjustcolor(colores$primario, alpha.f = 0.5),
               pch  = 16, cex = 0.6)
        qqline(res, col = colores$acento, lwd = 2)
      }, error = function(e) {
        plot.new()
        text(0.5, 0.5, paste("Error:", e$message),
             col = colores$peligro, cex = 0.9)
      })
    }, res = 96)

    output$plot_residuos_fitted <- renderPlot({
      fit <- modelo_ajustado()
      req(fit)
      tryCatch({
        res_obj  <- residuals(fit)
        res      <- as.vector(as.matrix(res_obj$residuals))
        # Use linear predictor (eta) as x-axis
        eta      <- as.vector(as.matrix(res_obj$linpred))
        keep     <- is.finite(res) & is.finite(eta)
        res      <- res[keep]
        eta      <- eta[keep]
        plot(eta, res,
             main = "Residuos vs predictor lineal",
             xlab = "Predictor lineal (η)",
             ylab = "Residuos de Dunn-Smyth",
             col  = adjustcolor(colores$primario, alpha.f = 0.5),
             pch  = 16, cex = 0.6)
        abline(h = 0, col = colores$acento, lwd = 2, lty = 2)
        lines(lowess(eta, res), col = colores$peligro, lwd = 2)
      }, error = function(e) {
        plot.new()
        text(0.5, 0.5, paste("Error:", e$message),
             col = colores$peligro, cex = 0.9)
      })
    }, res = 96)

    output$tabla_gof <- renderUI({
      fit <- modelo_ajustado()
      req(fit)
      tagList(
        tags$p(class = "small mb-1",
               tags$b("AIC: "), round(AIC(fit), 2)),
        tags$p(class = "small mb-1",
               tags$b("BIC: "), round(BIC(fit), 2)),
        tags$p(class = "small mb-1",
               tags$b("log-L: "), round(logLik(fit)[1], 2)),
        tags$p(class = "small mb-0",
               tags$b("Parámetros: "), attr(logLik(fit), "df"))
      )
    })

    # ══════════════════════════════════════════════════════════════════════════
    # CÓDIGO R REPRODUCIBLE
    # ══════════════════════════════════════════════════════════════════════════
    codigo_generado <- reactive({
      fit  <- modelo_ajustado()
      fam  <- input$familia %||% "negative.binomial"
      n_lv <- input$n_latentes %||% 2
      tipo <- input$tipo_modelo %||% "unconstrained"

      encabezado <- encabezado_script("GLLVM — Análisis de comunidad")

      carga <- if (!is.null(data()) && data()$source == "example") {
        nm <- data()$meta$name %||% "dataset"
        paste0(
          "# Cargar datos de ejemplo\n",
          "obj <- readRDS(system.file('data/",
          input$example_choice %||% "spider",
          ".rds', package = 'StatComm'))\n",
          "Y <- obj$Y\n",
          "X <- obj$X\n"
        )
      } else {
        paste0(
          "# Cargar tus datos\n",
          "Y <- read.csv('tu_matriz_Y.csv', row.names = 1)\n",
          "X <- read.csv('tus_predictores_X.csv', row.names = 1)\n"
        )
      }

      modelo_cod <- if (tipo == "unconstrained") {
        paste0(
          "fit <- gllvm(y = as.matrix(Y),\n",
          "             family = '", fam, "',\n",
          "             num.lv = ", n_lv, ")\n"
        )
      } else if (tipo == "constrained") {
        covs <- paste(input$covariables, collapse = "', '")
        paste0(
          "X_sel <- X[, c('", covs, "')]\n\n",
          "fit <- gllvm(y = as.matrix(Y),\n",
          "             X = X_sel,\n",
          "             family = '", fam, "',\n",
          "             num.lv = ", n_lv, ")\n"
        )
      } else {
        covs <- paste(input$covariables, collapse = "', '")
        trs  <- paste(input$traits_sel, collapse = "', '")
        paste0(
          "X_sel  <- X[, c('", covs, "')]\n",
          "TR_sel <- traits[, c('", trs, "')]\n\n",
          "fit <- gllvm(y = as.matrix(Y),\n",
          "             X = X_sel,\n",
          "             TR = TR_sel,\n",
          "             family = '", fam, "',\n",
          "             num.lv = ", n_lv, ")\n"
        )
      }

      paste0(
        encabezado,
        "# ── Paquetes ──────────────────────────────────────────\n",
        "library(gllvm)\n\n",
        "# ── Datos ─────────────────────────────────────────────\n",
        carga, "\n",
        "# ── Selección de número de factores latentes ──────────\n",
        "# Comparar d = 0 a 3\n",
        "AICs <- sapply(0:3, function(d) {\n",
        "  fit <- gllvm(as.matrix(Y), family = '", fam, "', num.lv = d,\n",
        "               trace = FALSE, silent = TRUE)\n",
        "  AIC(fit)\n",
        "})\n",
        "names(AICs) <- paste0('d=', 0:3)\n",
        "print(AICs)\n\n",
        "# ── Ajustar modelo ─────────────────────────────────────\n",
        modelo_cod, "\n",
        "# ── Resumen ───────────────────────────────────────────\n",
        "summary(fit)\n",
        "AIC(fit)\n",
        "BIC(fit)\n\n",
        "# ── Ordenación (biplot) ────────────────────────────────\n",
        "ordiplot(fit, biplot = TRUE, symbols = TRUE)\n\n",
        "# ── Correlaciones residuales ───────────────────────────\n",
        "cr <- getResidualCor(fit)\n",
        "corrplot::corrplot(cr, method = 'color', type = 'upper')\n\n",
        "# ── Coeficientes de especies ───────────────────────────\n",
        "coefplot(fit)\n\n",
        "# ── Diagnóstico ────────────────────────────────────────\n",
        "plot(fit, which = 1:2)  # residuos de Dunn-Smyth\n"
      )
    })

    output$codigo_r <- renderText({ codigo_generado() })

    output$descargar_script <- downloadHandler(
      filename = function()
        paste0("statcomm_gllvm_", format(Sys.Date(), "%Y%m%d"), ".R"),
      content = function(file)
        writeLines(codigo_generado(), file)
    )

  }) # /moduleServer
}
