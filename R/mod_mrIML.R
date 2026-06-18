# =============================================================================
# mod_mrIML.R — Multi-Response Interpretable Machine Learning
# StatComm · StatSuite · Manuel Spínola · ICOMVIS · UNA
#
# Paquete principal: mrIML (Becker et al.)
# Datos: compartidos desde mod_upload (app_data reactivo)
# Flujo: modelo → ajuste → performance → importancia →
#        dependencia parcial → interacciones → SHAP →
#        red de co-ocurrencia → código R
# =============================================================================

# ── UI ────────────────────────────────────────────────────────────────────────
mod_mrIML_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      class = "px-1 pt-2 pb-2",
      layout_columns(
        col_widths = c(9, 3),
        div(
          h4(style = paste0("color:", colores$primario, "; font-weight:700; margin-bottom:4px;"),
             bs_icon("robot", class = "me-2"),
             "Multi-Response Interpretable Machine Learning (mrIML)"),
          p(class = "text-muted small mb-0",
            "Ajusta modelos de ", strong("machine learning multirrespuesta"),
            " para cada especie en la comunidad. Incorpora ",
            strong("validación cruzada"), ", importancia de variables, ",
            strong("valores SHAP"), " y redes de co-ocurrencia. ",
            "Paquete: ", strong("mrIML"), " · Becker et al. (2022)")
        ),
        div(
          class = "text-end pt-1",
          tags$span(
            class = "badge",
            style = paste0("background:", colores$acento, "; font-size:0.8rem; padding:6px 12px;"),
            bs_icon("robot", class = "me-1"), "mrIML"
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
             "mrIML — Machine Learning Multirrespuesta Interpretable"),
          p(class = "text-muted small mb-3",
            "mrIML · Becker et al. (2022) · Methods in Ecology and Evolution"),

          layout_columns(
            col_widths = c(4, 4, 4),
            fill = FALSE,

            card(
              card_header(bs_icon("question-circle", class = "me-1"),
                          "¿Qué hace mrIML?"),
              card_body(
                tags$ul(class = "small mb-0",
                  tags$li("Ajusta un modelo de ML ", strong("por cada especie"),
                          " en la comunidad de forma simultánea"),
                  tags$li("Usa el framework de ", strong("tidymodels"),
                          " — cualquier algoritmo disponible"),
                  tags$li("Incorpora ", strong("validación cruzada"),
                          " k-fold para evaluar la performance real"),
                  tags$li("Calcula importancia de variables, SHAP, ",
                          "dependencia parcial e interacciones"),
                  tags$li("Puede ajustar ", strong("redes de co-ocurrencia"),
                          " usando las respuestas como predictores (X1)")
                )
              )
            ),

            card(
              card_header(bs_icon("arrow-left-right", class = "me-1"),
                          "mrIML vs GLLVM"),
              card_body(
                tags$table(
                  class = "table table-sm small mb-0",
                  tags$thead(tags$tr(
                    tags$th(""), tags$th("GLLVM"), tags$th("mrIML")
                  )),
                  tags$tbody(
                    tags$tr(tags$td("Marco"), tags$td("Probabilístico"), tags$td("ML")),
                    tags$tr(tags$td("Validación"), tags$td("Entrenamiento"), tags$td(strong("CV k-fold ✓"))),
                    tags$tr(tags$td("Interpretación"), tags$td("Parámetros"), tags$td("SHAP, VIP")),
                    tags$tr(tags$td("Distribución"), tags$td("Explícita"), tags$td("Flexible")),
                    tags$tr(tags$td("Gradientes"), tags$td(strong("Latentes ✓")), tags$td("No")),
                    tags$tr(tags$td("Co-ocurrencia"), tags$td(strong("Residuos ✓")), tags$td(strong("Red ✓")))
                  )
                ),
                div(class = "alert alert-info small py-2 px-3 mt-2 mb-0",
                    bs_icon("info-circle", class = "me-1"),
                    "Ambos enfoques son complementarios — usarlos juntos da una visión más completa.")
              )
            ),

            card(
              card_header(bs_icon("layers", class = "me-1"),
                          "Tipos de modelos mrIML"),
              card_body(
                tags$ul(class = "small mb-0",
                  tags$li(strong("Independiente"),
                          " — cada especie se modela con X solo. Las respuestas son independientes entre sí."),
                  tags$li(strong("Red de co-ocurrencia (GN)"),
                          " — las otras respuestas (X1) se usan como predictores adicionales, capturando interacciones bióticas."),
                  tags$li(strong("Clasificación"),
                          " — para datos presencia/ausencia (0/1)"),
                  tags$li(strong("Regresión"),
                          " — para abundancias o conteos continuos")
                )
              )
            )
          ),

          tags$hr(),

          layout_columns(
            col_widths = c(6, 6),
            fill = FALSE,

            card(
              card_header(bs_icon("shield-check", class = "me-1"),
                          "Validación cruzada en mrIML"),
              card_body(
                p(class = "small text-muted mb-2",
                  "A diferencia de GLLVM, mrIML evalúa la performance con ",
                  strong("validación cruzada k-fold"), ":"),
                tags$ol(class = "small mb-2",
                  tags$li("Divide los datos en k grupos (folds)"),
                  tags$li("Entrena con k-1 folds y evalúa en el fold restante"),
                  tags$li("Repite k veces y promedia las métricas"),
                  tags$li("Resultado: estimación ", strong("honesta"),
                          " de la performance en datos no vistos")
                ),
                div(class = "alert alert-success small py-2 px-3 mb-0",
                    bs_icon("check-circle", class = "me-1"),
                    "CV evita el sobreajuste y permite comparar modelos de forma justa.")
              )
            ),

            card(
              card_header(bs_icon("lightbulb", class = "me-1"),
                          "¿Cuándo usar mrIML?"),
              card_body(
                tags$ul(class = "small mb-0",
                  tags$li("Cuando querés ", strong("predecir"),
                          " la distribución de múltiples especies simultáneamente"),
                  tags$li("Cuando el interés es en la ",
                          strong("importancia de variables"), " por especie"),
                  tags$li("Cuando hay relaciones no lineales complejas entre predictores y respuestas"),
                  tags$li("Cuando querés inferir ", strong("interacciones bióticas"),
                          " mediante redes de co-ocurrencia"),
                  tags$li("Complementario a GLLVM cuando se quiere validación predictiva formal")
                )
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
             "Fundamentos de mrIML"),
          p(class = "text-muted small mb-3",
            "Algoritmos, interpretabilidad y selección de modelos"),

          layout_columns(
            col_widths = c(4, 4, 4),
            fill = FALSE,

            card(
              card_header(bs_icon("tree", class = "me-1"),
                          "Random Forest"),
              card_body(
                p(class = "small text-muted mb-2",
                  "Ensemble de árboles de decisión. Cada árbol se entrena con una muestra aleatoria de datos y variables."),
                tags$ul(class = "small mb-0",
                  tags$li(strong("Ventajas:"), " robusto, maneja no linealidades, pocos hiperparámetros"),
                  tags$li(strong("Desventajas:"), " menos interpretable que GLM, lento con muchas especies"),
                  tags$li(strong("Hiperparámetros:"), " número de árboles, variables por split (mtry)")
                )
              )
            ),

            card(
              card_header(bs_icon("lightning", class = "me-1"),
                          "XGBoost"),
              card_body(
                p(class = "small text-muted mb-2",
                  "Gradient boosting — construye árboles secuencialmente corrigiendo los errores del anterior."),
                tags$ul(class = "small mb-0",
                  tags$li(strong("Ventajas:"), " muy alta performance predictiva, maneja datos faltantes"),
                  tags$li(strong("Desventajas:"), " más hiperparámetros, puede sobreajustar"),
                  tags$li(strong("Hiperparámetros:"), " learning rate, profundidad, subsample")
                )
              )
            ),

            card(
              card_header(bs_icon("distribute-vertical", class = "me-1"),
                          "GLM / Regresión logística"),
              card_body(
                p(class = "small text-muted mb-2",
                  "Modelo lineal generalizado — relación lineal entre predictores y respuesta."),
                tags$ul(class = "small mb-0",
                  tags$li(strong("Ventajas:"), " interpretable, rápido, buen baseline"),
                  tags$li(strong("Desventajas:"), " asume linealidad, puede tener peor performance"),
                  tags$li(strong("Uso:"), " punto de partida para comparar con modelos más complejos")
                )
              )
            )
          ),

          tags$hr(),

          layout_columns(
            col_widths = c(6, 6),
            fill = FALSE,

            card(
              card_header(bs_icon("graph-up", class = "me-1"),
                          "Interpretabilidad — SHAP y VIP"),
              card_body(
                p(class = "small text-muted mb-2",
                  "Los modelos de ML son cajas negras — mrIML usa métodos de interpretabilidad:"),
                tags$ul(class = "small mb-0",
                  tags$li(strong("VIP (Variable Importance Permutation)"),
                          " — mide cuánto empeora el modelo al permutar cada variable. Global."),
                  tags$li(strong("SHAP (SHapley Additive exPlanations)"),
                          " — contribución de cada variable a cada predicción individual. Local y global."),
                  tags$li(strong("PDP (Partial Dependence Plots)"),
                          " — efecto marginal de una variable sobre la respuesta, promediando el resto."),
                  tags$li(strong("Interacciones"),
                          " — detecta pares de variables con efectos conjuntos no aditivos.")
                )
              )
            ),

            card(
              card_header(bs_icon("sliders", class = "me-1"),
                          "Tuning de hiperparámetros"),
              card_body(
                p(class = "small text-muted mb-2",
                  "mrIML optimiza los hiperparámetros automáticamente:"),
                tags$ul(class = "small mb-0",
                  tags$li(strong("Racing (ANOVA)"),
                          " — descarta combinaciones malas temprano. Más rápido. Recomendado."),
                  tags$li(strong("Grid search"),
                          " — evalúa todas las combinaciones. Más exhaustivo pero más lento."),
                  tags$li(strong("tune_grid_size"),
                          " — número de combinaciones a evaluar. Mayor = más lento pero potencialmente mejor.")
                ),
                div(class = "alert alert-warning small py-2 px-3 mt-2 mb-0",
                    bs_icon("exclamation-diamond", class = "me-1"),
                    "Con muchas especies el ajuste puede tardar varios minutos. ",
                    "Empezar con pocas especies o un grid pequeño.")
              )
            )
          )
        )
      ), # /PESTAÑA 2

      # ══════════════════════════════════════════════════════════════════════
      # PESTAÑA 3: Ajustar modelo
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
                            "Configuración"),
                card_body(

                  selectInput(
                    ns("algoritmo"),
                    label = "Algoritmo",
                    choices = list(
                      "Random Forest"       = "rf",
                      "XGBoost"             = "xgb",
                      "Regresión logística" = "logistic",
                      "GLM Poisson"         = "poisson",
                      "SVM (RBF)"           = "svm"
                    ),
                    selected = "rf"
                  ),

                  radioButtons(
                    ns("tipo_modelo"),
                    label = "Tipo de modelo",
                    choices = c(
                      "Independiente (solo X)"         = "independiente",
                      "Red de co-ocurrencia (X + X1)"  = "red"
                    ),
                    selected = "independiente"
                  ),

                  uiOutput(ns("sel_covariables")),

                  numericInput(
                    ns("prop"),
                    label = "Proporción entrenamiento",
                    value = 0.7, min = 0.5, max = 0.9, step = 0.05
                  ),

                  numericInput(
                    ns("k_folds"),
                    label = "Folds (k) de validación cruzada",
                    value = 5, min = 3, max = 10, step = 1
                  ),

                  numericInput(
                    ns("tune_grid"),
                    label = "Tamaño del grid de tuning",
                    value = 5, min = 3, max = 20, step = 1
                  ),

                  checkboxInput(
                    ns("racing"),
                    label = "Usar racing (más rápido)",
                    value = TRUE
                  ),

                  tags$hr(),

                  actionButton(
                    ns("btn_ajustar"),
                    label = tagList(bs_icon("play-circle", class = "me-1"),
                                    "Ajustar modelos"),
                    class = "btn btn-primary w-100"
                  ),

                  uiOutput(ns("modelo_status")),
                  tags$hr(),
                  numericInput(
                    ns("n_bootstrap"),
                    label = "Número de bootstraps",
                    value = 10, min = 5, max = 100, step = 5
                  ),
                  actionButton(
                    ns("btn_bootstrap"),
                    label = tagList(bs_icon("arrow-repeat", class = "me-1"),
                                    "Calcular bootstrap"),
                    class = "btn btn-outline-primary btn-sm w-100 mt-1"
                  ),
                  p(class = "text-muted small mt-1 mb-0",
                    bs_icon("info-circle", class = "me-1"),
                    "Requerido para PDP. Mejora VIP con IC 95%."),
                  uiOutput(ns("bootstrap_status"))
                )
              )
            ),

            div(
              uiOutput(ns("resumen_modelo")),
              uiOutput(ns("info_algoritmo"))
            )
          )
        )
      ), # /PESTAÑA 3

      # ══════════════════════════════════════════════════════════════════════
      # PESTAÑA 4: Diagnóstico
      # ══════════════════════════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("clipboard-check", class = "me-1"), "Diagnóstico"),
        card_body(
          layout_columns(
            col_widths = c(3, 9),
            fill = FALSE,
            div(
              card(
                card_header(bs_icon("sliders", class = "me-1"), "Opciones"),
                card_body(
                  uiOutput(ns("sel_especie_diag")),
                  radioButtons(
                    ns("tipo_diag"),
                    label = "Tipo de gráfico",
                    choices = c(
                      "Observado vs Predicho" = "obs_pred",
                      "Residuos vs Predicho"  = "resid_pred",
                      "Distribución residuos" = "hist_resid",
                      "Curva ROC (solo P/A)"  = "roc"
                    ),
                    selected = "obs_pred"
                  )
                )
              ),
              card(
                class = "mt-3",
                card_header(bs_icon("lightbulb", class = "me-1"), "Interpretación"),
                card_body(
                  tags$ul(class = "small mb-0",
                    tags$li(strong("Obs vs Pred"), " — puntos cerca de la diagonal = buen ajuste"),
                    tags$li(strong("Residuos vs Pred"), " — sin patrones = modelo adecuado"),
                    tags$li(strong("Histograma"), " — residuos centrados en 0 = sin sesgo"),
                    tags$li(strong("ROC"), " — solo P/A; AUC > 0.7 = buena discriminación")
                  )
                )
              )
            ),
            div(
              card(
                card_header(bs_icon("graph-up", class = "me-1"), "Diagnóstico"),
                card_body(
                  plotOutput(ns("plot_diagnostico"), height = "420px")
                )
              )
            )
          )
        )
      ), # /PESTAÑA 4

      # ══════════════════════════════════════════════════════════════════════
      # PESTAÑA 5: Performance
      # ══════════════════════════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("trophy", class = "me-1"), "Performance"),
        card_body(

          layout_columns(
            col_widths = c(3, 9),
            fill = FALSE,

            div(
              card(
                card_header(bs_icon("info-circle", class = "me-1"),
                            "Métricas"),
                card_body(
                  p(class = "small text-muted mb-2",
                    "Las métricas se calculan con ", strong("validación cruzada"),
                    " — representan la performance en datos no vistos."),
                  uiOutput(ns("gof_global_mrIML"))
                )
              ),
              card(
                class = "mt-3",
                card_header(bs_icon("lightbulb", class = "me-1"),
                            "Interpretación"),
                card_body(
                  tags$p(class = "small mb-1 fw-semibold", "Regresión (abundancias/conteos):"),
                  tags$ul(class = "small mb-2",
                    tags$li(strong("RMSE"), " — error cuadrático medio; menor = mejor"),
                    tags$li(strong("R²"), " — proporción de varianza explicada; 1 = ajuste perfecto")
                  ),
                  tags$p(class = "small mb-1 fw-semibold", "Clasificación (presencia/ausencia):"),
                  tags$ul(class = "small mb-0",
                    tags$li(strong("AUC"), " — área bajo la curva ROC; > 0.7 = bueno, 0.5 = aleatorio"),
                    tags$li(strong("Accuracy"), " — proporción correctamente clasificados")
                  )
                )
              )
            ),

            div(
              card(
                card_header(bs_icon("table", class = "me-1"),
                            "Performance por especie"),
                card_body(
                  DT::DTOutput(ns("tabla_performance"))
                )
              ),
              card(
                class = "mt-3",
                card_header(bs_icon("bar-chart-steps", class = "me-1"),
                            "Gráfico de performance"),
                card_body(
                  plotly::plotlyOutput(ns("plot_performance"), height = "380px")
                )
              )
            )
          )
        )
      ), # /PESTAÑA 5

      # ══════════════════════════════════════════════════════════════════════
      # PESTAÑA 6: Importancia de variables
      # ══════════════════════════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("bar-chart-steps", class = "me-1"), "Importancia"),
        card_body(

          layout_columns(
            col_widths = c(3, 9),
            fill = FALSE,

            div(
              card(
                card_header(bs_icon("sliders", class = "me-1"), "Opciones"),
                card_body(
                  p(class = "small text-muted mb-2",
                    bs_icon("info-circle", class = "me-1"),
                    "Con bootstrap activo se muestran intervalos de confianza."),
                  uiOutput(ns("sel_especie_vip"))
                )
              ),
              card(
                class = "mt-3",
                card_header(bs_icon("lightbulb", class = "me-1"),
                            "Interpretación"),
                card_body(
                  p(class = "small text-muted mb-0",
                    "Variables con mayor importancia contribuyen más a la predicción. ",
                    "El PCA de VIP agrupa especies con patrones de importancia similares.")
                )
              )
            ),

            div(
              card(
                card_header(bs_icon("bar-chart-steps", class = "me-1"),
                            "Importancia de variables"),
                card_body(
                  plotOutput(ns("plot_vip"), height = "480px")
                )
              )
            )
          )
        )
      ), # /PESTAÑA 5

      # ══════════════════════════════════════════════════════════════════════
      # PESTAÑA 6: Dependencia parcial
      # ══════════════════════════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("graph-up", class = "me-1"), "Dependencia parcial"),
        card_body(

          layout_columns(
            col_widths = c(3, 9),
            fill = FALSE,

            div(
              card(
                card_header(bs_icon("sliders", class = "me-1"), "Opciones"),
                card_body(
                  uiOutput(ns("sel_var_pdp")),
                  uiOutput(ns("sel_especie_pdp")),
                  numericInput(
                    ns("n_bootstrap"),
                    label = "Bootstraps",
                    value = 10, min = 5, max = 50, step = 5
                  )
                )
              ),
              card(
                class = "mt-3",
                card_header(bs_icon("lightbulb", class = "me-1"),
                            "Interpretación"),
                card_body(
                  p(class = "small text-muted mb-0",
                    "Muestra el efecto marginal de una variable sobre la respuesta, ",
                    "promediando el efecto de todas las demás.")
                )
              )
            ),

            div(
              card(
                card_header(bs_icon("graph-up", class = "me-1"),
                            "Gráfico de dependencia parcial"),
                card_body(
                  plotOutput(ns("plot_pdp"), height = "420px")
                )
              )
            )
          )
        )
      ), # /PESTAÑA 6

      # ══════════════════════════════════════════════════════════════════════
      # PESTAÑA 7: Interacciones
      # ══════════════════════════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("arrows-angle-expand", class = "me-1"), "Interacciones"),
        card_body(

          layout_columns(
            col_widths = c(3, 9),
            fill = FALSE,

            div(
              card(
                card_header(bs_icon("sliders", class = "me-1"), "Opciones"),
                card_body(
                  uiOutput(ns("sel_especie_int")),
                  actionButton(
                    ns("btn_interacciones"),
                    label = tagList(bs_icon("play-circle", class = "me-1"),
                                    "Calcular interacciones"),
                    class = "btn btn-primary btn-sm w-100 mt-2"
                  )
                )
              ),
              card(
                class = "mt-3",
                card_header(bs_icon("lightbulb", class = "me-1"),
                            "Interpretación"),
                card_body(
                  p(class = "small text-muted mb-0",
                    "Detecta pares de variables con efectos conjuntos no aditivos. ",
                    "Una interacción alta significa que el efecto de una variable ",
                    "depende del valor de otra.")
                )
              )
            ),

            div(
              card(
                card_header(bs_icon("arrows-angle-expand", class = "me-1"),
                            "Interacciones entre variables"),
                card_body(
                  plotOutput(ns("plot_interacciones"), height = "420px")
                )
              )
            )
          )
        )
      ), # /PESTAÑA 7

      # ══════════════════════════════════════════════════════════════════════
      # PESTAÑA 8: SHAP
      # ══════════════════════════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("stars", class = "me-1"), "SHAP"),
        card_body(

          layout_columns(
            col_widths = c(3, 9),
            fill = FALSE,

            div(
              card(
                card_header(bs_icon("sliders", class = "me-1"), "Opciones"),
                card_body(
                  uiOutput(ns("sel_especie_shap")),
                  actionButton(
                    ns("btn_shap"),
                    label = tagList(bs_icon("play-circle", class = "me-1"),
                                    "Calcular SHAP"),
                    class = "btn btn-primary btn-sm w-100 mt-2"
                  )
                )
              ),
              card(
                class = "mt-3",
                card_header(bs_icon("lightbulb", class = "me-1"),
                            "¿Qué son los valores SHAP?"),
                card_body(
                  p(class = "small text-muted mb-2",
                    "SHAP (SHapley Additive exPlanations) mide la contribución ",
                    "de cada variable a ", strong("cada predicción individual"),
                    ". A diferencia de VIP (global), SHAP es ", strong("local"),
                    " — explica por qué el modelo predijo lo que predijo para cada sitio."),
                  tags$hr(),
                  tags$p(class = "small fw-semibold mb-1", "Cómo interpretar el gráfico:"),
                  tags$ul(class = "small text-muted mb-0",
                    tags$li(strong("Cada punto"), " = un sitio/observación"),
                    tags$li(strong("Eje X"), " — valor SHAP: indica cuánto contribuye esa variable a que la predicción sea ",
                            strong("mayor (derecha)"), " o ", strong("menor (izquierda)"),
                            " que el promedio del modelo para ese sitio"),
                    tags$li(strong("Color"), " — valor de la variable en ese sitio: azul = bajo, rojo = alto"),
                    tags$li("Variables en la parte superior tienen mayor impacto promedio sobre la predicción"),
                    tags$li(strong("Punto rojo a la derecha"), " — valores altos de esa variable ",
                            strong("aumentan"), " la abundancia/presencia predicha en ese sitio respecto al promedio"),
                    tags$li(strong("Punto rojo a la izquierda"), " — valores altos de esa variable ",
                            strong("reducen"), " la abundancia/presencia predicha en ese sitio respecto al promedio"),
                    tags$li("Predicción base (promedio global del modelo) + suma de todos los SHAP del sitio = ",
                            strong("predicción final"), " para ese sitio específico")
                  )
                )
              )
            ),

            div(
              card(
                card_header(bs_icon("stars", class = "me-1"),
                            "Valores SHAP"),
                card_body(
                  plotOutput(ns("plot_shap"), height = "600px")
                )
              )
            )
          )
        )
      ), # /PESTAÑA 8

      # ══════════════════════════════════════════════════════════════════════
      # PESTAÑA 9: Red de co-ocurrencia
      # ══════════════════════════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("diagram-3", class = "me-1"), "Co-ocurrencia"),
        card_body(

          layout_columns(
            col_widths = c(3, 9),
            fill = FALSE,

            div(
              card(
                card_header(bs_icon("info-circle", class = "me-1"),
                            "Red de co-ocurrencia"),
                card_body(
                  p(class = "small text-muted mb-2",
                    "Requiere modelo ajustado en modo ",
                    strong("Red de co-ocurrencia (X + X1)"), "."),
                  p(class = "small text-muted mb-0",
                    "Las conexiones entre especies representan asociaciones ",
                    "controladas por los predictores ambientales — posibles ",
                    "interacciones bióticas o respuestas a gradientes no medidos.")
                )
              )
            ),

            div(
              card(
                card_header(bs_icon("diagram-3", class = "me-1"),
                            "Red de co-ocurrencia de especies"),
                card_body(
                  plotOutput(ns("plot_red"), height = "480px")
                )
              )
            )
          )
        )
      ), # /PESTAÑA 9

      # ══════════════════════════════════════════════════════════════════════
      # PESTAÑA 10: Código R
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
                    "Script R reproducible con el análisis completo."),
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
      ) # /PESTAÑA 10

    ) # /navset_card_tab
  )
}


# ── Server ────────────────────────────────────────────────────────────────────
mod_mrIML_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Datos desde mod_upload ──────────────────────────────────────────────
    Y      <- reactive({ req(data()); as.data.frame(data()$Y) })
    X      <- reactive({ data()$X })
    meta   <- reactive({ data()$meta })

    # ── Selector de covariables ─────────────────────────────────────────────
    output$sel_covariables <- renderUI({
      X_dat <- X()
      if (is.null(X_dat)) {
        return(div(class = "alert alert-warning small py-2 px-3",
                   bs_icon("exclamation-diamond", class = "me-1"),
                   "No hay predictores (X) disponibles."))
      }
      checkboxGroupInput(
        ns("covariables"),
        label    = "Predictores (X)",
        choices  = names(X_dat),
        selected = names(X_dat)[1:min(3, ncol(X_dat))],
        inline   = FALSE
      )
    })

    # ── Info del algoritmo ──────────────────────────────────────────────────
    output$info_algoritmo <- renderUI({
      algo <- input$algoritmo %||% "rf"
      info <- list(
        rf = list(
          titulo = "Random Forest",
          desc   = "Ensemble de árboles de decisión con muestreo aleatorio de variables y observaciones.",
          func   = "rand_forest(mtry = tune(), trees = tune()) |> set_engine('ranger')"
        ),
        xgb = list(
          titulo = "XGBoost",
          desc   = "Gradient boosting secuencial. Alta performance pero más hiperparámetros.",
          func   = "boost_tree(mtry = tune(), trees = tune(), learn_rate = tune()) |> set_engine('xgboost')"
        ),
        logistic = list(
          titulo = "Regresión logística",
          desc   = "Modelo lineal generalizado para datos binarios (presencia/ausencia).",
          func   = "logistic_reg(penalty = tune()) |> set_engine('glmnet')"
        ),
        poisson = list(
          titulo = "GLM Poisson",
          desc   = "Modelo lineal generalizado para conteos.",
          func   = "poisson_reg(penalty = tune()) |> set_engine('glmnet')"
        ),
        svm = list(
          titulo = "SVM (RBF kernel)",
          desc   = "Support Vector Machine con kernel radial. Robusto pero menos interpretable.",
          func   = "svm_rbf(cost = tune(), rbf_sigma = tune()) |> set_engine('kernlab')"
        )
      )
      i <- info[[algo]]
      card(
        class = "mt-3",
        card_header(bs_icon("info-circle", class = "me-1"),
                    paste("Algoritmo:", i$titulo)),
        card_body(
          p(class = "small text-muted mb-2", i$desc),
          div(class = "codigo-bloque", i$func)
        )
      )
    })

    # ══════════════════════════════════════════════════════════════════════════
    # AJUSTE DEL MODELO
    # ══════════════════════════════════════════════════════════════════════════
    modelo_ajustado  <- reactiveVal(NULL)
    bootstrap_obj    <- reactiveVal(NULL)
    warnings_modelo  <- reactiveVal(character(0))
    error_modelo     <- reactiveVal(NULL)

    # Función auxiliar para crear el modelo de tidymodels
    get_tidy_model <- function(algo, tipo_Y) {
      es_clasificacion <- all(unlist(lapply(tipo_Y, function(col)
        length(unique(col)) == 2 && all(col %in% c(0, 1)))))

      switch(algo,
        rf = {
          if (es_clasificacion)
            parsnip::rand_forest(mtry = tune::tune(), trees = tune::tune()) |>
              parsnip::set_engine("ranger") |>
              parsnip::set_mode("classification")
          else
            parsnip::rand_forest(mtry = tune::tune(), trees = tune::tune()) |>
              parsnip::set_engine("ranger") |>
              parsnip::set_mode("regression")
        },
        xgb = {
          if (es_clasificacion)
            parsnip::boost_tree(mtry = tune::tune(), trees = tune::tune(),
                                learn_rate = tune::tune()) |>
              parsnip::set_engine("xgboost") |>
              parsnip::set_mode("classification")
          else
            parsnip::boost_tree(mtry = tune::tune(), trees = tune::tune(),
                                learn_rate = tune::tune()) |>
              parsnip::set_engine("xgboost") |>
              parsnip::set_mode("regression")
        },
        logistic =
          parsnip::logistic_reg(penalty = tune::tune()) |>
            parsnip::set_engine("glmnet") |>
            parsnip::set_mode("classification"),
        poisson =
          parsnip::poisson_reg(penalty = tune::tune()) |>
            parsnip::set_engine("glmnet") |>
            parsnip::set_mode("regression"),
        svm = {
          if (es_clasificacion)
            parsnip::svm_rbf(cost = tune::tune(), rbf_sigma = tune::tune()) |>
              parsnip::set_engine("kernlab") |>
              parsnip::set_mode("classification")
          else
            parsnip::svm_rbf(cost = tune::tune(), rbf_sigma = tune::tune()) |>
              parsnip::set_engine("kernlab") |>
              parsnip::set_mode("regression")
        }
      )
    }

    observeEvent(input$btn_ajustar, {
      req(Y(), X())
      warnings_modelo(character(0))
      error_modelo(NULL)

      withProgress(message = "Ajustando modelos mrIML...", value = 0.1, {
        tryCatch({
          Y_mat  <- Y()
          X_sel  <- X()[, input$covariables %||% names(X()), drop = FALSE]
          algo   <- input$algoritmo %||% "rf"
          tipo   <- input$tipo_modelo %||% "independiente"

          Model <- get_tidy_model(algo, Y_mat)

          incProgress(0.2, detail = "Configurando modelo...")

          warns <- character(0)
          fit <- withCallingHandlers(
            if (tipo == "independiente") {
              mrIML::mrIMLpredicts(
                Y              = Y_mat,
                X              = X_sel,
                Model          = Model,
                prop           = input$prop %||% 0.7,
                k              = input$k_folds %||% 5,
                tune_grid_size = input$tune_grid %||% 5,
                racing         = input$racing %||% TRUE
              )
            } else {
              mrIML::mrIMLpredicts(
                Y              = Y_mat,
                X              = X_sel,
                X1             = Y_mat,
                Model          = Model,
                prop           = input$prop %||% 0.7,
                k              = input$k_folds %||% 5,
                tune_grid_size = input$tune_grid %||% 5,
                racing         = input$racing %||% TRUE
              )
            },
            warning = function(w) {
              warns <<- c(warns, conditionMessage(w))
              invokeRestart("muffleWarning")
            }
          )

          modelo_ajustado(fit)
          warnings_modelo(unique(warns))
          incProgress(0.8)

        }, error = function(e) {
          error_modelo(conditionMessage(e))
        })
      })
    })

    observeEvent(input$btn_bootstrap, {
      fit <- modelo_ajustado()
      req(fit)
      withProgress(message = "Calculando bootstrap...", value = 0.3, {
        tryCatch({
          bs <- mrIML::mrBootstrap(
            mrIMLobj      = fit,
            num_bootstrap = input$n_bootstrap %||% 10,
            downsample    = FALSE
          )
          bootstrap_obj(bs)
          incProgress(0.7)
        }, error = function(e) {
          showNotification(paste("Error bootstrap:", e$message),
                           type = "error", duration = 8)
        })
      })
    })

    output$bootstrap_status <- renderUI({
      bs <- bootstrap_obj()
      if (is.null(bs)) return(NULL)
      div(class = "alert alert-success small py-2 px-3 mt-1 mb-0",
          bs_icon("check-circle", class = "me-1"),
          "Bootstrap calculado (",
          strong(input$n_bootstrap %||% 10), " iteraciones).")
    })

    output$modelo_status <- renderUI({
      err <- error_modelo()
      if (!is.null(err)) {
        return(div(class = "alert alert-danger small py-2 px-3 mt-2 mb-0",
                   bs_icon("x-circle", class = "me-1"),
                   "Error al ajustar: ", err))
      }
      fit <- modelo_ajustado()
      if (is.null(fit)) return(NULL)
      warns <- warnings_modelo()
      tagList(
        div(class = "alert alert-success small py-2 px-3 mt-2 mb-1",
            bs_icon("check-circle", class = "me-1"),
            strong("Modelos ajustados. "),
            length(fit$Fits), " especies modeladas."),
        if (length(warns) > 0) {
          div(class = "alert alert-warning small py-2 px-3 mt-0 mb-0",
              bs_icon("exclamation-diamond", class = "me-1"),
              strong(paste0(length(warns), " advertencia(s):")),
              tags$ul(class = "mb-0 mt-1",
                      lapply(unique(warns), tags$li)))
        }
      )
    })

    output$resumen_modelo <- renderUI({
      fit <- modelo_ajustado()
      req(fit)
      card(
        card_header(bs_icon("info-circle", class = "me-1"), "Resumen"),
        card_body(
          tags$p(class = "small mb-1", tags$b("Algoritmo: "),
                 as.character(input$algoritmo)),
          tags$p(class = "small mb-1", tags$b("Tipo: "),
                 as.character(input$tipo_modelo)),
          tags$p(class = "small mb-1", tags$b("Especies modeladas: "),
                 as.character(length(fit$Fits))),
          tags$p(class = "small mb-1", tags$b("Predictores: "),
                 as.character(length(input$covariables))),
          tags$p(class = "small mb-1", tags$b("Folds CV: "),
                 as.character(input$k_folds)),
          tags$p(class = "small mb-0", tags$b("Prop. entrenamiento: "),
                 as.character(input$prop))
        )
      )
    })

    # ══════════════════════════════════════════════════════════════════════════
    # PERFORMANCE
    # ══════════════════════════════════════════════════════════════════════════
    performance_data <- reactive({
      fit <- modelo_ajustado()
      req(fit)
      tryCatch({
        mrIML::mrIMLperformance(fit)
      }, error = function(e) NULL)
    })

    # ══════════════════════════════════════════════════════════════════════════
    # DIAGNÓSTICO
    # ══════════════════════════════════════════════════════════════════════════
    output$sel_especie_diag <- renderUI({
      fit <- modelo_ajustado()
      req(fit)
      selectInput(ns("especie_diag"), label = "Especie",
                  choices = names(fit$Fits), selected = names(fit$Fits)[1])
    })

    output$plot_diagnostico <- renderPlot({
      fit  <- modelo_ajustado()
      req(fit, input$especie_diag)
      tipo <- input$tipo_diag %||% "obs_pred"
      tryCatch({
        esp_fit    <- fit$Fits[[input$especie_diag]]
        obs        <- esp_fit$data_train$class
        pred       <- esp_fit$yhat
        n          <- min(length(obs), length(pred))
        obs        <- obs[seq_len(n)]
        pred       <- pred[seq_len(n)]
        residuos   <- obs - pred
        es_binario <- length(unique(obs)) == 2 && all(obs %in% c(0, 1))

        if (tipo == "obs_pred") {
          df  <- data.frame(obs = obs, pred = pred)
          lim <- range(c(obs, pred), na.rm = TRUE)
          ggplot2::ggplot(df, ggplot2::aes(x = pred, y = obs)) +
            ggplot2::geom_abline(slope = 1, intercept = 0,
                                  color = colores$acento, linetype = "dashed", linewidth = 0.8) +
            ggplot2::geom_point(color = colores$primario, size = 2.5, alpha = 0.75) +
            ggrepel::geom_text_repel(ggplot2::aes(label = seq_along(obs)),
                                      size = 2.5, color = colores$texto, max.overlaps = 10) +
            ggplot2::coord_fixed(xlim = lim, ylim = lim) +
            ggplot2::labs(
              title    = paste("Observado vs Predicho —", input$especie_diag),
              subtitle = paste0("RMSE = ", round(sqrt(mean(residuos^2, na.rm=TRUE)), 3),
                                " · R² = ", round(cor(obs, pred, use="complete.obs")^2, 3),
                                "  (datos de entrenamiento)"),
              x = "Predicho", y = "Observado") +
            ggplot2::theme_light(base_size = 12) +
            ggplot2::theme(plot.title = ggplot2::element_text(color = colores$primario, face = "bold"))

        } else if (tipo == "resid_pred") {
          df <- data.frame(pred = pred, resid = residuos)
          ggplot2::ggplot(df, ggplot2::aes(x = pred, y = resid)) +
            ggplot2::geom_hline(yintercept = 0, color = colores$acento,
                                 linetype = "dashed", linewidth = 0.8) +
            ggplot2::geom_point(color = colores$primario, size = 2.5, alpha = 0.75) +
            ggplot2::geom_smooth(method = "loess", se = FALSE,
                                  color = colores$peligro, linewidth = 0.8) +
            ggplot2::labs(
              title    = paste("Residuos vs Predicho —", input$especie_diag),
              subtitle = "Sin patrones sistemáticos = modelo adecuado",
              x = "Predicho", y = "Residuo (obs - pred)") +
            ggplot2::theme_light(base_size = 12) +
            ggplot2::theme(plot.title = ggplot2::element_text(color = colores$primario, face = "bold"))

        } else if (tipo == "hist_resid") {
          df <- data.frame(resid = residuos)
          ggplot2::ggplot(df, ggplot2::aes(x = resid)) +
            ggplot2::geom_histogram(fill = colores$primario, color = "white",
                                     bins = 10, alpha = 0.85) +
            ggplot2::geom_vline(xintercept = 0, color = colores$acento,
                                 linetype = "dashed", linewidth = 0.8) +
            ggplot2::labs(
              title    = paste("Distribución de residuos —", input$especie_diag),
              subtitle = paste0("Media = ", round(mean(residuos, na.rm=TRUE), 3),
                                " · SD = ", round(sd(residuos, na.rm=TRUE), 3)),
              x = "Residuo", y = "Frecuencia") +
            ggplot2::theme_light(base_size = 12) +
            ggplot2::theme(plot.title = ggplot2::element_text(color = colores$primario, face = "bold"))

        } else if (tipo == "roc") {
          if (!es_binario) {
            plot.new()
            text(0.5, 0.5,
                 "La curva ROC solo aplica\npara datos presencia/ausencia (0/1).",
                 col = colores$texto, cex = 1.0, adj = 0.5)
          } else {
            roc_obj <- pROC::roc(obs, pred, quiet = TRUE)
            roc_df  <- data.frame(
              especificidad = 1 - roc_obj$specificities,
              sensibilidad  = roc_obj$sensitivities)
            ggplot2::ggplot(roc_df, ggplot2::aes(x = especificidad, y = sensibilidad)) +
              ggplot2::geom_abline(slope = 1, intercept = 0, color = colores$texto,
                                    linetype = "dashed", linewidth = 0.5, alpha = 0.5) +
              ggplot2::geom_line(color = colores$primario, linewidth = 1.2) +
              ggplot2::geom_area(fill = colores$primario, alpha = 0.15) +
              ggplot2::labs(
                title    = paste("Curva ROC —", input$especie_diag),
                subtitle = paste0("AUC = ", round(pROC::auc(roc_obj), 3)),
                x = "1 - Especificidad", y = "Sensibilidad") +
              ggplot2::coord_fixed() +
              ggplot2::theme_light(base_size = 12) +
              ggplot2::theme(plot.title = ggplot2::element_text(color = colores$primario, face = "bold"))
          }
        }
      }, error = function(e) {
        plot.new()
        text(0.5, 0.5, paste("Error:", e$message), col = colores$peligro, cex = 0.9)
      })
    }, res = 96)

    output$gof_global_mrIML <- renderUI({
      perf <- performance_data()
      req(perf)
      div(class = "alert alert-success small py-2 px-3 mb-0",
          bs_icon("check-circle", class = "me-1"),
          strong("RMSE global (CV): "),
          strong(round(perf$global_performance_summary, 3)))
    })

    output$tabla_performance <- DT::renderDT({
      perf <- performance_data()
      req(perf)
      df <- perf$model_performance
      DT::datatable(df, rownames = FALSE,
                    options = list(pageLength = 15, scrollX = TRUE),
                    class = "compact") |>
        DT::formatRound(columns = c("rmse", "rsquared"), digits = 4)
    })

    output$plot_performance <- plotly::renderPlotly({
      perf <- performance_data()
      req(perf)
      tryCatch({
        df <- perf$model_performance

        # Detectar métrica disponible
        metric_col <- if ("roc_auc" %in% names(df)) "roc_auc"
                      else if ("rsquared" %in% names(df)) "rsquared"
                      else "rmse"

        df <- df[order(df[[metric_col]]), ]
        df$response <- factor(df$response, levels = df$response)

        p <- ggplot2::ggplot(df,
               ggplot2::aes(x = .data[[metric_col]],
                            y = response,
                            fill = .data[[metric_col]],
                            text = paste0("<b>", response, "</b><br>",
                                          "RMSE: ", round(rmse, 3), "<br>",
                                          "R²: ", round(rsquared, 3)))) +
          ggplot2::geom_col(width = 0.75) +
          ggplot2::scale_fill_gradient(
            low = colores$secundario, high = colores$primario, guide = "none") +
          ggplot2::labs(
            title    = paste("Performance por especie —", metric_col),
            subtitle = paste0("Performance global (RMSE): ",
                              round(perf$global_performance_summary, 3)),
            x = metric_col, y = NULL) +
          ggplot2::theme_light(base_size = 11) +
          ggplot2::theme(
            plot.title = ggplot2::element_text(
              color = colores$primario, face = "bold", size = 11),
            plot.subtitle = ggplot2::element_text(
              color = colores$texto, size = 9))

        plotly::ggplotly(p, tooltip = "text")
      }, error = function(e) {
        plotly::plot_ly() |>
          plotly::add_annotations(text = paste("Error:", e$message),
                                   showarrow = FALSE)
      })
    })

    # ══════════════════════════════════════════════════════════════════════════
    # IMPORTANCIA DE VARIABLES
    # ══════════════════════════════════════════════════════════════════════════
    output$sel_especie_vip <- renderUI({
      fit <- modelo_ajustado()
      req(fit)
      selectInput(
        ns("especie_vip"),
        label    = "Especie (para detalle)",
        choices  = c("Todas" = "all", names(fit$Fits)),
        selected = "all"
      )
    })

    output$plot_vip <- renderPlot({
      fit <- modelo_ajustado()
      req(fit)
      tryCatch({
        tipo <- input$tipo_vip %||% "species"
        esp  <- input$especie_vip %||% "all"
        bs   <- isolate(bootstrap_obj())

        # Obtener datos VIP
        vip_obj <- if (!is.null(bs)) {
          mrIML::mrVip(fit, mrBootstrap_obj = bs)
        } else {
          mrIML::mrVip(fit)
        }

        df_vip <- vip_obj[[1]]  # tibble: var, sd_value, response, bootstrap

        # Filtrar por especie si corresponde
        if (esp != "all") {
          df_vip <- df_vip[df_vip$response == esp, ]
          titulo <- paste("Importancia de variables —", esp)
        } else {
          titulo <- "Importancia de variables — todas las especies"
        }

        # Calcular media por variable para ordenar
        medias <- tapply(df_vip$sd_value, df_vip$var, mean, na.rm = TRUE)
        orden  <- names(sort(medias))
        df_vip$var <- factor(df_vip$var, levels = orden)

        # Gráfico: barra (media) + boxplot (distribución bootstrap)
        df_media <- data.frame(
          var   = factor(names(medias), levels = orden),
          media = as.numeric(medias)
        )

        ggplot2::ggplot() +
          # Barra de media
          ggplot2::geom_col(
            data  = df_media,
            ggplot2::aes(x = media, y = var),
            fill  = colores$primario,
            alpha = 0.35,
            width = 0.7
          ) +
          # Boxplot del bootstrap
          ggplot2::geom_boxplot(
            data  = df_vip,
            ggplot2::aes(x = sd_value, y = var),
            fill  = colores$secundario,
            color = colores$primario,
            alpha = 0.7,
            width = 0.5,
            outlier.color = colores$acento,
            outlier.size  = 1.5
          ) +
          ggplot2::labs(
            title    = titulo,
            subtitle = if (!is.null(bs))
              "Barra = media · Boxplot = distribución bootstrap"
            else
              "Sin bootstrap — calculalo para ver intervalos de confianza",
            x = "Importancia (permutación)",
            y = NULL
          ) +
          ggplot2::theme_light(base_size = 12) +
          ggplot2::theme(
            panel.grid.minor   = ggplot2::element_blank(),
            panel.grid.major.y = ggplot2::element_blank(),
            plot.title   = ggplot2::element_text(color = colores$primario, face = "bold"),
            plot.subtitle = ggplot2::element_text(color = colores$texto, size = 9)
          )

      }, error = function(e) {
        plot.new()
        text(0.5, 0.5, paste("Error:", e$message),
             col = colores$peligro, cex = 0.9)
      })
    }, res = 96, height = 500)

    # ══════════════════════════════════════════════════════════════════════════
    # DEPENDENCIA PARCIAL
    # ══════════════════════════════════════════════════════════════════════════
    output$sel_var_pdp <- renderUI({
      X_dat <- X()
      req(X_dat, input$covariables)
      selectInput(
        ns("var_pdp"),
        label    = "Variable",
        choices  = input$covariables,
        selected = input$covariables[1]
      )
    })

    output$sel_especie_pdp <- renderUI({
      fit <- modelo_ajustado()
      req(fit)
      selectInput(
        ns("especie_pdp"),
        label    = "Especie",
        choices  = names(fit$Fits),
        selected = names(fit$Fits)[1]
      )
    })

    output$plot_pdp <- renderPlot({
      fit <- modelo_ajustado()
      req(fit, input$var_pdp, input$especie_pdp)
      tryCatch({
        bs <- bootstrap_obj()
        if (is.null(bs)) {
          plot.new()
          text(0.5, 0.5,
               "Calculá el bootstrap primero\nen la pestaña Ajustar modelo.",
               col = colores$texto, cex = 0.9, adj = 0.5)
        } else {
          mrIML::mrPdPlotBootstrap(
            mrIML_obj       = fit,
            mrBootstrap_obj = bs,
            target          = input$especie_pdp
          )
        }
      }, error = function(e) {
        plot.new()
        text(0.5, 0.5, paste("Error:", e$message),
             col = colores$peligro, cex = 0.9)
      })
    }, res = 96)

    # ══════════════════════════════════════════════════════════════════════════
    # INTERACCIONES
    # ══════════════════════════════════════════════════════════════════════════
    output$sel_especie_int <- renderUI({
      fit <- modelo_ajustado()
      req(fit)
      selectInput(
        ns("especie_int"),
        label    = "Especie",
        choices  = names(fit$Fits),
        selected = names(fit$Fits)[1]
      )
    })

    interacciones_data <- reactiveVal(NULL)

    observeEvent(input$btn_interacciones, {
      fit <- modelo_ajustado()
      req(fit, input$especie_int)
      withProgress(message = "Calculando interacciones...", value = 0.5, {
        tryCatch({
          int <- mrIML::mrInteractions(
            mrIMLobj  = fit,
            feature   = input$especie_int,
            num_bootstrap = 1
          )
          interacciones_data(int)
        }, error = function(e) {
          showNotification(paste("Error:", e$message), type = "error")
        })
      })
    })

    output$plot_interacciones <- renderPlot({
      int <- interacciones_data()
      req(int)
      tryCatch({
        df <- int$h2_pairwise_df

        # Promediar sobre bootstraps
        df_mean <- df |>
          dplyr::group_by(name) |>
          dplyr::summarise(value = mean(value, na.rm = TRUE)) |>
          dplyr::arrange(value) |>
          dplyr::mutate(name = factor(name, levels = name))

        ggplot2::ggplot(df_mean,
               ggplot2::aes(x = value, y = name, fill = value)) +
          ggplot2::geom_col(width = 0.75, alpha = 0.85) +
          ggplot2::scale_fill_gradient(
            low = colores$secundario, high = colores$primario, guide = "none") +
          ggplot2::labs(
            title    = paste("Interacciones entre variables —", input$especie_int),
            subtitle = "Estadístico H² de Friedman — mayor valor = mayor interacción",
            x = "H² (fuerza de interacción)",
            y = NULL
          ) +
          ggplot2::theme_light(base_size = 12) +
          ggplot2::theme(
            panel.grid.minor   = ggplot2::element_blank(),
            panel.grid.major.y = ggplot2::element_blank(),
            plot.title  = ggplot2::element_text(color = colores$primario, face = "bold"),
            plot.subtitle = ggplot2::element_text(color = colores$texto, size = 9)
          )
      }, error = function(e) {
        plot.new()
        text(0.5, 0.5, paste("Error:", e$message),
             col = colores$peligro, cex = 0.9)
      })
    }, res = 96, height = 450)

    # ══════════════════════════════════════════════════════════════════════════
    # SHAP
    # ══════════════════════════════════════════════════════════════════════════
    output$sel_especie_shap <- renderUI({
      fit <- modelo_ajustado()
      req(fit)
      selectInput(
        ns("especie_shap"),
        label    = "Especie",
        choices  = names(fit$Fits),
        selected = names(fit$Fits)[1]
      )
    })

    shap_data <- reactiveVal(NULL)

    observeEvent(input$btn_shap, {
      fit <- modelo_ajustado()
      req(fit, input$especie_shap)
      withProgress(message = "Calculando SHAP...", value = 0.5, {
        tryCatch({
          shap <- mrIML::mrShapely(
            mrIML_obj = fit,
            taxa      = input$especie_shap
          )
          shap_data(shap)
        }, error = function(e) {
          showNotification(paste("Error:", e$message), type = "error")
        })
      })
    })

    output$plot_shap <- renderPlot({
      shap <- shap_data()
      req(shap, input$especie_shap)
      tryCatch({
        shap_esp <- shap$SHAP_values[[input$especie_shap]]
        req(shap_esp)
        p <- shapviz::sv_importance(shap_esp, kind = "beeswarm", size = 2.5, alpha = 1) +
          ggplot2::labs(
            title    = paste("Valores SHAP —", input$especie_shap),
            x        = "Valor SHAP (contribución a la predicción)",
            y        = "Variable",
            color    = "Valor de\nla variable"
          ) +
          ggplot2::scale_color_gradient(
            low    = "#3B82F6",
            high   = "#EF4444",
            breaks = c(0, 1),
            labels = c("Bajo", "Alto"),
            limits = c(0, 1)
          ) +
          ggplot2::theme_light(base_size = 12) +
          ggplot2::theme(
            plot.title = ggplot2::element_text(
              color = colores$primario, face = "bold")
          )
        print(p)
      }, error = function(e) {
        plot.new()
        text(0.5, 0.5, paste("Error:", e$message),
             col = colores$peligro, cex = 0.9)
      })
    }, res = 96, height = 600)

    # ══════════════════════════════════════════════════════════════════════════
    # RED DE CO-OCURRENCIA
    # ══════════════════════════════════════════════════════════════════════════
    output$plot_red <- renderPlot({
      fit <- modelo_ajustado()
      req(fit)
      tryCatch({
        mrIML::mrCoOccurNet(fit)
      }, error = function(e) {
        plot.new()
        text(0.5, 0.5,
             paste0("La red de co-ocurrencia requiere\n",
                    "modelo ajustado en modo\n",
                    "'Red de co-ocurrencia (X + X1)'.\n\n",
                    e$message),
             col = colores$texto, cex = 0.9, adj = 0.5)
      })
    }, res = 96)

    # ══════════════════════════════════════════════════════════════════════════
    # CÓDIGO R
    # ══════════════════════════════════════════════════════════════════════════
    codigo_generado <- reactive({
      algo  <- input$algoritmo %||% "rf"
      tipo  <- input$tipo_modelo %||% "independiente"
      covs  <- input$covariables %||% "todas"

      encabezado <- encabezado_script("mrIML — Machine Learning Multirrespuesta")

      modelo_codigo <- switch(algo,
        rf       = "rand_forest(mtry = tune(), trees = tune()) |>\n  set_engine('ranger') |>\n  set_mode('classification')",
        xgb      = "boost_tree(mtry = tune(), trees = tune(), learn_rate = tune()) |>\n  set_engine('xgboost') |>\n  set_mode('classification')",
        logistic = "logistic_reg(penalty = tune()) |>\n  set_engine('glmnet') |>\n  set_mode('classification')",
        poisson  = "poisson_reg(penalty = tune()) |>\n  set_engine('glmnet') |>\n  set_mode('regression')",
        svm      = "svm_rbf(cost = tune(), rbf_sigma = tune()) |>\n  set_engine('kernlab') |>\n  set_mode('classification')"
      )

      paste0(
        encabezado,
        "# ── Paquetes ──────────────────────────────────────────\n",
        "library(mrIML)\n",
        "library(tidymodels)\n\n",
        "# ── Datos ─────────────────────────────────────────────\n",
        "obj <- readRDS(system.file('data/spider.rds', package = 'StatComm'))\n",
        "Y   <- obj$Y  # matriz de especies\n",
        "X   <- obj$X  # predictores ambientales\n\n",
        "# ── Definir el modelo ──────────────────────────────────\n",
        "Model <- ", modelo_codigo, "\n\n",
        "# ── Ajustar mrIML ──────────────────────────────────────\n",
        if (tipo == "independiente") {
          paste0(
            "MR_model <- mrIMLpredicts(\n",
            "  Y              = Y,\n",
            "  X              = X,\n",
            "  Model          = Model,\n",
            "  prop           = ", input$prop %||% 0.7, ",\n",
            "  k              = ", input$k_folds %||% 5, ",\n",
            "  tune_grid_size = ", input$tune_grid %||% 5, ",\n",
            "  racing         = ", input$racing %||% TRUE, "\n",
            ")\n\n"
          )
        } else {
          paste0(
            "MR_model <- mrIMLpredicts(\n",
            "  Y              = Y,\n",
            "  X              = X,\n",
            "  X1             = Y,  # co-occurrence network\n",
            "  Model          = Model,\n",
            "  prop           = ", input$prop %||% 0.7, ",\n",
            "  k              = ", input$k_folds %||% 5, ",\n",
            "  tune_grid_size = ", input$tune_grid %||% 5, ",\n",
            "  racing         = ", input$racing %||% TRUE, "\n",
            ")\n\n"
          )
        },
        "# ── Performance ───────────────────────────────────────\n",
        "perf <- mrIMLperformance(MR_model)\n",
        "mrPerformancePlot(perf)\n\n",
        "# ── Importancia de variables ───────────────────────────\n",
        "mrVip(MR_model)\n",
        "mrVipPCA(MR_model)\n\n",
        "# ── Dependencia parcial ────────────────────────────────\n",
        "mrPdPlotBootstrap(MR_model, taxa = 'especie1', X_var = 'var1')\n\n",
        "# ── Interacciones ──────────────────────────────────────\n",
        "int <- mrInteractions(MR_model, taxa = 'especie1')\n",
        "plot(int)\n\n",
        "# ── SHAP ───────────────────────────────────────────────\n",
        "shap <- mrShapely(MR_model, taxa = 'especie1')\n",
        "plot(shap)\n\n",
        "# ── Red de co-ocurrencia ───────────────────────────────\n",
        "mrCoOccurNet(MR_model)\n"
      )
    })

    output$codigo_r <- renderText({ codigo_generado() })

    output$descargar_script <- downloadHandler(
      filename = function()
        paste0("statcomm_mrIML_", format(Sys.Date(), "%Y%m%d"), ".R"),
      content = function(file)
        writeLines(codigo_generado(), file)
    )

  }) # /moduleServer
}
