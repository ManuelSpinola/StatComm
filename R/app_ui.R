#' Application UI
#'
#' @return A Shiny UI object.
#' @import shiny
#' @import bslib
#' @import bsicons
#' @noRd
app_ui <- function() {

  golem::add_resource_path(
    "www",
    system.file("app/www", package = "StatComm")
  )

  tagList(
    tags$style(HTML("
      body { margin-bottom: 36px; }
      .footer-fixed {
        position: fixed; bottom: 0; left: 0; right: 0; z-index: 1000;
        background-color: #1170AA; color: #ffffff;
        text-align: center; font-size: 0.78rem;
        padding: 6px 0; border-top: 1px solid #0d5a8a;
      }
    ")),

    bslib::page_navbar(
      title = div(
        style = "display: flex; align-items: center; gap: 10px; margin-top: 4px;",
        img(src = "www/hexsticker_StatComm.png", height = "38px"),
        span("StatComm", style = "font-weight: 600;")
      ),
      theme = tema_app,
      lang  = "es",

      bslib::nav_panel(
        title = "Datos",
        icon  = bsicons::bs_icon("upload"),
        mod_upload_ui("upload")
      ),

      bslib::nav_panel(
        title = "GLLVM",
        icon  = bsicons::bs_icon("diagram-3"),
        mod_gllvm_ui("gllvm")
      ),

      # Módulos futuros — descomentar cuando estén listos:
      # bslib::nav_panel(
      #   title = "Exploración",
      #   icon  = bsicons::bs_icon("bar-chart-steps"),
      #   mod_explore_ui("explore")
      # ),
      # bslib::nav_panel(
      #   title = "mrIML",
      #   icon  = bsicons::bs_icon("robot"),
      #   mod_mrIML_ui("mrIML")
      # ),
      # bslib::nav_panel(
      #   title = "Beta diversidad",
      #   icon  = bsicons::bs_icon("bezier2"),
      #   mod_beta_ui("beta")
      # ),

      bslib::nav_spacer(),

      bslib::nav_panel(
        title = "Acerca de",
        icon  = bsicons::bs_icon("info-circle"),
        mod_acerca_de_ui("acerca_de")
      ),

      bslib::nav_item(
        tags$span(class = "text-white-50 small", "StatComm v0.1")
      )
    ),

    div(
      class = "footer-fixed",
      "Manuel Sp\u00ednola \u00b7 ICOMVIS \u00b7 Universidad Nacional \u00b7 Costa Rica"
    )
  )
}
