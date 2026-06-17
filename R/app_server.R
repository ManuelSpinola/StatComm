#' Application Server
#'
#' @param input,output,session Internal parameters for Shiny.
#' @noRd
app_server <- function(input, output, session) {

  # Data module — returns reactive list: $Y, $X, $traits, $meta, $source
  app_data <- mod_upload_server("upload")

  mod_gllvm_server("gllvm", data = app_data)
  mod_acerca_de_server("acerca_de")

  # Módulos futuros — reciben app_data como argumento:
  # mod_explore_server("explore", data = app_data)
  # mod_gllvm_server("gllvm",     data = app_data)
  # mod_mrIML_server("mrIML",     data = app_data)
  # mod_beta_server("beta",       data = app_data)

  session$onSessionEnded(function() {})
}
