# ============================================================
# golem_utils.R — Utilidades internas de golem para StatComm
# ============================================================

#' @noRd
app_sys <- function(...) {
  system.file(..., package = "StatComm")
}

#' @noRd
app_prod <- function() {
  isTRUE(get_golem_config("production"))
}
