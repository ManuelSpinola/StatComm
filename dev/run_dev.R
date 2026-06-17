# ============================================================
# dev/run_dev.R — Correr StatComm en modo desarrollo
# ============================================================

# Detach package if loaded
if ("StatComm" %in% (.packages())) {
  pkgload::unload("StatComm")
}

# Load all
pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)

# Run the application
run_app()
