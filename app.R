# STATGUIDE R AI #############################################################
# Run install.packages("shiny") once if Shiny is not installed.
# Then open this file in RStudio and click Run App.
# Or double-click "Start StatGuide.command" (Mac) / "Start StatGuide.bat" (Windows).
###############################################################################

# Set when started from a terminal or launcher with `Rscript app.R`.
script_file <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
run_by_rscript <- length(script_file) == 1 && basename(script_file) == "app.R"

if (run_by_rscript) {
  # Launcher users may never have opened R: install what the app needs once.
  needed <- c("shiny", "curl", "jsonlite")
  missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    message("First run: installing R packages: ", paste(missing, collapse = ", "))
    user_lib <- Sys.getenv("R_LIBS_USER")
    dir.create(user_lib, recursive = TRUE, showWarnings = FALSE)
    .libPaths(c(user_lib, .libPaths()))
    install.packages(missing, lib = user_lib, repos = "https://cloud.r-project.org")
  }
}

if (!requireNamespace("shiny", quietly = TRUE)) {
  stop("The shiny package is required. Run install.packages('shiny') first.")
}

library(shiny)

get_app_folder <- function() {
  if (run_by_rscript) {
    return(dirname(normalizePath(script_file)))
  }
  if (!is.null(sys.frames()[[1]]$ofile)) {
    return(dirname(normalizePath(sys.frames()[[1]]$ofile)))
  }
  getwd()
}

app_dir <- get_app_folder()
source(file.path(app_dir, "R", "statistics_core.R"), local = .GlobalEnv)
source(file.path(app_dir, "R", "ai_providers.R"), local = .GlobalEnv)
source(file.path(app_dir, "R", "console_ai.R"), local = .GlobalEnv)

source(file.path(app_dir, "R", "calculator_helpers.R"), local = environment())
source(file.path(app_dir, "R", "content_data.R"), local = environment())
source(file.path(app_dir, "R", "formula_tex.R"), local = environment())
source(file.path(app_dir, "R", "formatting.R"), local = environment())
source(file.path(app_dir, "R", "calculator_reference.R"), local = environment())
source(file.path(app_dir, "R", "ui_tabs.R"), local = environment())
source(file.path(app_dir, "R", "server_ai.R"), local = environment())
source(file.path(app_dir, "R", "server_calculators.R"), local = environment())
source(file.path(app_dir, "R", "server_reference.R"), local = environment())

server <- function(input, output, session) {
  register_ai_server(input, output, session)

  register_calculator_server(input, output, session)

  register_reference_server(input, output, session)
}

if (run_by_rscript) {
  # Rscript is non-interactive, so Shiny would not open a browser on its own.
  runApp(shinyApp(ui, server), launch.browser = TRUE)
} else {
  shinyApp(ui, server)
}
