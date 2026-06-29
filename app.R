# STATGUIDE R AI #############################################################
# Run install.packages("shiny") once if Shiny is not installed.
# Then open this file in RStudio and click Run App.
###############################################################################

if (!requireNamespace("shiny", quietly = TRUE)) {
  stop("The shiny package is required. Run install.packages('shiny') first.")
}

library(shiny)

get_app_folder <- function() {
  if (!is.null(sys.frames()[[1]]$ofile)) {
    return(dirname(normalizePath(sys.frames()[[1]]$ofile)))
  }
  getwd()
}

app_dir <- get_app_folder()
source(file.path(app_dir, "R", "statistics_core.R"), local = .GlobalEnv)
source(file.path(app_dir, "R", "ai_providers.R"), local = .GlobalEnv)

source(file.path(app_dir, "R", "calculator_helpers.R"), local = environment())
source(file.path(app_dir, "R", "content_data.R"), local = environment())
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

shinyApp(ui, server)
