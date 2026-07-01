# STATGUIDE R AI CONSOLE HELPERS #############################################
# Source this file from the RStudio Console to load terminal/console helpers.
###############################################################################

statguide_is_project_dir <- function(path) {
  nzchar(path) &&
    file.exists(file.path(path, "app.R")) &&
    file.exists(file.path(path, "R", "statistics_core.R")) &&
    file.exists(file.path(path, "R", "console_menu.R"))
}

statguide_source_dir <- function() {
  frames <- sys.frames()
  ofiles <- vapply(frames, function(frame) {
    value <- frame$ofile
    if (is.null(value)) "" else value
  }, character(1))
  ofiles <- ofiles[nzchar(ofiles)]
  if (length(ofiles)) {
    return(dirname(normalizePath(ofiles[length(ofiles)], mustWork = FALSE)))
  }
  ""
}

statguide_rstudio_active_dir <- function() {
  if (!requireNamespace("rstudioapi", quietly = TRUE) || !rstudioapi::isAvailable()) {
    return("")
  }
  path <- tryCatch(rstudioapi::getActiveDocumentContext()$path, error = function(e) "")
  if (nzchar(path)) {
    dirname(normalizePath(path, mustWork = FALSE))
  } else {
    ""
  }
}

get_console_folder <- function() {
  candidates <- unique(c(
    statguide_source_dir(),
    statguide_rstudio_active_dir(),
    normalizePath(getwd(), mustWork = FALSE)
  ))
  candidates <- candidates[nzchar(candidates)]
  matches <- candidates[vapply(candidates, statguide_is_project_dir, logical(1))]
  if (length(matches)) {
    return(matches[1])
  }
  stop(
    paste(
      "Could not find the StatGuide_RAI project folder.",
      "Open the project folder in RStudio, set your working directory to the folder that contains app.R,",
      "or source run_console.R using the full path on your machine."
    ),
    call. = FALSE
  )
}

console_dir <- get_console_folder()

source(file.path(console_dir, "R", "statistics_core.R"), local = .GlobalEnv)
source(file.path(console_dir, "R", "ai_providers.R"), local = .GlobalEnv)
source(file.path(console_dir, "R", "console_ai.R"), local = .GlobalEnv)
source(file.path(console_dir, "R", "content_data.R"), local = .GlobalEnv)
source(file.path(console_dir, "R", "calculator_reference.R"), local = .GlobalEnv)
source(file.path(console_dir, "R", "console_menu.R"), local = .GlobalEnv)

if (interactive()) {
  statguide_console()
} else {
  message("StatGuide R AI console menu loaded. In RStudio, source('run_console.R') to start the menu.")
}
