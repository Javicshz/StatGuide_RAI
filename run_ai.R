# STATGUIDE R AI CONSOLE AI HELPERS ##########################################
# Source this file from the RStudio Console to load codex()/askcodex() only.
###############################################################################

statguide_ai_is_project_dir <- function(path) {
  nzchar(path) &&
    file.exists(file.path(path, "app.R")) &&
    file.exists(file.path(path, "R", "ai_providers.R")) &&
    file.exists(file.path(path, "R", "console_ai.R"))
}

statguide_ai_source_dir <- function() {
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

statguide_ai_rstudio_active_dir <- function() {
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

get_ai_folder <- function() {
  candidates <- unique(c(
    statguide_ai_source_dir(),
    statguide_ai_rstudio_active_dir(),
    normalizePath(getwd(), mustWork = FALSE)
  ))
  candidates <- candidates[nzchar(candidates)]
  matches <- candidates[vapply(candidates, statguide_ai_is_project_dir, logical(1))]
  if (length(matches)) {
    return(matches[1])
  }
  stop(
    paste(
      "Could not find the StatGuide_RAI project folder.",
      "Open the project folder in RStudio, set your working directory to the folder that contains app.R,",
      "or source run_ai.R using the full path on your machine."
    ),
    call. = FALSE
  )
}

ai_dir <- get_ai_folder()

source(file.path(ai_dir, "R", "ai_providers.R"), local = .GlobalEnv)
source(file.path(ai_dir, "R", "console_ai.R"), local = .GlobalEnv)

message("StatGuide R AI helpers loaded. Try codex(...), ollama(...), claude(...), openai(...), anthropic(...), openai_compatible(...), or ai_setup_help().")
