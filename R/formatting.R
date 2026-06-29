# Small formatting and fallback helpers shared by UI/server modules.

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

format_num_for_code <- function(x) {
  if (is.null(x) || is.na(x)) "NA" else format(x, scientific = FALSE, trim = TRUE)
}

code_vector <- function(values) {
  paste0("c(", paste(format(values, scientific = FALSE, trim = TRUE), collapse = ", "), ")")
}

