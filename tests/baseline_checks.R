# Baseline checks for migrated statistics functions.
# Uses only base R so the project has an immediate verification path.

source(file.path("R", "statistics_core.R"))

capture <- function(expr) {
  paste(capture.output(expr), collapse = "\n")
}

desc_output <- capture(descStats(c(2, 4, 6, 8), sample = TRUE))
stopifnot(grepl("Mean\\s+:\\s+5\\.000000", desc_output))
stopifnot(grepl("Variance\\s+:\\s+6\\.666667", desc_output))
stopifnot(grepl("Standard Deviation\\s+:\\s+2\\.581989", desc_output))

z_output <- capture(zScore(85, 75, 5))
stopifnot(grepl("z\\s+:\\s+2\\.000000", z_output))

normal_output <- capture(normalProb(0, 1, -1, 1))
stopifnot(grepl("Probability\\s+:\\s+0\\.682689", normal_output))

cat("Baseline checks passed\n")
