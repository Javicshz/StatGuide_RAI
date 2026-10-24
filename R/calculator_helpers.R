# Parsing, validation, and extra calculator helpers used by the Shiny app.
# These functions intentionally preserve the migrated calculator behavior.

read_app_reference <- function(filename) {
  path <- file.path(app_dir, filename)
  if (!file.exists(path)) {
    return(paste("Reference file not found:", filename))
  }
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

parse_nums <- function(text, label = "values") {
  text <- trimws(as.character(text %||% ""))
  if (!nzchar(text)) {
    return(numeric(0))
  }
  parts <- trimws(unlist(strsplit(gsub("[\n\r\t;]+", ",", text), ",")))
  parts <- parts[nzchar(parts)]
  if (!length(parts)) {
    return(numeric(0))
  }

  values <- suppressWarnings(as.numeric(parts))
  bad <- is.na(values) & !tolower(parts) %in% c("na", "nan")
  if (any(bad)) {
    stop(paste0(
      "Could not parse ", label, " as numbers. Check: ",
      paste(parts[bad], collapse = ", ")
    ))
  }
  values
}

parse_labels <- function(text) {
  text <- trimws(text)
  if (!nzchar(text)) {
    return(NULL)
  }
  trimws(strsplit(gsub("\\n", ",", text), ",")[[1]])
}

parse_optional_num <- function(text) {
  text <- trimws(text)
  if (!nzchar(text)) {
    return(NULL)
  }
  value <- suppressWarnings(as.numeric(text))
  if (is.na(value)) {
    stop(paste0("Expected one number, but got: ", text))
  }
  value
}

parse_data_numbers <- function(text) {
  number_pattern <- "[-+]?[0-9]*\\.?[0-9]+"
  matches <- gregexpr(number_pattern, gsub("\u2212", "-", text), perl = TRUE)[[1]]
  if (matches[1] < 0) {
    return(numeric(0))
  }
  as.numeric(regmatches(gsub("\u2212", "-", text), list(matches))[[1]])
}

capture_calculator_result <- function(expr) {
  tryCatch(
    {
      value <- NULL
      output <- paste(capture.output({
        value <- force(expr)
      }), collapse = "\n")
      if (inherits(value, "calculator_result")) {
        if (nzchar(trimws(output))) {
          value$output <- output
        }
        return(value)
      }
      calculator_result(TRUE, output = output, status = "ok")
    },
    error = function(e) {
      calculator_result(FALSE, message = conditionMessage(e), status = "input_error")
    }
  )
}

format_calculator_result <- function(result) {
  if (isTRUE(result$ok)) {
    output <- trimws(result$output %||% "")
    if (nzchar(output)) {
      return(output)
    }
    return("No output was produced. Check the selected calculator inputs.")
  }
  paste("Input error:", result$message)
}

alpha_value <- function(text, default = 0.05) {
  value <- parse_optional_num(as.character(text))
  if (is.null(value) || is.na(value)) {
    return(default)
  }
  value
}

probability_bounds <- function(type, lower_text, upper_text, exact_text,
                               min_default = -Inf, max_default = Inf) {
  lower <- parse_optional_num(as.character(lower_text))
  upper <- parse_optional_num(as.character(upper_text))
  exact <- parse_optional_num(as.character(exact_text))

  if (type == "between") {
    lower <- require_num(lower, "Lower bound")
    upper <- require_num(upper, "Upper bound")
    if (lower > upper) {
      stop("Lower bound must be less than or equal to upper bound.")
    }
    return(list(
      lower = lower,
      upper = upper
    ))
  }
  if (type == "less") {
    return(list(
      lower = min_default,
      upper = require_num(upper, "Upper bound")
    ))
  }
  if (type == "greater") {
    return(list(
      lower = require_num(lower, "Lower bound"),
      upper = max_default
    ))
  }
  if (type == "exact") {
    exact <- require_num(exact, "Exact value")
    return(list(lower = exact, upper = exact))
  }
  list(lower = min_default, upper = max_default)
}

probability_bounds_ui <- function() {
  tagList(
    selectInput(
      "prob_type",
      "Question wording",
      choices = c(
        "Between two values" = "between",
        "No more than / at most / less than a value" = "less",
        "At least / more than / greater than a value" = "greater",
        "Exactly equal to one value" = "exact"
      )
    ),
    textInput("lower", "Lower bound (use for between or at least)", ""),
    textInput("upper", "Upper bound (use for between or no more than)", ""),
    textInput("exact_value", "Exact value (only for exactly equal)", ""),
    helpText("Leave unused bound boxes blank. For one-sided questions, fill only the bound mentioned in the wording.")
  )
}

print_probability_result <- function(title, probability, fraction_text = NULL) {
  cat("--------------------------------------------------\n")
  cat(title, "\n", sep = "")
  cat("--------------------------------------------------\n")
  cat(sprintf("%-25s: %10.6f\n", "Probability", probability))
  cat(sprintf("%-25s: %9.6f%%\n", "Probability (%)", probability * 100))
  if (!is.null(fraction_text)) {
    cat(sprintf("%-25s: %s\n", "Fraction", fraction_text))
  }
  cat("--------------------------------------------------\n")
}

countingRuleCalc <- function(slot_options) {
  slot_options <- require_numeric_vector(slot_options, "Slot options")
  if (any(slot_options < 0) || any(slot_options != floor(slot_options))) {
    stop("Slot options must be nonnegative whole-number counts.")
  }
  total <- prod(slot_options)
  cat("--------------------------------------------------\n")
  cat("Counting Rule\n")
  cat("--------------------------------------------------\n")
  cat(sprintf("%-25s: %s\n", "Slot Options", paste(slot_options, collapse = " x ")))
  cat(sprintf("%-25s: %.0f\n", "Total Outcomes", total))
  cat("--------------------------------------------------\n")
  invisible(calculator_result(
    TRUE,
    values = list(slot_options = slot_options, total_outcomes = total),
    metadata = list(calculator = "countingRule")
  ))
}

factorialRatioCalc <- function(n, denominator_parts = NULL, mode = "parts", r = NULL) {
  if (mode == "combination") {
    value <- choose(n, r)
    expression_text <- paste0(n, "C", r, " = ", n, "! / (", r, "! * ", n - r, "!)")
  } else {
    denominator_parts <- denominator_parts[!is.na(denominator_parts)]
    value <- factorial(n) / prod(factorial(denominator_parts))
    expression_text <- paste0(n, "! / (", paste(paste0(denominator_parts, "!"), collapse = " * "), ")")
  }
  cat("--------------------------------------------------\n")
  cat("Factorial / Arrangement Count\n")
  cat("--------------------------------------------------\n")
  cat(sprintf("%-25s: %s\n", "Expression", expression_text))
  cat(sprintf("%-25s: %.0f\n", "Value", value))
  cat("--------------------------------------------------\n")
}

sequenceProbabilityCalc <- function(favorable_counts, total_counts) {
  favorable_counts <- require_numeric_vector(favorable_counts, "Favorable counts")
  total_counts <- require_numeric_vector(total_counts, "Total counts")
  if (length(favorable_counts) != length(total_counts)) {
    stop("Favorable counts and total counts must have the same number of entries.")
  }
  if (any(total_counts <= 0)) {
    stop("Total counts must be positive.")
  }
  if (any(favorable_counts < 0) || any(favorable_counts > total_counts)) {
    stop("Favorable counts must be between 0 and the matching total count.")
  }
  step_probs <- favorable_counts / total_counts
  probability <- prod(step_probs)
  cat("--------------------------------------------------\n")
  cat("Sequential Probability\n")
  cat("--------------------------------------------------\n")
  for (i in seq_along(step_probs)) {
    cat(sprintf("Step %-19s: %10.6f / %-10.6f = %10.6f\n",
                i, favorable_counts[i], total_counts[i], step_probs[i]))
  }
  print_probability_result("Final Probability", probability,
                           paste(paste0(favorable_counts, "/", total_counts), collapse = " * "))
}

cardSequenceCalc <- function(favorable_counts, draws = NULL, replacement = "without") {
  favorable_counts <- favorable_counts[!is.na(favorable_counts)]
  if (!length(favorable_counts)) {
    stop("Enter at least one favorable card count.")
  }
  if (is.null(draws) || is.na(draws)) {
    draws <- length(favorable_counts)
  }
  if (draws != length(favorable_counts)) {
    stop("Number of draws must match the number of favorable counts entered.")
  }

  total_counts <- if (replacement == "with") {
    rep(52, draws)
  } else {
    52 - seq(0, draws - 1)
  }

  cat("--------------------------------------------------\n")
  cat("Playing Card Probability\n")
  cat("--------------------------------------------------\n")
  cat("Use this for drawing cards from a standard 52-card deck.\n")
  cat(sprintf("%-25s: %s replacement\n", "Sampling", replacement))
  sequenceProbabilityCalc(favorable_counts, total_counts)
}

independentProbabilityCalc <- function(probabilities) {
  probabilities <- require_probability_vector(probabilities, "Event probabilities")
  probability <- prod(probabilities)
  cat("--------------------------------------------------\n")
  cat("Independent Event Product\n")
  cat("--------------------------------------------------\n")
  cat(sprintf("%-25s: %s\n", "Probabilities", paste(probabilities, collapse = " x ")))
  print_probability_result("Final Probability", probability)
}

tableProbabilityCalc <- function(table_data, row_name = "", col_name = "", mode = "row") {
  table_data <- as.matrix(table_data)
  grand_total <- sum(table_data)
  row_name <- trimws(row_name)
  col_name <- trimws(col_name)

  if (mode == "row") {
    if (!nzchar(row_name)) stop("Enter the row label for row probability.")
    count <- sum(table_data[row_name, ])
    label <- paste("Row:", row_name)
  } else if (mode == "column") {
    if (!nzchar(col_name)) stop("Enter the column label for column probability.")
    count <- sum(table_data[, col_name])
    label <- paste("Column:", col_name)
  } else if (mode == "joint") {
    if (!nzchar(row_name) || !nzchar(col_name)) stop("Enter both row and column labels for joint probability.")
    count <- table_data[row_name, col_name]
    label <- paste(row_name, "and", col_name)
  } else if (mode == "given_row") {
    if (!nzchar(row_name) || !nzchar(col_name)) stop("Enter both row and column labels for conditional probability.")
    count <- table_data[row_name, col_name]
    grand_total <- sum(table_data[row_name, ])
    label <- paste(col_name, "given", row_name)
  } else if (mode == "given_column") {
    if (!nzchar(row_name) || !nzchar(col_name)) stop("Enter both row and column labels for conditional probability.")
    count <- table_data[row_name, col_name]
    grand_total <- sum(table_data[, col_name])
    label <- paste(row_name, "given", col_name)
  } else if (mode == "row_or_column") {
    if (!nzchar(row_name) || !nzchar(col_name)) stop("Enter both row and column labels for OR probability.")
    count <- sum(table_data[row_name, ]) + sum(table_data[, col_name]) - table_data[row_name, col_name]
    label <- paste(row_name, "or", col_name)
  } else {
    stop("Unknown table probability mode.")
  }

  cat("--------------------------------------------------\n")
  cat("Table Probability\n")
  cat("--------------------------------------------------\n")
  print(table_data)
  cat("--------------------------------------------------\n")
  cat(sprintf("%-25s: %s\n", "Selected", label))
  cat(sprintf("%-25s: %10.6f\n", "Selected Count", count))
  cat(sprintf("%-25s: %10.6f\n", "Denominator", grand_total))
  print_probability_result("Final Probability", count / grand_total, paste(count, "/", grand_total))
}

additionRuleCalc <- function(a_count, b_count, overlap_count, total_count) {
  favorable <- a_count + b_count - overlap_count
  probability <- favorable / total_count
  cat("--------------------------------------------------\n")
  cat("Addition Rule Probability\n")
  cat("--------------------------------------------------\n")
  cat(sprintf("%-25s: %10.6f\n", "A Count", a_count))
  cat(sprintf("%-25s: %10.6f\n", "B Count", b_count))
  cat(sprintf("%-25s: %10.6f\n", "Overlap Count", overlap_count))
  cat(sprintf("%-25s: %10.6f\n", "Favorable Count", favorable))
  cat(sprintf("%-25s: %10.6f\n", "Total", total_count))
  print_probability_result("Final Probability", probability, paste(favorable, "/", total_count))
}

diceSumCalc <- function(type, value) {
  sums <- outer(1:6, 1:6, "+")
  favorable <- switch(type,
    equal = sum(sums == value),
    less = sum(sums < value),
    less_equal = sum(sums <= value),
    greater = sum(sums > value),
    greater_equal = sum(sums >= value),
    stop("Unknown dice question type.")
  )
  cat("--------------------------------------------------\n")
  cat("Two Dice Sum Probability\n")
  cat("--------------------------------------------------\n")
  cat(sprintf("%-25s: %10.6f\n", "Favorable Outcomes", favorable))
  cat(sprintf("%-25s: %10.6f\n", "Total Outcomes", 36))
  print_probability_result("Final Probability", favorable / 36, paste(favorable, "/36"))
}

simpleCountTableProbabilityCalc <- function(count_table, target_category) {
  target_category <- trimws(target_category)
  if (!nzchar(target_category)) {
    stop("Enter the target category from the pasted table.")
  }
  if (is.null(colnames(count_table)) || !(target_category %in% colnames(count_table))) {
    stop("Target category was not found in the pasted table. Check spelling/capitalization.")
  }
  favorable <- count_table[1, target_category]
  total <- sum(count_table)
  cat("--------------------------------------------------\n")
  cat("Simple Probability From Pasted Table\n")
  cat("--------------------------------------------------\n")
  print(count_table)
  cat("--------------------------------------------------\n")
  cat(sprintf("%-25s: %s\n", "Target Category", target_category))
  cat(sprintf("%-25s: %10.6f\n", "Favorable Count", favorable))
  cat(sprintf("%-25s: %10.6f\n", "Total Count", total))
  print_probability_result("Final Probability", favorable / total, paste(favorable, "/", total))
}

parse_probability_distribution_table <- function(text) {
  text <- trimws(gsub("\r", "\n", text))
  if (!nzchar(text)) {
    return(NULL)
  }

  lines <- trimws(unlist(strsplit(text, "\n+")))
  lines <- lines[nzchar(lines)]
  number_pattern <- "-?\\d+(?:\\.\\d+)?(?:/\\d+(?:\\.\\d+)?)?%?"
  numeric_lines <- lines[grepl("\\d", lines)]
  if (length(numeric_lines) < 2) {
    return(NULL)
  }

  parse_token <- function(token) {
    token <- gsub("%", "", token)
    if (grepl("/", token, fixed = TRUE)) {
      parts <- as.numeric(strsplit(token, "/", fixed = TRUE)[[1]])
      return(parts[1] / parts[2])
    }
    as.numeric(token)
  }

  extract_numbers <- function(line) {
    matches <- gregexpr(number_pattern, line, perl = TRUE)[[1]]
    if (matches[1] < 0) {
      return(numeric(0))
    }
    tokens <- regmatches(line, list(matches))[[1]]
    as.numeric(vapply(tokens, parse_token, numeric(1)))
  }

  first_values <- extract_numbers(numeric_lines[1])
  second_values <- extract_numbers(numeric_lines[2])
  if (!length(first_values) || !length(second_values) || length(first_values) != length(second_values)) {
    return(NULL)
  }

  list(x = first_values, prob = second_values)
}

parse_single_row_count_table <- function(text) {
  text <- trimws(gsub("\r", "\n", text))
  if (!nzchar(text)) {
    return(NULL)
  }

  lines <- trimws(unlist(strsplit(text, "\n+")))
  lines <- lines[nzchar(lines)]
  if (length(lines) < 2) {
    return(NULL)
  }

  number_pattern <- "[-+]?[0-9]*\\.?[0-9]+"
  numeric_index <- which(grepl(number_pattern, lines))[1]
  if (is.na(numeric_index) || numeric_index <= 1) {
    return(NULL)
  }

  header_parts <- split_table_header(lines[numeric_index - 1])
  matches <- gregexpr(number_pattern, lines[numeric_index], perl = TRUE)[[1]]
  counts <- as.numeric(regmatches(lines[numeric_index], list(matches))[[1]])
  if (length(header_parts) != length(counts)) {
    return(NULL)
  }

  count_matrix <- matrix(counts, nrow = 1)
  colnames(count_matrix) <- header_parts
  rownames(count_matrix) <- "Count"
  count_matrix
}

bayesProbabilityCalc <- function(priors, event_probs, target_index) {
  if (length(priors) != length(event_probs)) {
    stop("Prior probabilities and event probabilities must have the same number of entries.")
  }
  weights <- priors * event_probs
  denominator <- sum(weights)
  probability <- weights[target_index] / denominator
  cat("--------------------------------------------------\n")
  cat("Bayes / Reverse Conditional Probability\n")
  cat("--------------------------------------------------\n")
  cat(sprintf("%-25s: %s\n", "Prior probabilities", paste(priors, collapse = ", ")))
  cat(sprintf("%-25s: %s\n", "Event probabilities", paste(event_probs, collapse = ", ")))
  cat(sprintf("%-25s: %10.6f\n", "Denominator", denominator))
  cat(sprintf("%-25s: %10.6f\n", "Target numerator", weights[target_index]))
  print_probability_result("Final Probability", probability)
}

selectedPercentCalc <- function(frequencies, categories = NULL, selected = NULL) {
  total <- sum(frequencies)
  if (is.null(selected) || !length(selected)) {
    selected <- seq_along(frequencies)
  }
  if (any(selected < 1 | selected > length(frequencies))) {
    stop("Selected positions must match the row/category numbers shown in the table.")
  }
  selected_sum <- sum(frequencies[selected])
  percent <- selected_sum / total * 100
  cat("--------------------------------------------------\n")
  cat("Selected Frequency Percent\n")
  cat("--------------------------------------------------\n")
  if (!is.null(categories) && length(categories) == length(frequencies)) {
    print(data.frame(Category = categories, Frequency = frequencies))
  } else {
    print(data.frame(Category = paste("Category", seq_along(frequencies)), Frequency = frequencies))
  }
  cat("--------------------------------------------------\n")
  cat(sprintf("%-25s: %s\n", "Selected Positions", paste(selected, collapse = ", ")))
  cat(sprintf("%-25s: %10.6f\n", "Selected Frequency", selected_sum))
  cat(sprintf("%-25s: %10.6f\n", "Total", total))
  cat(sprintf("%-25s: %10.6f%%\n", "Percent", percent))
  cat("--------------------------------------------------\n")
}

percentAmountCalc <- function(total, percent) {
  amount <- total * percent / 100
  cat("--------------------------------------------------\n")
  cat("Percent to Amount\n")
  cat("--------------------------------------------------\n")
  cat(sprintf("%-25s: %10.6f\n", "Total", total))
  cat(sprintf("%-25s: %10.6f%%\n", "Percent", percent))
  cat(sprintf("%-25s: %10.6f\n", "Amount", amount))
  cat("--------------------------------------------------\n")
}

graphRatioCalc <- function(numerator_value, denominator_value) {
  ratio <- numerator_value / denominator_value
  cat("--------------------------------------------------\n")
  cat("Graph / Bar Ratio\n")
  cat("--------------------------------------------------\n")
  cat(sprintf("%-25s: %10.6f\n", "Numerator Value", numerator_value))
  cat(sprintf("%-25s: %10.6f\n", "Denominator Value", denominator_value))
  cat(sprintf("%-25s: %10.6f\n", "Times As Large", ratio))
  cat("--------------------------------------------------\n")
}

modeClassification <- function(data) {
  counts <- table(data)
  max_count <- max(counts)
  modes <- as.numeric(names(counts[counts == max_count]))
  if (max_count == 1) {
    list(label = "No Mode", modes = numeric(0))
  } else if (length(modes) == 1) {
    list(label = "Unimodal", modes = modes)
  } else if (length(modes) == 2) {
    list(label = "Bimodal", modes = sort(modes))
  } else {
    list(label = "Multimodal", modes = sort(modes))
  }
}

modeClassificationCalc <- function(data) {
  result <- modeClassification(data)
  cat("--------------------------------------------------\n")
  cat("Mode Classification\n")
  cat("--------------------------------------------------\n")
  cat(sprintf("%-25s: %s\n", "Decision", result$label))
  if (length(result$modes)) {
    cat(sprintf("%-25s: %s\n", "Modes", paste(result$modes, collapse = ", ")))
  }
  cat("--------------------------------------------------\n")
}

fiveNumberSummaryCalc <- function(data) {
  data <- sort(data)
  values <- as.numeric(quantile(data, probs = c(0, 0.25, 0.5, 0.75, 1), type = 2))
  names(values) <- c("Minimum", "Q1", "Median", "Q3", "Maximum")
  cat("--------------------------------------------------\n")
  cat("Five-Number Summary / Box Plot\n")
  cat("--------------------------------------------------\n")
  for (nm in names(values)) {
    cat(sprintf("%-25s: %10.6f\n", nm, values[[nm]]))
  }
  cat(sprintf("%-25s: %s\n", "Answer Order", paste(values, collapse = ", ")))
  cat("--------------------------------------------------\n")
}

groupedStatsCalc <- function(midpoints, frequencies, sample = FALSE) {
  if (length(midpoints) != length(frequencies)) {
    stop("Midpoints and frequencies must have the same number of entries.")
  }
  n <- sum(frequencies)
  mean_val <- sum(midpoints * frequencies) / n
  divisor <- if (sample) n - 1 else n
  variance <- sum(frequencies * (midpoints - mean_val)^2) / divisor
  cat("--------------------------------------------------\n")
  cat("Grouped Frequency Statistics\n")
  cat("--------------------------------------------------\n")
  print(data.frame(Midpoint = midpoints, Frequency = frequencies))
  cat("--------------------------------------------------\n")
  cat(sprintf("%-25s: %10.6f\n", "n", n))
  cat(sprintf("%-25s: %10.6f\n", "Mean", mean_val))
  cat(sprintf("%-25s: %10.6f\n", ifelse(sample, "Sample Variance", "Population Variance"), variance))
  cat(sprintf("%-25s: %10.6f\n", ifelse(sample, "Sample SD", "Population SD"), sqrt(variance)))
  cat("--------------------------------------------------\n")
}

empiricalRuleCalc <- function(mean_val, sd_val, lower, upper) {
  k_lower <- abs(lower - mean_val) / sd_val
  k_upper <- abs(upper - mean_val) / sd_val
  k <- min(k_lower, k_upper)
  percent <- if (abs(k - 1) < 1e-6) 68 else if (abs(k - 2) < 1e-6) 95 else if (abs(k - 3) < 1e-6) 99.7 else NA
  cat("--------------------------------------------------\n")
  cat("Empirical Rule\n")
  cat("--------------------------------------------------\n")
  cat(sprintf("%-25s: %10.6f\n", "k Standard Deviations", k))
  if (is.na(percent)) {
    cat("The interval is not exactly 1, 2, or 3 standard deviations from the mean.\n")
  } else {
    cat(sprintf("%-25s: %10.6f%%\n", "Approx Percent", percent))
  }
  cat("--------------------------------------------------\n")
}

trimmedMeanCalc <- function(data, trim_percent) {
  trim_fraction <- trim_percent / 100
  value <- mean(data, trim = trim_fraction)
  removed_each_tail <- floor(length(data) * trim_fraction)
  cat("--------------------------------------------------\n")
  cat("Trimmed Mean\n")
  cat("--------------------------------------------------\n")
  cat(sprintf("%-25s: %10.6f%%\n", "Trim Percent", trim_percent))
  cat(sprintf("%-25s: %10.0f\n", "Removed Each Tail", removed_each_tail))
  cat(sprintf("%-25s: %10.6f\n", "Trimmed Mean", value))
  cat("--------------------------------------------------\n")
}

weightedAverageCalc <- function(values, weights) {
  if (length(values) != length(weights)) {
    stop("Values and weights must have the same number of entries.")
  }
  weighted_mean <- sum(values * weights) / sum(weights)
  cat("--------------------------------------------------\n")
  cat("Weighted Average\n")
  cat("--------------------------------------------------\n")
  print(data.frame(Value = values, Weight = weights))
  cat("--------------------------------------------------\n")
  cat(sprintf("%-25s: %10.6f\n", "Weighted Average", weighted_mean))
  cat("--------------------------------------------------\n")
}

missingFromMeanCalc <- function(known_values, total_n, target_mean) {
  missing_value <- target_mean * total_n - sum(known_values)
  cat("--------------------------------------------------\n")
  cat("Missing Value From Known Mean\n")
  cat("--------------------------------------------------\n")
  cat(sprintf("%-25s: %10.6f\n", "Target Mean", target_mean))
  cat(sprintf("%-25s: %10.0f\n", "Total n", total_n))
  cat(sprintf("%-25s: %10.6f\n", "Known Sum", sum(known_values)))
  cat(sprintf("%-25s: %10.6f\n", "Missing Value", missing_value))
  cat("--------------------------------------------------\n")
}

chebyshevCalc <- function(mean_val, sd_val, mode, k = NULL, percent = NULL, lower = NULL, upper = NULL) {
  if (mode == "range_from_k") {
    k <- require_num(k, "k")
    chebyshevRange(mean_val, sd_val, k)
    return(invisible(NULL))
  }
  if (mode == "range_from_percent") {
    percent <- require_num(percent, "Percent")
    k <- sqrt(1 / (1 - percent / 100))
    chebyshevRange(mean_val, sd_val, k)
    return(invisible(NULL))
  }
  lower <- require_num(lower, "Lower value")
  upper <- require_num(upper, "Upper value")
  k <- min(abs(lower - mean_val), abs(upper - mean_val)) / sd_val
  percent <- (1 - 1 / k^2) * 100
  cat("--------------------------------------------------\n")
  cat("Chebyshev Minimum Percent\n")
  cat("--------------------------------------------------\n")
  cat(sprintf("%-25s: %10.6f\n", "k", k))
  cat(sprintf("%-25s: %10.6f%%\n", "At Least", percent))
  cat("--------------------------------------------------\n")
}

pointEstimateCalc <- function(type, x = NULL, n = NULL, xbar = NULL, s = NULL) {
  values <- list(type = type)
  cat("--------------------------------------------------\n")
  cat("Point Estimate\n")
  cat("--------------------------------------------------\n")
  if (type == "proportion") {
    x <- require_whole_num(x, "Successes x", 0)
    n <- require_whole_num(n, "n", 1)
    if (x > n) {
      stop("Successes x cannot be larger than n.")
    }
    phat <- x / n
    cat(sprintf("%-25s: %10.6f\n", "phat", phat))
    cat(sprintf("%-25s: %9.6f%%\n", "phat (%)", phat * 100))
    values <- c(values, list(successes = x, n = n, estimate = phat, percent = phat * 100))
  } else if (type == "mean") {
    xbar <- require_num(xbar, "xbar")
    cat(sprintf("%-25s: %10.6f\n", "xbar", xbar))
    values <- c(values, list(estimate = xbar))
  } else {
    s <- require_nonnegative_num(s, "Sample standard deviation")
    variance <- s^2
    cat(sprintf("%-25s: %10.6f\n", "Sample Variance", variance))
    values <- c(values, list(sample_sd = s, estimate = variance))
  }
  cat("--------------------------------------------------\n")
  invisible(calculator_result(
    TRUE,
    values = values,
    metadata = list(calculator = "pointEstimate")
  ))
}

sampleSizeCalc <- function(type, conf_level, margin_error, sigma = NULL, p_hat = NULL) {
  conf_level <- require_conf_level(conf_level)
  margin_error <- require_positive_num(margin_error, "Margin of error")
  z <- qnorm(1 - (1 - conf_level) / 2)
  if (type == "proportion") {
    p_hat <- if (is.null(p_hat) || is.na(p_hat)) 0.5 else p_hat
    p_hat <- require_probability_num(p_hat, "Estimated proportion")
    n <- ceiling(p_hat * (1 - p_hat) * (z / margin_error)^2)
  } else {
    sigma <- require_positive_num(sigma, "sigma")
    n <- ceiling((z * sigma / margin_error)^2)
  }
  cat("--------------------------------------------------\n")
  cat("Minimum Sample Size\n")
  cat("--------------------------------------------------\n")
  cat(sprintf("%-25s: %10.6f\n", "z Critical", z))
  cat(sprintf("%-25s: %10.6f\n", "Margin of Error", margin_error))
  cat(sprintf("%-25s: %10.0f\n", "Round Up n", n))
  cat("--------------------------------------------------\n")
  invisible(calculator_result(
    TRUE,
    values = list(
      type = type,
      confidence_level = conf_level,
      margin_error = margin_error,
      z_critical = z,
      sigma = if (type == "mean") sigma else NULL,
      p_hat = if (type == "proportion") p_hat else NULL,
      sample_size = n
    ),
    metadata = list(calculator = "sampleSize")
  ))
}

meanCIFromDataCalc <- function(data, conf_level = 0.95, sigma = NULL, method = "auto") {
  conf_level <- require_conf_level(conf_level)
  data <- data[!is.na(data)]
  if (length(data) < 2) {
    stop("Enter at least two raw data values for a mean confidence interval.")
  }
  n <- length(data)
  xbar <- mean(data)
  s <- sd(data)
  use_known_sigma <- method == "known" || (method == "auto" && !is.null(sigma) && !is.na(sigma))

  cat("--------------------------------------------------\n")
  cat("Guided Mean Confidence Interval\n")
  cat("--------------------------------------------------\n")
  cat(sprintf("%-25s: %10.0f\n", "n", n))
  cat(sprintf("%-25s: %10.6f\n", "xbar", xbar))
  cat(sprintf("%-25s: %10.6f\n", "Sample s", s))

  alpha <- 1 - conf_level
  if (use_known_sigma) {
    sigma <- require_positive_num(sigma, "Known sigma")
    crit <- qnorm(1 - alpha / 2)
    se <- sigma / sqrt(n)
    title <- "Mean CI (Known Sigma / z)"
    cat(sprintf("%-25s: %10.6f\n", "Known sigma", sigma))
    cat(sprintf("%-25s: %10.6f\n", "z Critical", crit))
  } else {
    crit <- qt(1 - alpha / 2, n - 1)
    se <- s / sqrt(n)
    title <- "Mean CI (Unknown Sigma / t)"
    cat(sprintf("%-25s: %10.0f\n", "Degrees of Freedom", n - 1))
    cat(sprintf("%-25s: %10.6f\n", "t Critical", crit))
  }

  me <- crit * se
  cat(sprintf("%-25s: %s\n", "Method", title))
  cat(sprintf("%-25s: %10.6f\n", "Standard Error", se))
  cat(sprintf("%-25s: %10.6f\n", "Margin of Error", me))
  cat(sprintf("%-25s: %10.6f\n", "Lower Endpoint", xbar - me))
  cat(sprintf("%-25s: %10.6f\n", "Upper Endpoint", xbar + me))
  cat("--------------------------------------------------\n")
  invisible(calculator_result(
    TRUE,
    values = list(
      n = n,
      mean = xbar,
      sample_sd = s,
      confidence_level = conf_level,
      alpha = alpha,
      method = title,
      known_sigma = if (use_known_sigma) sigma else NULL,
      critical_value = crit,
      standard_error = se,
      margin_error = me,
      lower_endpoint = xbar - me,
      upper_endpoint = xbar + me
    ),
    metadata = list(calculator = "meanCIFromData")
  ))
}

feelingLuckyCalc <- function(raw_data = numeric(0), x = NULL, n = NULL, xbar = NULL,
                             s = NULL, sigma = NULL, conf_level = NULL,
                             margin_error = NULL, p_hat = NULL,
                             analysis_type = "auto", sample_type = "sample",
                             sigma_mode = "unknown", favorable = NULL, total = NULL,
                             slot_options = numeric(0), event_probs = numeric(0),
                             dist_x = numeric(0), dist_prob = numeric(0),
                             prob_type = "less_equal", prob_value = NULL, upper_value = NULL,
                             n_trials = NULL, p_success = NULL,
                             poisson_rate = NULL, poisson_periods = NULL,
                             total_items = NULL, success_items = NULL, sample_size = NULL,
                             normal_mean = NULL, normal_sd = NULL,
                             lower = NULL, upper = NULL) {
  raw_data <- raw_data[!is.na(raw_data)]
  slot_options <- slot_options[!is.na(slot_options)]
  event_probs <- event_probs[!is.na(event_probs)]
  dist_x <- dist_x[!is.na(dist_x)]
  dist_prob <- dist_prob[!is.na(dist_prob)]
  conf_level <- if (is.null(conf_level) || is.na(conf_level)) NULL else conf_level
  has_num <- function(value) !is.null(value) && length(value) && !is.na(value)

  cat("--------------------------------------------------\n")
  cat("Auto Analysis Helper\n")
  cat("--------------------------------------------------\n")
  cat("This helper shows every matching result it can from the boxes you filled.\n")
  cat("Blank or NA boxes are ignored.\n")
  cat("--------------------------------------------------\n")

  if (length(raw_data)) {
    n_data <- length(raw_data)
    xbar_data <- mean(raw_data)
    s_data <- if (n_data > 1) sd(raw_data) else NA
    pop_sd <- sqrt(mean((raw_data - xbar_data)^2))
    cat(sprintf("%-25s: %10.0f\n", "Raw data n", n_data))
    cat(sprintf("%-25s: %10.6f\n", "Raw data mean", xbar_data))
    cat(sprintf("%-25s: %10.6f\n", "Sample s", s_data))
    cat(sprintf("%-25s: %10.6f\n", "Population SD", pop_sd))
    cat("--------------------------------------------------\n")

    if (analysis_type %in% c("auto", "descriptive", "raw_all")) {
      descStats(raw_data, sample_type == "sample")
    }
    if (analysis_type %in% c("auto", "raw_all", "mean_ci") && !is.null(conf_level)) {
      sigma_for_ci <- if (sigma_mode == "known") sigma else NULL
      meanCIFromDataCalc(raw_data, conf_level, sigma_for_ci, ifelse(sigma_mode == "known", "known", "unknown"))
    }
  }

  if (analysis_type %in% c("auto", "point_prop", "prop_all") && has_num(x) && has_num(n)) {
    pointEstimateCalc("proportion", x = x, n = n)
  }
  if (analysis_type %in% c("auto", "prop_ci", "prop_all") && has_num(x) && has_num(n) && !is.null(conf_level)) {
    onePropCI(x, n, conf_level)
  }
  if (analysis_type %in% c("auto", "mean_ci") && !length(raw_data) && !is.null(conf_level)) {
    if (sigma_mode == "known") {
      meanKnownSigmaCI(require_num(n, "n"), require_num(xbar, "xbar"), require_num(sigma, "sigma"), conf_level)
    } else {
      meanTCI(require_num(n, "n"), require_num(xbar, "xbar"), require_num(s, "s"), conf_level)
    }
  }
  if (analysis_type %in% c("sample_size_mean", "sample_size_prop")) {
    sampleSizeCalc(ifelse(analysis_type == "sample_size_mean", "mean", "proportion"), conf_level, margin_error, sigma, p_hat)
  }
  if (analysis_type %in% c("auto", "simple_probability") && has_num(favorable) && has_num(total)) {
    simpleProb(favorable, total)
  }
  if (analysis_type %in% c("auto", "counting") && length(slot_options)) {
    countingRuleCalc(slot_options)
  }
  if (analysis_type %in% c("auto", "independent_probability") && length(event_probs)) {
    independentProbabilityCalc(event_probs)
  }
  if (analysis_type %in% c("auto", "discrete_distribution") && length(dist_x) && length(dist_prob) && length(dist_x) == length(dist_prob)) {
    print_distribution_summary(dist_x, dist_prob)
    if (has_num(prob_value) && analysis_type != "auto") {
      probDistQueryCalc(dist_x, dist_prob, prob_type, prob_value, upper_value)
    }
  }
  if (analysis_type %in% c("auto", "binomial") && has_num(n_trials) && has_num(p_success) && has_num(prob_value)) {
    binomialCalc(n_trials, p_success, prob_type, prob_value, upper_value)
  }
  if (analysis_type %in% c("auto", "poisson") && has_num(poisson_rate) && has_num(poisson_periods) && has_num(prob_value)) {
    poissonCalc(poisson_rate, poisson_periods, prob_type, prob_value, upper_value)
  }
  if (analysis_type %in% c("auto", "hypergeometric") && has_num(total_items) && has_num(success_items) && has_num(sample_size)) {
    hypergeometricCalc(total_items, success_items, sample_size, ifelse(has_num(prob_value), prob_type, "expected"), prob_value, upper_value)
  }
  if (analysis_type %in% c("auto", "normal_probability") && has_num(normal_mean) && has_num(normal_sd) && (has_num(lower) || has_num(upper))) {
    normalProb(normal_mean, normal_sd, ifelse(has_num(lower), lower, -Inf), ifelse(has_num(upper), upper, Inf))
  }
}

normalApproxBinomialCalc <- function(n, p, type, value, upper_value = NULL) {
  n <- require_whole_num(n, "Trials n", 1)
  p <- require_probability_num(p, "Success probability p")
  value <- require_whole_num(value, "Value", 0)
  mean_val <- n * p
  sd_val <- sqrt(n * p * (1 - p))
  if (sd_val <= 0) {
    stop("Normal approximation requires p greater than 0 and less than 1.")
  }
  upper_value <- if (is.null(upper_value) || is.na(upper_value)) NULL else upper_value

  bounds <- switch(type,
    equal = c(value - 0.5, value + 0.5),
    less = c(-Inf, value - 0.5),
    less_equal = c(-Inf, value + 0.5),
    greater = c(value + 0.5, Inf),
    greater_equal = c(value - 0.5, Inf),
    between = {
      upper_value <- require_whole_num(upper_value, "Upper value", 0)
      require_between_values(value, upper_value, "Value", "Upper value")
      c(value - 0.5, upper_value + 0.5)
    },
    stop("Unknown probability type.")
  )

  probability <- pnorm(bounds[2], mean_val, sd_val) - pnorm(bounds[1], mean_val, sd_val)
  cat("--------------------------------------------------\n")
  cat("Normal Approximation to Binomial\n")
  cat("--------------------------------------------------\n")
  cat(sprintf("%-25s: %10.6f\n", "Mean np", mean_val))
  cat(sprintf("%-25s: %10.6f\n", "Standard Deviation", sd_val))
  cat(sprintf("%-25s: %10.6f\n", "Corrected Lower", bounds[1]))
  cat(sprintf("%-25s: %10.6f\n", "Corrected Upper", bounds[2]))
  print_probability_result("Requested Probability", probability)
  invisible(calculator_result(
    TRUE,
    values = list(
      n = n,
      p = p,
      query_type = type,
      value = value,
      upper_value = upper_value,
      mean = mean_val,
      standard_deviation = sd_val,
      corrected_lower = bounds[1],
      corrected_upper = bounds[2],
      probability = probability
    ),
    metadata = list(calculator = "normalApproxBinomial")
  ))
}

varianceTestCalc <- function(n, s, sigma0, alpha = 0.05, alternative = "two.sided") {
  n <- require_whole_num(n, "n", 2)
  s <- require_nonnegative_num(s, "Sample standard deviation")
  sigma0 <- require_positive_num(sigma0, "Hypothesized standard deviation")
  alpha <- require_alpha(alpha)
  df <- n - 1
  statistic <- df * s^2 / sigma0^2
  p_value <- switch(alternative,
    less = pchisq(statistic, df),
    greater = 1 - pchisq(statistic, df),
    two.sided = 2 * min(pchisq(statistic, df), 1 - pchisq(statistic, df)),
    stop("Unknown alternative.")
  )
  p_value <- min(p_value, 1)
  cat("--------------------------------------------------\n")
  cat("One Variance / Standard Deviation Chi-Square Test\n")
  cat("--------------------------------------------------\n")
  cat(sprintf("%-25s: %10.6f\n", "Degrees of Freedom", df))
  cat(sprintf("%-25s: %10.6f\n", "Chi-Square Statistic", statistic))
  cat(sprintf("%-25s: %10.6f\n", "P-value", p_value))
  cat(sprintf("%-25s: %s\n", "Decision", ifelse(p_value <= alpha, "Reject H0", "Fail to Reject H0")))
  cat("--------------------------------------------------\n")
  invisible(calculator_result(
    TRUE,
    values = list(
      n = n,
      sample_sd = s,
      hypothesized_sd = sigma0,
      alpha = alpha,
      alternative = alternative,
      df = df,
      statistic = statistic,
      p_value = p_value,
      decision = ifelse(p_value <= alpha, "Reject H0", "Fail to Reject H0")
    ),
    metadata = list(calculator = "varianceTest")
  ))
}

pairedDiffCI <- function(before, after, conf_level = 0.95) {
  before <- require_numeric_vector(before, "Before values", 2)
  after <- require_numeric_vector(after, "After values", 2)
  conf_level <- require_conf_level(conf_level)
  if (length(before) != length(after)) {
    stop("Both paired lists must have the same number of values.")
  }
  differences <- before - after
  n <- length(differences)
  mean_diff <- mean(differences)
  sd_diff <- sd(differences)
  alpha <- 1 - conf_level
  tcrit <- qt(1 - alpha / 2, n - 1)
  se <- sd_diff / sqrt(n)
  me <- tcrit * se
  cat("--------------------------------------------------\n")
  cat("Paired Difference Confidence Interval\n")
  cat("--------------------------------------------------\n")
  cat(sprintf("%-25s: %10.6f\n", "Mean Difference", mean_diff))
  cat(sprintf("%-25s: %10.6f\n", "SD of Differences", sd_diff))
  cat(sprintf("%-25s: %10.6f\n", "t Critical", tcrit))
  cat(sprintf("%-25s: %10.6f\n", "Standard Error", se))
  cat(sprintf("%-25s: %10.6f\n", "Lower Endpoint", mean_diff - me))
  cat(sprintf("%-25s: %10.6f\n", "Upper Endpoint", mean_diff + me))
  cat("--------------------------------------------------\n")
  invisible(calculator_result(
    TRUE,
    values = list(
      before = before,
      after = after,
      differences = differences,
      n = n,
      mean_difference = mean_diff,
      sd_difference = sd_diff,
      confidence_level = conf_level,
      alpha = alpha,
      t_critical = tcrit,
      standard_error = se,
      margin_error = me,
      lower_endpoint = mean_diff - me,
      upper_endpoint = mean_diff + me
    ),
    metadata = list(calculator = "pairedDiffCI")
  ))
}

twoVarianceRatioCalc <- function(n1, var1, n2, var2, conf_level = NULL, alpha = NULL, alternative = "two.sided") {
  if (is.null(conf_level) && !is.null(alpha)) {
    conf_level <- 1 - alpha
  }
  if (!is.null(conf_level) && is.null(alpha)) {
    alpha <- 1 - conf_level
  }
  if (is.null(alpha)) {
    alpha <- 0.05
    conf_level <- 0.95
  }
  n1 <- require_whole_num(n1, "n1", 2)
  n2 <- require_whole_num(n2, "n2", 2)
  var1 <- require_positive_num(var1, "Variance 1")
  var2 <- require_positive_num(var2, "Variance 2")
  alpha <- require_alpha(alpha)
  conf_level <- require_conf_level(conf_level)
  df1 <- n1 - 1
  df2 <- n2 - 1
  f_stat <- var1 / var2
  p_value <- switch(alternative,
    less = pf(f_stat, df1, df2),
    greater = 1 - pf(f_stat, df1, df2),
    two.sided = 2 * min(pf(f_stat, df1, df2), 1 - pf(f_stat, df1, df2)),
    stop("Unknown alternative.")
  )
  p_value <- min(p_value, 1)
  lower <- f_stat / qf(1 - alpha / 2, df1, df2)
  upper <- f_stat / qf(alpha / 2, df1, df2)
  cat("--------------------------------------------------\n")
  cat("Two Variances / F Ratio\n")
  cat("--------------------------------------------------\n")
  cat(sprintf("%-25s: %10.6f\n", "F Statistic", f_stat))
  cat(sprintf("%-25s: %10.6f\n", "df1", df1))
  cat(sprintf("%-25s: %10.6f\n", "df2", df2))
  cat(sprintf("%-25s: %10.6f\n", "P-value", p_value))
  cat(sprintf("%-25s: %10.6f\n", "Ratio CI Lower", lower))
  cat(sprintf("%-25s: %10.6f\n", "Ratio CI Upper", upper))
  cat(sprintf("%-25s: %s\n", "Decision", ifelse(p_value <= alpha, "Reject H0", "Fail to Reject H0")))
  cat("--------------------------------------------------\n")
  invisible(calculator_result(
    TRUE,
    values = list(
      n1 = n1,
      n2 = n2,
      variance1 = var1,
      variance2 = var2,
      confidence_level = conf_level,
      alpha = alpha,
      alternative = alternative,
      df1 = df1,
      df2 = df2,
      statistic = f_stat,
      p_value = p_value,
      lower_endpoint = lower,
      upper_endpoint = upper,
      decision = ifelse(p_value <= alpha, "Reject H0", "Fail to Reject H0")
    ),
    metadata = list(calculator = "twoVarianceRatio")
  ))
}

query_discrete_probability <- function(values, probabilities, type, value, upper_value = NULL) {
  value <- require_num(value, "Value")
  if (type == "equal") {
    return(sum(probabilities[values == value]))
  }
  if (type == "less") {
    return(sum(probabilities[values < value]))
  }
  if (type == "less_equal") {
    return(sum(probabilities[values <= value]))
  }
  if (type == "greater") {
    return(sum(probabilities[values > value]))
  }
  if (type == "greater_equal") {
    return(sum(probabilities[values >= value]))
  }
  if (type == "between") {
    upper_value <- require_num(upper_value, "Upper value")
    require_between_values(value, upper_value, "Value", "Upper value")
    return(sum(probabilities[values >= value & values <= upper_value]))
  }
  stop("Unknown probability type.")
}

print_distribution_summary <- function(values, probabilities, title = "Discrete Probability Distribution") {
  values <- require_numeric_vector(values, "x values")
  probabilities <- require_numeric_vector(probabilities, "Probabilities")
  if (length(values) != length(probabilities)) {
    stop("x values and probabilities must have the same number of entries.")
  }
  total_prob <- sum(probabilities)
  valid_range <- all(probabilities >= 0 & probabilities <= 1)
  valid_sum <- isTRUE(all.equal(total_prob, 1, tolerance = 1e-6))
  valid <- valid_range && valid_sum
  reason <- if (valid) {
    "Yes. Probabilities are between 0 and 1 and add to 1."
  } else if (!valid_range) {
    "No. At least one probability is less than 0 or greater than 1."
  } else {
    "No. The probabilities do not add to 1."
  }

  expected_value <- sum(values * probabilities)
  variance <- sum((values - expected_value)^2 * probabilities)
  sd_value <- sqrt(variance)

  cat("--------------------------------------------------\n")
  cat(title, "\n", sep = "")
  cat("--------------------------------------------------\n")
  print(data.frame(x = values, Probability = probabilities))
  cat("--------------------------------------------------\n")
  cat(sprintf("%-25s: %10.6f\n", "Sum of Probabilities", total_prob))
  cat(sprintf("%-25s: %s\n", "Valid Distribution", ifelse(valid, "Yes", "No")))
  cat(sprintf("%-25s: %s\n", "Reason", reason))
  if (valid) {
    cat(sprintf("%-25s: %10.6f\n", "Expected Value", expected_value))
    cat(sprintf("%-25s: %10.6f\n", "Variance", variance))
    cat(sprintf("%-25s: %10.6f\n", "Standard Deviation", sd_value))
  }
  cat("--------------------------------------------------\n")
  invisible(calculator_result(
    TRUE,
    values = list(
      x = values,
      probabilities = probabilities,
      total_probability = total_prob,
      valid = valid,
      valid_range = valid_range,
      valid_sum = valid_sum,
      reason = reason,
      expected_value = if (valid) expected_value else NA_real_,
      variance = if (valid) variance else NA_real_,
      standard_deviation = if (valid) sd_value else NA_real_
    ),
    metadata = list(calculator = "discreteDistribution", title = title)
  ))
}

probDistQueryCalc <- function(values, probabilities, type, value, upper_value = NULL) {
  values <- require_numeric_vector(values, "x values")
  probabilities <- require_probability_vector(probabilities, "Probabilities")
  if (length(values) != length(probabilities)) {
    stop("x values and probabilities must have the same number of entries.")
  }
  if (!isTRUE(all.equal(sum(probabilities), 1, tolerance = 1e-6))) {
    stop("Probabilities must add to 1 before using this probability calculator.")
  }
  probability <- query_discrete_probability(values, probabilities, type, value, upper_value)
  print_probability_result("Probability From Distribution", probability)
  invisible(calculator_result(
    TRUE,
    values = list(
      x = values,
      probabilities = probabilities,
      query_type = type,
      value = value,
      upper_value = upper_value,
      probability = probability
    ),
    metadata = list(calculator = "probFromDist")
  ))
}

expectedMonetaryValueCalc <- function(outcomes, probabilities) {
  outcomes <- require_numeric_vector(outcomes, "Outcomes")
  probabilities <- require_probability_vector(probabilities, "Probabilities")
  if (length(outcomes) != length(probabilities)) {
    stop("Outcomes and probabilities must have the same number of entries.")
  }
  expected_value <- sum(outcomes * probabilities)
  cat("--------------------------------------------------\n")
  cat("Expected Monetary Value\n")
  cat("--------------------------------------------------\n")
  print(data.frame(Outcome = outcomes, Probability = probabilities))
  cat("--------------------------------------------------\n")
  cat(sprintf("%-25s: %10.6f\n", "Expected Value", expected_value))
  cat("Interpretation: In many repeated identical situations, this is the long-run average gain or loss.\n")
  cat("--------------------------------------------------\n")
  invisible(calculator_result(
    TRUE,
    values = list(
      outcomes = outcomes,
      probabilities = probabilities,
      expected_value = expected_value
    ),
    metadata = list(calculator = "expectedMonetaryValue")
  ))
}

discreteUniformCalc <- function(min_value, max_value, type = "summary", value = NULL, upper_value = NULL) {
  min_value <- require_whole_num(min_value, "Minimum value", -Inf)
  max_value <- require_whole_num(max_value, "Maximum value", -Inf)
  require_between_values(min_value, max_value, "Minimum value", "Maximum value")
  values <- seq(min_value, max_value)
  probabilities <- rep(1 / length(values), length(values))
  expected_value <- mean(values)
  variance <- mean((values - expected_value)^2)
  sd_value <- sqrt(variance)

  cat("--------------------------------------------------\n")
  cat("Discrete Uniform Distribution\n")
  cat("--------------------------------------------------\n")
  cat(sprintf("%-25s: %s through %s\n", "Possible Values", min_value, max_value))
  cat(sprintf("%-25s: %10.6f\n", "Each Probability", 1 / length(values)))
  cat(sprintf("%-25s: %10.6f\n", "Expected Value", expected_value))
  cat(sprintf("%-25s: %10.6f\n", "Variance", variance))
  cat(sprintf("%-25s: %10.6f\n", "Standard Deviation", sd_value))
  if (type != "summary") {
    probability <- query_discrete_probability(values, probabilities, type, value, upper_value)
    cat("--------------------------------------------------\n")
    print_probability_result("Requested Probability", probability)
  } else {
    cat("--------------------------------------------------\n")
  }
  invisible(calculator_result(
    TRUE,
    values = list(
      min_value = min_value,
      max_value = max_value,
      values = values,
      probabilities = probabilities,
      query_type = type,
      value = value,
      upper_value = upper_value,
      expected_value = expected_value,
      variance = variance,
      standard_deviation = sd_value,
      probability = if (type != "summary") probability else NULL
    ),
    metadata = list(calculator = "discreteUniform")
  ))
}

binomialCalc <- function(n, p, type, value, upper_value = NULL) {
  n <- require_whole_num(n, "Trials n", 1)
  p <- require_probability_num(p, "Success probability p")
  value <- require_whole_num(value, "Value", 0)
  upper_value <- if (is.null(upper_value) || is.na(upper_value)) NULL else upper_value
  probability <- switch(type,
    equal = dbinom(value, n, p),
    less = pbinom(value - 1, n, p),
    less_equal = pbinom(value, n, p),
    greater = 1 - pbinom(value, n, p),
    greater_equal = 1 - pbinom(value - 1, n, p),
    between = {
      upper_value <- require_whole_num(upper_value, "Upper value", 0)
      require_between_values(value, upper_value, "Value", "Upper value")
      sum(dbinom(value:upper_value, n, p))
    },
    stop("Unknown probability type.")
  )

  cat("--------------------------------------------------\n")
  cat("Binomial Distribution\n")
  cat("--------------------------------------------------\n")
  cat(sprintf("%-25s: %10.6f\n", "Trials n", n))
  cat(sprintf("%-25s: %10.6f\n", "Success Probability p", p))
  cat(sprintf("%-25s: %10.6f\n", "Expected Value", n * p))
  cat(sprintf("%-25s: %10.6f\n", "Standard Deviation", sqrt(n * p * (1 - p))))
  cat("--------------------------------------------------\n")
  print_probability_result("Requested Probability", probability)
  invisible(calculator_result(
    TRUE,
    values = list(
      n = n,
      p = p,
      query_type = type,
      value = value,
      upper_value = upper_value,
      expected_value = n * p,
      standard_deviation = sqrt(n * p * (1 - p)),
      probability = probability
    ),
    metadata = list(calculator = "binomialDistribution")
  ))
}

poissonCalc <- function(rate, periods, type, value, upper_value = NULL) {
  rate <- require_positive_num(rate, "Rate per period")
  periods <- require_positive_num(periods, "Number of periods")
  lambda <- rate * periods
  value <- require_whole_num(value, "Value", 0)
  upper_value <- if (is.null(upper_value) || is.na(upper_value)) NULL else upper_value
  probability <- switch(type,
    equal = dpois(value, lambda),
    less = ppois(value - 1, lambda),
    less_equal = ppois(value, lambda),
    greater = 1 - ppois(value, lambda),
    greater_equal = 1 - ppois(value - 1, lambda),
    between = {
      upper_value <- require_whole_num(upper_value, "Upper value", 0)
      require_between_values(value, upper_value, "Value", "Upper value")
      sum(dpois(value:upper_value, lambda))
    },
    stop("Unknown probability type.")
  )

  cat("--------------------------------------------------\n")
  cat("Poisson Distribution\n")
  cat("--------------------------------------------------\n")
  cat(sprintf("%-25s: %10.6f\n", "Rate per Period", rate))
  cat(sprintf("%-25s: %10.6f\n", "Number of Periods", periods))
  cat(sprintf("%-25s: %10.6f\n", "lambda", lambda))
  cat(sprintf("%-25s: %10.6f\n", "Expected Value", lambda))
  cat(sprintf("%-25s: %10.6f\n", "Standard Deviation", sqrt(lambda)))
  cat("--------------------------------------------------\n")
  print_probability_result("Requested Probability", probability)
  invisible(calculator_result(
    TRUE,
    values = list(
      rate = rate,
      periods = periods,
      lambda = lambda,
      query_type = type,
      value = value,
      upper_value = upper_value,
      expected_value = lambda,
      standard_deviation = sqrt(lambda),
      probability = probability
    ),
    metadata = list(calculator = "poissonDistribution")
  ))
}

hypergeometricCalc <- function(total_items, success_items, sample_size, type, value = NULL, upper_value = NULL) {
  total_items <- require_whole_num(total_items, "Total items", 1)
  success_items <- require_whole_num(success_items, "Success items", 0)
  sample_size <- require_whole_num(sample_size, "Sample size", 0)
  failure_items <- total_items - success_items
  if (failure_items < 0) {
    stop("Success items cannot be larger than total items.")
  }
  if (sample_size > total_items) {
    stop("Sample size cannot be larger than total items.")
  }
  expected_value <- sample_size * success_items / total_items
  sd_value <- sqrt(sample_size * (success_items / total_items) * (1 - success_items / total_items) *
                     ((total_items - sample_size) / (total_items - 1)))

  probability <- NULL
  if (type != "expected") {
    value <- require_whole_num(value, "Value", 0)
    upper_value <- if (is.null(upper_value) || is.na(upper_value)) NULL else upper_value
    probability <- switch(type,
      equal = dhyper(value, success_items, failure_items, sample_size),
      less = phyper(value - 1, success_items, failure_items, sample_size),
      less_equal = phyper(value, success_items, failure_items, sample_size),
      greater = 1 - phyper(value, success_items, failure_items, sample_size),
      greater_equal = 1 - phyper(value - 1, success_items, failure_items, sample_size),
      between = {
        upper_value <- require_whole_num(upper_value, "Upper value", 0)
        require_between_values(value, upper_value, "Value", "Upper value")
        sum(dhyper(value:upper_value, success_items, failure_items, sample_size))
      },
      stop("Unknown probability type.")
    )
  }

  cat("--------------------------------------------------\n")
  cat("Hypergeometric Distribution / Marble Problems\n")
  cat("--------------------------------------------------\n")
  cat("Use this for marble, light bulb, card, or item samples taken without replacement.\n")
  cat(sprintf("%-25s: %10.6f\n", "Total Items", total_items))
  cat(sprintf("%-25s: %10.6f\n", "Success Items", success_items))
  cat(sprintf("%-25s: %10.6f\n", "Failure Items", failure_items))
  cat(sprintf("%-25s: %10.6f\n", "Sample Size", sample_size))
  cat(sprintf("%-25s: %10.6f\n", "Expected Value", expected_value))
  cat(sprintf("%-25s: %10.6f\n", "Standard Deviation", sd_value))
  if (!is.null(probability)) {
    cat("--------------------------------------------------\n")
    print_probability_result("Requested Probability", probability)
  } else {
    cat("--------------------------------------------------\n")
  }
  invisible(calculator_result(
    TRUE,
    values = list(
      total_items = total_items,
      success_items = success_items,
      failure_items = failure_items,
      sample_size = sample_size,
      query_type = type,
      value = value,
      upper_value = upper_value,
      expected_value = expected_value,
      standard_deviation = sd_value,
      probability = probability
    ),
    metadata = list(calculator = "hypergeometricDistribution")
  ))
}

conf_alpha_values <- function(conf_text, alpha_text) {
  conf_level <- parse_optional_num(conf_text)
  alpha <- parse_optional_num(alpha_text)
  if (!is.null(conf_level) && !is.null(alpha)) {
    stop("Enter either confidence level OR alpha, not both. Leave one blank.")
  }
  if (is.null(conf_level) && is.null(alpha)) {
    stop("Enter a confidence level OR alpha. One field must be filled.")
  }
  if (!is.null(conf_level)) {
    conf_level <- require_conf_level(conf_level)
  }
  if (!is.null(alpha)) {
    alpha <- require_alpha(alpha)
  }
  list(conf_level = conf_level, alpha = alpha)
}

matrix_from_text <- function(values_text, nrow, row_names_text, col_names_text) {
  vals <- require_numeric_vector(parse_nums(values_text, "matrix values"), "Matrix values")
  nrow <- require_num(nrow, "Number of rows")
  if (nrow <= 0 || nrow != floor(nrow)) {
    stop("Number of rows must be a positive whole number.")
  }
  if (length(vals) %% nrow != 0) {
    stop("Matrix values must divide evenly into the requested number of rows.")
  }
  tab <- matrix(vals, nrow = nrow, byrow = TRUE)
  row_names <- parse_labels(row_names_text)
  col_names <- parse_labels(col_names_text)
  if (!is.null(row_names) && length(row_names) != nrow(tab)) {
    stop("Row label count must match the requested number of rows.")
  }
  if (!is.null(col_names) && length(col_names) != ncol(tab)) {
    stop("Column label count must match the number of matrix columns.")
  }
  if (!is.null(row_names) && length(row_names) == nrow(tab)) {
    rownames(tab) <- row_names
  }
  if (!is.null(col_names) && length(col_names) == ncol(tab)) {
    colnames(tab) <- col_names
  }
  tab
}

split_table_header <- function(line) {
  parts <- trimws(unlist(strsplit(line, "\t+| {2,}|,")))
  parts <- parts[nzchar(parts)]
  if (length(parts) <= 1) {
    parts <- trimws(unlist(strsplit(line, "[[:space:]]+")))
    parts <- parts[nzchar(parts)]
  }
  parts
}

parse_contingency_table_text <- function(text) {
  text <- trimws(gsub("\r", "\n", text))
  if (!nzchar(text)) {
    return(NULL)
  }

  lines <- trimws(unlist(strsplit(text, "\n+")))
  lines <- lines[nzchar(lines)]
  number_pattern <- "[-+]?[0-9]*\\.?[0-9]+"
  data_line <- grepl(number_pattern, lines)
  if (!any(data_line)) {
    stop("The pasted table does not contain any numeric counts.")
  }

  first_data_index <- which(data_line)[1]
  header_line <- if (first_data_index > 1) lines[first_data_index - 1] else ""
  header_parts <- split_table_header(header_line)

  total_in_header <- any(tolower(header_parts) == "total")
  if (total_in_header) {
    total_index <- which(tolower(header_parts) == "total")[1]
    possible_cols <- header_parts[seq_len(total_index - 1)]
    if (length(possible_cols) > 2 && !grepl(number_pattern, possible_cols[1])) {
      column_labels <- possible_cols[-1]
    } else {
      column_labels <- possible_cols
    }
    count_with_total <- length(column_labels) + 1
  } else {
    column_labels <- header_parts
    count_with_total <- NA_integer_
    if (length(header_parts)) {
      data_rows <- lines[data_line & !grepl("^total([[:space:],]|$)", lines, ignore.case = TRUE)]
      number_counts <- lengths(regmatches(data_rows, gregexpr(number_pattern, data_rows, perl = TRUE)))
      if (length(number_counts) && all(number_counts == length(header_parts) - 1)) {
        column_labels <- header_parts[-1]
      }
      count_with_total <- length(column_labels)
    }
  }

  row_labels <- c()
  rows <- list()

  for (line in lines[data_line]) {
    matches <- gregexpr(number_pattern, line, perl = TRUE)[[1]]
    nums <- as.numeric(regmatches(line, list(matches))[[1]])

    count_to_take <- if (!is.na(count_with_total) && length(nums) >= count_with_total) {
      count_with_total
    } else {
      length(nums)
    }
    count_start_match <- matches[length(matches) - count_to_take + 1]
    label <- trimws(substr(line, 1, count_start_match - 1))
    label <- sub("[,\t]+$", "", label)
    nums <- tail(nums, count_to_take)

    if (!nzchar(label)) {
      label <- paste("Row", length(rows) + 1)
    }
    if (tolower(label) == "total") {
      next
    }
    rows[[length(rows) + 1]] <- nums
    row_labels <- c(row_labels, label)
  }

  if (!length(rows)) {
    stop("No observed-count rows were found after removing totals.")
  }

  column_count <- unique(lengths(rows))
  if (length(column_count) != 1) {
    stop("Rows have different numbers of counts. Check the pasted table formatting.")
  }

  drop_total <- total_in_header || (length(rows) >= 2 && all(vapply(rows, function(nums) {
    length(nums) >= 2 && isTRUE(all.equal(sum(nums[-length(nums)]), nums[length(nums)]))
  }, logical(1))))
  if (drop_total) {
    rows <- lapply(rows, function(nums) nums[-length(nums)])
  }

  observed <- matrix(unlist(rows), nrow = length(rows), byrow = TRUE)
  rownames(observed) <- row_labels
  if (length(column_labels) == ncol(observed) + 1 && !grepl(number_pattern, column_labels[1])) {
    column_labels <- column_labels[-1]
  }
  if (length(column_labels) == ncol(observed)) {
    colnames(observed) <- column_labels
  } else {
    colnames(observed) <- paste("Column", seq_len(ncol(observed)))
  }
  observed
}

parse_gof_table_text <- function(text) {
  text <- trimws(gsub("\r", "\n", text))
  if (!nzchar(text)) {
    return(NULL)
  }

  lines <- trimws(unlist(strsplit(text, "\n+")))
  lines <- lines[nzchar(lines)]
  number_pattern <- "[-+]?[0-9]*\\.?[0-9]+%?"
  data_line <- grepl("[0-9]", lines)
  if (!any(data_line)) {
    stop("The pasted table does not contain observed counts.")
  }

  if (any(grepl("%", lines))) {
    stop("This pasted table contains percentages. Chi-square goodness-of-fit needs observed counts, not percentages. Use a count table or enter observed counts manually.")
  }

  numeric_indices <- which(data_line)
  first_data_index <- numeric_indices[1]
  header_line <- if (first_data_index > 1) lines[first_data_index - 1] else ""
  header_parts <- split_table_header(header_line)

  # Format: category headers across the top, one observed-count row underneath.
  # Example: Brown Yellow Red ... / Number of candies 190 185 110 ...
  if (length(numeric_indices) == 1 && nzchar(header_line)) {
    line <- lines[first_data_index]
    matches <- gregexpr(number_pattern, line, perl = TRUE)[[1]]
    observed <- as.numeric(gsub("%", "", regmatches(line, list(matches))[[1]]))
    if (length(header_parts) == length(observed)) {
      return(list(observed = observed, categories = header_parts, expected_probs = NULL))
    }
  }

  # Format: one category and one count per row.
  # Example: Mon 18 / Tue 14 / ...
  categories <- c()
  observed <- c()
  for (line in lines[data_line]) {
    matches <- gregexpr(number_pattern, line, perl = TRUE)[[1]]
    nums <- as.numeric(gsub("%", "", regmatches(line, list(matches))[[1]]))
    label <- trimws(substr(line, 1, matches[1] - 1))
    label <- sub("[,\t]+$", "", label)

    if (!nzchar(label)) {
      next
    }
    if (tolower(label) == "total") {
      next
    }

    count <- tail(nums, 1)
    categories <- c(categories, label)
    observed <- c(observed, count)
  }

  if (!length(observed)) {
    stop("Could not find category/count rows in the pasted table. Try the manual observed-count field.")
  }

  list(observed = observed, categories = categories, expected_probs = NULL)
}
