# HAWKS FUNCTION DEFINITIONS ONLY ############################################
# Source this file once to load helper functions without running examples.
###############################################################################



###############################################################################
# SHARED INPUT VALIDATION
# Every calculator below checks its inputs with these helpers and returns a
# calculator_result (invisibly) after printing, so the console and the Shiny
# app share one implementation.
###############################################################################

require_num <- function(value, label) {
  if (is.null(value) || is.na(value)) {
    stop(paste(label, "is required for this question type."))
  }
  value
}

require_numeric_vector <- function(values, label, min_length = 1) {
  if (is.null(values) || length(values) < min_length || any(is.na(values))) {
    stop(paste(label, "must contain at least", min_length, "numeric value(s)."))
  }
  values
}

require_probability_vector <- function(values, label, min_length = 1) {
  values <- require_numeric_vector(values, label, min_length)
  if (any(values < 0 | values > 1)) {
    stop(paste(label, "must contain probabilities between 0 and 1."))
  }
  values
}

require_positive_num <- function(value, label) {
  value <- require_num(value, label)
  if (value <= 0) {
    stop(paste(label, "must be greater than 0."))
  }
  value
}

require_nonnegative_num <- function(value, label) {
  value <- require_num(value, label)
  if (value < 0) {
    stop(paste(label, "must be 0 or greater."))
  }
  value
}

require_whole_num <- function(value, label, min_value = 0) {
  value <- require_num(value, label)
  if (value != floor(value) || value < min_value) {
    stop(paste(label, "must be a whole number at least", min_value, "."))
  }
  value
}

require_probability_num <- function(value, label, allow_zero_one = TRUE) {
  value <- require_num(value, label)
  if (allow_zero_one) {
    invalid <- value < 0 || value > 1
  } else {
    invalid <- value <= 0 || value >= 1
  }
  if (invalid) {
    boundary <- if (allow_zero_one) "between 0 and 1" else "greater than 0 and less than 1"
    stop(paste(label, "must be", boundary, "."))
  }
  value
}

require_conf_level <- function(conf_level, label = "Confidence level") {
  require_probability_num(conf_level, label, allow_zero_one = FALSE)
}

require_alpha <- function(alpha, label = "Alpha") {
  require_probability_num(alpha, label, allow_zero_one = FALSE)
}

require_between_values <- function(lower, upper, lower_label = "Lower value", upper_label = "Upper value") {
  lower <- require_num(lower, lower_label)
  upper <- require_num(upper, upper_label)
  if (lower > upper) {
    stop(paste(lower_label, "must be less than or equal to", paste0(upper_label, ".")))
  }
  list(lower = lower, upper = upper)
}

calculator_result <- function(ok, output = "", message = "", status = NULL,
                              values = list(), metadata = list()) {
  structure(
    list(
      ok = isTRUE(ok),
      output = if (is.null(output)) "" else output,
      message = if (is.null(message)) "" else message,
      status = if (!is.null(status)) status else if (isTRUE(ok)) "ok" else "error",
      values = values,
      metadata = metadata
    ),
    class = "calculator_result"
  )
}

resolve_conf_alpha <- function(conf_level = NULL, alpha = NULL) {
  if (is.null(conf_level) && is.null(alpha)) {
    conf_level <- 0.95
    alpha <- 0.05
  } else if (is.null(conf_level)) {
    alpha <- require_alpha(alpha)
    conf_level <- 1 - alpha
  } else if (is.null(alpha)) {
    conf_level <- require_conf_level(conf_level)
    alpha <- 1 - conf_level
  } else {
    conf_level <- require_conf_level(conf_level)
    alpha <- require_alpha(alpha)
    if (abs(conf_level - (1 - alpha)) > 1e-9) {
      stop("Enter either a confidence level or alpha, not two values that disagree.")
    }
  }
  list(conf_level = conf_level, alpha = alpha)
}

validate_chisq_table <- function(observed_table) {
  observed_table <- as.matrix(observed_table)
  if (nrow(observed_table) < 2 || ncol(observed_table) < 2) {
    stop("Chi-square table must have at least 2 rows and 2 columns.")
  }
  if (any(is.na(observed_table)) || any(observed_table < 0)) {
    stop("Chi-square observed counts must be nonnegative numbers.")
  }
  if (sum(observed_table) <= 0) {
    stop("Chi-square observed counts must have a positive grand total.")
  }
  observed_table
}

print_two_sample_ci_interpretation <- function(conf_level, lower, upper, digits = 2, subject = "") {
  cat("CI Interpretation:\n")
  if (lower > 0 && upper > 0) {
    cat("We are", conf_level * 100, paste0("% confident that ", subject, "Population 1 is between"),
        round(lower, digits), "and", round(upper, digits),
        paste0("larger than ", subject, "Population 2.\n"))
  } else if (lower < 0 && upper < 0) {
    cat("We are", conf_level * 100, paste0("% confident that ", subject, "Population 1 is between"),
        abs(round(upper, digits)), "and", abs(round(lower, digits)),
        paste0("smaller than ", subject, "Population 2.\n"))
  } else if (nzchar(subject)) {
    cat("Since the confidence interval contains 0, there is not sufficient evidence that the two population means are different.\n")
  } else {
    cat("Since the interval contains 0, there is not sufficient evidence of a difference.\n")
  }
  cat("--------------------------------------------------\n")
}

###############################################################################
# CHAPTER 1
###############################################################################

# CHAPTER 1 ##################################################################
# EXAM REFERENCE NOTES
# Chapter 1 is conceptual, so this file is meant to be read as a reference sheet.
###############################################################################

###############################################################################
# POPULATION vs SAMPLE
###############################################################################

# Population:
# The entire group being studied.
#
# Clue words:
# "all", "every", "entire", "everyone", "each member"
#
# Example:
# All students at a college.

# Sample:
# The smaller group actually selected, surveyed, or measured.
#
# Clue words:
# "selected", "surveyed", "sample of", "randomly chosen"
#
# Example:
# 50 students selected from a college.

###############################################################################
# PARAMETER vs STATISTIC
###############################################################################

# Parameter:
# A number that describes a population.
#
# Common symbols:
# mu    = population mean
# sigma = population standard deviation
# p     = population proportion
#
# Clue:
# The value describes all members of the population.

# Statistic:
# A number that describes a sample.
#
# Common symbols:
# xbar = sample mean
# s    = sample standard deviation
# phat = sample proportion
#
# Clue:
# The value comes from selected/surveyed/measured sample data.

###############################################################################
# DESCRIPTIVE vs INFERENTIAL STATISTICS
###############################################################################

# Descriptive Statistic:
# Summarizes or describes only the data collected.
#
# Clue:
# The statement does not generalize beyond the data set.
#
# Example:
# The average of this class's test scores is 82.

# Inferential Statistic:
# Uses sample data to make a claim about a larger population.
#
# Clue:
# The statement generalizes from a sample to a population.
#
# Example:
# Based on a survey, 48% of all voters support the proposal.

###############################################################################
# INDUCTIVE vs DEDUCTIVE REASONING
###############################################################################

# Inductive Reasoning:
# Uses specific observations or data to make a general conclusion.
#
# Pattern:
# Specific examples -> general conclusion

# Deductive Reasoning:
# Applies a general rule or known fact to a specific case.
#
# Pattern:
# General rule -> specific conclusion

###############################################################################
# COMMON HAWKS WORDING
###############################################################################

# "The average of all..."       -> population parameter
# "A survey/sample showed..."   -> sample statistic
# "Recent poll of 500 people..." -> sample
# "All homeowners in..."        -> population
# "The data set has a mean..."  -> descriptive statistic
# "Based on a sample, we estimate..." -> inferential statistic

########################################


###############################################################################
# CHAPTER 2
###############################################################################

# CHAPTER 2 ##################################################################
# EXAM REFERENCE NOTES
# Chapter 2 is conceptual, so this file is meant to be read as a reference sheet.
###############################################################################

###############################################################################
# QUALITATIVE vs QUANTITATIVE DATA
###############################################################################

# Qualitative Data:
# Categories, labels, names, or descriptions.
#
# Clue:
# Arithmetic usually does not make sense.
#
# Examples:
# Eye color, movie genre, type of car, political party.

# Quantitative Data:
# Numerical counts or measurements.
#
# Clue:
# Arithmetic usually makes sense.
#
# Examples:
# Height, weight, age, number of defects, test score.

###############################################################################
# DISCRETE vs CONTINUOUS DATA
###############################################################################

# Discrete Data:
# Countable values, often whole numbers.
#
# Clue words:
# "number of", "count", "how many"
#
# Examples:
# Number of students, number of cars, number of defective parts.

# Continuous Data:
# Measured values. Decimals are possible.
#
# Clue words:
# "measured", "time", "height", "weight", "distance"
#
# Examples:
# Time, temperature, weight, length, speed.

###############################################################################
# LEVELS OF MEASUREMENT
###############################################################################

# Nominal:
# Categories only. No natural order.
#
# Examples:
# Gender, color, state, brand, type of pet.

# Ordinal:
# Categories with a meaningful order/rank.
# Differences between ranks are not necessarily equal.
#
# Examples:
# Class rank, satisfaction rating, small/medium/large.

# Interval:
# Numerical scale where differences are meaningful.
# There is no true zero.
#
# Examples:
# Temperature in Fahrenheit or Celsius, calendar years.

# Ratio:
# Numerical scale with meaningful differences and a true zero.
#
# Examples:
# Money, age, height, weight, distance, time.

###############################################################################
# STUDY DESIGN TERMS
###############################################################################

# Observational Study:
# Researchers observe or measure, but do not assign treatments.

# Experimental Study:
# Researchers impose or assign a treatment.

# Completely Randomized Design:
# Subjects are randomly assigned to treatment groups.

# Double-Blind Study:
# Neither the subjects nor the researchers interacting with them know who gets
# which treatment.

# Placebo:
# A fake treatment used for comparison.

# Control Variable:
# A variable intentionally held constant.

# Confounding Variable:
# A variable that affects the response and is mixed with the explanatory variable.

# Explanatory Variable:
# The variable that may explain or predict the response.

# Response Variable:
# The outcome being measured.

###############################################################################
# DATA STRUCTURE TERMS
###############################################################################

# Structured Data:
# Organized in a clear format, usually rows and columns.

# Unstructured Data:
# No fixed format.
#
# Examples:
# Photos, videos, free-response text.

# Semi-Structured Data:
# Has some organization but not a strict table format.
#
# Examples:
# Web forms, emails, tagged data.

###############################################################################
# BIG DATA
###############################################################################

# Common Big Data traits:
# Volume   = large amount of data
# Velocity = data arrives quickly
# Variety  = many different forms/types
# Veracity = reliability/trustworthiness of data

###############################################################################
# COMMON HAWKS WORDING
###############################################################################

# "number of..."             -> usually quantitative and discrete
# "height/weight/time..."    -> quantitative and continuous
# "rating from poor to good" -> ordinal
# "type/category/name"       -> nominal
# "treatment is assigned"    -> experimental
# "researchers only observe" -> observational
# "outcome being measured"   -> response variable
# "possible cause/predictor" -> explanatory variable

########################################


###############################################################################
# CHAPTER 3
###############################################################################

# CHAPTER 3 ##################################################################
# NOTE:
# Chapter 3 focuses on frequency tables, percentages, and graphs.
###############################################################################
# IDENTIFYING PROBLEM TYPE + STRUCTURE

# FREQUENCY / PERCENT
# Use frequencyPercent() when the problem asks "what percent" from a table.
#
# PIE CHART COUNTS
# Use percentToCount() when given total and a pie-chart percent.
#
# CLASS WIDTH
# Use classWidth() when asked for the width of grouped classes.

###############################################################################
# IMPORTANT : RUN THESE FUNCTIONS ONCE BEFORE YOU START CALLING THE FUNCTIONS!!!
###############################################################################
# Frequency / Percent Table
###############################################################################

frequencyPercent <- function(frequencies, categories = NULL) {
  frequencies <- require_numeric_vector(frequencies, "Frequencies")
  if (any(frequencies < 0)) {
    stop("Frequencies must be nonnegative.")
  }
  require_positive_num(sum(frequencies), "Total")

  # frequencies = vector of category frequencies
  # categories = optional category names
  
  if (is.null(categories)) {
    categories <- paste("Category", seq_along(frequencies))
  }
  
  total <- sum(frequencies)
  percent <- frequencies / total * 100
  
  cat("--------------------------------------------------\n")
  cat("Frequency / Percent Table\n")
  cat("--------------------------------------------------\n")
  cat(sprintf("%-15s | %10s | %10s\n", "Category", "Frequency", "Percent"))
  cat("------------------------------------------\n")
  for (i in seq_along(frequencies)) {
    cat(sprintf("%-15s | %10.6f | %9.6f%%\n", categories[i], frequencies[i], percent[i]))
  }
  cat("--------------------------------------------------\n")
  cat(sprintf("%-25s: %10.6f\n", "Total", total))
  cat("--------------------------------------------------\n")
}

###############################################################################
# Percent to Count
###############################################################################

percentToCount <- function(total, percent) {
  total <- require_positive_num(total, "Total")
  percent <- require_num(percent, "Percent")
  require_probability_num(percent / 100, "Percent divided by 100")

  # total = total number of observations
  # percent = percent as a whole number, such as 20 for 20%
  
  count <- total * percent / 100
  
  cat("--------------------------------------------------\n")
  cat("Percent to Count\n")
  cat("--------------------------------------------------\n")
  cat(sprintf("%-25s: %10.6f\n", "Total", total))
  cat(sprintf("%-25s: %10.6f%%\n", "Percent", percent))
  cat(sprintf("%-25s: %10.6f\n", "Count", count))
  cat("--------------------------------------------------\n")
}

###############################################################################
# Class Width
###############################################################################

classWidth <- function(lower_limits) {
  lower_limits <- require_numeric_vector(lower_limits, "Class lower limits", 2)

  # lower_limits = class lower limits from a grouped frequency table
  
  widths <- diff(lower_limits)
  
  cat("--------------------------------------------------\n")
  cat("Class Width\n")
  cat("--------------------------------------------------\n")
  cat(sprintf("%-25s: %10.6f\n", "Class Width", widths[1]))
  cat("--------------------------------------------------\n")
}

###############################################################################


###############################################################################
# CHAPTER 4
###############################################################################

# CHAPTER 4 ##################################################################
# NOTE:
# Chapter 4 focuses on descriptive statistics.
###############################################################################
# IDENTIFYING PROBLEM TYPE + STRUCTURE

# MEAN / MEDIAN / MODE -> descStats()
# VARIANCE / STANDARD DEVIATION -> descStats()
# COEFFICIENT OF VARIATION -> coeffVariation()
# CHEBYSHEV -> chebyshevRange()
# PERCENTILE -> percentileValue()

###############################################################################
# IMPORTANT : RUN THESE FUNCTIONS ONCE BEFORE YOU START CALLING THE FUNCTIONS!!!
###############################################################################
# Descriptive Statistics
###############################################################################

descStats <- function(data, sample = TRUE) {
  data <- require_numeric_vector(data, "Data", if (sample) 2 else 1)

  # data = raw numerical data
  # sample = TRUE for sample variance/sd, FALSE for population variance/sd
  
  n <- length(data)
  mean_val <- mean(data)
  median_val <- median(data)
  freq <- table(data)
  modes <- as.numeric(names(freq[freq == max(freq)]))
  if (length(modes) == n) {
    mode_text <- "No mode"
  } else {
    mode_text <- paste(modes, collapse = ", ")
  }
  
  range_val <- max(data) - min(data)
  variance_val <- if (sample) var(data) else mean((data - mean_val)^2)
  sd_val <- sqrt(variance_val)
  
  cat("--------------------------------------------------\n")
  cat("Descriptive Statistics\n")
  cat("--------------------------------------------------\n")
  cat(sprintf("%-25s: %10.6f\n", "n", n))
  cat(sprintf("%-25s: %10.6f\n", "Mean", mean_val))
  cat(sprintf("%-25s: %10.6f\n", "Median", median_val))
  cat(sprintf("%-25s: %s\n", "Mode", mode_text))
  cat(sprintf("%-25s: %10.6f\n", "Range", range_val))
  cat(sprintf("%-25s: %10.6f\n", "Variance", variance_val))
  cat(sprintf("%-25s: %10.6f\n", "Standard Deviation", sd_val))
  cat("--------------------------------------------------\n")
}

###############################################################################
# Coefficient of Variation
###############################################################################

coeffVariation <- function(mean_val, sd_val) {
  mean_val <- require_num(mean_val, "Mean")
  sd_val <- require_nonnegative_num(sd_val, "Standard deviation")
  if (mean_val == 0) {
    stop("Mean cannot be 0.")
  }

  cv <- sd_val / mean_val * 100
  
  cat("--------------------------------------------------\n")
  cat("Coefficient of Variation\n")
  cat("--------------------------------------------------\n")
  cat(sprintf("%-25s: %10.6f\n", "Mean", mean_val))
  cat(sprintf("%-25s: %10.6f\n", "Standard Deviation", sd_val))
  cat(sprintf("%-25s: %10.6f%%\n", "CV", cv))
  cat("--------------------------------------------------\n")
}

###############################################################################
# Chebyshev Range
###############################################################################

chebyshevRange <- function(mean_val, sd_val, k) {
  mean_val <- require_num(mean_val, "Mean")
  sd_val <- require_positive_num(sd_val, "Standard deviation")
  k <- require_num(k, "k")
  if (k <= 1) {
    stop("k must be greater than 1.")
  }

  lower <- mean_val - k * sd_val
  upper <- mean_val + k * sd_val
  percent <- (1 - 1 / k^2) * 100
  
  cat("--------------------------------------------------\n")
  cat("Chebyshev Range\n")
  cat("--------------------------------------------------\n")
  cat(sprintf("%-25s: %10.6f%%\n", "At Least", percent))
  cat(sprintf("%-25s: %10.6f\n", "Lower", lower))
  cat(sprintf("%-25s: %10.6f\n", "Upper", upper))
  cat("--------------------------------------------------\n")
}

###############################################################################
# Percentile Value
###############################################################################

percentileValue <- function(data, p) {
  data <- require_numeric_vector(data, "Data")
  p <- require_num(p, "Percentile")
  if (p < 0 || p > 100) {
    stop("Percentile must be between 0 and 100.")
  }

  # p = percentile as a whole number, such as 50 for the 50th percentile
  
  value <- as.numeric(quantile(data, probs = p / 100, type = 7))
  
  cat("--------------------------------------------------\n")
  cat("Percentile\n")
  cat("--------------------------------------------------\n")
  cat(sprintf("%-25s: %10.6f\n", "Percentile", p))
  cat(sprintf("%-25s: %10.6f\n", "Value", value))
  cat("--------------------------------------------------\n")
}

###############################################################################


###############################################################################
# CHAPTER 5
###############################################################################

# CHAPTER 5 ##################################################################
# NOTE:
# Chapter 5 focuses on scatterplots, correlation, and regression.
###############################################################################
# IDENTIFYING PROBLEM TYPE + STRUCTURE

# CORRELATION -> corrReg()
# Look for: "correlation coefficient", "positive/negative/no correlation".
#
# REGRESSION LINE -> corrReg()
# Look for: "estimated regression equation", "least squares", y-hat.
#
# PREDICTION -> predictY()
# Look for: "estimate/predict y when x is ...".
#
# COEFFICIENT OF DETERMINATION -> corrReg()
# Look for: "R-squared", "variation explained".

###############################################################################
# IMPORTANT : RUN THESE FUNCTIONS ONCE BEFORE YOU START CALLING THE FUNCTIONS!!!
###############################################################################
# Correlation and Regression
###############################################################################

corrReg <- function(x, y) {
  x <- require_numeric_vector(x, "x", 2)
  y <- require_numeric_vector(y, "y", 2)
  if (length(x) != length(y)) {
    stop("x and y must have the same length.")
  }

  model <- lm(y ~ x)
  intercept <- coef(model)[1]
  slope <- coef(model)[2]
  r <- cor(x, y)
  r_squared <- r^2
  
  cat("--------------------------------------------------\n")
  cat("Correlation and Regression\n")
  cat("--------------------------------------------------\n")
  cat(sprintf("%-25s: %10.6f\n", "Intercept b0", intercept))
  cat(sprintf("%-25s: %10.6f\n", "Slope b1", slope))
  cat("Regression Equation: yhat =", round(intercept, 6), "+",
      round(slope, 6), "* x\n")
  cat(sprintf("%-25s: %10.6f\n", "Correlation r", r))
  cat(sprintf("%-25s: %10.6f\n", "R-squared", r_squared))
  cat(sprintf("%-25s: %10.6f%%\n", "Percent Explained", r_squared * 100))
  cat("--------------------------------------------------\n")
}

###############################################################################
# Prediction from Regression
###############################################################################

predictY <- function(x, y, new_x) {
  x <- require_numeric_vector(x, "x", 2)
  y <- require_numeric_vector(y, "y", 2)
  if (length(x) != length(y)) {
    stop("x and y must have the same length.")
  }
  new_x <- require_num(new_x, "New x")

  model <- lm(y ~ x)
  yhat <- predict(model, newdata = data.frame(x = new_x))
  
  cat("--------------------------------------------------\n")
  cat("Regression Prediction\n")
  cat("--------------------------------------------------\n")
  cat(sprintf("%-25s: %10.6f\n", "x value", new_x))
  cat(sprintf("%-25s: %10.6f\n", "Predicted y", yhat))
  cat("--------------------------------------------------\n")
}

###############################################################################


###############################################################################
# CHAPTER 6
###############################################################################

# CHAPTER 6 ##################################################################
# NOTE:
# Chapter 6 focuses on basic probability, complements, and tables.
###############################################################################
# IDENTIFYING PROBLEM TYPE + STRUCTURE

# SIMPLE PROBABILITY -> simpleProb()
# Look for: favorable outcomes / total outcomes.
#
# COMPLEMENT -> complementProb()
# Look for: "not", "at least one", "not selected".
#
# CONDITIONAL PROBABILITY -> conditionalProb()
# Look for: "given", "if", "among those".
#
# TWO-WAY TABLE -> tableProb()
# Look for: row/column categories and a probability from a table.

###############################################################################
# IMPORTANT : RUN THESE FUNCTIONS ONCE BEFORE YOU START CALLING THE FUNCTIONS!!!
###############################################################################
# Simple Probability
###############################################################################

simpleProb <- function(favorable, total) {
  favorable <- require_nonnegative_num(favorable, "Favorable")
  total <- require_positive_num(total, "Total")
  if (favorable > total) {
    stop("Favorable cannot be larger than total.")
  }

  prob <- favorable / total
  
  cat("--------------------------------------------------\n")
  cat("Simple Probability\n")
  cat("--------------------------------------------------\n")
  cat(sprintf("%-25s: %10.6f\n", "Favorable", favorable))
  cat(sprintf("%-25s: %10.6f\n", "Total", total))
  cat(sprintf("%-25s: %10.6f\n", "Probability", prob))
  cat(sprintf("%-25s: %9.6f%%\n", "Probability (%)", prob * 100))
  cat("Fraction:", favorable, "/", total, "\n")
  cat("--------------------------------------------------\n")
}

###############################################################################
# Complement Probability
###############################################################################

complementProb <- function(prob_event = NULL, favorable = NULL, total = NULL) {
  if (is.null(prob_event)) {
    favorable <- require_nonnegative_num(favorable, "Favorable")
    total <- require_positive_num(total, "Total")
    if (favorable > total) {
      stop("Favorable cannot be larger than total.")
    }
    prob_event <- favorable / total
  }
  prob_event <- require_probability_num(prob_event, "Event probability")
  complement <- 1 - prob_event
  
  cat("--------------------------------------------------\n")
  cat("Complement Probability\n")
  cat("--------------------------------------------------\n")
  cat(sprintf("%-25s: %10.6f\n", "P(Event)", prob_event))
  cat(sprintf("%-25s: %9.6f%%\n", "P(Event) (%)", prob_event * 100))
  cat(sprintf("%-25s: %10.6f\n", "P(Not Event)", complement))
  cat(sprintf("%-25s: %9.6f%%\n", "P(Not Event) (%)", complement * 100))
  cat("--------------------------------------------------\n")
}

###############################################################################
# Conditional Probability
###############################################################################

conditionalProb <- function(joint_count, given_count) {
  joint_count <- require_nonnegative_num(joint_count, "Joint count")
  given_count <- require_positive_num(given_count, "Given count")
  if (joint_count > given_count) {
    stop("Joint count cannot be larger than given count.")
  }

  prob <- joint_count / given_count
  
  cat("--------------------------------------------------\n")
  cat("Conditional Probability\n")
  cat("--------------------------------------------------\n")
  cat(sprintf("%-25s: %10.6f\n", "Joint Count", joint_count))
  cat(sprintf("%-25s: %10.6f\n", "Given Count", given_count))
  cat(sprintf("%-25s: %10.6f\n", "Probability", prob))
  cat(sprintf("%-25s: %9.6f%%\n", "Probability (%)", prob * 100))
  cat("--------------------------------------------------\n")
}

###############################################################################
# Two-Way Table Probability
###############################################################################

tableProb <- function(table_data, row_name = NULL, col_name = NULL) {
  # table_data = matrix/table of counts
  # row_name and col_name = optional row/column names for a joint probability
  
  table_data <- as.matrix(table_data)
  require_numeric_vector(table_data, "Table counts")
  if (any(table_data < 0)) {
    stop("Table counts must be nonnegative.")
  }
  total <- require_positive_num(sum(table_data), "Total")
  
  cat("--------------------------------------------------\n")
  cat("Two-Way Table Probability\n")
  cat("--------------------------------------------------\n")
  print(table_data)
  cat("--------------------------------------------------\n")
  cat(sprintf("%-25s: %10.6f\n", "Total", total))
  if (!is.null(row_name) && !is.null(col_name)) {
    count <- table_data[row_name, col_name]
    cat(sprintf("%-25s: %10.6f\n", "Selected Count", count))
    cat(sprintf("%-25s: %10.6f\n", "Joint Probability", count / total))
    cat(sprintf("%-25s: %9.6f%%\n", "Joint Probability (%)", count / total * 100))
  }
  cat("--------------------------------------------------\n")
}

###############################################################################


###############################################################################
# CHAPTER 7
###############################################################################

# CHAPTER 7 ##################################################################
# NOTE:
# Chapter 7 focuses on discrete random variables and expected value.
###############################################################################
# IDENTIFYING PROBLEM TYPE + STRUCTURE

# DISCRETE PROBABILITY DISTRIBUTION -> discreteDist()
# Look for: x values and P(X = x) values in a table.
#
# EXPECTED VALUE / VARIANCE / STANDARD DEVIATION -> discreteDist()
# Look for: E(X), variance, standard deviation, risk.
#
# PROBABILITY FROM TABLE -> probFromDist()
# Look for: P(X = a), P(X < a), P(X > a), or P(X <= a).

###############################################################################
# IMPORTANT : RUN THESE FUNCTIONS ONCE BEFORE YOU START CALLING THE FUNCTIONS!!!
###############################################################################
# Discrete Probability Distribution
###############################################################################

discreteDist <- function(x, prob) {
  x <- require_numeric_vector(x, "x")
  prob <- require_probability_vector(prob, "Probabilities")
  if (length(x) != length(prob)) {
    stop("x and probabilities must have the same length.")
  }

  valid <- all(prob >= 0 & prob <= 1) && abs(sum(prob) - 1) < 0.000001
  expected <- sum(x * prob)
  variance <- sum((x - expected)^2 * prob)
  sd_val <- sqrt(variance)
  
  cat("--------------------------------------------------\n")
  cat("Discrete Probability Distribution\n")
  cat("--------------------------------------------------\n")
  cat(sprintf("%-25s: %s\n", "Valid Distribution", valid))
  cat(sprintf("%-25s: %10.6f\n", "Sum of Probabilities", sum(prob)))
  cat(sprintf("%-25s: %10.6f\n", "Expected Value", expected))
  cat(sprintf("%-25s: %10.6f\n", "Variance", variance))
  cat(sprintf("%-25s: %10.6f\n", "Standard Deviation", sd_val))
  cat("--------------------------------------------------\n")
}

###############################################################################
# Probability from Discrete Distribution
###############################################################################

probFromDist <- function(x, prob, type = "equal", value) {
  x <- require_numeric_vector(x, "x")
  prob <- require_probability_vector(prob, "Probabilities")
  if (length(x) != length(prob)) {
    stop("x and probabilities must have the same length.")
  }
  value <- require_num(value, "Value")

  # type = "equal", "less", "less_equal", "greater", "greater_equal"
  
  if (type == "equal") {
    ans <- sum(prob[x == value])
  } else if (type == "less") {
    ans <- sum(prob[x < value])
  } else if (type == "less_equal") {
    ans <- sum(prob[x <= value])
  } else if (type == "greater") {
    ans <- sum(prob[x > value])
  } else if (type == "greater_equal") {
    ans <- sum(prob[x >= value])
  } else {
    stop("Unknown probability type.")
  }
  
  cat("--------------------------------------------------\n")
  cat("Probability from Distribution\n")
  cat("--------------------------------------------------\n")
  cat(sprintf("%-25s: %s\n", "Type", type))
  cat(sprintf("%-25s: %10.6f\n", "Value", value))
  cat(sprintf("%-25s: %10.6f\n", "Probability", ans))
  cat(sprintf("%-25s: %9.6f%%\n", "Probability (%)", ans * 100))
  cat("--------------------------------------------------\n")
}

###############################################################################


###############################################################################
# CHAPTER 8
###############################################################################

# CHAPTER 8 ##################################################################
# NOTE:
# Chapter 8 focuses on uniform and normal distributions.
###############################################################################
# IDENTIFYING PROBLEM TYPE + STRUCTURE

# UNIFORM DISTRIBUTION -> uniformDist()
# Look for: "uniformly distributed between a and b".
#
# NORMAL PROBABILITY -> normalProb()
# Look for: normal distribution, mean, standard deviation, area/probability.
#
# Z-SCORE -> zScore()
# Look for: "z value", "standard normal", or convert x to z.
#
# INVERSE NORMAL -> invNormal()
# Look for: percentile, cutoff, or "find the value".

###############################################################################
# IMPORTANT : RUN THESE FUNCTIONS ONCE BEFORE YOU START CALLING THE FUNCTIONS!!!
###############################################################################
# Uniform Distribution
###############################################################################

uniformDist <- function(a, b, lower = NULL, upper = NULL) {
  a <- require_num(a, "a")
  b <- require_num(b, "b")
  if (a >= b) {
    stop("a must be less than b.")
  }
  if (!is.null(lower) && !is.null(upper)) {
    require_between_values(lower, upper)
  }

  mean_val <- (a + b) / 2
  sd_val <- sqrt((b - a)^2 / 12)
  
  cat("--------------------------------------------------\n")
  cat("Uniform Distribution\n")
  cat("--------------------------------------------------\n")
  cat(sprintf("%-25s: %10.6f\n", "Mean", mean_val))
  cat(sprintf("%-25s: %10.6f\n", "Standard Deviation", sd_val))
  
  if (!is.null(lower) && !is.null(upper)) {
    prob <- max(0, min(upper, b) - max(lower, a)) / (b - a)
    cat(sprintf("%-25s: %10.6f\n", "Probability", prob))
    cat(sprintf("%-25s: %9.6f%%\n", "Probability (%)", prob * 100))
  }
  cat("--------------------------------------------------\n")
}

###############################################################################
# Z-Score
###############################################################################

zScore <- function(x, mean_val, sd_val) {
  x <- require_num(x, "x")
  mean_val <- require_num(mean_val, "Mean")
  sd_val <- require_positive_num(sd_val, "Standard deviation")

  z <- (x - mean_val) / sd_val
  
  cat("--------------------------------------------------\n")
  cat("Z-Score\n")
  cat("--------------------------------------------------\n")
  cat(sprintf("%-25s: %10.6f\n", "z", z))
  cat("--------------------------------------------------\n")
}

###############################################################################
# Normal Probability
###############################################################################

normalProb <- function(mean_val = 0, sd_val = 1, lower = -Inf, upper = Inf) {
  mean_val <- require_num(mean_val, "Mean")
  sd_val <- require_positive_num(sd_val, "Standard deviation")
  require_between_values(lower, upper)

  prob <- pnorm(upper, mean_val, sd_val) - pnorm(lower, mean_val, sd_val)
  
  cat("--------------------------------------------------\n")
  cat("Normal Probability\n")
  cat("--------------------------------------------------\n")
  cat(sprintf("%-25s: %10.6f\n", "Mean", mean_val))
  cat(sprintf("%-25s: %10.6f\n", "Standard Deviation", sd_val))
  cat(sprintf("%-25s: %10.6f\n", "Probability", prob))
  cat(sprintf("%-25s: %9.6f%%\n", "Probability (%)", prob * 100))
  cat("--------------------------------------------------\n")
}

###############################################################################
# Inverse Normal
###############################################################################

invNormal <- function(prob_left, mean_val = 0, sd_val = 1) {
  prob_left <- require_probability_num(prob_left, "Area left", allow_zero_one = FALSE)
  mean_val <- require_num(mean_val, "Mean")
  sd_val <- require_positive_num(sd_val, "Standard deviation")

  value <- qnorm(prob_left, mean_val, sd_val)
  
  cat("--------------------------------------------------\n")
  cat("Inverse Normal\n")
  cat("--------------------------------------------------\n")
  cat(sprintf("%-25s: %10.6f\n", "Area Left", prob_left))
  cat(sprintf("%-25s: %9.6f%%\n", "Area Left (%)", prob_left * 100))
  cat(sprintf("%-25s: %10.6f\n", "Value", value))
  cat("--------------------------------------------------\n")
}

###############################################################################


###############################################################################
# CHAPTER 9
###############################################################################

# CHAPTER 9 ##################################################################
# NOTE:
# Chapter 9 focuses on sampling distributions.
###############################################################################
# IDENTIFYING PROBLEM TYPE + STRUCTURE

# SAMPLE MEAN -> sampleMeanDist()
# Look for: sample mean, x-bar, mean of sampling distribution.
#
# SAMPLE PROPORTION -> samplePropDist()
# Look for: sample proportion, p-hat, proportion in a sample.
#
# PROBABILITY FOR X-BAR -> sampleMeanProb()
# Look for: probability that sample mean is above/below a value.
#
# PROBABILITY FOR P-HAT -> samplePropProb()
# Look for: probability that sample proportion differs from p.

###############################################################################
# IMPORTANT : RUN THESE FUNCTIONS ONCE BEFORE YOU START CALLING THE FUNCTIONS!!!
###############################################################################
# Sampling Distribution of Sample Mean
###############################################################################

sampleMeanDist <- function(mu, sigma, n) {
  mu <- require_num(mu, "Mean")
  sigma <- require_positive_num(sigma, "sigma")
  n <- require_whole_num(n, "n", 1)

  mean_xbar <- mu
  se <- sigma / sqrt(n)
  
  cat("--------------------------------------------------\n")
  cat("Sampling Distribution of Sample Mean\n")
  cat("--------------------------------------------------\n")
  cat(sprintf("%-25s: %10.6f\n", "Mean of xbar", mean_xbar))
  cat(sprintf("%-25s: %10.6f\n", "Standard Error", se))
  cat("--------------------------------------------------\n")
}

###############################################################################
# Probability for Sample Mean
###############################################################################

sampleMeanProb <- function(mu, sigma, n, lower = -Inf, upper = Inf) {
  mu <- require_num(mu, "Mean")
  sigma <- require_positive_num(sigma, "sigma")
  n <- require_whole_num(n, "n", 1)
  require_between_values(lower, upper)

  se <- sigma / sqrt(n)
  prob <- pnorm(upper, mu, se) - pnorm(lower, mu, se)
  
  cat("--------------------------------------------------\n")
  cat("Probability for Sample Mean\n")
  cat("--------------------------------------------------\n")
  cat(sprintf("%-25s: %10.6f\n", "Standard Error", se))
  cat(sprintf("%-25s: %10.6f\n", "Probability", prob))
  cat(sprintf("%-25s: %9.6f%%\n", "Probability (%)", prob * 100))
  cat("--------------------------------------------------\n")
}

###############################################################################
# Sampling Distribution of Sample Proportion
###############################################################################

samplePropDist <- function(p, n) {
  p <- require_probability_num(p, "Proportion")
  n <- require_whole_num(n, "n", 1)

  mean_phat <- p
  se <- sqrt(p * (1 - p) / n)
  
  cat("--------------------------------------------------\n")
  cat("Sampling Distribution of Sample Proportion\n")
  cat("--------------------------------------------------\n")
  cat(sprintf("%-25s: %10.6f\n", "Mean of phat", mean_phat))
  cat(sprintf("%-25s: %9.6f%%\n", "Mean of phat (%)", mean_phat * 100))
  cat(sprintf("%-25s: %10.6f\n", "Standard Error", se))
  cat("--------------------------------------------------\n")
}

###############################################################################
# Probability for Sample Proportion
###############################################################################

samplePropProb <- function(p, n, lower = -Inf, upper = Inf) {
  p <- require_probability_num(p, "Proportion")
  n <- require_whole_num(n, "n", 1)
  require_between_values(lower, upper)

  se <- sqrt(p * (1 - p) / n)
  prob <- pnorm(upper, p, se) - pnorm(lower, p, se)
  
  cat("--------------------------------------------------\n")
  cat("Probability for Sample Proportion\n")
  cat("--------------------------------------------------\n")
  cat(sprintf("%-25s: %10.6f\n", "Standard Error", se))
  cat(sprintf("%-25s: %10.6f\n", "Probability", prob))
  cat(sprintf("%-25s: %9.6f%%\n", "Probability (%)", prob * 100))
  cat("--------------------------------------------------\n")
}

###############################################################################


###############################################################################
# CHAPTER 10
###############################################################################

# CHAPTER 10 #################################################################
# NOTE:
# Chapter 10 focuses on confidence intervals and point estimates.
###############################################################################
# IDENTIFYING PROBLEM TYPE + STRUCTURE

# ONE PROPORTION CI -> onePropCI()
# Look for: sample proportion, successes, percent, confidence interval.
#
# ONE MEAN CI KNOWN SIGMA -> meanKnownSigmaCI()
# Look for: population standard deviation or sigma is known.
#
# ONE MEAN CI UNKNOWN SIGMA -> meanTCI()
# Look for: sample standard deviation s.
#
# VARIANCE / STANDARD DEVIATION CI -> varianceCI()
# Look for: confidence interval for population variance or standard deviation.

###############################################################################
# IMPORTANT : RUN THESE FUNCTIONS ONCE BEFORE YOU START CALLING THE FUNCTIONS!!!
###############################################################################
# One Proportion Confidence Interval
###############################################################################

onePropCI <- function(x, n, conf_level = 0.95) {
  x <- require_whole_num(x, "Successes x", 0)
  n <- require_whole_num(n, "n", 1)
  conf_level <- require_conf_level(conf_level)
  if (x > n) {
    stop("Successes x cannot be larger than n.")
  }

  phat <- x / n
  alpha <- 1 - conf_level
  z <- qnorm(1 - alpha / 2)
  se <- sqrt(phat * (1 - phat) / n)
  me <- z * se
  lower <- phat - me
  upper <- phat + me

  cat("--------------------------------------------------\n")
  cat("One Proportion Confidence Interval\n")
  cat("--------------------------------------------------\n")
  cat(sprintf("%-25s: %10.6f\n", "phat", phat))
  cat(sprintf("%-25s: %9.6f%%\n", "phat (%)", phat * 100))
  cat(sprintf("%-25s: %10.6f\n", "z Critical", z))
  cat(sprintf("%-25s: %10.6f\n", "Margin of Error", me))
  cat(sprintf("%-25s: %10.6f\n", "Lower Endpoint", lower))
  cat(sprintf("%-25s: %10.6f\n", "Upper Endpoint", upper))
  cat(sprintf("%-25s: %9.6f%%\n", "Lower Endpoint (%)", lower * 100))
  cat(sprintf("%-25s: %9.6f%%\n", "Upper Endpoint (%)", upper * 100))
  cat("--------------------------------------------------\n")

  invisible(calculator_result(
    TRUE,
    values = list(
      successes = x,
      n = n,
      estimate = phat,
      percent = phat * 100,
      confidence_level = conf_level,
      alpha = alpha,
      z_critical = z,
      standard_error = se,
      margin_error = me,
      lower_endpoint = lower,
      upper_endpoint = upper,
      lower_percent = lower * 100,
      upper_percent = upper * 100
    ),
    metadata = list(calculator = "onePropCI")
  ))
}

###############################################################################
# Mean Confidence Interval with Known Sigma
###############################################################################

meanKnownSigmaCI <- function(n, xbar, sigma, conf_level = 0.95) {
  n <- require_whole_num(n, "n", 1)
  xbar <- require_num(xbar, "xbar")
  sigma <- require_positive_num(sigma, "sigma")
  conf_level <- require_conf_level(conf_level)

  alpha <- 1 - conf_level
  z <- qnorm(1 - alpha / 2)
  se <- sigma / sqrt(n)
  me <- z * se
  lower <- xbar - me
  upper <- xbar + me

  cat("--------------------------------------------------\n")
  cat("Mean CI (Known Sigma)\n")
  cat("--------------------------------------------------\n")
  cat(sprintf("%-25s: %10.6f\n", "z Critical", z))
  cat(sprintf("%-25s: %10.6f\n", "Standard Error", se))
  cat(sprintf("%-25s: %10.6f\n", "Margin of Error", me))
  cat(sprintf("%-25s: %10.6f\n", "Lower Endpoint", lower))
  cat(sprintf("%-25s: %10.6f\n", "Upper Endpoint", upper))
  cat("--------------------------------------------------\n")

  invisible(calculator_result(
    TRUE,
    values = list(
      n = n,
      mean = xbar,
      sigma = sigma,
      confidence_level = conf_level,
      alpha = alpha,
      z_critical = z,
      standard_error = se,
      margin_error = me,
      lower_endpoint = lower,
      upper_endpoint = upper
    ),
    metadata = list(calculator = "meanKnownSigmaCI")
  ))
}

###############################################################################
# Mean Confidence Interval with Unknown Sigma
###############################################################################

meanTCI <- function(n, xbar, s, conf_level = 0.95) {
  n <- require_whole_num(n, "n", 2)
  xbar <- require_num(xbar, "xbar")
  s <- require_positive_num(s, "s")
  conf_level <- require_conf_level(conf_level)

  alpha <- 1 - conf_level
  df <- n - 1
  tcrit <- qt(1 - alpha / 2, df)
  se <- s / sqrt(n)
  me <- tcrit * se
  lower <- xbar - me
  upper <- xbar + me

  cat("--------------------------------------------------\n")
  cat("Mean CI (Unknown Sigma / t)\n")
  cat("--------------------------------------------------\n")
  cat(sprintf("%-25s: %10.6f\n", "Degrees of Freedom", df))
  cat(sprintf("%-25s: %10.6f\n", "t Critical", tcrit))
  cat(sprintf("%-25s: %10.6f\n", "Standard Error", se))
  cat(sprintf("%-25s: %10.6f\n", "Margin of Error", me))
  cat(sprintf("%-25s: %10.6f\n", "Lower Endpoint", lower))
  cat(sprintf("%-25s: %10.6f\n", "Upper Endpoint", upper))
  cat("--------------------------------------------------\n")

  invisible(calculator_result(
    TRUE,
    values = list(
      n = n,
      mean = xbar,
      sample_sd = s,
      confidence_level = conf_level,
      alpha = alpha,
      df = df,
      t_critical = tcrit,
      standard_error = se,
      margin_error = me,
      lower_endpoint = lower,
      upper_endpoint = upper
    ),
    metadata = list(calculator = "meanTCI")
  ))
}

###############################################################################
# Variance / Standard Deviation Confidence Interval
###############################################################################

varianceCI <- function(n, s, conf_level = 0.95) {
  n <- require_whole_num(n, "n", 2)
  s <- require_nonnegative_num(s, "s")
  conf_level <- require_conf_level(conf_level)

  alpha <- 1 - conf_level
  df <- n - 1
  lower_var <- df * s^2 / qchisq(1 - alpha / 2, df)
  upper_var <- df * s^2 / qchisq(alpha / 2, df)

  cat("--------------------------------------------------\n")
  cat("Variance / Standard Deviation CI\n")
  cat("--------------------------------------------------\n")
  cat(sprintf("%-25s: %10.6f\n", "Degrees of Freedom", df))
  cat(sprintf("%-25s: %10.6f\n", "Variance Lower", lower_var))
  cat(sprintf("%-25s: %10.6f\n", "Variance Upper", upper_var))
  cat(sprintf("%-25s: %10.6f\n", "SD Lower", sqrt(lower_var)))
  cat(sprintf("%-25s: %10.6f\n", "SD Upper", sqrt(upper_var)))
  cat("--------------------------------------------------\n")

  invisible(calculator_result(
    TRUE,
    values = list(
      n = n,
      sample_sd = s,
      confidence_level = conf_level,
      alpha = alpha,
      df = df,
      variance_lower = lower_var,
      variance_upper = upper_var,
      sd_lower = sqrt(lower_var),
      sd_upper = sqrt(upper_var)
    ),
    metadata = list(calculator = "varianceCI")
  ))
}

###############################################################################


###############################################################################
# CHAPTER 11
###############################################################################

# CHAPTER 11 #################################################################
# NOTE:
# Chapter 11 focuses on one-sample hypothesis tests.
###############################################################################
# IDENTIFYING PROBLEM TYPE + STRUCTURE

# ONE MEAN KNOWN SIGMA -> meanKnownSigmaTest()
# Look for: population standard deviation sigma is known.
#
# ONE MEAN UNKNOWN SIGMA -> meanTTest()
# Look for: sample standard deviation s.
#
# ONE PROPORTION -> onePropTest()
# Look for: count of successes, sample size, claim about a percent/proportion.
#
# TAIL RULE
# "different" / "not equal" -> alternative = "two.sided"
# "less than" / "below" -> alternative = "less"
# "greater than" / "above" -> alternative = "greater"

###############################################################################
# IMPORTANT : RUN THESE FUNCTIONS ONCE BEFORE YOU START CALLING THE FUNCTIONS!!!
###############################################################################
# One Mean Test with Known Sigma
###############################################################################

meanKnownSigmaTest <- function(n, xbar, mu0, sigma, alpha = 0.05,
                               alternative = "two.sided") {
  n <- require_whole_num(n, "n", 1)
  xbar <- require_num(xbar, "xbar")
  mu0 <- require_num(mu0, "Hypothesized mean")
  sigma <- require_positive_num(sigma, "sigma")
  alpha <- require_alpha(alpha)

  se <- sigma / sqrt(n)
  z <- (xbar - mu0) / se

  if (alternative == "two.sided") {
    p_value <- 2 * pnorm(-abs(z))
    critical <- qnorm(1 - alpha / 2)
  } else if (alternative == "less") {
    p_value <- pnorm(z)
    critical <- qnorm(alpha)
  } else if (alternative == "greater") {
    p_value <- pnorm(z, lower.tail = FALSE)
    critical <- qnorm(1 - alpha)
  } else {
    stop("Unknown alternative.")
  }

  decision <- ifelse(p_value < alpha, "Reject", "Fail to Reject")

  cat("--------------------------------------------------\n")
  cat("One Mean z Test (Known Sigma)\n")
  cat("--------------------------------------------------\n")
  cat(sprintf("%-25s: %10.6f\n", "Standard Error", se))
  cat(sprintf("%-25s: %10.6f\n", "Critical Value", critical))
  cat(sprintf("%-25s: %10.6f\n", "Test Statistic", z))
  cat(sprintf("%-25s: %10.6f\n", "P-value", p_value))
  cat(sprintf("%-25s: %s\n", "Decision", decision))
  cat("--------------------------------------------------\n")

  invisible(calculator_result(
    TRUE,
    values = list(
      n = n,
      mean = xbar,
      hypothesized_mean = mu0,
      sigma = sigma,
      alpha = alpha,
      alternative = alternative,
      standard_error = se,
      critical_value = critical,
      statistic = z,
      p_value = p_value,
      decision = decision
    ),
    metadata = list(calculator = "meanKnownSigmaTest")
  ))
}

###############################################################################
# One Mean t Test
###############################################################################

meanTTest <- function(n, xbar, mu0, s, alpha = 0.05,
                      alternative = "two.sided") {
  n <- require_whole_num(n, "n", 2)
  xbar <- require_num(xbar, "xbar")
  mu0 <- require_num(mu0, "Hypothesized mean")
  s <- require_positive_num(s, "s")
  alpha <- require_alpha(alpha)

  df <- n - 1
  se <- s / sqrt(n)
  t_stat <- (xbar - mu0) / se

  if (alternative == "two.sided") {
    p_value <- 2 * pt(-abs(t_stat), df)
    critical <- qt(1 - alpha / 2, df)
  } else if (alternative == "less") {
    p_value <- pt(t_stat, df)
    critical <- qt(alpha, df)
  } else if (alternative == "greater") {
    p_value <- pt(t_stat, df, lower.tail = FALSE)
    critical <- qt(1 - alpha, df)
  } else {
    stop("Unknown alternative.")
  }

  decision <- ifelse(p_value < alpha, "Reject", "Fail to Reject")

  cat("--------------------------------------------------\n")
  cat("One Mean t Test\n")
  cat("--------------------------------------------------\n")
  cat(sprintf("%-25s: %10.6f\n", "Degrees of Freedom", df))
  cat(sprintf("%-25s: %10.6f\n", "Standard Error", se))
  cat(sprintf("%-25s: %10.6f\n", "Critical Value", critical))
  cat(sprintf("%-25s: %10.6f\n", "Test Statistic", t_stat))
  cat(sprintf("%-25s: %10.6f\n", "P-value", p_value))
  cat(sprintf("%-25s: %s\n", "Decision", decision))
  cat("--------------------------------------------------\n")

  invisible(calculator_result(
    TRUE,
    values = list(
      n = n,
      mean = xbar,
      hypothesized_mean = mu0,
      sample_sd = s,
      alpha = alpha,
      alternative = alternative,
      df = df,
      standard_error = se,
      critical_value = critical,
      statistic = t_stat,
      p_value = p_value,
      decision = decision
    ),
    metadata = list(calculator = "meanTTest")
  ))
}

###############################################################################
# One Proportion Test
###############################################################################

onePropTest <- function(x, n, p0, alpha = 0.05,
                        alternative = "two.sided") {
  x <- require_whole_num(x, "Successes x", 0)
  n <- require_whole_num(n, "n", 1)
  p0 <- require_probability_num(p0, "Hypothesized proportion", allow_zero_one = FALSE)
  alpha <- require_alpha(alpha)
  if (x > n) {
    stop("Successes x cannot be larger than n.")
  }

  phat <- x / n
  se <- sqrt(p0 * (1 - p0) / n)
  z <- (phat - p0) / se

  if (alternative == "two.sided") {
    p_value <- 2 * pnorm(-abs(z))
    critical <- qnorm(1 - alpha / 2)
  } else if (alternative == "less") {
    p_value <- pnorm(z)
    critical <- qnorm(alpha)
  } else if (alternative == "greater") {
    p_value <- pnorm(z, lower.tail = FALSE)
    critical <- qnorm(1 - alpha)
  } else {
    stop("Unknown alternative.")
  }

  decision <- ifelse(p_value < alpha, "Reject", "Fail to Reject")

  cat("--------------------------------------------------\n")
  cat("One Proportion z Test\n")
  cat("--------------------------------------------------\n")
  cat(sprintf("%-25s: %10.6f\n", "phat", phat))
  cat(sprintf("%-25s: %9.6f%%\n", "phat (%)", phat * 100))
  cat(sprintf("%-25s: %10.6f\n", "Standard Error", se))
  cat(sprintf("%-25s: %10.6f\n", "Critical Value", critical))
  cat(sprintf("%-25s: %10.6f\n", "Test Statistic", z))
  cat(sprintf("%-25s: %10.6f\n", "P-value", p_value))
  cat(sprintf("%-25s: %s\n", "Decision", decision))
  cat("--------------------------------------------------\n")

  invisible(calculator_result(
    TRUE,
    values = list(
      successes = x,
      n = n,
      estimate = phat,
      percent = phat * 100,
      hypothesized_proportion = p0,
      alpha = alpha,
      alternative = alternative,
      standard_error = se,
      critical_value = critical,
      statistic = z,
      p_value = p_value,
      decision = decision
    ),
    metadata = list(calculator = "onePropTest")
  ))
}

###############################################################################


###############################################################################
# CHAPTER 12
###############################################################################

# CHAPTER 12 ##################################################################
# NOTE:
# The data does NOT determine the tail of the test.
# The tail is determined by the wording of the claim (alternative hypothesis).
#
# Use this rule:
# "different" / "not equal"  → alternative = "two.sided"
# "greater than" / "higher" → alternative = "greater"
# "less than" / "lower"     → alternative = "less"
#
# Always identify the claim first, then set the tail accordingly.
###############################################################################
# IDENTIFYING PROBLEM TYPE + STRUCTURE

# TWO PROPORTIONS → twoIndSampleProp()
# Look for:
# "proportion", "percentage", "rate"
# "success/failure", "yes/no"
# "number of people who..."
#
# TWO MEANS (KNOWN σ) → twoIndMeansKnownSigma()
# Look for:
# "population standard deviation", "σ is known"
#
# TWO MEANS (UNEQUAL VARIANCES) → twoIndMeanUneqVar()
# Look for:
# "sample standard deviation", "s"
# OR nothing stated about variances
#
# TWO MEANS (EQUAL VARIANCES) → twoIndMeansEqVar()
# Look for:
# "assume equal variances", "variances are equal"

###############################################################################
# COMMON PROBLEM STRUCTURE
###############################################################################

# Hypothesis Test:
# Step 1: State H0 and Ha
# Step 2: Compute test statistic
# Step 3: Find P-value
# Step 4: Decision (Reject / Fail to Reject)
# Step 5: Interpretation (sufficient / insufficient evidence)

# Confidence Interval:
# Step 1: Find critical value
# Step 2: Compute standard error
# Step 3: Construct interval

###############################################################################
# IMPORTANT : RUN THESE FUNCTIONS ONCE BEFORE YOU START CALLING THE FUNCTIONS!!!
###############################################################################
# Two independent Means Known Sigma
###############################################################################
twoIndMeansKnownSigma <- function(n1, x1, sigma1, n2, x2, sigma2, conf_level = NULL, alpha = NULL, alternative = "two.sided") {
  # n1 = sample size for population 1
  # x1 = sample mean for population 1
  # sigma1 = population standard deviation for population 1
  # n2 = sample size for population 2
  # x2 = sample mean for population 2
  # sigma2 = population standard deviation for population 2
  # conf_level = confidence level
  # alpha = significance level
  # alternative = "two.sided", "less", or "greater"
  n1 <- require_whole_num(n1, "n1", 1)
  n2 <- require_whole_num(n2, "n2", 1)
  x1 <- require_num(x1, "x1")
  x2 <- require_num(x2, "x2")
  sigma1 <- require_positive_num(sigma1, "sigma1")
  sigma2 <- require_positive_num(sigma2, "sigma2")
  levels <- resolve_conf_alpha(conf_level, alpha)
  conf_level <- levels$conf_level
  alpha <- levels$alpha

  diff_mean <- x1 - x2
  stnd_err <- sqrt((sigma1^2 / n1) + (sigma2^2 / n2))
  z_stat <- diff_mean / stnd_err
  if (alternative == "two.sided") {
    z_critical <- qnorm(1 - alpha / 2)
    p_value <- 2 * pnorm(-abs(z_stat))
  } else if (alternative == "less") {
    z_critical <- qnorm(alpha)
    p_value <- pnorm(z_stat)
  } else if (alternative == "greater") {
    z_critical <- qnorm(1 - alpha)
    p_value <- pnorm(z_stat, lower.tail = FALSE)
  } else {
    stop("Unknown alternative.")
  }
  marg_err <- qnorm(1 - alpha / 2) * stnd_err
  lower <- diff_mean - marg_err
  upper <- diff_mean + marg_err
  if (p_value < alpha) {
    decision <- "Reject"
    evidence <- "sufficient evidence"
  } else {
    decision <- "Fail to Reject"
    evidence <- "insufficient evidence"
  }
  cat("H0: mu1 - mu2 = 0\n")
  if (alternative == "two.sided") {
    cat("Ha: mu1 - mu2 != 0\n")
  } else if (alternative == "less") {
    cat("Ha: mu1 - mu2 < 0\n")
  } else {
    cat("Ha: mu1 - mu2 > 0\n")
  }
  cat("--------------------------------------------------\n")
  cat("Two Independent Means (Known Population Standard Deviations)\n")
  cat("--------------------------------------------------\n")
  cat(sprintf("%-25s: %10.6f\n", "Difference in Means", diff_mean))
  cat(sprintf("%-25s: %10.6f\n", "Standard Error", stnd_err))
  cat(sprintf("%-25s: %10.6f\n", "z Critical", z_critical))
  cat(sprintf("%-25s: %10.6f\n", "Test Statistic", z_stat))
  cat(sprintf("%-25s: %10.6f\n", "P-value", p_value))
  cat("--------------------------------------------------\n")
  cat(sprintf("%-25s: %10.6f\n", "Margin of Error", marg_err))
  cat(sprintf("%-25s: %10.6f\n", "Lower Endpoint", lower))
  cat(sprintf("%-25s: %10.6f\n", "Upper Endpoint", upper))
  cat("--------------------------------------------------\n")
  cat(sprintf("%-25s: %s (%s)\n", "Decision", decision, evidence))
  cat("--------------------------------------------------\n")
  print_two_sample_ci_interpretation(conf_level, lower, upper, digits = 2)

  invisible(calculator_result(
    TRUE,
    values = list(
      n1 = n1,
      n2 = n2,
      mean1 = x1,
      mean2 = x2,
      sigma1 = sigma1,
      sigma2 = sigma2,
      confidence_level = conf_level,
      alpha = alpha,
      alternative = alternative,
      difference = diff_mean,
      standard_error = stnd_err,
      critical_value = z_critical,
      statistic = z_stat,
      p_value = p_value,
      margin_error = marg_err,
      lower_endpoint = lower,
      upper_endpoint = upper,
      decision = decision,
      evidence = evidence
    ),
    metadata = list(calculator = "twoIndMeansKnownSigma")
  ))
}

###############################################################################
# Two Independent Means Unequal Variances
###############################################################################

twoIndMeanUneqVar <- function(n1, x1, s1, n2, x2, s2,
                              conf_level = NULL, alpha = NULL,
                              alternative = "two.sided") {
  # n1 = sample size for population 1
  # x1 = sample mean for population 1
  # s1 = sample standard deviation for population 1
  # n2 = sample size for population 2
  # x2 = sample mean for population 2
  # s2 = sample standard deviation for population 2
  # alternative = "two.sided", "less", or "greater"
  n1 <- require_whole_num(n1, "n1", 2)
  n2 <- require_whole_num(n2, "n2", 2)
  x1 <- require_num(x1, "x1")
  x2 <- require_num(x2, "x2")
  s1 <- require_positive_num(s1, "s1")
  s2 <- require_positive_num(s2, "s2")
  levels <- resolve_conf_alpha(conf_level, alpha)
  conf_level <- levels$conf_level
  alpha <- levels$alpha

  diff_mean <- x1 - x2
  stnd_err <- sqrt((s1^2 / n1) + (s2^2 / n2))
  t_stat <- diff_mean / stnd_err

  df_num <- ((s1^2 / n1) + (s2^2 / n2))^2
  df_den <- (((s1^2 / n1)^2) / (n1 - 1)) + (((s2^2 / n2)^2) / (n2 - 1))
  df <- df_num / df_den

  if (alternative == "two.sided") {
    t_critical <- qt(1 - alpha / 2, df)
    p_value <- 2 * pt(-abs(t_stat), df)
  } else if (alternative == "less") {
    t_critical <- qt(alpha, df)
    p_value <- pt(t_stat, df)
  } else if (alternative == "greater") {
    t_critical <- qt(1 - alpha, df)
    p_value <- pt(t_stat, df, lower.tail = FALSE)
  } else {
    stop("Unknown alternative.")
  }

  marg_err <- qt(1 - alpha / 2, df) * stnd_err
  lower <- diff_mean - marg_err
  upper <- diff_mean + marg_err

  if (p_value < alpha) {
    decision <- "Reject"
    evidence <- "sufficient evidence"
  } else {
    decision <- "Fail to Reject"
    evidence <- "insufficient evidence"
  }

  cat("--------------------------------------------------\n")
  cat("Two Independent Means (Unequal Variances)\n")
  cat("--------------------------------------------------\n")

  cat("H0: mu1 - mu2 = 0\n")
  if (alternative == "two.sided") {
    cat("Ha: mu1 - mu2 != 0\n")
  } else if (alternative == "less") {
    cat("Ha: mu1 - mu2 < 0\n")
  } else {
    cat("Ha: mu1 - mu2 > 0\n")
  }

  cat("--------------------------------------------------\n")

  cat(sprintf("%-25s: %10.6f\n", "Difference in Means", diff_mean))
  cat(sprintf("%-25s: %10.6f\n", "Standard Error", stnd_err))
  cat(sprintf("%-25s: %10.6f\n", "Degrees of Freedom", df))
  cat(sprintf("%-25s: %10.6f\n", "t Critical", t_critical))
  cat(sprintf("%-25s: %10.6f\n", "Test Statistic", t_stat))
  cat(sprintf("%-25s: %10.6f\n", "P-value", p_value))

  cat("--------------------------------------------------\n")

  cat(sprintf("%-25s: %10.6f\n", "Margin of Error", marg_err))
  cat(sprintf("%-25s: %10.6f\n", "Lower Endpoint", lower))
  cat(sprintf("%-25s: %10.6f\n", "Upper Endpoint", upper))

  cat("--------------------------------------------------\n")

  cat(sprintf("%-25s: %s (%s)\n", "Decision", decision, evidence))
  cat("--------------------------------------------------\n")
  print_two_sample_ci_interpretation(conf_level, lower, upper, digits = 2, subject = "the mean for ")

  invisible(calculator_result(
    TRUE,
    values = list(
      n1 = n1,
      n2 = n2,
      mean1 = x1,
      mean2 = x2,
      sample_sd1 = s1,
      sample_sd2 = s2,
      confidence_level = conf_level,
      alpha = alpha,
      alternative = alternative,
      difference = diff_mean,
      standard_error = stnd_err,
      df = df,
      critical_value = t_critical,
      statistic = t_stat,
      p_value = p_value,
      margin_error = marg_err,
      lower_endpoint = lower,
      upper_endpoint = upper,
      decision = decision,
      evidence = evidence
    ),
    metadata = list(calculator = "twoIndMeanUneqVar")
  ))
}

###############################################################################
# Two Independent Means Equal Variances
###############################################################################

twoIndMeansEqVar <- function(n1, x1, s1, n2, x2, s2,
                             conf_level = NULL, alpha = NULL,
                             alternative = "two.sided") {
  # n1 = sample size for population 1
  # x1 = sample mean for population 1
  # s1 = sample standard deviation for population 1
  # n2 = sample size for population 2
  # x2 = sample mean for population 2
  # s2 = sample standard deviation for population 2
  # conf_level = confidence level
  # alpha = significance level
  # alternative = "two.sided", "less", or "greater"
  n1 <- require_whole_num(n1, "n1", 2)
  n2 <- require_whole_num(n2, "n2", 2)
  x1 <- require_num(x1, "x1")
  x2 <- require_num(x2, "x2")
  s1 <- require_positive_num(s1, "s1")
  s2 <- require_positive_num(s2, "s2")
  levels <- resolve_conf_alpha(conf_level, alpha)
  conf_level <- levels$conf_level
  alpha <- levels$alpha

  diff_mean <- x1 - x2

  sp2 <- (((n1 - 1) * s1^2) + ((n2 - 1) * s2^2)) / (n1 + n2 - 2)
  stnd_err <- sqrt(sp2 * (1 / n1 + 1 / n2))
  df <- n1 + n2 - 2
  t_stat <- diff_mean / stnd_err

  if (alternative == "two.sided") {
    t_critical <- qt(1 - alpha / 2, df)
    p_value <- 2 * pt(-abs(t_stat), df)
  } else if (alternative == "less") {
    t_critical <- qt(alpha, df)
    p_value <- pt(t_stat, df)
  } else if (alternative == "greater") {
    t_critical <- qt(1 - alpha, df)
    p_value <- pt(t_stat, df, lower.tail = FALSE)
  } else {
    stop("Unknown alternative.")
  }

  marg_err <- qt(1 - alpha / 2, df) * stnd_err
  lower <- diff_mean - marg_err
  upper <- diff_mean + marg_err

  if (p_value < alpha) {
    decision <- "Reject"
    evidence <- "sufficient evidence"
  } else {
    decision <- "Fail to Reject"
    evidence <- "insufficient evidence"
  }

  cat("--------------------------------------------------\n")
  cat("Two Independent Means (Equal Variances)\n")
  cat("--------------------------------------------------\n")

  cat("H0: mu1 - mu2 = 0\n")
  if (alternative == "two.sided") {
    cat("Ha: mu1 - mu2 != 0\n")
  } else if (alternative == "less") {
    cat("Ha: mu1 - mu2 < 0\n")
  } else {
    cat("Ha: mu1 - mu2 > 0\n")
  }

  cat("--------------------------------------------------\n")

  cat(sprintf("%-25s: %10.6f\n", "Difference in Means", diff_mean))
  cat(sprintf("%-25s: %10.6f\n", "Pooled Variance", sp2))
  cat(sprintf("%-25s: %10.6f\n", "Standard Error", stnd_err))
  cat(sprintf("%-25s: %10.6f\n", "Degrees of Freedom", df))
  cat(sprintf("%-25s: %10.6f\n", "t Critical", t_critical))
  cat(sprintf("%-25s: %10.6f\n", "Test Statistic", t_stat))
  cat(sprintf("%-25s: %10.6f\n", "P-value", p_value))

  cat("--------------------------------------------------\n")

  cat(sprintf("%-25s: %10.6f\n", "Margin of Error", marg_err))
  cat(sprintf("%-25s: %10.6f\n", "Lower Endpoint", lower))
  cat(sprintf("%-25s: %10.6f\n", "Upper Endpoint", upper))

  cat("--------------------------------------------------\n")

  cat(sprintf("%-25s: %s (%s)\n", "Decision", decision, evidence))
  cat("--------------------------------------------------\n")
  print_two_sample_ci_interpretation(conf_level, lower, upper, digits = 2)

  invisible(calculator_result(
    TRUE,
    values = list(
      n1 = n1,
      n2 = n2,
      mean1 = x1,
      mean2 = x2,
      sample_sd1 = s1,
      sample_sd2 = s2,
      confidence_level = conf_level,
      alpha = alpha,
      alternative = alternative,
      difference = diff_mean,
      pooled_variance = sp2,
      standard_error = stnd_err,
      df = df,
      critical_value = t_critical,
      statistic = t_stat,
      p_value = p_value,
      margin_error = marg_err,
      lower_endpoint = lower,
      upper_endpoint = upper,
      decision = decision,
      evidence = evidence
    ),
    metadata = list(calculator = "twoIndMeansEqVar")
  ))
}

################################################################################


###############################################################################
# CHAPTER 16
###############################################################################

# CHAPTER 16 ##################################################################
# NOTE:
# Chapter 16 is mostly CHI-SQUARE problems.
#
# Chi-square tests always use the RIGHT tail.
# The test statistic is always positive or zero.
#
# Use this rule:
# "fits a distribution", "equal proportions", "specified proportions"
#     -> chiSqGoodnessFit()
#
# "association", "relationship", "independent", "dependent"
#     -> chiSqIndependence()
#
# "same distribution", "homogeneous", "different groups have same proportions"
#     -> chiSqHomogeneity()
#
# "critical value", "degrees of freedom", "area to the right"
#     -> chiSqCriticalValue()
#
# "difference between two proportions", "west side vs east side"
#     -> twoIndSampleProp()
###############################################################################
# IDENTIFYING PROBLEM TYPE + STRUCTURE

# GOODNESS OF FIT -> chiSqGoodnessFit()
# Look for:
# one categorical variable
# one row of observed counts
# expected proportions are equal OR specified in the problem
#
# TEST OF INDEPENDENCE -> chiSqIndependence()
# Look for:
# one sample from one population
# two categorical variables in one contingency table
# wording asks if there is an association / relationship
#
# TEST OF HOMOGENEITY -> chiSqHomogeneity()
# Look for:
# multiple populations or groups being compared
# one categorical variable measured across the groups
# wording asks if the distributions/proportions are the same
#
# TWO PROPORTIONS -> twoIndSampleProp()
# Look for:
# two groups
# yes/no responses
# confidence interval for the difference in proportions

###############################################################################
# COMMON PROBLEM STRUCTURE
###############################################################################

# Hypothesis Test:
# Step 1: State H0 and Ha
# Step 2: Find expected counts
# Step 3: Compute chi-square test statistic
# Step 4: Find P-value and/or critical value
# Step 5: Decision (Reject / Fail to Reject)
# Step 6: Interpretation (sufficient / insufficient evidence)

# Important conditions:
# Observed counts must be counts, not percentages.
# Expected counts should usually be at least 5.
# For chi-square tests, df depends on the problem type.

###############################################################################
# IMPORTANT : RUN THESE FUNCTIONS ONCE BEFORE YOU START CALLING THE FUNCTIONS!!!
###############################################################################
# Formatted Table Printer
###############################################################################

printBarTable <- function(table_data, digits = 6) {
  # table_data = matrix or table to print with | separators
  # digits = number of decimal places for numeric values
  
  table_data <- as.matrix(table_data)
  
  if (is.null(rownames(table_data))) {
    rownames(table_data) <- paste("Row", seq_len(nrow(table_data)))
  }
  if (is.null(colnames(table_data))) {
    colnames(table_data) <- paste("Column", seq_len(ncol(table_data)))
  }
  
  row_labels <- substr(rownames(table_data), 1, 12)
  col_labels <- substr(colnames(table_data), 1, 12)
  
  row_width <- max(nchar(c("", row_labels))) + 2
  col_widths <- sapply(seq_len(ncol(table_data)), function(j) {
    formatted_values <- sprintf(paste0("%.", digits, "f"), as.numeric(table_data[, j]))
    max(nchar(c(col_labels[j], formatted_values))) + 2
  })
  
  cat(sprintf(paste0("%-", row_width, "s"), ""))
  for (j in seq_len(ncol(table_data))) {
    cat("|", sprintf(paste0("%", col_widths[j], "s"), col_labels[j]))
  }
  cat("\n")
  
  total_width <- row_width + sum(col_widths) + ncol(table_data)
  cat(paste(rep("-", total_width), collapse = ""), "\n", sep = "")
  
  for (i in seq_len(nrow(table_data))) {
    cat(sprintf(paste0("%-", row_width, "s"), row_labels[i]))
    for (j in seq_len(ncol(table_data))) {
      value <- sprintf(paste0("%.", digits, "f"), as.numeric(table_data[i, j]))
      cat("|", sprintf(paste0("%", col_widths[j], "s"), value))
    }
    cat("\n")
  }
}

###############################################################################
# Chi-Square Critical Value
###############################################################################

chi_square_table_parts <- function(observed_table, alpha) {
  row_totals <- rowSums(observed_table)
  col_totals <- colSums(observed_table)
  grand_total <- sum(observed_table)
  expected_table <- outer(row_totals, col_totals) / grand_total
  if (any(expected_table <= 0)) {
    stop("Expected counts must be greater than 0 for a chi-square test.")
  }
  chi_components <- (observed_table - expected_table)^2 / expected_table
  statistic <- sum(chi_components)
  df <- (nrow(observed_table) - 1) * (ncol(observed_table) - 1)
  p_value <- pchisq(statistic, df, lower.tail = FALSE)
  list(
    observed_table = observed_table,
    expected_table = expected_table,
    chi_components = chi_components,
    row_totals = row_totals,
    col_totals = col_totals,
    grand_total = grand_total,
    df = df,
    critical_value = qchisq(1 - alpha, df),
    statistic = statistic,
    p_value = p_value,
    alpha = alpha,
    decision = ifelse(p_value < alpha, "Reject", "Fail to Reject"),
    evidence = ifelse(p_value < alpha, "sufficient evidence", "insufficient evidence")
  )
}

print_chi_square_table_parts <- function(chi) {
  cat("--------------------------------------------------\n")
  cat("Observed Counts:\n")
  printBarTable(chi$observed_table, digits = 6)
  cat("--------------------------------------------------\n")
  cat("Expected Values:\n")
  printBarTable(chi$expected_table, digits = 6)
  cat("--------------------------------------------------\n")
  cat("Chi-Square Components:\n")
  printBarTable(chi$chi_components, digits = 6)
  cat("--------------------------------------------------\n")
  cat(sprintf("%-25s: %10.6f\n", "Grand Total", chi$grand_total))
  cat(sprintf("%-25s: %10.6f\n", "Degrees of Freedom", chi$df))
  cat(sprintf("%-25s: %10.6f\n", "Chi-Square Critical", chi$critical_value))
  cat(sprintf("%-25s: %10.6f\n", "Test Statistic", chi$statistic))
  cat(sprintf("%-25s: %10.6f\n", "P-value", chi$p_value))
  cat("--------------------------------------------------\n")
  cat(sprintf("%-25s: %s (%s)\n", "Decision", chi$decision, chi$evidence))
  cat("--------------------------------------------------\n")
}

chiSqCriticalValue <- function(df, alpha) {
  df <- require_whole_num(df, "Degrees of freedom", 1)
  alpha <- require_alpha(alpha)

  # df = degrees of freedom
  # alpha = significance level / area to the right
  
  critical_value <- qchisq(1 - alpha, df)
  
  cat("--------------------------------------------------\n")
  cat("Chi-Square Critical Value\n")
  cat("--------------------------------------------------\n")
  cat(sprintf("%-25s: %10.6f\n", "Degrees of Freedom", df))
  cat(sprintf("%-25s: %10.6f\n", "Alpha", alpha))
  cat(sprintf("%-25s: %10.6f\n", "Critical Value", critical_value))
  cat("--------------------------------------------------\n")
}

###############################################################################
# Two Independent Sample Proportions
###############################################################################

twoIndSampleProp <- function(x1 = NULL, n1 = NULL, x2 = NULL, n2 = NULL,
                             data1 = NULL, data2 = NULL,
                             conf_level = NULL, alpha = NULL,
                             alternative = "two.sided") {
  # x1 = number of successes in population 1
  # n1 = sample size for population 1
  # x2 = number of successes in population 2
  # n2 = sample size for population 2
  # data1 = raw 0/1 data for sample 1, where 1 = success and 0 = failure
  # data2 = raw 0/1 data for sample 2, where 1 = success and 0 = failure
  # conf_level = confidence level
  # alpha = significance level
  # alternative = "two.sided", "less", or "greater"

  if (!is.null(data1)) {
    data1 <- require_numeric_vector(data1, "Sample 1 data")
    x1 <- sum(data1)
    n1 <- length(data1)
  }
  if (!is.null(data2)) {
    data2 <- require_numeric_vector(data2, "Sample 2 data")
    x2 <- sum(data2)
    n2 <- length(data2)
  }
  x1 <- require_whole_num(x1, "Successes x1", 0)
  n1 <- require_whole_num(n1, "n1", 1)
  x2 <- require_whole_num(x2, "Successes x2", 0)
  n2 <- require_whole_num(n2, "n2", 1)
  if (x1 > n1) {
    stop("Successes x1 cannot be larger than n1.")
  }
  if (x2 > n2) {
    stop("Successes x2 cannot be larger than n2.")
  }
  levels <- resolve_conf_alpha(conf_level, alpha)
  conf_level <- levels$conf_level
  alpha <- levels$alpha

  phat_1 <- x1 / n1
  phat_2 <- x2 / n2
  diff_phat <- phat_1 - phat_2
  stnd_err <- sqrt((phat_1 * (1 - phat_1) / n1) + (phat_2 * (1 - phat_2) / n2))
  pooled_phat <- (x1 + x2) / (n1 + n2)
  test_stnd_err <- sqrt(pooled_phat * (1 - pooled_phat) * (1 / n1 + 1 / n2))
  if (test_stnd_err <= 0) {
    stop("Standard error for the two-proportion test is 0. Check successes and sample sizes.")
  }
  z_stat <- diff_phat / test_stnd_err

  if (alternative == "two.sided") {
    z_critical <- qnorm(1 - alpha / 2)
    p_value <- 2 * pnorm(-abs(z_stat))
  } else if (alternative == "less") {
    z_critical <- qnorm(alpha)
    p_value <- pnorm(z_stat)
  } else if (alternative == "greater") {
    z_critical <- qnorm(1 - alpha)
    p_value <- pnorm(z_stat, lower.tail = FALSE)
  } else {
    stop("Unknown alternative.")
  }

  marg_err <- qnorm(1 - alpha / 2) * stnd_err
  lower <- diff_phat - marg_err
  upper <- diff_phat + marg_err

  if (p_value < alpha) {
    decision <- "Reject"
    evidence <- "sufficient evidence"
  } else {
    decision <- "Fail to Reject"
    evidence <- "insufficient evidence"
  }

  cat("--------------------------------------------------\n")
  cat("Two Independent Sample Proportions\n")
  cat("--------------------------------------------------\n")
  cat("H0: p1 - p2 = 0\n")
  if (alternative == "two.sided") {
    cat("Ha: p1 - p2 != 0\n")
  } else if (alternative == "less") {
    cat("Ha: p1 - p2 < 0\n")
  } else {
    cat("Ha: p1 - p2 > 0\n")
  }
  cat("--------------------------------------------------\n")
  cat(sprintf("%-28s: %10.6f\n", "Successes Sample 1", x1))
  cat(sprintf("%-28s: %10.6f\n", "Sample Size 1", n1))
  cat(sprintf("%-28s: %10.6f\n", "p1_hat", phat_1))
  cat(sprintf("%-28s: %9.6f%%\n", "p1_hat (%)", phat_1 * 100))
  cat(sprintf("%-28s: %10.6f\n", "Successes Sample 2", x2))
  cat(sprintf("%-28s: %10.6f\n", "Sample Size 2", n2))
  cat(sprintf("%-28s: %10.6f\n", "p2_hat", phat_2))
  cat(sprintf("%-28s: %9.6f%%\n", "p2_hat (%)", phat_2 * 100))
  cat(sprintf("%-28s: %10.6f\n", "Difference p1_hat - p2_hat", diff_phat))
  cat(sprintf("%-28s: %9.6f%%\n", "Difference (%)", diff_phat * 100))
  cat("--------------------------------------------------\n")
  cat(sprintf("%-28s: %10.6f\n", "Standard Error for CI", stnd_err))
  cat(sprintf("%-28s: %10.6f\n", "z Critical", z_critical))
  cat(sprintf("%-28s: %10.6f\n", "Margin of Error", marg_err))
  cat(sprintf("%-28s: %10.6f\n", "Lower Endpoint", lower))
  cat(sprintf("%-28s: %10.6f\n", "Upper Endpoint", upper))
  cat(sprintf("%-28s: %9.6f%%\n", "Lower Endpoint (%)", lower * 100))
  cat(sprintf("%-28s: %9.6f%%\n", "Upper Endpoint (%)", upper * 100))
  cat("--------------------------------------------------\n")
  cat(sprintf("%-28s: %10.6f\n", "Pooled Proportion", pooled_phat))
  cat(sprintf("%-28s: %9.6f%%\n", "Pooled Proportion (%)", pooled_phat * 100))
  cat(sprintf("%-28s: %10.6f\n", "Standard Error for Test", test_stnd_err))
  cat(sprintf("%-28s: %10.6f\n", "Test Statistic", z_stat))
  cat(sprintf("%-28s: %10.6f\n", "P-value", p_value))
  cat("--------------------------------------------------\n")
  cat(sprintf("%-28s: %s (%s)\n", "Decision", decision, evidence))
  cat("--------------------------------------------------\n")
  print_two_sample_ci_interpretation(conf_level, lower, upper, digits = 4)

  invisible(calculator_result(
    TRUE,
    values = list(
      successes1 = x1,
      n1 = n1,
      estimate1 = phat_1,
      successes2 = x2,
      n2 = n2,
      estimate2 = phat_2,
      difference = diff_phat,
      confidence_level = conf_level,
      alpha = alpha,
      alternative = alternative,
      standard_error_ci = stnd_err,
      critical_value = z_critical,
      margin_error = marg_err,
      lower_endpoint = lower,
      upper_endpoint = upper,
      pooled_proportion = pooled_phat,
      standard_error_test = test_stnd_err,
      statistic = z_stat,
      p_value = p_value,
      decision = decision,
      evidence = evidence
    ),
    metadata = list(calculator = "twoIndSampleProp")
  ))
}

###############################################################################
# Chi-Square Goodness of Fit
###############################################################################

chiSqGoodnessFit <- function(observed, expected_probs = NULL, expected_counts = NULL,
                             alpha = 0.05, categories = NULL,
                             claim = "the observed frequencies fit the expected distribution") {
  # observed = vector of observed counts
  # expected_probs = vector of expected proportions, such as c(.2, .3, .5)
  # expected_counts = vector of expected counts, if the problem gives them
  # alpha = significance level
  # categories = optional category names
  # claim = wording used for the conclusion

  observed <- require_numeric_vector(observed, "Observed counts", 2)
  alpha <- require_alpha(alpha)
  if (any(observed < 0)) {
    stop("Observed counts must be nonnegative.")
  }
  if (sum(observed) <= 0) {
    stop("Observed counts must have a positive total.")
  }
  if (is.null(categories)) {
    categories <- paste("Category", seq_along(observed))
  } else if (length(categories) != length(observed)) {
    stop("Category labels must match the number of observed counts.")
  }

  if (is.null(expected_counts)) {
    if (is.null(expected_probs)) {
      expected_probs <- rep(1 / length(observed), length(observed))
    } else {
      expected_probs <- require_probability_vector(expected_probs, "Expected probabilities", length(observed))
      if (length(expected_probs) != length(observed)) {
        stop("Expected probabilities must match the number of observed counts.")
      }
      if (!isTRUE(all.equal(sum(expected_probs), 1, tolerance = 1e-6))) {
        stop("Expected probabilities must add to 1.")
      }
    }
  } else {
    expected_counts <- require_numeric_vector(expected_counts, "Expected counts", length(observed))
    if (length(expected_counts) != length(observed)) {
      stop("Expected counts must match the number of observed counts.")
    }
    if (any(expected_counts <= 0)) {
      stop("Expected counts must be greater than 0.")
    }
    expected_probs <- expected_counts / sum(expected_counts)
  }
  # Scale to the observed total so given expected counts always match it.
  expected_counts <- sum(observed) * expected_probs
  if (any(expected_counts <= 0)) {
    stop("Expected counts must be greater than 0.")
  }

  chi_components <- (observed - expected_counts)^2 / expected_counts
  test_stat <- sum(chi_components)
  df <- length(observed) - 1
  critical_value <- qchisq(1 - alpha, df)
  p_value <- pchisq(test_stat, df, lower.tail = FALSE)

  if (p_value < alpha) {
    decision <- "Reject"
    evidence <- "sufficient evidence"
  } else {
    decision <- "Fail to Reject"
    evidence <- "insufficient evidence"
  }

  cat("--------------------------------------------------\n")
  cat("Chi-Square Goodness of Fit Test\n")
  cat("--------------------------------------------------\n")
  cat("H0:", claim, "is true.\n")
  cat("Ha:", claim, "is false.\n")
  cat("--------------------------------------------------\n")

  category_labels <- substr(categories, 1, 12)

  cat(sprintf("%-12s | %10s | %10s | %10s | %10s\n",
              "Category", "Obs", "ExpProp", "ExpValue", "ChiPart"))
  cat(paste(rep("-", 68), collapse = ""), "\n", sep = "")
  for (i in seq_along(observed)) {
    cat(sprintf("%-12s | %10.6f | %10.3f | %10.3f | %10.6f\n",
                category_labels[i], observed[i], expected_probs[i],
                expected_counts[i], chi_components[i]))
  }
  cat("--------------------------------------------------\n")
  cat(sprintf("%-25s: %10.6f\n", "Sample Size", sum(observed)))
  cat(sprintf("%-25s: %10.6f\n", "Degrees of Freedom", df))
  cat(sprintf("%-25s: %10.6f\n", "Chi-Square Critical", critical_value))
  cat(sprintf("%-25s: %10.6f\n", "Test Statistic", test_stat))
  cat(sprintf("%-25s: %10.6f\n", "P-value", p_value))
  cat("--------------------------------------------------\n")
  cat(sprintf("%-25s: %s (%s)\n", "Decision", decision, evidence))
  cat("--------------------------------------------------\n")

  if (decision == "Reject") {
    cat("Interpretation:\n")
    cat("There is sufficient evidence to conclude that", claim, "is false.\n")
  } else {
    cat("Interpretation:\n")
    cat("There is not sufficient evidence to conclude that", claim, "is false.\n")
  }
  cat("--------------------------------------------------\n")

  invisible(calculator_result(
    TRUE,
    values = list(
      observed = observed,
      expected_probs = expected_probs,
      expected_counts = expected_counts,
      chi_components = chi_components,
      sample_size = sum(observed),
      df = df,
      critical_value = critical_value,
      statistic = test_stat,
      p_value = p_value,
      alpha = alpha,
      decision = decision,
      evidence = evidence,
      categories = categories
    ),
    metadata = list(calculator = "chiSqGoodnessFit", claim = claim)
  ))
}

###############################################################################
# Chi-Square Test of Independence
###############################################################################

chiSqIndependence <- function(observed_table, alpha = 0.05,
                              row_var = "row variable",
                              col_var = "column variable") {
  # observed_table = matrix or table of observed counts
  # alpha = significance level
  # row_var = name of the row variable
  # col_var = name of the column variable

  observed_table <- validate_chisq_table(observed_table)
  alpha <- require_alpha(alpha)
  chi <- chi_square_table_parts(observed_table, alpha)

  cat("--------------------------------------------------\n")
  cat("Chi-Square Test of Independence\n")
  cat("--------------------------------------------------\n")
  cat("H0:", row_var, "and", col_var, "are independent.\n")
  cat("Ha:", row_var, "and", col_var, "are dependent / associated.\n")
  print_chi_square_table_parts(chi)

  if (chi$decision == "Reject") {
    cat("Interpretation:\n")
    cat(sprintf("There is sufficient evidence to conclude that there is an association between %s and %s.\n",
                row_var, col_var))
  } else {
    cat("Interpretation:\n")
    cat(sprintf("There is not sufficient evidence to conclude that there is an association between %s and %s.\n",
                row_var, col_var))
  }
  cat("--------------------------------------------------\n")

  invisible(calculator_result(
    TRUE,
    values = chi,
    metadata = list(calculator = "chiSqIndependence", row_var = row_var, col_var = col_var)
  ))
}

###############################################################################
# Chi-Square Test of Homogeneity
###############################################################################

chiSqHomogeneity <- function(observed_table, alpha = 0.05,
                             populations = "populations",
                             response = "response categories") {
  # observed_table = matrix or table of observed counts
  # alpha = significance level
  # populations = name of the groups/populations being compared
  # response = name of the response categories

  observed_table <- validate_chisq_table(observed_table)
  alpha <- require_alpha(alpha)
  chi <- chi_square_table_parts(observed_table, alpha)

  cat("--------------------------------------------------\n")
  cat("Chi-Square Test of Homogeneity\n")
  cat("--------------------------------------------------\n")
  cat(sprintf("H0: The distribution of %s is the same for all %s.\n",
              response, populations))
  cat(sprintf("Ha: The distribution of %s is not the same for all %s.\n",
              response, populations))
  print_chi_square_table_parts(chi)

  if (chi$decision == "Reject") {
    cat("Interpretation:\n")
    cat(sprintf("There is sufficient evidence to conclude that the distribution of %s is not the same for all %s.\n",
                response, populations))
  } else {
    cat("Interpretation:\n")
    cat(sprintf("There is not sufficient evidence to conclude that the distribution of %s is different for the %s.\n",
                response, populations))
  }
  cat("--------------------------------------------------\n")

  invisible(calculator_result(
    TRUE,
    values = chi,
    metadata = list(calculator = "chiSqHomogeneity", populations = populations, response = response)
  ))
}

###############################################################################
# Expected Value for One Cell in a Contingency Table
###############################################################################

expectedCellCount <- function(row_total, column_total, grand_total) {
  row_total <- require_nonnegative_num(row_total, "Row total")
  column_total <- require_nonnegative_num(column_total, "Column total")
  grand_total <- require_positive_num(grand_total, "Grand total")
  if (row_total > grand_total || column_total > grand_total) {
    stop("Row and column totals cannot be larger than grand total.")
  }

  # row_total = total for the row
  # column_total = total for the column
  # grand_total = total number of observations in the table
  
  expected_count <- row_total * column_total / grand_total
  
  cat("--------------------------------------------------\n")
  cat("Expected Cell Value\n")
  cat("--------------------------------------------------\n")
  cat(sprintf("%-25s: %10.6f\n", "Row Total", row_total))
  cat(sprintf("%-25s: %10.6f\n", "Column Total", column_total))
  cat(sprintf("%-25s: %10.6f\n", "Grand Total", grand_total))
  cat(sprintf("%-25s: %10.6f\n", "Expected Value", expected_count))
  cat("--------------------------------------------------\n")
}

###############################################################################
