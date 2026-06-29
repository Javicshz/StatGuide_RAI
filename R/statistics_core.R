# HAWKS FUNCTION DEFINITIONS ONLY ############################################
# Source this file once to load helper functions without running examples.
###############################################################################



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
    prob_event <- favorable / total
  }
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
  total <- sum(table_data)
  
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
}

###############################################################################
# Mean Confidence Interval with Known Sigma
###############################################################################

meanKnownSigmaCI <- function(n, xbar, sigma, conf_level = 0.95) {
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
}

###############################################################################
# Mean Confidence Interval with Unknown Sigma
###############################################################################

meanTCI <- function(n, xbar, s, conf_level = 0.95) {
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
}

###############################################################################
# Variance / Standard Deviation Confidence Interval
###############################################################################

varianceCI <- function(n, s, conf_level = 0.95) {
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
  se <- sigma / sqrt(n)
  z <- (xbar - mu0) / se
  
  if (alternative == "two.sided") {
    p_value <- 2 * pnorm(-abs(z))
    critical <- qnorm(1 - alpha / 2)
  } else if (alternative == "less") {
    p_value <- pnorm(z)
    critical <- qnorm(alpha)
  } else {
    p_value <- 1 - pnorm(z)
    critical <- qnorm(1 - alpha)
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
}

###############################################################################
# One Mean t Test
###############################################################################

meanTTest <- function(n, xbar, mu0, s, alpha = 0.05,
                      alternative = "two.sided") {
  df <- n - 1
  se <- s / sqrt(n)
  t_stat <- (xbar - mu0) / se
  
  if (alternative == "two.sided") {
    p_value <- 2 * pt(-abs(t_stat), df)
    critical <- qt(1 - alpha / 2, df)
  } else if (alternative == "less") {
    p_value <- pt(t_stat, df)
    critical <- qt(alpha, df)
  } else {
    p_value <- 1 - pt(t_stat, df)
    critical <- qt(1 - alpha, df)
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
}

###############################################################################
# One Proportion Test
###############################################################################

onePropTest <- function(x, n, p0, alpha = 0.05,
                        alternative = "two.sided") {
  phat <- x / n
  se <- sqrt(p0 * (1 - p0) / n)
  z <- (phat - p0) / se
  
  if (alternative == "two.sided") {
    p_value <- 2 * pnorm(-abs(z))
    critical <- qnorm(1 - alpha / 2)
  } else if (alternative == "less") {
    p_value <- pnorm(z)
    critical <- qnorm(alpha)
  } else {
    p_value <- 1 - pnorm(z)
    critical <- qnorm(1 - alpha)
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
  if (is.null(conf_level) && !is.null(alpha)) {
    conf_level <- 1 - alpha
  }
  if (!is.null(conf_level) && is.null(alpha)) {
    alpha <- 1 - conf_level
  }
  diff_mean <- x1 - x2
  stnd_err <- sqrt((sigma1^2 / n1) + (sigma2^2 / n2))
  z_stat <- diff_mean / stnd_err
  z_for_p <- round(z_stat, 2)
  if (alternative == "two.sided") {
    z_critical <- qnorm(1 - alpha / 2)
    p_value <- 2 * pnorm(-abs(z_for_p))
  } else if (alternative == "less") {
    z_critical <- qnorm(1 - alpha)
    p_value <- pnorm(z_for_p)
  } else if (alternative == "greater") {
    z_critical <- qnorm(1 - alpha)
    p_value <- 1 - pnorm(z_for_p)
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
  if (!is.null(conf_level)) {
    if (lower > 0 && upper > 0) {
      cat("CI Interpretation:\n")
      cat("We are", conf_level * 100, "% confident that Population 1 is between",
          round(lower, 2), "and", round(upper, 2),
          "larger than Population 2.\n")
    } else if (lower < 0 && upper < 0) {
      cat("CI Interpretation:\n")
      cat("We are", conf_level * 100, "% confident that Population 1 is between",
          abs(round(upper, 2)), "and", abs(round(lower, 2)),
          "smaller than Population 2.\n")
    } else {
      cat("CI Interpretation:\n")
      cat("Since the interval contains 0, there is not sufficient evidence of a difference.\n")
    }
    
    cat("--------------------------------------------------\n")
  }
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
    x1 <- sum(data1)
    n1 <- length(data1)
  }
  if (!is.null(data2)) {
    x2 <- sum(data2)
    n2 <- length(data2)
  }
  if (is.null(conf_level) && !is.null(alpha)) {
    conf_level <- 1 - alpha
  }
  if (!is.null(conf_level) && is.null(alpha)) {
    alpha <- 1 - conf_level
  }
  phat_1 <- x1 / n1
  phat_2 <- x2 / n2
  diff_phat <- phat_1 - phat_2
  stnd_err <- sqrt((phat_1 * (1 - phat_1) / n1) + (phat_2 * (1 - phat_2) / n2))
  pooled_phat <- (x1 + x2) / (n1 + n2)
  test_stnd_err <- sqrt(pooled_phat * (1 - pooled_phat) * (1 / n1 + 1 / n2))
  z_stat <- diff_phat / test_stnd_err
  if (alternative == "two.sided") {
    z_critical <- qnorm(1 - alpha / 2)
    p_value <- 2 * pnorm(-abs(z_stat))
  } else if (alternative == "less") {
    z_critical <- qnorm(1 - alpha)
    p_value <- pnorm(z_stat)
  } else if (alternative == "greater") {
    z_critical <- qnorm(1 - alpha)
    p_value <- 1 - pnorm(z_stat)
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
  if (!is.null(conf_level)) {
    if (lower > 0 && upper > 0) {
      cat("CI Interpretation:\n")
      cat("We are", conf_level * 100, "% confident that Population 1 is between",
          round(lower, 4), "and", round(upper, 4),
          "larger than Population 2.\n")
    } else if (lower < 0 && upper < 0) {
      cat("CI Interpretation:\n")
      cat("We are", conf_level * 100, "% confident that Population 1 is between",
          abs(round(upper, 4)), "and", abs(round(lower, 4)),
          "smaller than Population 2.\n")
    } else {
      cat("CI Interpretation:\n")
      cat("Since the interval contains 0, there is not sufficient evidence of a difference.\n")
    }
    
    cat("--------------------------------------------------\n")
  }
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
  
  if (is.null(conf_level) && !is.null(alpha)) {
    conf_level <- 1 - alpha
  }
  
  if (!is.null(conf_level) && is.null(alpha)) {
    alpha <- 1 - conf_level
  }
  
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
    t_critical <- qt(1 - alpha, df)
    p_value <- pt(t_stat, df)
  } else if (alternative == "greater") {
    t_critical <- qt(1 - alpha, df)
    p_value <- 1 - pt(t_stat, df)
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
  if (lower > 0 && upper > 0) {
    cat("CI Interpretation:\n")
    cat("We are", conf_level * 100, "% confident that the mean for Population 1 is between",
        round(lower, 2), "and", round(upper, 2),
        "larger than the mean for Population 2.\n")
  } else if (lower < 0 && upper < 0) {
    cat("CI Interpretation:\n")
    cat("We are", conf_level * 100, "% confident that the mean for Population 1 is between",
        abs(round(upper, 2)), "and", abs(round(lower, 2)),
        "smaller than the mean for Population 2.\n")
  } else {
    cat("CI Interpretation:\n")
    cat("Since the confidence interval contains 0, there is not sufficient evidence that the two population means are different.\n")
  }
  
  cat("--------------------------------------------------\n")
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
  
  if (is.null(conf_level) && !is.null(alpha)) {
    conf_level <- 1 - alpha
  }
  
  if (!is.null(conf_level) && is.null(alpha)) {
    alpha <- 1 - conf_level
  }
  
  diff_mean <- x1 - x2
  
  sp2 <- (((n1 - 1) * s1^2) + ((n2 - 1) * s2^2)) / (n1 + n2 - 2)
  stnd_err <- sqrt(sp2 * (1 / n1 + 1 / n2))
  df <- n1 + n2 - 2
  t_stat <- diff_mean / stnd_err
  
  if (alternative == "two.sided") {
    t_critical <- qt(1 - alpha / 2, df)
    p_value <- 2 * pt(-abs(t_stat), df)
  } else if (alternative == "less") {
    t_critical <- qt(1 - alpha, df)
    p_value <- pt(t_stat, df)
  } else if (alternative == "greater") {
    t_critical <- qt(1 - alpha, df)
    p_value <- 1 - pt(t_stat, df)
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
  if (!is.null(conf_level)) {
    if (lower > 0 && upper > 0) {
      cat("CI Interpretation:\n")
      cat("We are", conf_level * 100, "% confident that Population 1 is between",
          round(lower, 2), "and", round(upper, 2),
          "larger than Population 2.\n")
    } else if (lower < 0 && upper < 0) {
      cat("CI Interpretation:\n")
      cat("We are", conf_level * 100, "% confident that Population 1 is between",
          abs(round(upper, 2)), "and", abs(round(lower, 2)),
          "smaller than Population 2.\n")
    } else {
      cat("CI Interpretation:\n")
      cat("Since the interval contains 0, there is not sufficient evidence of a difference.\n")
    }
    
    cat("--------------------------------------------------\n")
  }
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

chiSqCriticalValue <- function(df, alpha) {
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
    x1 <- sum(data1)
    n1 <- length(data1)
  }
  if (!is.null(data2)) {
    x2 <- sum(data2)
    n2 <- length(data2)
  }
  if (is.null(conf_level) && !is.null(alpha)) {
    conf_level <- 1 - alpha
  }
  if (!is.null(conf_level) && is.null(alpha)) {
    alpha <- 1 - conf_level
  }
  
  phat_1 <- x1 / n1
  phat_2 <- x2 / n2
  diff_phat <- phat_1 - phat_2
  stnd_err <- sqrt((phat_1 * (1 - phat_1) / n1) + (phat_2 * (1 - phat_2) / n2))
  pooled_phat <- (x1 + x2) / (n1 + n2)
  test_stnd_err <- sqrt(pooled_phat * (1 - pooled_phat) * (1 / n1 + 1 / n2))
  z_stat <- diff_phat / test_stnd_err
  
  if (alternative == "two.sided") {
    z_critical <- qnorm(1 - alpha / 2)
    p_value <- 2 * pnorm(-abs(z_stat))
  } else if (alternative == "less") {
    z_critical <- qnorm(1 - alpha)
    p_value <- pnorm(z_stat)
  } else if (alternative == "greater") {
    z_critical <- qnorm(1 - alpha)
    p_value <- 1 - pnorm(z_stat)
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
  
  if (!is.null(conf_level)) {
    if (lower > 0 && upper > 0) {
      cat("CI Interpretation:\n")
      cat("We are", conf_level * 100, "% confident that Population 1 is between",
          round(lower, 4), "and", round(upper, 4),
          "larger than Population 2.\n")
    } else if (lower < 0 && upper < 0) {
      cat("CI Interpretation:\n")
      cat("We are", conf_level * 100, "% confident that Population 1 is between",
          abs(round(upper, 4)), "and", abs(round(lower, 4)),
          "smaller than Population 2.\n")
    } else {
      cat("CI Interpretation:\n")
      cat("Since the interval contains 0, there is not sufficient evidence of a difference.\n")
    }
    
    cat("--------------------------------------------------\n")
  }
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
  
  if (is.null(categories)) {
    categories <- paste("Category", seq_along(observed))
  }
  
  if (is.null(expected_counts)) {
    if (is.null(expected_probs)) {
      expected_probs <- rep(1 / length(observed), length(observed))
    }
    expected_counts <- sum(observed) * expected_probs
  } else {
    expected_probs <- expected_counts / sum(expected_counts)
  }
  
  chi_components <- (observed - expected_counts)^2 / expected_counts
  test_stat <- sum(chi_components)
  df <- length(observed) - 1
  critical_value <- qchisq(1 - alpha, df)
  p_value <- 1 - pchisq(test_stat, df)
  
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
  
  observed_table <- as.matrix(observed_table)
  
  row_totals <- rowSums(observed_table)
  col_totals <- colSums(observed_table)
  grand_total <- sum(observed_table)
  
  expected_table <- outer(row_totals, col_totals) / grand_total
  chi_components <- (observed_table - expected_table)^2 / expected_table
  test_stat <- sum(chi_components)
  df <- (nrow(observed_table) - 1) * (ncol(observed_table) - 1)
  critical_value <- qchisq(1 - alpha, df)
  p_value <- 1 - pchisq(test_stat, df)
  
  if (p_value < alpha) {
    decision <- "Reject"
    evidence <- "sufficient evidence"
  } else {
    decision <- "Fail to Reject"
    evidence <- "insufficient evidence"
  }
  
  cat("--------------------------------------------------\n")
  cat("Chi-Square Test of Independence\n")
  cat("--------------------------------------------------\n")
  cat("H0:", row_var, "and", col_var, "are independent.\n")
  cat("Ha:", row_var, "and", col_var, "are dependent / associated.\n")
  cat("--------------------------------------------------\n")
  cat("Observed Counts:\n")
  printBarTable(observed_table, digits = 6)
  cat("--------------------------------------------------\n")
  cat("Expected Values:\n")
  printBarTable(expected_table, digits = 6)
  cat("--------------------------------------------------\n")
  cat("Chi-Square Components:\n")
  printBarTable(chi_components, digits = 6)
  cat("--------------------------------------------------\n")
  cat(sprintf("%-25s: %10.6f\n", "Grand Total", grand_total))
  cat(sprintf("%-25s: %10.6f\n", "Degrees of Freedom", df))
  cat(sprintf("%-25s: %10.6f\n", "Chi-Square Critical", critical_value))
  cat(sprintf("%-25s: %10.6f\n", "Test Statistic", test_stat))
  cat(sprintf("%-25s: %10.6f\n", "P-value", p_value))
  cat("--------------------------------------------------\n")
  cat(sprintf("%-25s: %s (%s)\n", "Decision", decision, evidence))
  cat("--------------------------------------------------\n")
  
  if (decision == "Reject") {
    cat("Interpretation:\n")
    cat(sprintf("There is sufficient evidence to conclude that there is an association between %s and %s.\n",
                row_var, col_var))
  } else {
    cat("Interpretation:\n")
    cat(sprintf("There is not sufficient evidence to conclude that there is an association between %s and %s.\n",
                row_var, col_var))
  }
  cat("--------------------------------------------------\n")
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
  
  observed_table <- as.matrix(observed_table)
  
  row_totals <- rowSums(observed_table)
  col_totals <- colSums(observed_table)
  grand_total <- sum(observed_table)
  
  expected_table <- outer(row_totals, col_totals) / grand_total
  chi_components <- (observed_table - expected_table)^2 / expected_table
  test_stat <- sum(chi_components)
  df <- (nrow(observed_table) - 1) * (ncol(observed_table) - 1)
  critical_value <- qchisq(1 - alpha, df)
  p_value <- 1 - pchisq(test_stat, df)
  
  if (p_value < alpha) {
    decision <- "Reject"
    evidence <- "sufficient evidence"
  } else {
    decision <- "Fail to Reject"
    evidence <- "insufficient evidence"
  }
  
  cat("--------------------------------------------------\n")
  cat("Chi-Square Test of Homogeneity\n")
  cat("--------------------------------------------------\n")
  cat(sprintf("H0: The distribution of %s is the same for all %s.\n",
              response, populations))
  cat(sprintf("Ha: The distribution of %s is not the same for all %s.\n",
              response, populations))
  cat("--------------------------------------------------\n")
  cat("Observed Counts:\n")
  printBarTable(observed_table, digits = 6)
  cat("--------------------------------------------------\n")
  cat("Expected Values:\n")
  printBarTable(expected_table, digits = 6)
  cat("--------------------------------------------------\n")
  cat("Chi-Square Components:\n")
  printBarTable(chi_components, digits = 6)
  cat("--------------------------------------------------\n")
  cat(sprintf("%-25s: %10.6f\n", "Grand Total", grand_total))
  cat(sprintf("%-25s: %10.6f\n", "Degrees of Freedom", df))
  cat(sprintf("%-25s: %10.6f\n", "Chi-Square Critical", critical_value))
  cat(sprintf("%-25s: %10.6f\n", "Test Statistic", test_stat))
  cat(sprintf("%-25s: %10.6f\n", "P-value", p_value))
  cat("--------------------------------------------------\n")
  cat(sprintf("%-25s: %s (%s)\n", "Decision", decision, evidence))
  cat("--------------------------------------------------\n")
  
  if (decision == "Reject") {
    cat("Interpretation:\n")
    cat(sprintf("There is sufficient evidence to conclude that the distribution of %s is not the same for all %s.\n",
                response, populations))
  } else {
    cat("Interpretation:\n")
    cat(sprintf("There is not sufficient evidence to conclude that the distribution of %s is different for the %s.\n",
                response, populations))
  }
  cat("--------------------------------------------------\n")
}

###############################################################################
# Expected Value for One Cell in a Contingency Table
###############################################################################

expectedCellCount <- function(row_total, column_total, grand_total) {
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
