# Calculator quick-reference text and rendering helpers.

render_list <- function(items) {
  tags$ul(lapply(items, tags$li))
}

calculator_reference <- function(fn) {
  refs <- list(
    feelingLucky = list(
      use = "Use when you are not sure which calculator fits. It can show likely results for raw data, probability, counting, distributions, and confidence intervals.",
      example = "Paste raw data, enter successes/n, or enter binomial/Poisson/hypergeometric values and leave the rest blank.",
      code = "mean(x); sd(x); x/n; dbinom(k, n, p); pnorm(upper, mean, sd) - pnorm(lower, mean, sd)"
    ),
    pointEstimate = list(
      use = "Use for estimate the proportion, estimate the mean, or estimate variance/standard deviation.",
      example = "Estimate the proportion who were caught: successes divided by total.",
      code = "phat <- x / n"
    ),
    onePropCI = list(
      use = "Use when a later step asks for a confidence interval for one population proportion.",
      example = "Construct a 90% confidence interval for the proportion who were caught.",
      code = "phat <- x/n; phat + c(-1, 1) * qnorm(1 - alpha/2) * sqrt(phat*(1-phat)/n)"
    ),
    meanCIFromData = list(
      use = "Use for mean confidence intervals when raw data is provided.",
      example = "Calculate xbar and s, then construct a 95% confidence interval for the true mean.",
      code = "xbar <- mean(x); s <- sd(x); xbar + c(-1, 1) * qt(1 - alpha/2, length(x)-1) * s/sqrt(length(x))"
    ),
    meanKnownSigmaCI = list(
      use = "Use when the problem gives a population standard deviation sigma.",
      example = "Past tests show the population standard deviation is 4 days.",
      code = "xbar + c(-1, 1) * qnorm(1 - alpha/2) * sigma/sqrt(n)"
    ),
    meanTCI = list(
      use = "Use when the problem gives sample standard deviation s, or raw sample data only.",
      example = "A sample of 21 patients had xbar = 6.4 and sample standard deviation s = 1.1.",
      code = "xbar + c(-1, 1) * qt(1 - alpha/2, n - 1) * s/sqrt(n)"
    ),
    sampleSize = list(
      use = "Use for wording like sample size needed, estimate with accuracy, or margin of error.",
      example = "How many packages are needed to estimate the mean with accuracy of 1 day?",
      code = "ceiling((qnorm(1 - alpha/2) * sigma / margin_error)^2)"
    ),
    descStats = list(
      use = "Use for sample mean, median, mode, variance, standard deviation, or raw data summary.",
      example = "Calculate the sample mean and sample standard deviation of the data.",
      code = "mean(x); sd(x); var(x)"
    ),
    countingRule = list(
      use = "Use for account numbers, passwords, or codes with several slots.",
      example = "2 letters followed by 3 digits with repetition allowed.",
      code = "prod(c(26, 26, 10, 10, 10))"
    ),
    sequenceProbability = list(
      use = "Use for events in order, including cards with or without replacement.",
      example = "Heart and then club without replacement.",
      code = "prod(c(13/52, 13/51))"
    ),
    tableProbability = list(
      use = "Use for copied two-way tables with row, column, joint, given, or OR wording.",
      example = "What is the probability the customer lives in Florida?",
      code = "row_total / grand_total"
    ),
    chiSqGoodnessFit = list(
      use = "Use for one categorical variable and observed counts compared with equal/specified proportions.",
      example = "Are candy colors equally distributed?",
      code = "chisq.test(observed, p = expected_probs)"
    ),
    chiSqIndependence = list(
      use = "Use for two-way tables asking if two categorical variables are associated or independent.",
      example = "Is brand perception dependent on age?",
      code = "chisq.test(observed_table)"
    )
  )
  refs[[fn]] %||% list(
    use = guide_details[[fn]][1],
    example = "Match the problem wording to the calculator name and fill only the boxes the problem gives.",
    code = "Use the simple formula shown in the Formulas tab for this topic."
  )
}

calculator_reference_details <- function(fn) {
  if (fn %in% c("frequencyPercent", "selectedPercent", "percentToCount", "graphRatio", "groupedStats")) {
    return(list(
      check = "Confirm whether entries are raw data, category counts, percentages, or grouped-class summaries. Do not mix counts and percentages unless the formula explicitly asks for both.",
      interpret = "Report the answer in the requested unit: count, proportion, percent, ratio, mean, variance, or standard deviation. Mention that grouped-data summaries are approximations when class midpoints are used.",
      assumptions = "Tables should have consistent categories, no accidental total rows included as categories, and matching frequency/category lengths."
    ))
  }
  if (fn %in% c("descStats", "modeClass", "fiveNumberSummary", "trimmedMean", "weightedAverage", "missingFromMean", "coeffVariation", "chebyshev", "empiricalRule")) {
    return(list(
      check = "Confirm whether the data are a sample or a population, whether outliers are meaningful, and whether the distribution is roughly bell-shaped before using empirical-rule language.",
      interpret = "Describe center, spread, shape, and unusual values in context. Use units for mean and standard deviation; variance is in squared units.",
      assumptions = "Chebyshev works for any shape. The empirical rule needs an approximately normal or bell-shaped distribution."
    ))
  }
  if (fn %in% c("corrReg", "predictY")) {
    return(list(
      check = "Confirm x and y values are paired in the same order and inspect whether a linear relationship is reasonable.",
      interpret = "Correlation describes linear strength and direction. Regression predictions are expected y values, not guaranteed outcomes.",
      assumptions = "Avoid extrapolation outside the observed x range. Correlation/regression alone does not prove causation."
    ))
  }
  if (fn %in% c("simpleProb", "diceSum", "complementProb", "additionRule", "conditionalProb", "countingRule", "sequenceProbability", "independentProbability", "tableProbability", "factorialArrangements", "bayesProbability")) {
    return(list(
      check = "Identify the sample space, favorable outcomes, whether order matters, whether replacement occurs, and whether the wording is AND, OR, GIVEN, or NOT.",
      interpret = "Report probability as a decimal, fraction, or percent as requested. For conditional probability, clearly state the reduced sample space.",
      assumptions = "Independence must be justified before multiplying unchanged probabilities. Without replacement usually changes later denominators."
    ))
  }
  if (fn %in% c("discreteDist", "probFromDist", "expectedMonetaryValue", "discreteUniform", "binomialDistribution", "poissonDistribution", "hypergeometricDistribution")) {
    return(list(
      check = "Identify the distribution type before calculating. Check whether values are countable, whether trials are independent, and whether sampling is with or without replacement.",
      interpret = "For expected value, interpret the long-run average. For probability queries, match the output to exact, less-than, at-most, greater-than, at-least, or between wording.",
      assumptions = "Binomial requires fixed n, two outcomes, independent trials, and constant p. Poisson uses counts over exposure. Hypergeometric uses finite sampling without replacement."
    ))
  }
  if (fn %in% c("uniformDist", "zScoreCalc", "normalProb", "invNormal", "normalApproxBinomial", "sampleMeanProb", "samplePropProb")) {
    return(list(
      check = "Determine whether the value is an individual observation, a sample mean, a sample proportion, or a binomial count. Match probability bounds to the wording.",
      interpret = "Z-scores are distances in standard deviations. Normal probabilities are areas under a curve. Inverse normal returns a cutoff value for a given area.",
      assumptions = "Normal models require a stated or reasonable normal approximation. Sample proportion approximations need enough expected successes and failures."
    ))
  }
  if (fn %in% c("pointEstimate", "meanCIFromData", "onePropCI", "meanKnownSigmaCI", "meanTCI", "sampleSize", "varianceCI")) {
    return(list(
      check = "Identify the parameter being estimated: mean, proportion, variance, or standard deviation. Use confidence level for intervals and do not also enter alpha unless requested.",
      interpret = "Interpret confidence intervals as plausible parameter values from a method with a long-run capture rate. Include population, parameter, confidence level, and units.",
      assumptions = "Mean t intervals need random/representative data and roughly normal data or adequate sample size. Proportion intervals need enough successes and failures."
    ))
  }
  if (fn %in% c("meanKnownSigmaTest", "meanTTest", "onePropTest", "varianceTest", "twoIndSampleProp", "twoIndMeansKnownSigma", "twoIndMeanUneqVar", "twoIndMeansEqVar", "pairedDiffCI", "twoVarianceRatio")) {
    return(list(
      check = "Write H0 and Ha first, identify the parameter, choose the tail from the wording, and confirm whether samples are one-sample, independent two-sample, or paired.",
      interpret = "Use the P-value compared with alpha to reject or fail to reject H0, then write the conclusion in context without overclaiming.",
      assumptions = "Check independence, random/representative sampling, normality or large-sample conditions, and equal-variance assumptions only when the selected method requires them."
    ))
  }
  if (fn %in% c("chiSqGoodnessFit", "chiSqIndependence", "chiSqHomogeneity")) {
    return(list(
      check = "Use observed counts, not percentages. Remove total rows/columns before entering a contingency table.",
      interpret = "A small P-value means observed counts differ more from expected counts than random variation would usually explain under H0.",
      assumptions = "Expected counts should generally be large enough, often at least 5. Chi-square tests identify evidence of difference/association, not which cells caused it."
    ))
  }
  list(
    check = "Read the problem wording carefully and enter only values that are given or directly computed.",
    interpret = "Connect the numerical output back to the original question, including units and the population or process being described.",
    assumptions = "Check data type, independence, sample size, and whether the chosen calculator matches the question wording."
  )
}

