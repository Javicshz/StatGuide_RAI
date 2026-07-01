# Calculator quick-reference text and rendering helpers.

content_format_plan <- c(
  "Concept rows use one plain text style so definitions, theorems, and notes do not mix bold and regular text.",
  "Formula rows use one consistent two-part layout: a compact label on the left and a neutral formula/note on the right.",
  "Only R code examples use code blocks. Formula text should not switch between red code styling, bold text, and plain text.",
  "When formula content does not provide a clean label, the renderer assigns a neutral label such as Formula or Note.",
  "Internal links connect formula pages to the closest Concepts & Definitions topic."
)

render_list <- function(items) {
  tags$ul(lapply(items, tags$li))
}

first_nonempty <- function(values, fallback = "") {
  values <- values[nzchar(trimws(values))]
  if (length(values)) {
    trimws(values[1])
  } else {
    fallback
  }
}

looks_like_formula_text <- function(text) {
  text <- trimws(text %||% "")
  if (grepl("->", text, fixed = TRUE)) {
    return(FALSE)
  }
  grepl(
    "=|\\+/-|sqrt\\(|sum\\(|P\\(|E\\(|Var\\(|\\^|\\*|choose\\(|qnorm|qt\\(|pnorm|dbinom|pbinom|dpois|ppois|dhyper|qchisq|[A-Za-z0-9_)%]\\s+/\\s+[A-Za-z0-9_(]",
    text
  )
}

clean_content_label <- function(label, fallback, max_chars = 48) {
  label <- trimws(label)
  label <- gsub("\\.$", "", label)
  if (!nzchar(label) || nchar(label) > max_chars || grepl("[.;]", label)) {
    return(fallback)
  }
  label
}

split_labeled_item <- function(item, fallback_label, infer_sentence_label = FALSE, max_label_chars = 48) {
  item <- trimws(item %||% "")
  colon_parts <- strsplit(item, ":", fixed = TRUE)[[1]]
  if (length(colon_parts) > 1) {
    label <- clean_content_label(colon_parts[1], fallback_label, max_chars = max_label_chars)
    body <- trimws(paste(colon_parts[-1], collapse = ":"))
    return(list(label = label, body = body))
  }

  equals_parts <- strsplit(item, "=", fixed = TRUE)[[1]]
  if (length(equals_parts) > 1) {
    label <- clean_content_label(equals_parts[1], fallback_label, max_chars = max_label_chars)
    return(list(label = label, body = item))
  }

  if (infer_sentence_label) {
    concept_match <- regexec(
      "^((?:A|An|The)\\s+)?([A-Za-z0-9][A-Za-z0-9' /&-]{0,32}?)\\s+(is|are|means|measures|describes|uses|use|gives|give|compares|compare|splits|split|summarizes|summarize|predicts|predict|explains|explain|helps|help|moves|move|applies|apply|affects|affect|depends|depend|requires|require|reduces|reduce|increases|increase|decreases|decrease|cannot|should|can)\\b(.*)$",
      item
    )
    concept_parts <- regmatches(item, concept_match)[[1]]
    if (length(concept_parts) > 1) {
      label <- clean_content_label(concept_parts[3], fallback_label, max_chars = max_label_chars)
      body <- trimws(paste0(concept_parts[4], concept_parts[5]))
      return(list(
        label = label,
        body = body
      ))
    }
  }

  list(label = fallback_label, body = item)
}

render_concept_items <- function(items) {
  tags$div(
    class = "concept-list-clean",
    lapply(items, function(item) {
      tags$p(class = "concept-prose", trimws(item %||% ""))
    })
  )
}

render_formula_items <- function(items) {
  tags$div(
    class = "reference-list-clean formula-list-clean",
    lapply(items, function(item) {
      parsed <- split_labeled_item(
        item,
        if (looks_like_formula_text(item)) "Formula" else "Note",
        infer_sentence_label = FALSE,
        max_label_chars = 34
      )
      body_class <- if (looks_like_formula_text(parsed$body)) {
        "reference-body formula-body formula-equation-inline"
      } else {
        "reference-body formula-body formula-note-inline"
      }
      tags$div(
        class = "reference-row formula-row",
        tags$div(class = "reference-label formula-label-inline", parsed$label),
        tags$div(class = body_class, parsed$body)
      )
    })
  )
}

format_example_code <- function(code) {
  lines <- unlist(strsplit(code %||% "", "\n", fixed = TRUE))
  expanded <- unlist(lapply(lines, function(line) {
    trimmed <- trimws(line)
    if (!nzchar(trimmed) || grepl("^#", trimmed)) {
      return(line)
    }
    if (grepl(";", line, fixed = TRUE)) {
      parts <- trimws(unlist(strsplit(line, ";", fixed = TRUE)))
      parts[nzchar(parts)]
    } else {
      line
    }
  }), use.names = FALSE)
  paste(expanded, collapse = "\n")
}

render_markdownish <- function(text) {
  lines <- unlist(strsplit(text %||% "", "\n", fixed = TRUE))
  out <- list()
  code_lines <- character(0)
  in_code <- FALSE

  flush_code <- function() {
    if (length(code_lines)) {
      out[[length(out) + 1]] <<- tags$pre(class = "markdown-code", paste(code_lines, collapse = "\n"))
      code_lines <<- character(0)
    }
  }

  inline_markup <- function(x) {
    x <- htmltools::htmlEscape(x)
    x <- gsub("\\*\\*([^*]+)\\*\\*", "<strong>\\1</strong>", x)
    x <- gsub("`([^`]+)`", "<code>\\1</code>", x)
    htmltools::HTML(x)
  }

  for (line in lines) {
    if (grepl("^```", trimws(line))) {
      if (in_code) {
        flush_code()
        in_code <- FALSE
      } else {
        in_code <- TRUE
      }
      next
    }
    if (in_code) {
      code_lines <- c(code_lines, line)
      next
    }

    trimmed <- trimws(line)
    if (!nzchar(trimmed)) {
      next
    }
    if (grepl("^###\\s+", trimmed)) {
      out[[length(out) + 1]] <- tags$h5(sub("^###\\s+", "", trimmed))
    } else if (grepl("^##\\s+", trimmed)) {
      out[[length(out) + 1]] <- tags$h4(sub("^##\\s+", "", trimmed))
    } else if (grepl("^#\\s+", trimmed)) {
      out[[length(out) + 1]] <- tags$h4(sub("^#\\s+", "", trimmed))
    } else if (grepl("^[-*]\\s+", trimmed)) {
      out[[length(out) + 1]] <- tags$div(class = "markdown-bullet", inline_markup(sub("^[-*]\\s+", "", trimmed)))
    } else if (grepl("^[0-9]+\\.\\s+", trimmed)) {
      out[[length(out) + 1]] <- tags$div(class = "markdown-numbered", inline_markup(trimmed))
    } else {
      out[[length(out) + 1]] <- tags$p(inline_markup(trimmed))
    }
  }
  flush_code()

  tags$div(class = "markdown-output", out)
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
    fiveNumberSummary = list(
      use = "Use for medians, quartiles, box plots, outlier fences, or five-number summaries.",
      example = "Find min, Q1, median, Q3, max and identify possible outliers.",
      code = "summary(x); fivenum(x); boxplot.stats(x)$out"
    ),
    groupedStats = list(
      use = "Use when data are summarized in class intervals with frequencies instead of raw values.",
      example = "A frequency table gives class midpoints and counts; estimate the mean and standard deviation.",
      code = "grouped_mean <- sum(midpoint * frequency) / sum(frequency)"
    ),
    weightedAverage = list(
      use = "Use when each value has a frequency, weight, credit, day count, or importance value.",
      example = "Find an average price weighted by the number of days at each price.",
      code = "weighted.mean(values, weights)"
    ),
    corrReg = list(
      use = "Use when x and y are paired numeric values and you need correlation or a regression equation.",
      example = "Find the correlation between study hours and exam score, then fit a prediction line.",
      code = "cor(x, y); summary(lm(y ~ x))"
    ),
    predictY = list(
      use = "Use after fitting a regression line when the problem asks for a predicted y value from a new x value.",
      example = "Use the regression equation to estimate sales when advertising spend is 75.",
      code = "fit <- lm(y ~ x); predict(fit, newdata = data.frame(x = new_x))"
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
    binomialDistribution = list(
      use = "Use for a fixed number of independent trials with two outcomes and the same success probability each time.",
      example = "Out of 19 parts, find the probability that no more than 1 is defective when p = 0.20.",
      code = "dbinom(x, n, p); pbinom(x, n, p)"
    ),
    poissonDistribution = list(
      use = "Use for counts over a time, space, area, or exposure interval when an average rate is given.",
      example = "A road averages 0.5 crashes per day; find the probability of exactly 1 crash in 7 days.",
      code = "lambda <- rate * periods; dpois(x, lambda); ppois(x, lambda)"
    ),
    hypergeometricDistribution = list(
      use = "Use for samples taken without replacement from a finite group.",
      example = "Draw 5 items from 20 total items where 10 are target items; find the chance of 4 target items.",
      code = "dhyper(x, success_items, failure_items, draws)"
    ),
    normalProb = list(
      use = "Use for normal distribution area/probability questions with a mean and standard deviation.",
      example = "Find the probability that x is between 40 and 55.",
      code = "pnorm(upper, mean, sd) - pnorm(lower, mean, sd)"
    ),
    invNormal = list(
      use = "Use when the problem gives an area or percentile and asks for the cutoff value.",
      example = "Find the score that cuts off the top 5 percent.",
      code = "qnorm(area_left, mean, sd)"
    ),
    sampleMeanProb = list(
      use = "Use when the probability question is about a sample mean or average.",
      example = "Find the probability the average of 123 observations is below 72.",
      code = "se <- sigma / sqrt(n); pnorm(upper, mu, se) - pnorm(lower, mu, se)"
    ),
    samplePropProb = list(
      use = "Use when the probability question is about a sample proportion or percentage.",
      example = "Find the probability the sample proportion is more than 0.48.",
      code = "se <- sqrt(p*(1-p)/n); pnorm(upper, p, se) - pnorm(lower, p, se)"
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
    ),
    meanKnownSigmaTest = list(
      use = "Use for a one-mean hypothesis test when the population standard deviation sigma is known.",
      example = "Test whether the mean fill amount differs from 61 when sigma = 14.",
      code = "z <- (xbar - mu0) / (sigma / sqrt(n))"
    ),
    meanTTest = list(
      use = "Use for a one-mean hypothesis test when sigma is unknown and sample standard deviation s is given.",
      example = "Test whether the average score is below 94 using n, xbar, and s.",
      code = "t <- (xbar - mu0) / (s / sqrt(n))"
    ),
    onePropTest = list(
      use = "Use for a hypothesis test about one population proportion or percentage.",
      example = "Test whether the defect rate is greater than 40 percent.",
      code = "phat <- x/n; z <- (phat - p0) / sqrt(p0*(1-p0)/n)"
    ),
    twoIndSampleProp = list(
      use = "Use for comparing two independent population proportions.",
      example = "Compare the pass rates from two independent groups.",
      code = "prop.test(c(x1, x2), c(n1, n2), correct = FALSE)"
    ),
    pairedDiffCI = list(
      use = "Use when the same subjects/items are measured twice, such as before/after or matched pairs.",
      example = "Construct a confidence interval for the average before-after difference.",
      code = "d <- before - after; t.test(d)$conf.int"
    ),
    twoIndMeanUneqVar = list(
      use = "Use for comparing two independent means when population variances are not assumed equal.",
      example = "Compare two group means using sample standard deviations.",
      code = "se <- sqrt(s1^2/n1 + s2^2/n2); t <- (xbar1 - xbar2) / se"
    ),
    twoIndMeansEqVar = list(
      use = "Use for comparing two independent means only when equal population variances are assumed.",
      example = "Compare two group means when the problem says to assume equal variances.",
      code = "sp2 <- ((n1-1)*s1^2 + (n2-1)*s2^2)/(n1+n2-2)"
    )
  )
  extra_refs <- list(
    frequencyPercent = list(
      use = "Use when the problem gives category or class frequencies and asks for relative frequency or percent for every row.",
      example = "A frequency table lists ratings or class intervals; find each row's percent of the total.",
      code = "f <- c(...)\nrelative_frequency <- f / sum(f)\npercent <- relative_frequency * 100"
    ),
    selectedPercent = list(
      use = "Use when only selected rows from a frequency table count toward the requested percent.",
      example = "Find the percent of ratings less than 4 stars by adding the 1-, 2-, and 3-star rows.",
      code = "f <- c(...)\nselected <- c(1, 2, 3)\nsum(f[selected]) / sum(f) * 100"
    ),
    percentToCount = list(
      use = "Use when a chart or sentence gives a total and a percent, and the answer should be a count.",
      example = "A pie chart says 20% of 3000 customers chose one department; find the number of customers.",
      code = "count <- total * percent / 100"
    ),
    graphRatio = list(
      use = "Use when a graph question asks how many times larger one bar/value is than another.",
      example = "Compare Year 5 with Year 2 by dividing the Year 5 value by the Year 2 value.",
      code = "ratio <- value_on_top / comparison_value"
    ),
    modeClass = list(
      use = "Use when the problem asks whether a data set has no mode, one mode, two modes, or multiple modes.",
      example = "A list repeats -9, 7, and 11 the same number of times; classify the data as multimodal.",
      code = "tab <- table(x)\nmodes <- names(tab)[tab == max(tab)]"
    ),
    trimmedMean = list(
      use = "Use when the problem asks for a trimmed mean after removing the same percent from both tails.",
      example = "Find the 10% trimmed mean of batting averages after trimming the lowest and highest 10%.",
      code = "mean(x, trim = trim_percent / 100)"
    ),
    missingFromMean = list(
      use = "Use when one data value is missing but the target mean and total number of values are known.",
      example = "Seven charges and the mean for all eight charges are given; find the missing eighth charge.",
      code = "missing_value <- target_mean * total_n - sum(known_values)"
    ),
    coeffVariation = list(
      use = "Use to compare relative variation when means and standard deviations are on different scales.",
      example = "Compare two investments by coefficient of variation to see which is more consistent relative to its mean.",
      code = "cv_percent <- sd_value / mean_value * 100"
    ),
    chebyshev = list(
      use = "Use for minimum-percent or range questions based on Chebyshev's theorem for any distribution shape.",
      example = "Find the minimum percent within 3 standard deviations of the mean.",
      code = "minimum_percent <- (1 - 1 / k^2) * 100"
    ),
    empiricalRule = list(
      use = "Use only for bell-shaped or normal data when the interval is about 1, 2, or 3 standard deviations from the mean.",
      example = "For a bell-shaped distribution, estimate the percent between mean - sd and mean + sd.",
      code = "lower <- mean_value - k * sd_value\nupper <- mean_value + k * sd_value"
    ),
    simpleProb = list(
      use = "Use when probability is just favorable outcomes divided by total outcomes, including one-row count tables.",
      example = "A hair-color table gives 43 gray-haired people out of 208 total; find P(gray).",
      code = "probability <- favorable / total"
    ),
    diceSum = list(
      use = "Use for the sum of two standard six-sided dice, where the sample space has 36 ordered outcomes.",
      example = "Find the probability that the sum of two dice is at least 9.",
      code = "sums <- outer(1:6, 1:6, `+`)\nmean(sums >= target)"
    ),
    complementProb = list(
      use = "Use when wording asks for not, none, no, or the opposite of an event whose probability is known.",
      example = "If P(passes inspection) is 0.78, find P(does not pass inspection).",
      code = "complement <- 1 - prob_event"
    ),
    additionRule = list(
      use = "Use for OR probability when two events can overlap and the overlap must be subtracted once.",
      example = "For a card drawn from a deck, find P(spade or ace).",
      code = "prob_or <- (a_count + b_count - overlap_count) / total_count"
    ),
    conditionalProb = list(
      use = "Use for given, among, or selected from wording where the denominator is the given group.",
      example = "Among customers from Florida, find the probability that one owns a pet.",
      code = "conditional_probability <- joint_count / given_count"
    ),
    independentProbability = list(
      use = "Use when independent events all need to happen and each event probability stays unchanged.",
      example = "Find the chance a student fails two independent tests with probabilities 0.07 and 0.04.",
      code = "probability_all <- prod(event_probabilities)"
    ),
    factorialArrangements = list(
      use = "Use for combinations, nCr, factorial ratios, or arrangements with repeated groups.",
      example = "Evaluate 13 choose 6, or count arrangements represented by 13!/(6!7!).",
      code = "choose(n, r)\nfactorial(n) / prod(factorial(parts))"
    ),
    bayesProbability = list(
      use = "Use when the problem reverses conditional probability, such as finding P(group | event).",
      example = "Given someone favors reform, find the probability the person is in the second political group.",
      code = "weights <- prior * likelihood\nposterior <- weights[target] / sum(weights)"
    ),
    discreteDist = list(
      use = "Use to check or summarize a discrete probability distribution table with x values and P(X = x).",
      example = "Decide whether a probability table is valid and find its expected value and standard deviation.",
      code = "sum(prob)\nmu <- sum(x * prob)\nsigma <- sqrt(sum((x - mu)^2 * prob))"
    ),
    probFromDist = list(
      use = "Use when a probability table is given and the question asks for equal, less than, at most, greater than, at least, or between.",
      example = "From a table, find P(X <= 9) by adding probabilities for all x values at or below 9.",
      code = "sum(prob[x <= value])"
    ),
    expectedMonetaryValue = list(
      use = "Use for long-run average profit/loss in games, investments, insurance, or payoff tables.",
      example = "A project earns 40000 with probability 0.60 and loses 10000 with probability 0.40.",
      code = "expected_value <- sum(money_outcomes * probabilities)"
    ),
    discreteUniform = list(
      use = "Use when whole-number outcomes from a minimum to a maximum are equally likely.",
      example = "Delivery date is equally likely from June 1 through June 9; find P(date <= 4).",
      code = "values <- minimum:maximum\nmean(values <= cutoff)"
    ),
    uniformDist = list(
      use = "Use for continuous uniform probability across an interval from a to b.",
      example = "Arrival time is uniformly distributed from 20 to 50 minutes; find the probability of arriving between 25 and 35.",
      code = "probability <- (upper - lower) / (b - a)"
    ),
    zScoreCalc = list(
      use = "Use to standardize one observed value by measuring how many standard deviations it is from the mean.",
      example = "A value of 39.2 comes from a distribution with mean 35.3 and standard deviation 30.6; find its z-score.",
      code = "z <- (x_value - mean_value) / sd_value"
    ),
    normalApproxBinomial = list(
      use = "Use when a binomial count is approximated with a normal curve and a continuity correction.",
      example = "Approximate P(X <= 78) for a binomial setting with n = 109 and p = 0.29.",
      code = "mu <- n * p\nsigma <- sqrt(n * p * (1 - p))\npnorm(value + 0.5, mu, sigma)"
    ),
    varianceCI = list(
      use = "Use for confidence intervals about a population variance or population standard deviation.",
      example = "A sample of 13 values has sample standard deviation 4.76; build a 90% interval for sigma.",
      code = "variance_interval <- (n - 1) * s^2 / qchisq(c(1 - alpha/2, alpha/2), n - 1)\nsqrt(variance_interval)"
    ),
    varianceTest = list(
      use = "Use for a hypothesis test about one population variance or standard deviation.",
      example = "Test whether the population standard deviation is greater than 2.9 using n and sample s.",
      code = "chisq <- (n - 1) * s^2 / sigma0^2"
    ),
    twoIndMeansKnownSigma = list(
      use = "Use for comparing two independent means when both population standard deviations are known.",
      example = "Compare average scores for two independent groups when sigma1 and sigma2 are given.",
      code = "se <- sqrt(sigma1^2 / n1 + sigma2^2 / n2)\nz <- (xbar1 - xbar2) / se"
    ),
    twoVarianceRatio = list(
      use = "Use for comparing two population variances with an F statistic, F test, or variance-ratio interval.",
      example = "Two samples give variances 34.86 and 15.06; test whether the population variances differ.",
      code = "f_stat <- var1 / var2\npf(f_stat, n1 - 1, n2 - 1)"
    )
  )

  refs <- modifyList(refs, extra_refs)

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
