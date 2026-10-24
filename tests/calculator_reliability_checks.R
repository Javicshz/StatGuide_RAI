# Calculator reliability checks for shared parsing and validation helpers.
# Uses base R so it can run without extra testing packages.

env <- new.env(parent = globalenv())
source("app.R", local = env)

check <- function(label, condition) {
  if (!isTRUE(condition)) {
    stop(paste("Calculator reliability check failed:", label), call. = FALSE)
  }
}

expect_error <- function(label, expr, pattern) {
  err <- tryCatch(
    {
      force(expr)
      NULL
    },
    error = function(e) e
  )
  if (is.null(err)) {
    stop(paste("Calculator reliability check failed:", label, "did not error"), call. = FALSE)
  }
  if (!grepl(pattern, conditionMessage(err), fixed = TRUE)) {
    stop(paste("Calculator reliability check failed:", label, "had unexpected error:", conditionMessage(err)), call. = FALSE)
  }
}

check("parse nums handles commas newlines tabs and semicolons",
      identical(env$parse_nums("1, 2\n3\t4;5"), c(1, 2, 3, 4, 5)))
check("parse nums blank is empty", identical(env$parse_nums("   "), numeric(0)))
expect_error("parse nums rejects text token", env$parse_nums("1, nope, 3", "test values"), "Could not parse test values")

expect_error("optional number rejects text", env$parse_optional_num("abc"), "Expected one number")
expect_error("between bounds reject inverted range", env$probability_bounds("between", "10", "2", ""), "Lower bound must be less than or equal")

expect_error("counting rule rejects empty vector", env$countingRuleCalc(numeric(0)), "Slot options must contain")
expect_error("counting rule rejects non-whole counts", env$countingRuleCalc(c(2, 3.5)), "whole-number")
expect_error("independent probability rejects percent-like number", env$independentProbabilityCalc(c(0.2, 20)), "between 0 and 1")
expect_error("sequence probability rejects favorable greater than total", env$sequenceProbabilityCalc(c(4), c(3)), "between 0 and the matching total")

matrix_ok <- env$matrix_from_text("1,2,3,4", 2, "A,B", "Yes,No")
check("matrix parser dimensions", identical(dim(matrix_ok), c(2L, 2L)))
check("matrix parser row labels", identical(rownames(matrix_ok), c("A", "B")))
check("matrix parser col labels", identical(colnames(matrix_ok), c("Yes", "No")))
expect_error("matrix parser rejects uneven values", env$matrix_from_text("1,2,3,4,5", 2, "", ""), "divide evenly")
expect_error("matrix parser rejects bad row label count", env$matrix_from_text("1,2,3,4", 2, "A,B,C", ""), "Row label count")
expect_error("matrix parser rejects bad column label count", env$matrix_from_text("1,2,3,4", 2, "", "Yes,No,Maybe"), "Column label count")

expect_error("confidence level rejects one", env$sampleSizeCalc("mean", 1, 2, sigma = 10), "Confidence level must be greater than 0 and less than 1")
expect_error("sample size rejects nonpositive margin", env$sampleSizeCalc("mean", 0.95, 0, sigma = 10), "Margin of error must be greater than 0")
expect_error("sample size rejects invalid p hat", env$sampleSizeCalc("proportion", 0.95, 0.05, p_hat = 1.2), "Estimated proportion must be between 0 and 1")
expect_error("mean CI raw data needs two values", env$meanCIFromDataCalc(c(10), 0.95), "at least two raw data values")
expect_error("mean CI rejects nonpositive known sigma", env$meanCIFromDataCalc(c(10, 12), 0.95, sigma = 0, method = "known"), "Known sigma must be greater than 0")
expect_error("point estimate rejects successes over n", env$pointEstimateCalc("proportion", x = 12, n = 10), "Successes x cannot be larger than n")

expect_error("binomial rejects non-whole trials", env$binomialCalc(10.5, 0.2, "equal", 2), "Trials n must be a whole number")
expect_error("binomial rejects invalid probability", env$binomialCalc(10, 1.2, "equal", 2), "Success probability p must be between 0 and 1")
expect_error("binomial rejects inverted between bounds", env$binomialCalc(10, 0.2, "between", 5, 3), "Value must be less than or equal")
expect_error("normal approximation rejects p of zero", env$normalApproxBinomialCalc(10, 0, "equal", 2), "Normal approximation requires")
expect_error("poisson rejects zero rate", env$poissonCalc(0, 2, "equal", 1), "Rate per period must be greater than 0")
expect_error("poisson rejects decimal count", env$poissonCalc(2, 2, "equal", 1.5), "Value must be a whole number")
expect_error("hypergeometric rejects sample over population", env$hypergeometricCalc(10, 4, 11, "expected"), "Sample size cannot be larger than total items")
expect_error("hypergeometric rejects successes over population", env$hypergeometricCalc(10, 12, 3, "expected"), "Success items cannot be larger than total items")
expect_error("discrete uniform rejects inverted bounds", env$discreteUniformCalc(5, 1), "Minimum value must be less than or equal")
check("discrete uniform allows negative endpoints", {
  invisible(capture.output(env$discreteUniformCalc(-2, 2)))
  TRUE
})
expect_error("probability distribution query rejects inverted bounds", env$probDistQueryCalc(c(1, 2, 3), c(0.2, 0.3, 0.5), "between", 3, 2), "Value must be less than or equal")
expect_error("expected monetary value rejects invalid probability", env$expectedMonetaryValueCalc(c(10, -5), c(0.6, 1.2)), "Probabilities must contain probabilities between 0 and 1")
expect_error("conf alpha rejects both fields", env$conf_alpha_values("0.95", "0.05"), "Enter either confidence level OR alpha")
expect_error("conf alpha rejects invalid alpha", env$conf_alpha_values("", "1"), "Alpha must be greater than 0 and less than 1")
expect_error("variance test rejects n under two", env$varianceTestCalc(1, 2, 3), "n must be a whole number at least 2")
expect_error("variance test rejects nonpositive sigma0", env$varianceTestCalc(10, 2, 0), "Hypothesized standard deviation must be greater than 0")
expect_error("paired CI rejects short vectors", env$pairedDiffCI(c(1), c(2), 0.95), "Before values must contain at least 2")
expect_error("two variance ratio rejects zero variance", env$twoVarianceRatioCalc(10, 0, 10, 2), "Variance 1 must be greater than 0")

success_result <- env$capture_calculator_result({
  cat("hello calculator\n")
})
check("calculator result success class", inherits(success_result, "calculator_result"))
check("calculator result success ok", identical(success_result$ok, TRUE))
check("calculator result captures output", identical(trimws(success_result$output), "hello calculator"))
check("calculator result formats success", identical(env$format_calculator_result(success_result), "hello calculator"))

error_result <- env$capture_calculator_result({
  stop("bad calculator input")
})
check("calculator result error status", identical(error_result$status, "input_error"))
check("calculator result error ok false", identical(error_result$ok, FALSE))
check("calculator result formats error", identical(env$format_calculator_result(error_result), "Input error: bad calculator input"))

empty_result <- env$capture_calculator_result({
  invisible(NULL)
})
check("calculator result empty output message", identical(env$format_calculator_result(empty_result), "No output was produced. Check the selected calculator inputs."))

counting_result <- env$capture_calculator_result(env$countingRuleCalc(c(2, 3, 4)))
check("counting rule structured result", inherits(counting_result, "calculator_result"))
check("counting rule structured total", identical(counting_result$values$total_outcomes, 24))
check("counting rule preserves output text", grepl("Total Outcomes", counting_result$output, fixed = TRUE))

point_result <- env$capture_calculator_result(env$pointEstimateCalc("proportion", x = 3, n = 10))
check("point estimate structured estimate", identical(point_result$values$estimate, 0.3))
check("point estimate structured percent", identical(point_result$values$percent, 30))

emv_result <- env$capture_calculator_result(env$expectedMonetaryValueCalc(c(100, -25), c(0.2, 0.8)))
check("emv structured expected value", identical(emv_result$values$expected_value, 0))

binomial_result <- env$capture_calculator_result(env$binomialCalc(10, 0.2, "equal", 2))
check("binomial structured probability", isTRUE(all.equal(binomial_result$values$probability, dbinom(2, 10, 0.2))))
check("binomial structured expected value", identical(binomial_result$values$expected_value, 2))

poisson_result <- env$capture_calculator_result(env$poissonCalc(3, 2, "less_equal", 4))
check("poisson structured lambda", identical(poisson_result$values$lambda, 6))
check("poisson structured probability", isTRUE(all.equal(poisson_result$values$probability, ppois(4, 6))))

dist_summary_result <- env$capture_calculator_result(env$print_distribution_summary(c(0, 1, 2), c(0.2, 0.5, 0.3)))
check("distribution summary structured valid", identical(dist_summary_result$values$valid, TRUE))
check("distribution summary structured expected value", isTRUE(all.equal(dist_summary_result$values$expected_value, 1.1)))
check("distribution summary preserves output text", grepl("Valid Distribution", dist_summary_result$output, fixed = TRUE))

invalid_dist_result <- env$capture_calculator_result(env$print_distribution_summary(c(0, 1), c(0.2, 0.2)))
check("invalid distribution structured invalid", identical(invalid_dist_result$values$valid, FALSE))
check("invalid distribution expected value withheld", is.na(invalid_dist_result$values$expected_value))

prob_query_result <- env$capture_calculator_result(env$probDistQueryCalc(c(0, 1, 2), c(0.2, 0.5, 0.3), "between", 1, 2))
check("probability query structured probability", isTRUE(all.equal(prob_query_result$values$probability, 0.8)))

uniform_result <- env$capture_calculator_result(env$discreteUniformCalc(1, 6, "less_equal", 3))
check("discrete uniform structured expected value", identical(uniform_result$values$expected_value, 3.5))
check("discrete uniform structured probability", isTRUE(all.equal(uniform_result$values$probability, 0.5)))

normal_approx_result <- env$capture_calculator_result(env$normalApproxBinomialCalc(100, 0.4, "less_equal", 45))
check("normal approximation structured mean", identical(normal_approx_result$values$mean, 40))
check("normal approximation structured probability", isTRUE(all.equal(normal_approx_result$values$probability, pnorm(45.5, 40, sqrt(24)))))

hyper_result <- env$capture_calculator_result(env$hypergeometricCalc(20, 7, 5, "less_equal", 2))
check("hypergeometric structured expected value", isTRUE(all.equal(hyper_result$values$expected_value, 1.75)))
check("hypergeometric structured probability", isTRUE(all.equal(hyper_result$values$probability, phyper(2, 7, 13, 5))))

sample_size_result <- env$capture_calculator_result(env$sampleSizeCalc("mean", 0.95, 2, sigma = 10))
check("sample size structured n", identical(sample_size_result$values$sample_size, ceiling((qnorm(0.975) * 10 / 2)^2)))
check("sample size structured z", isTRUE(all.equal(sample_size_result$values$z_critical, qnorm(0.975))))

mean_ci_result <- env$capture_calculator_result(env$meanCIFromDataCalc(c(10, 12, 14, 16), 0.95, method = "unknown"))
mean_ci_x <- c(10, 12, 14, 16)
mean_ci_me <- qt(0.975, length(mean_ci_x) - 1) * sd(mean_ci_x) / sqrt(length(mean_ci_x))
check("mean CI structured lower", isTRUE(all.equal(mean_ci_result$values$lower_endpoint, mean(mean_ci_x) - mean_ci_me)))
check("mean CI structured upper", isTRUE(all.equal(mean_ci_result$values$upper_endpoint, mean(mean_ci_x) + mean_ci_me)))

variance_test_result <- env$capture_calculator_result(env$varianceTestCalc(10, 2, 3, 0.05, "two.sided"))
variance_stat <- 9 * 2^2 / 3^2
variance_p <- min(2 * min(pchisq(variance_stat, 9), 1 - pchisq(variance_stat, 9)), 1)
check("variance test structured statistic", isTRUE(all.equal(variance_test_result$values$statistic, variance_stat)))
check("variance test structured p value", isTRUE(all.equal(variance_test_result$values$p_value, variance_p)))

paired_result <- env$capture_calculator_result(env$pairedDiffCI(c(10, 12, 13), c(8, 9, 10), 0.95))
paired_diff <- c(10, 12, 13) - c(8, 9, 10)
paired_me <- qt(0.975, length(paired_diff) - 1) * sd(paired_diff) / sqrt(length(paired_diff))
check("paired CI structured mean diff", isTRUE(all.equal(paired_result$values$mean_difference, mean(paired_diff))))
check("paired CI structured upper", isTRUE(all.equal(paired_result$values$upper_endpoint, mean(paired_diff) + paired_me)))

two_var_result <- env$capture_calculator_result(env$twoVarianceRatioCalc(12, 8, 10, 4, conf_level = 0.95, alternative = "two.sided"))
f_stat <- 8 / 4
two_var_p <- min(2 * min(pf(f_stat, 11, 9), 1 - pf(f_stat, 11, 9)), 1)
check("two variance structured statistic", isTRUE(all.equal(two_var_result$values$statistic, f_stat)))
check("two variance structured p value", isTRUE(all.equal(two_var_result$values$p_value, two_var_p)))

one_prop_result <- env$capture_calculator_result(onePropCI(40, 100, 0.95))
one_prop_phat <- 0.4
one_prop_me <- qnorm(0.975) * sqrt(one_prop_phat * (1 - one_prop_phat) / 100)
check("one proportion CI structured lower", isTRUE(all.equal(one_prop_result$values$lower_endpoint, one_prop_phat - one_prop_me)))
check("one proportion CI preserves legacy text", grepl("One Proportion Confidence Interval", one_prop_result$output, fixed = TRUE))

mean_known_result <- env$capture_calculator_result(meanKnownSigmaCI(25, 10, 2, 0.95))
mean_known_me <- qnorm(0.975) * 2 / sqrt(25)
check("mean known sigma CI structured lower", isTRUE(all.equal(mean_known_result$values$lower_endpoint, 10 - mean_known_me)))
check("mean known sigma CI structured upper", isTRUE(all.equal(mean_known_result$values$upper_endpoint, 10 + mean_known_me)))

mean_t_result <- env$capture_calculator_result(meanTCI(16, 20, 4, 0.95))
mean_t_me <- qt(0.975, 15) * 4 / sqrt(16)
check("mean t CI structured lower", isTRUE(all.equal(mean_t_result$values$lower_endpoint, 20 - mean_t_me)))
check("mean t CI structured df", identical(mean_t_result$values$df, 15))

variance_ci_result <- env$capture_calculator_result(varianceCI(12, 3, 0.95))
variance_lower <- 11 * 3^2 / qchisq(0.975, 11)
variance_upper <- 11 * 3^2 / qchisq(0.025, 11)
check("variance CI structured lower", isTRUE(all.equal(variance_ci_result$values$variance_lower, variance_lower)))
check("variance CI structured sd upper", isTRUE(all.equal(variance_ci_result$values$sd_upper, sqrt(variance_upper))))
expect_error("one proportion CI rejects successes over n", onePropCI(12, 10, 0.95), "Successes x cannot be larger than n")
expect_error("mean t CI rejects n under two", meanTCI(1, 10, 2, 0.95), "n must be a whole number at least 2")

mean_z_test_result <- env$capture_calculator_result(meanKnownSigmaTest(25, 104, 100, 10, 0.05, "greater"))
mean_z_stat <- (104 - 100) / (10 / sqrt(25))
mean_z_p <- 1 - pnorm(mean_z_stat)
check("mean known sigma test structured statistic", isTRUE(all.equal(mean_z_test_result$values$statistic, mean_z_stat)))
check("mean known sigma test structured p value", isTRUE(all.equal(mean_z_test_result$values$p_value, mean_z_p)))
check("mean known sigma test preserves legacy text", grepl("One Mean z Test", mean_z_test_result$output, fixed = TRUE))

mean_t_test_result <- env$capture_calculator_result(meanTTest(16, 20, 18, 4, 0.05, "two.sided"))
mean_t_stat <- (20 - 18) / (4 / sqrt(16))
mean_t_p <- 2 * pt(-abs(mean_t_stat), 15)
check("mean t test structured statistic", isTRUE(all.equal(mean_t_test_result$values$statistic, mean_t_stat)))
check("mean t test structured p value", isTRUE(all.equal(mean_t_test_result$values$p_value, mean_t_p)))
check("mean t test structured df", identical(mean_t_test_result$values$df, 15))

one_prop_test_result <- env$capture_calculator_result(onePropTest(55, 100, 0.5, 0.05, "greater"))
one_prop_test_stat <- (0.55 - 0.5) / sqrt(0.5 * 0.5 / 100)
one_prop_test_p <- 1 - pnorm(one_prop_test_stat)
check("one proportion test structured statistic", isTRUE(all.equal(one_prop_test_result$values$statistic, one_prop_test_stat)))
check("one proportion test structured p value", isTRUE(all.equal(one_prop_test_result$values$p_value, one_prop_test_p)))
expect_error("mean known sigma test rejects bad alpha", meanKnownSigmaTest(25, 104, 100, 10, 1, "greater"), "Alpha must be greater than 0 and less than 1")
expect_error("mean t test rejects n under two", meanTTest(1, 20, 18, 4, 0.05, "two.sided"), "n must be a whole number at least 2")
expect_error("one proportion test rejects p0 boundary", onePropTest(5, 10, 1, 0.05, "greater"), "Hypothesized proportion must be greater than 0 and less than 1")

two_mean_known_result <- env$capture_calculator_result(
  twoIndMeansKnownSigma(40, 52, 8, 35, 48, 7, conf_level = 0.95, alternative = "greater")
)
two_mean_diff <- 52 - 48
two_mean_se <- sqrt(8^2 / 40 + 7^2 / 35)
two_mean_stat <- two_mean_diff / two_mean_se
two_mean_p <- 1 - pnorm(two_mean_stat)
check("two mean known sigma structured difference", identical(two_mean_known_result$values$difference, two_mean_diff))
check("two mean known sigma structured statistic", isTRUE(all.equal(two_mean_known_result$values$statistic, two_mean_stat)))
check("two mean known sigma structured p value", isTRUE(all.equal(two_mean_known_result$values$p_value, two_mean_p)))
check("two mean known sigma preserves legacy text", grepl("Two Independent Means", two_mean_known_result$output, fixed = TRUE))

two_prop_result <- env$capture_calculator_result(
  twoIndSampleProp(x1 = 45, n1 = 100, x2 = 30, n2 = 90, conf_level = 0.95, alternative = "greater")
)
p1 <- 45 / 100
p2 <- 30 / 90
diff_p <- p1 - p2
pooled <- (45 + 30) / (100 + 90)
se_test <- sqrt(pooled * (1 - pooled) * (1 / 100 + 1 / 90))
z_stat <- diff_p / se_test
check("two proportion structured difference", isTRUE(all.equal(two_prop_result$values$difference, diff_p)))
check("two proportion structured pooled", isTRUE(all.equal(two_prop_result$values$pooled_proportion, pooled)))
check("two proportion structured p value", isTRUE(all.equal(two_prop_result$values$p_value, 1 - pnorm(z_stat))))
expect_error("two mean known sigma rejects bad sigma", twoIndMeansKnownSigma(40, 52, 0, 35, 48, 7), "sigma1 must be greater than 0")
expect_error("two proportion rejects successes over n", twoIndSampleProp(x1 = 12, n1 = 10, x2 = 3, n2 = 10), "Successes x1 cannot be larger than n1")

uneq_result <- env$capture_calculator_result(
  twoIndMeanUneqVar(12, 20, 4, 15, 17, 5, conf_level = 0.95, alternative = "two.sided")
)
uneq_diff <- 20 - 17
uneq_se <- sqrt(4^2 / 12 + 5^2 / 15)
uneq_stat <- uneq_diff / uneq_se
uneq_df <- ((4^2 / 12) + (5^2 / 15))^2 / (((4^2 / 12)^2) / 11 + ((5^2 / 15)^2) / 14)
uneq_p <- 2 * pt(-abs(uneq_stat), uneq_df)
check("unequal variance two mean structured df", isTRUE(all.equal(uneq_result$values$df, uneq_df)))
check("unequal variance two mean structured p value", isTRUE(all.equal(uneq_result$values$p_value, uneq_p)))
check("unequal variance preserves legacy text", grepl("Unequal Variances", uneq_result$output, fixed = TRUE))

eq_result <- env$capture_calculator_result(
  twoIndMeansEqVar(12, 20, 4, 15, 17, 5, conf_level = 0.95, alternative = "greater")
)
pooled_var <- (11 * 4^2 + 14 * 5^2) / (12 + 15 - 2)
eq_se <- sqrt(pooled_var * (1 / 12 + 1 / 15))
eq_stat <- (20 - 17) / eq_se
eq_p <- 1 - pt(eq_stat, 25)
check("equal variance two mean structured pooled variance", isTRUE(all.equal(eq_result$values$pooled_variance, pooled_var)))
check("equal variance two mean structured p value", isTRUE(all.equal(eq_result$values$p_value, eq_p)))
expect_error("unequal variance rejects n under two", twoIndMeanUneqVar(1, 20, 4, 15, 17, 5), "n1 must be a whole number at least 2")
expect_error("equal variance rejects bad sd", twoIndMeansEqVar(12, 20, 0, 15, 17, 5), "s1 must be greater than 0")

gof_result <- env$capture_calculator_result(
  chiSqGoodnessFit(c(18, 22, 20), expected_probs = c(1/3, 1/3, 1/3), alpha = 0.05, categories = c("A", "B", "C"))
)
gof_expected <- rep(20, 3)
gof_stat <- sum((c(18, 22, 20) - gof_expected)^2 / gof_expected)
check("gof structured statistic", isTRUE(all.equal(gof_result$values$statistic, gof_stat)))
check("gof structured p value", isTRUE(all.equal(gof_result$values$p_value, 1 - pchisq(gof_stat, 2))))
check("gof preserves legacy text", grepl("Chi-Square Goodness of Fit Test", gof_result$output, fixed = TRUE))

observed_table <- matrix(c(10, 20, 30, 25, 15, 20), nrow = 2, byrow = TRUE)
ind_result <- env$capture_calculator_result(chiSqIndependence(observed_table, alpha = 0.05))
ind_expected <- outer(rowSums(observed_table), colSums(observed_table)) / sum(observed_table)
ind_stat <- sum((observed_table - ind_expected)^2 / ind_expected)
check("independence structured statistic", isTRUE(all.equal(ind_result$values$statistic, ind_stat)))
check("independence structured df", isTRUE(all.equal(ind_result$values$df, 2)))
check("independence structured p value", isTRUE(all.equal(ind_result$values$p_value, 1 - pchisq(ind_stat, 2))))

hom_result <- env$capture_calculator_result(chiSqHomogeneity(observed_table, alpha = 0.05))
check("homogeneity structured statistic", isTRUE(all.equal(hom_result$values$statistic, ind_stat)))
check("homogeneity structured expected table", isTRUE(all.equal(hom_result$values$expected_table, ind_expected)))
expect_error("gof rejects probability sum", chiSqGoodnessFit(c(10, 20), expected_probs = c(0.2, 0.2)), "Expected probabilities must add to 1")
expect_error("chi-square rejects one-row table", chiSqIndependence(matrix(c(1, 2), nrow = 1)), "at least 2 rows and 2 columns")
expect_error("chi-square rejects negative count", chiSqHomogeneity(matrix(c(1, -2, 3, 4), nrow = 2)), "nonnegative")

for (fn in c(
  "binomialDistribution", "poissonDistribution", "hypergeometricDistribution",
  "normalProb", "sampleMeanProb", "corrReg", "meanTTest",
  "onePropTest", "twoIndSampleProp", "chiSqIndependence"
)) {
  ref <- env$calculator_reference(fn)
  check(paste("calculator reference use exists for", fn), nzchar(ref$use))
  check(paste("calculator reference code exists for", fn), nzchar(ref$code))
}
all_calculator_ids <- unique(unname(env$function_choices))
all_calculator_refs <- lapply(all_calculator_ids, env$calculator_reference)
all_reference_examples <- vapply(all_calculator_refs, `[[`, character(1), "example")
all_reference_uses <- vapply(all_calculator_refs, `[[`, character(1), "use")
check("every calculator has a specific quick reference",
      !any(all_reference_examples == "Match the problem wording to the calculator name and fill only the boxes the problem gives."))
check("calculator quick reference use text is unique by calculator",
      length(unique(all_reference_uses)) == length(all_reference_uses))

check("examples include one numeric sample method choice",
      any(env$example_guides$title == "One Numeric Sample"))
check("examples include object not found R help",
      any(env$example_guides$title == "Object Not Found"))
check("examples removed student portfolio option",
      !any(env$example_guides$title == "Student Portfolio Plan"))
check("examples removed shiny input output option",
      !any(env$example_guides$title == "Shiny Input/Output"))
check("examples removed broad database query option",
      !any(env$example_guides$title == "Database Query"))
check("search includes paired method choice",
      any(env$search_guides$label == "Method Choice - Paired Before/After Numeric Data"))
check("search includes join row count changed R help",
      any(env$search_guides$label == "R Code Help - Join Row Count Changed"))
check("search removed student pathway options",
      !any(grepl("Student Pathway|portfolio|career|resume", env$search_guides$label, ignore.case = TRUE)))
formatted_example <- env$format_example_code("n <- 21; xbar <- 6.4; s <- 1.1; conf <- .99\nalpha <- 1 - conf")
check("example code semicolon assignments split", grepl("xbar <- 6.4\ns <- 1.1", formatted_example, fixed = TRUE))
check("concept markup helper exists", is.function(env$render_concept_items))
check("formula markup helper exists", is.function(env$render_formula_items))
check("markdownish helper exists", is.function(env$render_markdownish))
check("content format plan exists", exists("content_format_plan", envir = env) && length(env$content_format_plan) >= 4)
concept_markup <- paste(as.character(env$render_concept_items(c(
  "Population: the entire group being studied.",
  "Qualitative data are categories or labels.",
  "Always interpret intervals in context."
))), collapse = "\n")
formula_markup <- paste(as.character(env$render_formula_items(c(
  "Mean: xbar = sum(x) / n",
  "Standard deviation = sqrt(variance)",
  "Use the matching calculator for conceptual topics."
))), collapse = "\n")
check("concept rendering uses natural concept list", grepl("concept-list-clean", concept_markup, fixed = TRUE))
check("concept rendering keeps unlabeled prose plain", grepl("concept-prose", concept_markup, fixed = TRUE))
check("concept rendering does not bold terms", !grepl("<strong", concept_markup, fixed = TRUE))
check("concept rendering does not use concept term class", !grepl("concept-term", concept_markup, fixed = TRUE))
check("formula rendering uses shared rows", grepl("reference-row formula-row", formula_markup, fixed = TRUE))
check("formula rendering keeps neutral equation class", grepl("formula-equation-inline", formula_markup, fixed = TRUE))
ui_text <- paste(as.character(env$ui), collapse = "\n")
check("examples tab hidden from main navigation", !grepl(">Examples<", ui_text, fixed = TRUE))
check("concept bold term class removed", !grepl("concept-term", ui_text, fixed = TRUE))
server_reference_text <- paste(readLines("R/server_reference.R", warn = FALSE), collapse = "\n")
check("formula to concept link exists", grepl("Read related concept", server_reference_text, fixed = TRUE))

two_z <- env$capture_calculator_result(twoIndMeansKnownSigma(50, 10.3911, 1, 50, 10, 1, alpha = 0.05))
check("two-mean z test uses unrounded z for p-value", abs(two_z$values$p_value - 2 * pnorm(-1.9555)) < 1e-9)
check("two-mean z test decision follows exact p-value", identical(two_z$values$decision, "Fail to Reject"))
left_t <- env$capture_calculator_result(twoIndMeansEqVar(10, 4, 1, 10, 5, 1, alpha = 0.05, alternative = "less"))
check("left-tailed two-sample critical value is negative", left_t$values$critical_value < 0)
check("legacy two-sample test defaults alpha", !inherits(try(capture.output(twoIndMeanUneqVar(10, 5, 1, 10, 4, 1)), silent = TRUE), "try-error"))

cat("Calculator reliability checks passed\n")
