# Server wiring for calculator inputs, outputs, references, and generated R code.

register_calculator_server <- function(input, output, session) {
  output$inputs <- renderUI({
    fn <- input$fn
    tagList(
      helpText("For lists of values, separate entries with commas. Example: 12, 8, 15, 10"),
      helpText("If both confidence level and alpha are shown, enter only one and leave the other blank."),
      switch(fn,
        feelingLucky = tagList(
          selectInput("lucky_analysis", "What do you want?", c(
            "Auto: show all matching useful results" = "auto",
            "Raw data: descriptive stats and mean CI" = "raw_all",
            "Descriptive stats from raw data" = "descriptive",
            "Mean confidence interval" = "mean_ci",
            "Proportion: point estimate and CI" = "prop_all",
            "Proportion point estimate only" = "point_prop",
            "Proportion confidence interval only" = "prop_ci",
            "Minimum sample size for mean" = "sample_size_mean",
            "Minimum sample size for proportion" = "sample_size_prop",
            "Simple probability" = "simple_probability",
            "Counting rule" = "counting",
            "Independent probability product" = "independent_probability",
            "Discrete distribution table" = "discrete_distribution",
            "Binomial distribution" = "binomial",
            "Poisson distribution" = "poisson",
            "Hypergeometric / without replacement" = "hypergeometric",
            "Normal probability" = "normal_probability"
          )),
          textAreaInput("lucky_raw_data", "Raw data, if given", "", placeholder = "15, 12, 13, 18, 14"),
          selectInput("lucky_sample_type", "Raw data type", c("Sample" = "sample", "Population" = "population")),
          selectInput("lucky_sigma_mode", "Mean CI sigma setting", c("Unknown sigma / use sample s" = "unknown", "Known sigma / use z" = "known")),
          numericInput("lucky_x", "Successes x", NA),
          numericInput("lucky_n", "n", NA),
          numericInput("lucky_xbar", "xbar, if raw data is not given", NA),
          numericInput("lucky_s", "sample standard deviation s", NA),
          numericInput("lucky_sigma", "population standard deviation sigma", NA),
          numericInput("lucky_conf_level", "Confidence level", 0.95),
          numericInput("lucky_margin_error", "Margin of error / accuracy", NA),
          numericInput("lucky_p_hat", "Estimated proportion p-hat", NA),
          numericInput("lucky_favorable", "Favorable outcomes", NA),
          numericInput("lucky_total", "Total outcomes", NA),
          textInput("lucky_slot_options", "Counting rule slot options", "", placeholder = "26, 26, 10, 10, 10"),
          textInput("lucky_event_probs", "Independent probabilities", "", placeholder = "0.25, 0.2549"),
          textInput("lucky_dist_x", "Distribution x values", "", placeholder = "6, 7, 8, 9, 10"),
          textInput("lucky_dist_prob", "Distribution probabilities", "", placeholder = "0.3, 0.2, 0.1, 0.2, 0.2"),
          selectInput("lucky_prob_type", "Probability wording", c("P(X = value)" = "equal", "P(X < value)" = "less", "P(X <= value) / no more than" = "less_equal", "P(X > value)" = "greater", "P(X >= value) / at least" = "greater_equal", "P(lower <= X <= upper)" = "between")),
          numericInput("lucky_prob_value", "Probability value / lower value", NA),
          numericInput("lucky_upper_value", "Upper value, only for between", NA),
          numericInput("lucky_n_trials", "Binomial n trials", NA),
          numericInput("lucky_p_success", "Binomial p success", NA),
          numericInput("lucky_poisson_rate", "Poisson average rate", NA),
          numericInput("lucky_poisson_periods", "Poisson number of periods", NA),
          numericInput("lucky_total_items", "Hypergeometric total items", NA),
          numericInput("lucky_success_items", "Hypergeometric success/target items", NA),
          numericInput("lucky_sample_size", "Hypergeometric draws/sample size", NA),
          numericInput("lucky_normal_mean", "Normal mean", NA),
          numericInput("lucky_normal_sd", "Normal standard deviation", NA),
          numericInput("lucky_lower", "Normal lower bound", NA),
          numericInput("lucky_upper", "Normal upper bound", NA),
          helpText("This applies across topic areas. Fill only the boxes the problem gives; leave everything else blank or NA.")
        ),
        frequencyPercent = tagList(
          textAreaInput("frequencies", "Frequencies", "2, 4, 7, 8, 6, 1"),
          textInput("categories", "Categories (optional)", "68-71, 72-75, 76-79, 80-83, 84-87, 88-91"),
          helpText("Use this for percent below/above a category by adding the relevant frequencies first, then divide by total.")
        ),
        selectedPercent = tagList(
          textAreaInput(
            "frequency_pasted_table",
            "Paste copied count table (optional)",
            "",
            rows = 5,
            placeholder = "Number of Stars\tFrequency\n1\t2\n2\t3\n3\t7\n4\t7\n5\t6"
          ),
          textAreaInput("frequencies", "Frequencies", "2, 4, 7, 8, 6, 1"),
          textInput("categories", "Categories (optional)", "1 star, 2 stars, 3 stars, 4 stars, 5 stars"),
          textInput("selected_positions", "Positions to include", "1, 2, 3"),
          helpText("Use for questions like less than 4 stars, more than 66 F, or percent from Big 12. Paste the copied table if available, then enter row positions to add, such as 1,2,3.")
        ),
        percentToCount = tagList(numericInput("total", "Total", 3000), numericInput("percent", "Percent", 20)),
        graphRatio = tagList(numericInput("numerator_value", "Value on top / later year", 290), numericInput("denominator_value", "Value on bottom / comparison year", 245), helpText("Use for graph questions like how many times as large Year 5 is compared to Year 2.")),
        descStats = tagList(textAreaInput("data", "Data", "8, 14, 12, 4, 13, 15"), checkboxInput("sample", "Sample data", TRUE), helpText("Sample data uses n - 1 for variance/standard deviation. Population data uses n.")),
        modeClass = tagList(textAreaInput("data", "Data", "-9, 11, 7, 11, 7, -9"), helpText("Use when a problem asks for no mode, unimodal, bimodal, or multimodal. The output lists the exact modes.")),
        fiveNumberSummary = tagList(textAreaInput("data", "Data", "8, 24, 10, 21, 17, 6, 4, 19, 23, 17, 10, 5, 3, 2, 18"), helpText("Use for five-number summary or box plot questions. Enter answers as min, Q1, median, Q3, max.")),
        groupedStats = tagList(textAreaInput("midpoints", "Class midpoints", "6.45, 7.45, 8.45, 9.45, 10.45, 11.45, 12.45"), textAreaInput("frequencies", "Frequencies", "23, 24, 36, 44, 48, 30, 17"), checkboxInput("sample", "Sample grouped data", FALSE), helpText("Use grouped tables by entering each class midpoint and frequency. Example: 6-6.9 has midpoint 6.45.")),
        trimmedMean = tagList(textAreaInput("data", "Data", "0.278, 0.313, 0.285, 0.313, 0.299, 0.315, 0.304, 0.306, 0.306, 0.306, 0.308, 0.317, 0.317, 0.318, 0.322, 0.322, 0.309, 0.324, 0.310, 0.326"), numericInput("trim_percent", "Trim percent", 10), helpText("Use for questions like 10% trimmed mean. Enter the percent as 10, not 0.10.")),
        weightedAverage = tagList(textAreaInput("weighted_values", "Values", "473.93, 337.85, 219.77"), textAreaInput("weights", "Weights", "31, 31, 30"), helpText("Use when values are weighted by days, frequency, credits, or another weight.")),
        missingFromMean = tagList(textAreaInput("known_values", "Known values", "46.06, 39.70, 47.29, 39.13, 44.66, 38.53, 46.89"), numericInput("total_n", "Total number of values", 8), numericInput("target_mean", "Known mean", 39.77), helpText("Use when one value is missing but the overall mean and total n are given.")),
        coeffVariation = tagList(numericInput("mean_val", "Mean", 400), numericInput("sd_val", "Standard deviation", 10)),
        chebyshev = tagList(numericInput("mean_val", "Mean", 51), numericInput("sd_val", "Standard deviation", 7), selectInput("cheb_mode", "Question type", c("Find range from k" = "range_from_k", "Find range from percent" = "range_from_percent", "Find minimum percent between values" = "percent_between")), numericInput("k_value", "k (only for range from k)", 3), numericInput("percent", "Percent (only for range from percent)", 88.89), numericInput("lower", "Lower value (only for percent between)", 37), numericInput("upper", "Upper value (only for percent between)", 65)),
        empiricalRule = tagList(numericInput("mean_val", "Mean", 8.06), numericInput("sd_val", "Standard deviation", 1.52), numericInput("lower", "Lower value", 6.54), numericInput("upper", "Upper value", 9.58), helpText("Use for bell-shaped distribution / empirical rule questions. Output is 68%, 95%, or 99.7% when the interval matches 1, 2, or 3 standard deviations.")),
        corrReg = tagList(textAreaInput("x", "x values", "29, 85, 48, 54, 60, 41, 100"), textAreaInput("y", "y values", "2.9, 8.5, 3.6, 3.2, 4.8, 3.3, 10"), helpText("Each x value must match the y value in the same position.")),
        predictY = tagList(textAreaInput("x", "x values", "29, 85, 48, 54, 60, 41, 100"), textAreaInput("y", "y values", "2.9, 8.5, 3.6, 3.2, 4.8, 3.3, 10"), numericInput("new_x", "New x", 75), helpText("Use this when a regression equation/data set is used to estimate y for a given x.")),
        simpleProb = tagList(
          textAreaInput(
            "simple_pasted_table",
            "Paste copied one-row count table (optional)",
            "",
            rows = 5,
            placeholder = "Counting People\nBlonde\tRed\tBrown\tBlack\tGray\n36\t40\t34\t55\t43"
          ),
          textInput("simple_target", "Target category from pasted table", "Gray"),
          numericInput("favorable", "Manual favorable", 47),
          numericInput("total", "Manual total", 215),
          helpText("If the problem gives a copyable one-row count table, paste it and enter the category you need. Otherwise use manual favorable and total.")
        ),
        diceSum = tagList(
          selectInput("dice_type", "Question wording", c("Sum equals value" = "equal", "Sum is less than value" = "less", "Sum is no more than / at most value" = "less_equal", "Sum is greater than value" = "greater", "Sum is at least value" = "greater_equal")),
          numericInput("dice_value", "Sum value", 5),
          helpText("Use for a standard pair of six-sided dice. Total outcomes are 36.")
        ),
        complementProb = tagList(numericInput("prob_event", "P(Event) as decimal", 0.78), helpText("Use for wording like not, none, no more than the opposite, or at least one. Enter probability as a decimal, so 78% is 0.78.")),
        additionRule = tagList(
          numericInput("a_count", "A count", 13),
          numericInput("b_count", "B count", 4),
          numericInput("overlap_count", "Overlap count", 1),
          numericInput("total_count", "Total count", 52),
          helpText("Use for OR problems. Example: spade or ace uses 13 spades + 4 aces - 1 ace of spades over 52.")
        ),
        conditionalProb = tagList(numericInput("joint_count", "Joint count", 173), numericInput("given_count", "Given count", 473), helpText("Use for wording like given, among, or selected from. Joint count means both conditions happen.")),
        countingRule = tagList(textInput("slot_options", "Options for each slot", "26, 26, 10, 10, 10, 10"), helpText("Use for account numbers/codes/passwords. Example: 2 letters followed by 4 digits with repetition allowed is 26, 26, 10, 10, 10, 10.")),
        sequenceProbability = tagList(
          selectInput("seq_replacement", "Playing cards / replacement option", c("Use manual totals" = "manual", "Playing cards without replacement" = "without", "Playing cards with replacement" = "with")),
          textInput("seq_favorable", "Favorable counts in order", "13, 13"),
          textInput("seq_total", "Manual total counts in order", "52, 51"),
          helpText("Playing card note: standard deck has 52 cards, 13 per suit, 4 per rank. Choose with/without replacement to auto-fill the card denominators; use manual totals for non-card problems.")
        ),
        independentProbability = tagList(textInput("event_probs", "Probabilities as decimals", "0.07, 0.04"), helpText("Use when events are independent and you need both/all to happen. Example: fail two independent tests: 0.07, 0.04.")),
        tableProbability = tagList(
          textAreaInput(
            "ch6_table",
            "Paste copied table",
            "",
            rows = 7,
            placeholder = "State\tOwns a Pet\tDoes Not Own a Pet\nArizona\t56\t32\nColorado\t63\t64\nFlorida\t121\t37\nSouth Carolina\t134\t56"
          ),
          selectInput("table_prob_mode", "Question type", c("Row probability" = "row", "Column probability" = "column", "Joint probability" = "joint", "Column given row" = "given_row", "Row given column" = "given_column", "Row OR column" = "row_or_column")),
          textInput("table_row", "Row label, if needed", "Florida"),
          textInput("table_col", "Column label, if needed", ""),
          helpText("Use row probability for wording like lives in Florida. Use conditional modes for wording like given/among. Use OR mode for either/or wording.")
        ),
        factorialArrangements = tagList(
          selectInput("factorial_mode", "Question style", c("Factorial ratio / repeated groups" = "parts", "Combination nCr" = "combination")),
          numericInput("factorial_n", "n or top factorial", 13),
          numericInput("factorial_r", "r (only for nCr)", 6),
          textInput("factorial_parts", "Denominator factorial parts", "6, 7"),
          helpText("Use Combination nCr when the problem shows nCr or asks combinations. Use factorial parts for expressions like 13!/(6!7!) or arrangements like 9 rolls with 3 fours, 5 sixes, and 1 two.")
        ),
        bayesProbability = tagList(textInput("bayes_priors", "Group probabilities as decimals", "0.23, 0.56, 0.21"), textInput("bayes_event_probs", "Event probabilities within each group", "0.40, 0.42, 0.18"), numericInput("bayes_target", "Target group position", 2), helpText("Use for P(group | event). Example target position 2 means the second group, such as Democrats.")),
        discreteDist = tagList(
          textAreaInput(
            "dist_pasted_table",
            "Paste copied probability table (optional)",
            "",
            rows = 5,
            placeholder = "x\t6\t7\t8\t9\t10\nP(X = x)\t0.3\t0.2\t0.1\t0.2\t0.2"
          ),
          textAreaInput("x", "x values", "6, 7, 8, 9, 10"),
          textAreaInput("prob", "Probabilities as decimals", "0.3, 0.2, 0.1, 0.2, 0.2"),
          helpText("Use for a probability distribution table. If a Copy Data button is available, paste it above; otherwise enter x and probabilities manually.")
        ),
        probFromDist = tagList(
          textAreaInput(
            "dist_pasted_table",
            "Paste copied probability table (optional)",
            "",
            rows = 5,
            placeholder = "x\t6\t7\t8\t9\t10\nP(X = x)\t0.3\t0.2\t0.1\t0.2\t0.2"
          ),
          textAreaInput("x", "x values", "6, 7, 8, 9, 10"),
          textAreaInput("prob", "Probabilities as decimals", "0.3, 0.2, 0.1, 0.2, 0.2"),
          selectInput("type", "Question wording", c("P(X = value)" = "equal", "P(X < value)" = "less", "P(X <= value)" = "less_equal", "P(X > value)" = "greater", "P(X >= value)" = "greater_equal", "P(lower <= X <= upper)" = "between")),
          numericInput("value", "Value or lower value", 9),
          numericInput("upper_value", "Upper value (only for between)", NA),
          helpText("Use for table questions like P(X <= 9) or P(X < 8). Enter probabilities as decimals, so 30% is 0.30.")
        ),
        expectedMonetaryValue = tagList(
          textAreaInput("emv_outcomes", "Money outcomes", "40000, -10000"),
          textAreaInput("emv_prob", "Probabilities as decimals", "0.60, 0.40"),
          helpText("Use for investment, game, profit, loss, or long-run average money questions.")
        ),
        discreteUniform = tagList(
          numericInput("min_value", "Smallest whole-number value", 1),
          numericInput("max_value", "Largest whole-number value", 9),
          selectInput("type", "Question wording", c("Summary only" = "summary", "P(X = value)" = "equal", "P(X < value)" = "less", "P(X <= value)" = "less_equal", "P(X > value)" = "greater", "P(X >= value)" = "greater_equal", "P(lower <= X <= upper)" = "between")),
          numericInput("value", "Value or lower value", 4),
          numericInput("upper_value", "Upper value (only for between)", 6),
          helpText("Use when all whole-number outcomes are equally likely. Example: delivery dates June 1 through June 9.")
        ),
        binomialDistribution = tagList(
          numericInput("n", "Number of trials n", 19),
          numericInput("p", "Success probability p", 0.20),
          selectInput("type", "Question wording", c("P(X = value)" = "equal", "P(X < value)" = "less", "P(X <= value) / no more than" = "less_equal", "P(X > value)" = "greater", "P(X >= value) / at least" = "greater_equal", "P(lower <= X <= upper)" = "between")),
          numericInput("value", "Value or lower value", 1),
          numericInput("upper_value", "Upper value (only for between)", NA),
          helpText("Use for fixed n, independent trials, two outcomes, and same p. Example: defective/non-defective with p = 0.20.")
        ),
        poissonDistribution = tagList(
          numericInput("rate", "Average rate per period", 0.5),
          numericInput("periods", "Number of periods", 7),
          selectInput("type", "Question wording", c("P(X = value)" = "equal", "P(X < value)" = "less", "P(X <= value) / no more than" = "less_equal", "P(X > value)" = "greater", "P(X >= value) / at least" = "greater_equal", "P(lower <= X <= upper)" = "between")),
          numericInput("value", "Value or lower value", 1),
          numericInput("upper_value", "Upper value (only for between)", NA),
          helpText("Use for counts over time/space when an average rate is given. Example: 0.5 crashes per day for 7 days gives lambda = 3.5.")
        ),
        hypergeometricDistribution = tagList(
          numericInput("total_items", "Total items", 20),
          numericInput("success_items", "Target-color/success items", 10),
          numericInput("sample_size", "Draws or sample size", 5),
          selectInput("type", "Question wording", c("Expected value only" = "expected", "P(X = value)" = "equal", "P(X < value)" = "less", "P(X <= value) / no more than" = "less_equal", "P(X > value)" = "greater", "P(X >= value) / at least" = "greater_equal", "P(lower <= X <= upper)" = "between")),
          numericInput("value", "Value or lower value", 4),
          numericInput("upper_value", "Upper value (only for between)", NA),
          helpText("Marble problem calculator. Also use this for light bulbs/cards/items sampled without replacement.")
        ),
        uniformDist = tagList(numericInput("a", "Minimum a", 20), numericInput("b", "Maximum b", 50), probability_bounds_ui(), helpText("For time intervals, convert times to minutes from the start. Example: 8:00 = 0, 8:10 = 10, 8:35 = 35.")),
        zScoreCalc = tagList(numericInput("x_value", "x value", 39.2), numericInput("mean_val", "Mean", 35.3), numericInput("sd_val", "Standard deviation", 30.6)),
        normalProb = tagList(numericInput("mean_val", "Mean", 0), numericInput("sd_val", "Standard deviation", 1), probability_bounds_ui(), helpText("Use no more than/at most for left-tail probability; use at least/more than for right-tail probability.")),
        invNormal = tagList(numericInput("prob_left", "Area left", 0.95), numericInput("mean_val", "Mean", 0), numericInput("sd_val", "Standard deviation", 1)),
        normalApproxBinomial = tagList(numericInput("n", "Number of trials n", 109), numericInput("p", "Success probability p", 0.29), selectInput("type", "Question wording", c("P(X = value)" = "equal", "P(X < value)" = "less", "P(X <= value) / no more than" = "less_equal", "P(X > value)" = "greater", "P(X >= value) / at least / no less than" = "greater_equal", "P(lower <= X <= upper)" = "between")), numericInput("value", "Value or lower value", 78), numericInput("upper_value", "Upper value (only for between)", NA), helpText("Use only when the problem asks for normal approximation to binomial or continuity correction.")),
        sampleMeanProb = tagList(numericInput("mu", "mu", 70), numericInput("sigma", "sigma", 8), numericInput("n", "n", 123), probability_bounds_ui(), helpText("Use this for wording about sample mean, average, or xbar.")),
        samplePropProb = tagList(numericInput("p", "p as decimal", 0.43), numericInput("n", "n", 100), probability_bounds_ui(), helpText("Use this for wording about sample proportion, percent, rate, or phat. Enter 43% as 0.43.")),
        pointEstimate = tagList(selectInput("point_type", "Estimate type", c("Proportion phat = x/n" = "proportion", "Mean xbar" = "mean", "Variance from sample standard deviation" = "variance")), numericInput("x_success", "Successes x", 51), numericInput("n", "n", 221), numericInput("xbar", "xbar", 137), numericInput("s", "Sample standard deviation s", 15.9), helpText("Use only the boxes needed for the selected estimate type.")),
        meanCIFromData = tagList(
          textAreaInput("ch10_raw_data", "Raw data", "15, 12, 13, 18, 14, 17, 17, 19, 11, 14"),
          numericInput("conf_level", "Confidence level", 0.95),
          selectInput("ch10_sigma_mode", "Sigma setting", c("Unknown sigma / sample s / t interval" = "unknown", "Known sigma / z interval" = "known")),
          numericInput("sigma", "Known sigma (only if given)", NA),
          helpText("Use this for mean confidence interval questions that first ask for xbar and s, then ask for a mean confidence interval. If the problem gives only sample data/sample standard deviation, use Unknown sigma.")
        ),
        onePropCI = tagList(numericInput("x_success", "Successes x", 51), numericInput("n", "n", 221), numericInput("conf_level", "Confidence level", 0.95)),
        meanKnownSigmaCI = tagList(numericInput("n", "n", 62), numericInput("xbar", "xbar", 137), numericInput("sigma", "sigma", 24), numericInput("conf_level", "Confidence level", 0.90)),
        meanTCI = tagList(numericInput("n", "n", 12), numericInput("xbar", "xbar", 15), numericInput("s", "s", 2.26), numericInput("conf_level", "Confidence level", 0.99)),
        sampleSize = tagList(selectInput("sample_size_type", "Sample size type", c("Proportion" = "proportion", "Mean with known sigma" = "mean")), numericInput("conf_level", "Confidence level", 0.95), numericInput("margin_error", "Margin of error", 0.04), numericInput("p_hat", "Estimated proportion (blank/NA uses 0.50)", 0.27), numericInput("sigma", "sigma for mean sample size", 20), helpText("For proportions, enter p-hat if given. If not given, use 0.50. For mean questions, enter sigma.")),
        varianceCI = tagList(numericInput("n", "n", 13), numericInput("s", "Sample standard deviation s", 4.76), numericInput("conf_level", "Confidence level", 0.90), helpText("Use for confidence intervals for population variance or standard deviation.")),
        meanKnownSigmaTest = tagList(numericInput("n", "n", 120), numericInput("xbar", "xbar", 61), numericInput("mu0", "Claim mu0", 61), numericInput("sigma", "sigma", 14), numericInput("alpha", "alpha", 0.05), selectInput("alternative", "Alternative", c("different / not equal" = "two.sided", "less than / below" = "less", "greater than / above" = "greater")), helpText("Choose the alternative from the claim wording, not from the sample data.")),
        meanTTest = tagList(numericInput("n", "n", 120), numericInput("xbar", "xbar", 90), numericInput("mu0", "Claim mu0", 94), numericInput("s", "s", 15), numericInput("alpha", "alpha", 0.05), selectInput("alternative", "Alternative", c("different / not equal" = "two.sided", "less than / below" = "less", "greater than / above" = "greater")), helpText("Use this when sigma is unknown and the sample standard deviation s is given.")),
        onePropTest = tagList(numericInput("x_success", "Successes x", 44), numericInput("n", "n", 100), numericInput("p0", "Claim p0 as decimal", 0.40), numericInput("alpha", "alpha", 0.02), selectInput("alternative", "Alternative", c("different / not equal" = "two.sided", "less than / below" = "less", "greater than / above" = "greater")), helpText("Use for a claim about one population proportion p. Enter 40% as 0.40.")),
        varianceTest = tagList(numericInput("n", "n", 20), numericInput("s", "Sample standard deviation s", 3.2), numericInput("sigma0", "Claimed standard deviation", 2.9), numericInput("alpha", "alpha", 0.05), selectInput("alternative", "Alternative", c("different / not equal" = "two.sided", "less than / below" = "less", "greater than / above" = "greater")), helpText("Use for claims about one population standard deviation or variance. If the claim gives variance, enter its square root as the claimed standard deviation.")),
        twoIndSampleProp = tagList(numericInput("x1", "x1", 28), numericInput("n1", "n1", 160), numericInput("x2", "x2", 12), numericInput("n2", "n2", 140), textInput("conf_level", "Confidence level (leave blank if using alpha)", "0.90"), textInput("alpha", "Alpha (leave blank if using confidence level)", ""), selectInput("alternative", "Alternative", c("two.sided", "less", "greater"))),
        twoIndMeansKnownSigma = tagList(numericInput("n1", "n1", 163), numericInput("x1", "xbar1", 73.8), numericInput("sigma1", "sigma1", 9.17), numericInput("n2", "n2", 179), numericInput("x2", "xbar2", 76.2), numericInput("sigma2", "sigma2", 15.25), textInput("conf_level", "Confidence level (leave blank if using alpha)", "0.95"), textInput("alpha", "Alpha (leave blank if using confidence level)", ""), selectInput("alternative", "Alternative", c("two.sided", "less", "greater"))),
        twoIndMeanUneqVar = tagList(numericInput("n1", "n1", 6), numericInput("x1", "xbar1", 16.2), numericInput("s1", "s1", 1.6), numericInput("n2", "n2", 15), numericInput("x2", "xbar2", 17.5), numericInput("s2", "s2", 1.2), textInput("conf_level", "Confidence level (leave blank if using alpha)", ""), textInput("alpha", "Alpha (leave blank if using confidence level)", "0.05"), selectInput("alternative", "Alternative", c("two.sided", "less", "greater"))),
        twoIndMeansEqVar = tagList(numericInput("n1", "n1", 11), numericInput("x1", "xbar1", 75.8), numericInput("s1", "s1", 6.8), numericInput("n2", "n2", 19), numericInput("x2", "xbar2", 80.1), numericInput("s2", "s2", 5.8), textInput("conf_level", "Confidence level (leave blank if using alpha)", ""), textInput("alpha", "Alpha (leave blank if using confidence level)", "0.01"), selectInput("alternative", "Alternative", c("two.sided", "less", "greater")), helpText("Use only when the problem says population variances are equal or assume equal variances.")),
        pairedDiffCI = tagList(textAreaInput("before_values", "First / before values", "7.1, 6.8, 8.2, 7.9"), textAreaInput("after_values", "Second / after values", "6.4, 6.2, 7.5, 7.1"), numericInput("conf_level", "Confidence level", 0.95), helpText("Use for paired/dependent samples: same subjects measured twice, before/after, or with/without.")),
        twoVarianceRatio = tagList(numericInput("n1", "n1", 17), numericInput("var1", "Sample variance 1", 34.86), numericInput("n2", "n2", 19), numericInput("var2", "Sample variance 2", 15.06), textInput("conf_level", "Confidence level (leave blank if using alpha)", ""), textInput("alpha", "Alpha (leave blank if using confidence level)", "0.10"), selectInput("alternative", "Alternative", c("two.sided", "less", "greater")), helpText("Use when the problem gives sample variances and asks for an F statistic, F test, or confidence interval for variance ratio.")),
        chiSqGoodnessFit = tagList(
          textAreaInput(
            "gof_pasted_table",
            "Paste copied count table (optional)",
            "",
            rows = 7,
            placeholder = "Candy Colors\n\tBrown\tYellow\tRed\tOrange\tGreen\tBlue\nNumber of candies\t190\t185\t110\t168\t117\t189\n\nDays of the Week\tNumber of Calls\nMon\t18\nTue\t14\nWed\t15\nThu\t16\nFri\t17"
          ),
          helpText("If you paste a count table here, the app will collect categories and observed counts automatically. Percentage-only tables are not valid for chi-square unless you also have counts."),
          textAreaInput("observed", "Manual observed counts", "22, 29, 16, 34, 19"),
          textInput("categories", "Manual categories", "Mon, Tue, Wed, Thu, Fri"),
          textInput("expected_probs", "Expected probabilities (blank = equal)", ""),
          textInput("alpha", "alpha (blank = 0.05)", "0.01"),
          textInput("claim", "Claim", "the categories are equally distributed"),
          helpText("Manual fallback: leave expected probabilities blank when the problem says equally distributed. If alpha is not given, leave it blank to use 0.05.")
        ),
        chiSqIndependence = tagList(
          textAreaInput(
            "pasted_table",
            "Paste copied observed table (optional)",
            "",
            rows = 8,
            placeholder = "Observed Sample of Voters\n\tDemocrat\tRepublican\tTotal\nDoctor\t13\t3\t16\nLawyer\t37\t9\t46\nTeacher\t21\t18\t39\nFarmer\t40\t31\t71\nLaborer\t15\t12\t27\nTotal\t126\t73\t199"
          ),
          helpText("If you paste the full table here, the app will collect row labels, column labels, and observed counts automatically. It ignores Total rows/columns."),
          textAreaInput("matrix_vals", "Manual table values by row", "36, 47, 47\n42, 36, 41"),
          numericInput("nrow", "Manual number of rows", 2),
          textInput("row_names", "Manual row names, optional", "On Campus, Off Campus"),
          textInput("col_names", "Manual column names, optional", "Below Avg, Average, Above Avg"),
          textInput("alpha", "alpha (blank = 0.05)", "0.05"),
          helpText("Manual fallback: paste only observed counts, each row on a new line, values separated with commas.")
        ),
        chiSqHomogeneity = tagList(textAreaInput("matrix_vals", "Table values by row", "49, 43\n56, 62"), numericInput("nrow", "Number of rows", 2), textInput("row_names", "Row names, optional", "Vaccinated, Not Vaccinated"), textInput("col_names", "Column names, optional", "Diseased, Not Diseased"), textInput("alpha", "alpha (blank = 0.05)", "0.10"), helpText("Use homogeneity when separate groups/populations are compared across the same categories. Blank alpha uses 0.05."))
      )
    )
  })

  output$quick_reference <- renderUI({
    ref <- calculator_reference(input$fn)
    details <- calculator_reference_details(input$fn)
    tags$div(
      class = "reference-box",
      tags$p(tags$strong("Use when: "), ref$use),
      tags$p(tags$strong("Example wording: "), ref$example),
      tags$p(tags$strong("Check before running: "), details$check),
      tags$p(tags$strong("Assumptions and cautions: "), details$assumptions),
      tags$p(tags$strong("How to interpret output: "), details$interpret),
      tags$p(tags$strong("Simple R template:")),
      tags$pre(ref$code)
    )
  })

  build_r_code <- function() {
    fn <- input$fn
    switch(fn,
      feelingLucky = {
        data <- parse_data_numbers(input$lucky_raw_data)
        lines <- c()
        if (length(data)) {
          lines <- c(lines, paste0("x <- ", code_vector(data)), "mean(x)", "sd(x)")
          if (!is.null(input$lucky_conf_level) && !is.na(input$lucky_conf_level)) {
            alpha <- 1 - input$lucky_conf_level
            if (input$lucky_sigma_mode == "known") {
              lines <- c(lines, paste0("alpha <- ", format_num_for_code(alpha)), paste0("sigma <- ", format_num_for_code(input$lucky_sigma)), "mean(x) + c(-1, 1) * qnorm(1 - alpha/2) * sigma / sqrt(length(x))")
            } else {
              lines <- c(lines, paste0("alpha <- ", format_num_for_code(alpha)), "mean(x) + c(-1, 1) * qt(1 - alpha/2, length(x) - 1) * sd(x) / sqrt(length(x))")
            }
          }
        }
        if (!is.null(input$lucky_x) && !is.na(input$lucky_x) && !is.null(input$lucky_n) && !is.na(input$lucky_n)) {
          lines <- c(lines, paste0("x_success <- ", input$lucky_x), paste0("n <- ", input$lucky_n), "phat <- x_success / n")
        }
        if (!is.null(input$lucky_favorable) && !is.na(input$lucky_favorable) && !is.null(input$lucky_total) && !is.na(input$lucky_total)) {
          lines <- c(lines, paste0("favorable <- ", input$lucky_favorable), paste0("total <- ", input$lucky_total), "favorable / total")
        }
        slots <- parse_data_numbers(input$lucky_slot_options)
        if (length(slots)) {
          lines <- c(lines, paste0("slot_options <- ", code_vector(slots)), "prod(slot_options)")
        }
        probs <- parse_data_numbers(input$lucky_event_probs)
        if (length(probs)) {
          lines <- c(lines, paste0("probabilities <- ", code_vector(probs)), "prod(probabilities)")
        }
        dist_x <- parse_data_numbers(input$lucky_dist_x)
        dist_prob <- parse_data_numbers(input$lucky_dist_prob)
        if (length(dist_x) && length(dist_prob)) {
          lines <- c(lines, paste0("x <- ", code_vector(dist_x)), paste0("prob <- ", code_vector(dist_prob)), "sum(x * prob)")
        }
        if (!is.null(input$lucky_n_trials) && !is.na(input$lucky_n_trials) && !is.null(input$lucky_p_success) && !is.na(input$lucky_p_success) && !is.null(input$lucky_prob_value) && !is.na(input$lucky_prob_value)) {
          lines <- c(lines, paste0("dbinom_or_pbinom_value <- ", input$lucky_prob_value), paste0("dbinom(dbinom_or_pbinom_value, ", input$lucky_n_trials, ", ", input$lucky_p_success, ")"))
        }
        if (!is.null(input$lucky_poisson_rate) && !is.na(input$lucky_poisson_rate) && !is.null(input$lucky_poisson_periods) && !is.na(input$lucky_poisson_periods) && !is.null(input$lucky_prob_value) && !is.na(input$lucky_prob_value)) {
          lines <- c(lines, paste0("lambda <- ", input$lucky_poisson_rate, " * ", input$lucky_poisson_periods), paste0("dpois(", input$lucky_prob_value, ", lambda)"))
        }
        if (!is.null(input$lucky_normal_mean) && !is.na(input$lucky_normal_mean) && !is.null(input$lucky_normal_sd) && !is.na(input$lucky_normal_sd)) {
          lines <- c(lines, paste0("pnorm(", format_num_for_code(input$lucky_upper), ", ", input$lucky_normal_mean, ", ", input$lucky_normal_sd, ") - pnorm(", format_num_for_code(input$lucky_lower), ", ", input$lucky_normal_mean, ", ", input$lucky_normal_sd, ")"))
        }
        if (!length(lines)) {
          lines <- "Fill any matching boxes, then press Run. Blank/NA fields are ignored."
        }
        paste(unique(lines), collapse = "\n")
      },
      descStats = {
        x <- parse_data_numbers(input$data)
        paste(c(paste0("x <- ", code_vector(x)), "mean(x)", "median(x)", "var(x)", "sd(x)"), collapse = "\n")
      },
      pointEstimate = {
        if (input$point_type == "proportion") {
          paste0("x <- ", input$x_success, "\nn <- ", input$n, "\nphat <- x / n\nphat")
        } else if (input$point_type == "mean") {
          paste0("xbar <- ", input$xbar)
        } else {
          paste0("s <- ", input$s, "\ns^2")
        }
      },
      onePropCI = {
        alpha <- 1 - input$conf_level
        paste0("x <- ", input$x_success, "\nn <- ", input$n, "\nalpha <- ", format_num_for_code(alpha), "\nphat <- x / n\nphat + c(-1, 1) * qnorm(1 - alpha/2) * sqrt(phat * (1 - phat) / n)")
      },
      meanCIFromData = {
        x <- parse_data_numbers(input$ch10_raw_data)
        alpha <- 1 - input$conf_level
        if (input$ch10_sigma_mode == "known") {
          paste(c(paste0("x <- ", code_vector(x)), paste0("alpha <- ", format_num_for_code(alpha)), paste0("sigma <- ", format_num_for_code(input$sigma)), "mean(x) + c(-1, 1) * qnorm(1 - alpha/2) * sigma / sqrt(length(x))"), collapse = "\n")
        } else {
          paste(c(paste0("x <- ", code_vector(x)), paste0("alpha <- ", format_num_for_code(alpha)), "mean(x) + c(-1, 1) * qt(1 - alpha/2, length(x) - 1) * sd(x) / sqrt(length(x))"), collapse = "\n")
        }
      },
      meanKnownSigmaCI = {
        alpha <- 1 - input$conf_level
        paste0("n <- ", input$n, "\nxbar <- ", input$xbar, "\nsigma <- ", input$sigma, "\nalpha <- ", format_num_for_code(alpha), "\nxbar + c(-1, 1) * qnorm(1 - alpha/2) * sigma / sqrt(n)")
      },
      meanTCI = {
        alpha <- 1 - input$conf_level
        paste0("n <- ", input$n, "\nxbar <- ", input$xbar, "\ns <- ", input$s, "\nalpha <- ", format_num_for_code(alpha), "\nxbar + c(-1, 1) * qt(1 - alpha/2, n - 1) * s / sqrt(n)")
      },
      sampleSize = {
        alpha <- 1 - input$conf_level
        if (input$sample_size_type == "mean") {
          paste0("alpha <- ", format_num_for_code(alpha), "\nsigma <- ", input$sigma, "\nE <- ", input$margin_error, "\nceiling((qnorm(1 - alpha/2) * sigma / E)^2)")
        } else {
          paste0("alpha <- ", format_num_for_code(alpha), "\nphat <- ", input$p_hat, "\nE <- ", input$margin_error, "\nceiling(phat * (1 - phat) * (qnorm(1 - alpha/2) / E)^2)")
        }
      },
      countingRule = paste0("prod(c(", paste(parse_nums(input$slot_options), collapse = ", "), "))"),
      frequencyPercent = {
        f <- parse_nums(input$frequencies)
        paste(c(paste0("f <- ", code_vector(f)), "percent <- f / sum(f) * 100", "percent"), collapse = "\n")
      },
      selectedPercent = {
        f <- parse_nums(input$frequencies)
        selected <- as.integer(parse_nums(input$selected_positions))
        paste(c(paste0("f <- ", code_vector(f)), paste0("selected <- ", code_vector(selected)), "sum(f[selected]) / sum(f) * 100"), collapse = "\n")
      },
      "Press Run to show a simple copyable R calculation for this calculator."
    )
  }

  output$rcode_used <- renderText({
    input$run
    isolate(build_r_code())
  })

  output$result <- renderText({
    input$run
    isolate({
      fn <- input$fn
      format_calculator_result(capture_calculator_result({
          if (fn == "feelingLucky") feelingLuckyCalc(
            raw_data = parse_data_numbers(input$lucky_raw_data),
            x = input$lucky_x,
            n = input$lucky_n,
            xbar = input$lucky_xbar,
            s = input$lucky_s,
            sigma = input$lucky_sigma,
            conf_level = input$lucky_conf_level,
            margin_error = input$lucky_margin_error,
            p_hat = input$lucky_p_hat,
            analysis_type = input$lucky_analysis,
            sample_type = input$lucky_sample_type,
            sigma_mode = input$lucky_sigma_mode,
            favorable = input$lucky_favorable,
            total = input$lucky_total,
            slot_options = parse_data_numbers(input$lucky_slot_options),
            event_probs = parse_data_numbers(input$lucky_event_probs),
            dist_x = parse_data_numbers(input$lucky_dist_x),
            dist_prob = parse_data_numbers(input$lucky_dist_prob),
            prob_type = input$lucky_prob_type,
            prob_value = input$lucky_prob_value,
            upper_value = input$lucky_upper_value,
            n_trials = input$lucky_n_trials,
            p_success = input$lucky_p_success,
            poisson_rate = input$lucky_poisson_rate,
            poisson_periods = input$lucky_poisson_periods,
            total_items = input$lucky_total_items,
            success_items = input$lucky_success_items,
            sample_size = input$lucky_sample_size,
            normal_mean = input$lucky_normal_mean,
            normal_sd = input$lucky_normal_sd,
            lower = input$lucky_lower,
            upper = input$lucky_upper
          )
          else if (fn == "frequencyPercent") frequencyPercent(parse_nums(input$frequencies), parse_labels(input$categories))
          else if (fn == "selectedPercent") {
            pasted_freq <- tryCatch(parse_gof_table_text(input$frequency_pasted_table), error = function(e) NULL)
            if (!is.null(pasted_freq)) {
              selectedPercentCalc(pasted_freq$observed, pasted_freq$categories, as.integer(parse_nums(input$selected_positions)))
            } else {
              selectedPercentCalc(parse_nums(input$frequencies), parse_labels(input$categories), as.integer(parse_nums(input$selected_positions)))
            }
          }
          else if (fn == "percentToCount") percentAmountCalc(input$total, input$percent)
          else if (fn == "graphRatio") graphRatioCalc(input$numerator_value, input$denominator_value)
          else if (fn == "descStats") descStats(parse_data_numbers(input$data), input$sample)
          else if (fn == "modeClass") modeClassificationCalc(parse_data_numbers(input$data))
          else if (fn == "fiveNumberSummary") fiveNumberSummaryCalc(parse_data_numbers(input$data))
          else if (fn == "groupedStats") groupedStatsCalc(parse_nums(input$midpoints), parse_nums(input$frequencies), input$sample)
          else if (fn == "trimmedMean") trimmedMeanCalc(parse_nums(input$data), input$trim_percent)
          else if (fn == "weightedAverage") weightedAverageCalc(parse_nums(input$weighted_values), parse_nums(input$weights))
          else if (fn == "missingFromMean") missingFromMeanCalc(parse_nums(input$known_values), input$total_n, input$target_mean)
          else if (fn == "coeffVariation") coeffVariation(input$mean_val, input$sd_val)
          else if (fn == "chebyshev") chebyshevCalc(input$mean_val, input$sd_val, input$cheb_mode, input$k_value, input$percent, input$lower, input$upper)
          else if (fn == "empiricalRule") empiricalRuleCalc(input$mean_val, input$sd_val, input$lower, input$upper)
          else if (fn == "corrReg") {
            x <- parse_nums(input$x)
            y <- parse_nums(input$y)
            if (length(x) != length(y)) stop("x values and y values must have the same number of entries.")
            corrReg(x, y)
          }
          else if (fn == "predictY") {
            x <- parse_nums(input$x)
            y <- parse_nums(input$y)
            if (length(x) != length(y)) stop("x values and y values must have the same number of entries.")
            predictY(x, y, input$new_x)
          }
          else if (fn == "simpleProb") {
            pasted_counts <- parse_single_row_count_table(input$simple_pasted_table)
            if (!is.null(pasted_counts)) {
              simpleCountTableProbabilityCalc(pasted_counts, input$simple_target)
            } else {
              simpleProb(input$favorable, input$total)
            }
          }
          else if (fn == "diceSum") diceSumCalc(input$dice_type, input$dice_value)
          else if (fn == "complementProb") complementProb(prob_event = input$prob_event)
          else if (fn == "additionRule") additionRuleCalc(input$a_count, input$b_count, input$overlap_count, input$total_count)
          else if (fn == "conditionalProb") conditionalProb(input$joint_count, input$given_count)
          else if (fn == "countingRule") countingRuleCalc(parse_nums(input$slot_options))
          else if (fn == "sequenceProbability") {
            favorable <- parse_nums(input$seq_favorable)
            if (input$seq_replacement == "manual") {
              sequenceProbabilityCalc(favorable, parse_nums(input$seq_total))
            } else {
              cardSequenceCalc(favorable, replacement = input$seq_replacement)
            }
          }
          else if (fn == "independentProbability") independentProbabilityCalc(parse_nums(input$event_probs))
          else if (fn == "tableProbability") {
            pasted_table <- parse_contingency_table_text(input$ch6_table)
            if (is.null(pasted_table)) stop("Paste a table first.")
            tableProbabilityCalc(pasted_table, input$table_row, input$table_col, input$table_prob_mode)
          }
          else if (fn == "factorialArrangements") factorialRatioCalc(input$factorial_n, parse_nums(input$factorial_parts), input$factorial_mode, input$factorial_r)
          else if (fn == "bayesProbability") bayesProbabilityCalc(parse_nums(input$bayes_priors), parse_nums(input$bayes_event_probs), input$bayes_target)
          else if (fn == "discreteDist") {
            pasted_dist <- parse_probability_distribution_table(input$dist_pasted_table)
            if (!is.null(pasted_dist)) {
              cat("Parsed pasted table:\n")
              print(data.frame(x = pasted_dist$x, Probability = pasted_dist$prob))
              cat("--------------------------------------------------\n")
              print_distribution_summary(pasted_dist$x, pasted_dist$prob)
            } else {
              prob <- parse_nums(input$prob)
              print_distribution_summary(parse_nums(input$x), prob)
            }
          }
          else if (fn == "probFromDist") {
            pasted_dist <- parse_probability_distribution_table(input$dist_pasted_table)
            if (!is.null(pasted_dist)) {
              cat("Parsed pasted table:\n")
              print(data.frame(x = pasted_dist$x, Probability = pasted_dist$prob))
              cat("--------------------------------------------------\n")
              probDistQueryCalc(pasted_dist$x, pasted_dist$prob, input$type, input$value, input$upper_value)
            } else {
              prob <- parse_nums(input$prob)
              probDistQueryCalc(parse_nums(input$x), prob, input$type, input$value, input$upper_value)
            }
          }
          else if (fn == "expectedMonetaryValue") {
            expectedMonetaryValueCalc(parse_nums(input$emv_outcomes), parse_nums(input$emv_prob))
          }
          else if (fn == "discreteUniform") {
            discreteUniformCalc(input$min_value, input$max_value, input$type, input$value, input$upper_value)
          }
          else if (fn == "binomialDistribution") {
            binomialCalc(input$n, input$p, input$type, input$value, input$upper_value)
          }
          else if (fn == "poissonDistribution") {
            poissonCalc(input$rate, input$periods, input$type, input$value, input$upper_value)
          }
          else if (fn == "hypergeometricDistribution") {
            hypergeometricCalc(input$total_items, input$success_items, input$sample_size, input$type, input$value, input$upper_value)
          }
          else if (fn == "uniformDist") {
            bounds <- probability_bounds(input$prob_type, input$lower, input$upper, input$exact_value, input$a, input$b)
            uniformDist(input$a, input$b, bounds$lower, bounds$upper)
          }
          else if (fn == "zScoreCalc") zScore(input$x_value, input$mean_val, input$sd_val)
          else if (fn == "normalProb") {
            bounds <- probability_bounds(input$prob_type, input$lower, input$upper, input$exact_value)
            normalProb(input$mean_val, input$sd_val, bounds$lower, bounds$upper)
          }
          else if (fn == "invNormal") invNormal(input$prob_left, input$mean_val, input$sd_val)
          else if (fn == "normalApproxBinomial") normalApproxBinomialCalc(input$n, input$p, input$type, input$value, input$upper_value)
          else if (fn == "sampleMeanProb") {
            bounds <- probability_bounds(input$prob_type, input$lower, input$upper, input$exact_value)
            sampleMeanProb(input$mu, input$sigma, input$n, bounds$lower, bounds$upper)
          }
          else if (fn == "samplePropProb") {
            bounds <- probability_bounds(input$prob_type, input$lower, input$upper, input$exact_value)
            samplePropProb(input$p, input$n, bounds$lower, bounds$upper)
          }
          else if (fn == "pointEstimate") pointEstimateCalc(input$point_type, input$x_success, input$n, input$xbar, input$s)
          else if (fn == "meanCIFromData") meanCIFromDataCalc(parse_data_numbers(input$ch10_raw_data), input$conf_level, input$sigma, input$ch10_sigma_mode)
          else if (fn == "onePropCI") onePropCI(input$x_success, input$n, input$conf_level)
          else if (fn == "meanKnownSigmaCI") meanKnownSigmaCI(input$n, input$xbar, input$sigma, input$conf_level)
          else if (fn == "meanTCI") meanTCI(input$n, input$xbar, input$s, input$conf_level)
          else if (fn == "sampleSize") sampleSizeCalc(input$sample_size_type, input$conf_level, input$margin_error, input$sigma, input$p_hat)
          else if (fn == "varianceCI") varianceCI(input$n, input$s, input$conf_level)
          else if (fn == "meanKnownSigmaTest") meanKnownSigmaTest(input$n, input$xbar, input$mu0, input$sigma, input$alpha, input$alternative)
          else if (fn == "meanTTest") meanTTest(input$n, input$xbar, input$mu0, input$s, input$alpha, input$alternative)
          else if (fn == "onePropTest") onePropTest(input$x_success, input$n, input$p0, input$alpha, input$alternative)
          else if (fn == "varianceTest") varianceTestCalc(input$n, input$s, input$sigma0, input$alpha, input$alternative)
          else if (fn == "twoIndSampleProp") {
            levels <- conf_alpha_values(input$conf_level, input$alpha)
            twoIndSampleProp(x1 = input$x1, n1 = input$n1, x2 = input$x2, n2 = input$n2, conf_level = levels$conf_level, alpha = levels$alpha, alternative = input$alternative)
          }
          else if (fn == "twoIndMeansKnownSigma") {
            levels <- conf_alpha_values(input$conf_level, input$alpha)
            twoIndMeansKnownSigma(input$n1, input$x1, input$sigma1, input$n2, input$x2, input$sigma2, conf_level = levels$conf_level, alpha = levels$alpha, alternative = input$alternative)
          }
          else if (fn == "twoIndMeanUneqVar") {
            levels <- conf_alpha_values(input$conf_level, input$alpha)
            twoIndMeanUneqVar(input$n1, input$x1, input$s1, input$n2, input$x2, input$s2, conf_level = levels$conf_level, alpha = levels$alpha, alternative = input$alternative)
          } else if (fn == "twoIndMeansEqVar") {
            levels <- conf_alpha_values(input$conf_level, input$alpha)
            twoIndMeansEqVar(input$n1, input$x1, input$s1, input$n2, input$x2, input$s2, conf_level = levels$conf_level, alpha = levels$alpha, alternative = input$alternative)
          } else if (fn == "pairedDiffCI") {
            pairedDiffCI(parse_data_numbers(input$before_values), parse_data_numbers(input$after_values), input$conf_level)
          } else if (fn == "twoVarianceRatio") {
            levels <- conf_alpha_values(input$conf_level, input$alpha)
            twoVarianceRatioCalc(input$n1, input$var1, input$n2, input$var2, conf_level = levels$conf_level, alpha = levels$alpha, alternative = input$alternative)
          }
          else if (fn == "chiSqGoodnessFit") {
            pasted_gof <- parse_gof_table_text(input$gof_pasted_table)
            if (!is.null(pasted_gof)) {
              cat("Parsed pasted table:\n")
              print(data.frame(Category = pasted_gof$categories, Observed = pasted_gof$observed))
              cat("--------------------------------------------------\n")
              chiSqGoodnessFit(
                pasted_gof$observed,
                expected_probs = pasted_gof$expected_probs,
                alpha = alpha_value(input$alpha),
                categories = pasted_gof$categories,
                claim = input$claim
              )
            } else {
              expected <- if (nzchar(trimws(input$expected_probs))) parse_nums(input$expected_probs) else NULL
              chiSqGoodnessFit(parse_nums(input$observed), expected_probs = expected, alpha = alpha_value(input$alpha), categories = parse_labels(input$categories), claim = input$claim)
            }
          } else if (fn == "chiSqIndependence") {
            pasted_table <- parse_contingency_table_text(input$pasted_table)
            observed_table <- if (!is.null(pasted_table)) {
              cat("Parsed pasted table:\n")
              print(pasted_table)
              cat("--------------------------------------------------\n")
              pasted_table
            } else {
              matrix_from_text(input$matrix_vals, input$nrow, input$row_names, input$col_names)
            }
            chiSqIndependence(observed_table, alpha = alpha_value(input$alpha))
          } else if (fn == "chiSqHomogeneity") {
            chiSqHomogeneity(matrix_from_text(input$matrix_vals, input$nrow, input$row_names, input$col_names), alpha = alpha_value(input$alpha))
          }
      }))
    })
  })

}
