# Numbered RStudio Console menu for StatGuide R AI.

statguide_console_line <- function(char = "-", width = 72) {
  cat(paste(rep(char, width), collapse = ""), "\n", sep = "")
}

statguide_console_header <- function(title) {
  cat("\n")
  statguide_console_line("=")
  cat(title, "\n", sep = "")
  statguide_console_line("=")
}

statguide_console_pause <- function(interactive = base::interactive()) {
  if (isTRUE(interactive)) {
    invisible(readline("Press Enter to continue..."))
  }
  invisible(TRUE)
}

statguide_console_choice <- function(prompt, max_choice, allow_back = TRUE) {
  repeat {
    raw <- trimws(readline(prompt))
    if (allow_back && raw %in% c("", "0", "b", "B", "back", "Back")) {
      return(0L)
    }
    choice <- suppressWarnings(as.integer(raw))
    if (!is.na(choice) && choice >= 1L && choice <= max_choice) {
      return(choice)
    }
    cat("Enter a number between 1 and ", max_choice, if (allow_back) ", or 0 to go back." else ".", "\n", sep = "")
  }
}

statguide_console_print_menu <- function(title, labels, back_label = "Back") {
  statguide_console_header(title)
  for (i in seq_along(labels)) {
    cat(sprintf("%2d. %s\n", i, labels[i]))
  }
  if (!is.null(back_label)) {
    cat(sprintf("%2d. %s\n", 0L, back_label))
  }
}

statguide_console_wrap <- function(text, indent = 0, width = getOption("width", 80)) {
  text <- trimws(text %||% "")
  if (!nzchar(text)) {
    return(invisible(NULL))
  }
  prefix <- paste(rep(" ", indent), collapse = "")
  lines <- strwrap(text, width = max(20, width - indent), simplify = FALSE)[[1]]
  cat(paste0(prefix, lines), sep = "\n")
  cat("\n")
}

statguide_formula_topic_for_fn <- function(fn) {
  map <- c(
    frequencyPercent = "s3_1", selectedPercent = "s3_1", percentToCount = "s4_6", graphRatio = "s3_4",
    descStats = "s4_1", modeClass = "s4_1", fiveNumberSummary = "s4_3", groupedStats = "s4_5",
    trimmedMean = "s4_1", weightedAverage = "s4_1", missingFromMean = "s4_1", coeffVariation = "s4_2",
    chebyshev = "s4_2", empiricalRule = "s8_2", corrReg = "s5_1", predictY = "s5_2",
    simpleProb = "s6_1", diceSum = "s6_1", complementProb = "s6_1", additionRule = "s6_2",
    conditionalProb = "s6_3", countingRule = "s6_4", sequenceProbability = "s6_3",
    independentProbability = "s6_3", tableProbability = "s6_3", factorialArrangements = "s6_4",
    bayesProbability = "s6_5", discreteDist = "s7_1", probFromDist = "s7_1", expectedMonetaryValue = "s7_2",
    discreteUniform = "s7_3", binomialDistribution = "s7_4", poissonDistribution = "s7_5",
    hypergeometricDistribution = "s7_6", uniformDist = "s8_1", zScoreCalc = "s8_3",
    normalProb = "s8_4", invNormal = "s8_4", normalApproxBinomial = "s8_6",
    sampleMeanProb = "s9_2", samplePropProb = "s9_3", pointEstimate = "s10_1",
    meanCIFromData = "s10_3", onePropCI = "s10_4", meanKnownSigmaCI = "s10_2",
    meanTCI = "s10_3", sampleSize = "s10_1", varianceCI = "s10_5",
    meanKnownSigmaTest = "s11_2", meanTTest = "s11_3", onePropTest = "s11_5",
    varianceTest = "s11_6", twoIndSampleProp = "s12_4", twoIndMeansKnownSigma = "s12_1",
    twoIndMeanUneqVar = "s12_2", twoIndMeansEqVar = "s12_2", pairedDiffCI = "s12_3",
    twoVarianceRatio = "s12_5", chiSqGoodnessFit = "s16_2", chiSqIndependence = "s16_3",
    chiSqHomogeneity = "s16_3"
  )
  if (!fn %in% names(map)) {
    return(NULL)
  }
  unname(map[[fn]])
}

statguide_console_calculator_groups <- function() {
  labels <- names(function_choices)
  groups <- sub(" - .*", "", labels)
  split(seq_along(labels), groups)
}

statguide_console_show_calculator <- function(fn, label = NULL) {
  ref <- calculator_reference(fn)
  details <- calculator_reference_details(fn)
  statguide_console_header(label %||% fn)
  cat("Function ID: ", fn, "\n\n", sep = "")
  cat("Use when:\n")
  statguide_console_wrap(ref$use, indent = 2)
  cat("\nExample wording:\n")
  statguide_console_wrap(ref$example, indent = 2)
  cat("\nSimple R code:\n")
  cat(ref$code, "\n", sep = "")
  if (!is.null(details)) {
    cat("\nQuick reference:\n")
    for (name in names(details)) {
      cat("- ", name, ": ", details[[name]], "\n", sep = "")
    }
  }
  formula_topic <- statguide_formula_topic_for_fn(fn)
  if (!is.null(formula_topic)) {
    formula_label <- names(formula_topic_choices)[formula_topic_choices == formula_topic]
    cat("\nRelated formula topic: ", formula_label, "\n", sep = "")
  }
}

statguide_console_calculators <- function() {
  groups <- statguide_console_calculator_groups()
  repeat {
    statguide_console_print_menu("Calculators", names(groups))
    group_choice <- statguide_console_choice("Choose a calculator group: ", length(groups))
    if (group_choice == 0L) {
      return(invisible(TRUE))
    }
    indexes <- groups[[group_choice]]
    labels <- names(function_choices)[indexes]
    repeat {
      statguide_console_print_menu(names(groups)[group_choice], labels)
      calc_choice <- statguide_console_choice("Choose a calculator reference: ", length(labels))
      if (calc_choice == 0L) {
        break
      }
      idx <- indexes[calc_choice]
      statguide_console_show_calculator(unname(function_choices[idx]), names(function_choices)[idx])
      statguide_console_pause()
    }
  }
}

statguide_console_concepts <- function() {
  labels <- names(concept_topic_choices)
  repeat {
    statguide_console_print_menu("Concepts & Definitions", labels)
    choice <- statguide_console_choice("Choose a topic: ", length(labels))
    if (choice == 0L) {
      return(invisible(TRUE))
    }
    topic <- unname(concept_topic_choices[choice])
    statguide_console_header(labels[choice])
    cat(paste(concepts[[topic]], collapse = "\n\n"), "\n", sep = "")
    statguide_console_pause()
  }
}

statguide_console_formulas <- function() {
  labels <- names(formula_topic_choices)
  repeat {
    statguide_console_print_menu("Formulas", labels)
    choice <- statguide_console_choice("Choose a formula topic: ", length(labels))
    if (choice == 0L) {
      return(invisible(TRUE))
    }
    topic <- unname(formula_topic_choices[choice])
    statguide_console_header(labels[choice])
    cat("Formulas:\n")
    cat(paste0("- ", topic_formulas[[topic]]), sep = "\n")
    cat("\n")
    var_notes <- topic_variable_notes[[topic]]
    if (!is.null(var_notes)) {
      cat("\nVariables to set first:\n")
      cat(paste(var_notes, collapse = "\n"), "\n", sep = "")
    }
    r_example <- topic_r_examples[[topic]]
    if (!is.null(r_example)) {
      cat("\nR code quick reference:\n")
      cat(r_example$code, "\n", sep = "")
      cat("\nMost helpful for: ", r_example$use, "\n", sep = "")
      cat("Problem wording example: ", r_example$example, "\n", sep = "")
    }
    statguide_console_pause()
  }
}

statguide_console_search_matches <- function(query, max_results = 5) {
  query <- tolower(trimws(query %||% ""))
  if (!nzchar(query)) {
    return(search_guides[0, ])
  }
  query_words <- unique(unlist(strsplit(gsub("[^a-z0-9.]+", " ", query), "\\s+")))
  query_words <- query_words[nzchar(query_words)]
  scores <- vapply(seq_len(nrow(search_guides)), function(i) {
    guide_text <- tolower(paste(search_guides$label[i], search_guides$use_when[i], search_guides$keywords[i]))
    word_hits <- sum(query_words %in% unlist(strsplit(guide_text, "\\s+")))
    phrase_hits <- sum(vapply(unlist(strsplit(search_guides$keywords[i], " ")), function(k) {
      nzchar(k) && grepl(k, query, fixed = TRUE)
    }, logical(1)))
    word_hits + phrase_hits
  }, numeric(1))
  top <- order(scores, decreasing = TRUE)[seq_len(min(max_results, length(scores)))]
  top <- top[scores[top] > 0]
  search_guides[top, , drop = FALSE]
}

statguide_console_search <- function() {
  repeat {
    statguide_console_header("Search Helper")
    query <- readline("Paste problem wording, or press Enter to go back: ")
    if (!nzchar(trimws(query))) {
      return(invisible(TRUE))
    }
    matches <- statguide_console_search_matches(query)
    if (!nrow(matches)) {
      cat("No strong match found. Try words like probability, confidence interval, normal, given, two groups, or chi-square.\n")
      statguide_console_pause()
      next
    }
    for (i in seq_len(nrow(matches))) {
      cat(sprintf("\n%d. %s\n", i, matches$label[i]))
      cat("   Function: ", matches$fn[i], "\n", sep = "")
      statguide_console_wrap(matches$use_when[i], indent = 3)
    }
    choice <- statguide_console_choice("Choose a match to view details, or 0 to search again: ", nrow(matches))
    if (choice > 0L) {
      statguide_console_show_calculator(matches$fn[choice], matches$label[choice])
      statguide_console_pause()
    }
  }
}

statguide_console_ask_ai <- function() {
  statguide_console_header("Ask AI")
  question <- readline("Question/request, or Enter to go back: ")
  if (!nzchar(trimws(question))) {
    return(invisible(TRUE))
  }

  provider_labels <- names(ai_provider_choices)
  statguide_console_print_menu("AI Provider", provider_labels)
  provider_choice <- statguide_console_choice("Choose provider: ", length(provider_labels))
  if (provider_choice == 0L) {
    return(invisible(TRUE))
  }
  provider <- unname(ai_provider_choices[provider_choice])

  task_labels <- names(ai_task_choices)
  statguide_console_print_menu("AI Task", task_labels)
  task_choice <- statguide_console_choice("Choose task type: ", length(task_labels))
  if (task_choice == 0L) {
    return(invisible(TRUE))
  }
  context <- readline("Optional context, or Enter to skip: ")
  askai(
    question,
    provider = provider,
    task = unname(ai_task_choices[task_choice]),
    context = context
  )
  statguide_console_pause()
}

statguide_console_codex <- function() {
  statguide_console_ask_ai()
}

statguide_console <- function() {
  repeat {
    statguide_console_print_menu(
      "StatGuide R AI Console",
      c("Calculators", "Concepts & Definitions", "Formulas", "Search Helper", "Ask AI"),
      back_label = "Exit"
    )
    choice <- statguide_console_choice("Choose an option: ", 5)
    if (choice == 0L) {
      cat("Goodbye.\n")
      return(invisible(TRUE))
    }
    switch(
      choice,
      statguide_console_calculators(),
      statguide_console_concepts(),
      statguide_console_formulas(),
      statguide_console_search(),
      statguide_console_ask_ai()
    )
  }
}
