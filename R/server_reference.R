# Server wiring for Concepts, Formulas, Search, and Examples tabs.

register_reference_server <- function(input, output, session) {
  selected_search_detail <- reactiveVal(NULL)

  formula_topic_for_fn <- function(fn) {
    map <- c(
      frequencyPercent = "s3_1",
      selectedPercent = "s3_1",
      percentToCount = "s4_6",
      graphRatio = "s3_4",
      descStats = "s4_1",
      modeClass = "s4_1",
      fiveNumberSummary = "s4_3",
      groupedStats = "s4_5",
      trimmedMean = "s4_1",
      weightedAverage = "s4_1",
      missingFromMean = "s4_1",
      coeffVariation = "s4_2",
      chebyshev = "s4_2",
      empiricalRule = "s8_2",
      corrReg = "s5_1",
      predictY = "s5_2",
      simpleProb = "s6_1",
      diceSum = "s6_1",
      complementProb = "s6_1",
      additionRule = "s6_2",
      conditionalProb = "s6_3",
      countingRule = "s6_4",
      sequenceProbability = "s6_3",
      independentProbability = "s6_3",
      tableProbability = "s6_3",
      factorialArrangements = "s6_4",
      bayesProbability = "s6_5",
      discreteDist = "s7_1",
      probFromDist = "s7_1",
      expectedMonetaryValue = "s7_2",
      discreteUniform = "s7_3",
      binomialDistribution = "s7_4",
      poissonDistribution = "s7_5",
      hypergeometricDistribution = "s7_6",
      uniformDist = "s8_1",
      zScoreCalc = "s8_3",
      normalProb = "s8_4",
      invNormal = "s8_4",
      normalApproxBinomial = "s8_6",
      sampleMeanProb = "s9_2",
      samplePropProb = "s9_3",
      pointEstimate = "s10_1",
      meanCIFromData = "s10_3",
      onePropCI = "s10_4",
      meanKnownSigmaCI = "s10_2",
      meanTCI = "s10_3",
      sampleSize = "s10_1",
      varianceCI = "s10_5",
      meanKnownSigmaTest = "s11_2",
      meanTTest = "s11_3",
      onePropTest = "s11_5",
      varianceTest = "s11_6",
      twoIndSampleProp = "s12_4",
      twoIndMeansKnownSigma = "s12_1",
      twoIndMeanUneqVar = "s12_2",
      twoIndMeansEqVar = "s12_2",
      pairedDiffCI = "s12_3",
      twoVarianceRatio = "s12_5",
      chiSqGoodnessFit = "s16_2",
      chiSqIndependence = "s16_3",
      chiSqHomogeneity = "s16_3"
    )
    if (!fn %in% names(map)) {
      return(NULL)
    }
    unname(map[[fn]])
  }

  concept_topic_for_formula <- function(topic) {
    map <- c(
      s3_1 = "ch3", s3_2 = "ch3", s3_3 = "ch3", s3_4 = "ch3",
      s4_1 = "ch4", s4_2 = "ch4", s4_3 = "ch4", s4_5 = "ch4", s4_6 = "ch4",
      s5_1 = "ch5", s5_2 = "ch5", s5_3 = "ch5", s5_4 = "ch5",
      s6_1 = "ch6", s6_2 = "ch6", s6_3 = "ch6", s6_4 = "ch6", s6_5 = "ch6",
      s7_1 = "ch7", s7_2 = "ch7", s7_3 = "ch7", s7_4 = "ch7", s7_5 = "ch7", s7_6 = "ch7",
      s8_1 = "ch8", s8_2 = "ch8", s8_3 = "ch8", s8_4 = "ch8", s8_5 = "ch8", s8_6 = "ch8",
      s9_1 = "ch9", s9_2 = "ch9", s9_3 = "ch9",
      s10_1 = "ch10", s10_2 = "ch10", s10_3 = "ch10", s10_4 = "ch10", s10_5 = "ch10",
      s11_1 = "ch11", s11_2 = "ch11", s11_3 = "ch11", s11_4 = "ch11", s11_5 = "ch11", s11_6 = "ch11",
      s12_1 = "ch12", s12_2 = "ch12", s12_3 = "ch12", s12_4 = "ch12", s12_5 = "ch12",
      s16_1 = "ch16", s16_2 = "ch16", s16_3 = "ch16"
    )
    if (!topic %in% names(map)) {
      return(NULL)
    }
    unname(map[[topic]])
  }

  observeEvent(input$search_open_formula, {
    topic <- input$search_open_formula %||% ""
    if (nzchar(topic) && topic %in% unname(formula_topic_choices)) {
      selected_search_detail(NULL)
      updateSelectInput(session, "formula_topic", selected = topic)
      updateTabsetPanel(session, "main_tabs", selected = "Formulas")
    }
  })

  observeEvent(input$search_show_detail, {
    index <- suppressWarnings(as.integer(input$search_show_detail))
    if (!is.na(index) && index >= 1 && index <= nrow(search_guides)) {
      selected_search_detail(index)
    }
  })

  observeEvent(input$formula_open_concept, {
    topic <- input$formula_open_concept %||% ""
    if (nzchar(topic) && topic %in% unname(concept_topic_choices)) {
      updateSelectInput(session, "concept_topic", selected = topic)
      updateTabsetPanel(session, "main_tabs", selected = "Concepts & Definitions")
    }
  })

  output$concept_title <- renderText(names(concept_topic_choices)[concept_topic_choices == input$concept_topic])
  output$concept_list <- renderUI({
    items <- concepts[[input$concept_topic]]
    tags$div(
      class = "concept-card",
      render_concept_items(items)
    )
  })

  output$formula_title <- renderText(names(formula_topic_choices)[formula_topic_choices == input$formula_topic])
  output$formula_list <- renderUI({
    r_example <- topic_r_examples[[input$formula_topic]]
    var_notes <- topic_variable_notes[[input$formula_topic]]
    concept_topic <- concept_topic_for_formula(input$formula_topic)
    concept_link <- NULL
    if (!is.null(concept_topic) && nzchar(concept_topic)) {
      concept_label <- names(concept_topic_choices)[concept_topic_choices == concept_topic]
      onclick <- sprintf(
        "Shiny.setInputValue('formula_open_concept', '%s', {priority: 'event'}); return false;",
        concept_topic
      )
      concept_link <- tags$p(
        class = "formula-note",
        tags$a(class = "match-link", href = "#", onclick = onclick, paste("Read related concept:", concept_label))
      )
    }
    var_block <- if (!is.null(var_notes)) {
      tags$div(
        class = "formula-card",
        tags$h5("Variables To Set First"),
        tags$pre(paste(var_notes, collapse = "\n"))
      )
    } else NULL
    r_block <- if (!is.null(r_example)) {
      tags$div(
        class = "formula-card",
        tags$h5("R Code Quick Reference"),
        tags$pre(r_example$code),
        tags$p(tags$strong("Most helpful for: "), r_example$use),
        tags$p(tags$strong("Problem wording example: "), r_example$example)
      )
    } else {
      tags$div(
        class = "formula-card",
        tags$h5("R Code Quick Reference"),
        tags$pre("Use the matching calculator. After you press Run, the Calculators tab shows copyable R code with your values filled in."),
        tags$p(tags$strong("Most helpful for: "), "Conceptual or low-calculation topics.")
      )
    }
    tagList(
      tags$p(class = "formula-note", "These formulas are organized by general statistics topic."),
      concept_link,
      tags$h5("Formulas"),
      render_formula_items(topic_formulas[[input$formula_topic]]),
      var_block,
      r_block
    )
  })

  output$search_results <- renderUI({
    query <- tolower(trimws(input$search_text))
    if (!nzchar(query)) {
      return(tags$p("Type problem wording to see suggestions."))
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

    top <- order(scores, decreasing = TRUE)[seq_len(min(5, length(scores)))]
    top <- top[scores[top] > 0]

    if (!length(top)) {
      return(tags$p("No strong match found. Try adding clue words like probability, confidence interval, normal, given, two groups, or chi-square."))
    }

    tagList(lapply(top, function(i) {
      fn <- search_guides$fn[i]
      formula_topic <- formula_topic_for_fn(fn)
      if (!is.null(formula_topic) && nzchar(formula_topic)) {
        onclick <- sprintf(
          "Shiny.setInputValue('search_open_formula', '%s', {priority: 'event'}); return false;",
          formula_topic
        )
      } else {
        onclick <- sprintf(
          "Shiny.setInputValue('search_show_detail', %d, {priority: 'event'}); return false;",
          i
        )
      }
      tags$div(
        class = "match-box",
        tags$a(class = "match-title match-link", href = "#", onclick = onclick, search_guides$label[i]),
        tags$div(class = "match-meta", paste("Function:", search_guides$fn[i])),
        tags$p(search_guides$use_when[i]),
        if (!is.null(formula_topic) && nzchar(formula_topic)) {
          tags$div(class = "match-meta", "Click title to open the related formula/reference page.")
        } else {
          tags$div(class = "match-meta", "Click title to show more detail below.")
        }
      )
    }))
  })

  output$search_selected_details <- renderUI({
    i <- selected_search_detail()
    if (is.null(i)) {
      return(NULL)
    }
    tags$div(
      class = "reference-box",
      tags$h4(search_guides$label[i]),
      tags$p(tags$strong("Suggested destination: "), search_guides$fn[i]),
      tags$p(search_guides$use_when[i]),
      tags$p(tags$strong("Helpful wording clues: "), search_guides$keywords[i])
    )
  })

  output$example_title <- renderText({
    row <- example_guides[as.integer(input$example_choice), ]
    paste(row$topic, "-", row$title)
  })

  output$example_details <- renderUI({
    row <- example_guides[as.integer(input$example_choice), ]
    tagList(
      tags$div(
        class = "formula-card",
        tags$h5("Problem Style"),
        tags$p(row$problem),
        tags$p(tags$strong("Use calculator: "), row$calculator)
      ),
      tags$div(
        class = "formula-card",
        tags$h5("R Code Example"),
        tags$pre(format_example_code(row$code))
      ),
      tags$div(
        class = "formula-card",
        tags$h5("What To Enter / How To Identify It"),
        tags$p(row$tips)
      )
    )
  })
}
