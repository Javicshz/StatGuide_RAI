# Server wiring for Concepts, Formulas, Search, and Examples tabs.

register_reference_server <- function(input, output, session) {
  output$concept_title <- renderText(names(concept_topic_choices)[concept_topic_choices == input$concept_topic])
  output$concept_list <- renderUI({
    items <- concepts[[input$concept_topic]]
    tags$div(
      class = "concept-card",
      render_list(items)
    )
  })

  output$formula_title <- renderText(names(formula_topic_choices)[formula_topic_choices == input$formula_topic])
  output$formula_list <- renderUI({
    r_example <- topic_r_examples[[input$formula_topic]]
    var_notes <- topic_variable_notes[[input$formula_topic]]
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
      tags$h5("Formulas"),
      render_list(topic_formulas[[input$formula_topic]]),
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
      tags$div(
        class = "match-box",
        tags$div(class = "match-title", search_guides$label[i]),
        tags$div(class = "match-meta", paste("Function:", search_guides$fn[i])),
        tags$p(search_guides$use_when[i])
      )
    }))
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
        tags$pre(row$code)
      ),
      tags$div(
        class = "formula-card",
        tags$h5("What To Enter / How To Identify It"),
        tags$p(row$tips)
      )
    )
  })
}
