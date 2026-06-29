# Shiny tab and top-level UI definitions.

ask_ai_tab <- tabPanel(
  "Ask AI",
  sidebarLayout(
    sidebarPanel(
      selectInput("ai_task", "What do you want AI to help with?", choices = ai_task_choices),
      textAreaInput(
        "ai_question",
        "Question or request",
        "",
        rows = 6,
        placeholder = "Example: Which test should I use for two independent groups with a numeric outcome?"
      ),
      textAreaInput(
        "ai_context",
        "Optional data summary, R code, output, or error message",
        "",
        rows = 8,
        placeholder = "Paste a small summary, variable descriptions, current R code, or an error message. Avoid private data."
      ),
      checkboxInput("ai_allow_external", "I have reviewed this prompt and it is okay to send to the selected provider later", FALSE),
      actionButton("ai_prepare", "Prepare AI Prompt"),
      actionButton("ai_send", "Ask AI")
    ),
    mainPanel(
      tags$h4("AI Workflow"),
      tags$div(
        class = "reference-box",
        tags$p(tags$strong("Current mode: "), "provider-enabled assistant. Local CLI providers, Ollama, and configured online API providers can return direct answers when their settings are valid."),
        tags$p("Use this tab to shape a high-quality AI request for statistics, R code, debugging, interpretation, or workflow planning."),
        tags$p("Provider selection and connection planning live in the AI Settings tab.")
      ),
      tags$h4("Prepared Prompt"),
      verbatimTextOutput("ai_prompt_preview"),
      tags$h4("AI Response"),
      verbatimTextOutput("ai_response"),
      tags$h4("Local Guidance"),
      uiOutput("ai_local_guidance")
    )
  )
)

calculator_tab <- tabPanel(
  "Calculators",
  sidebarLayout(
    sidebarPanel(
      selectInput("fn", "Choose a problem type", choices = function_choices),
      uiOutput("inputs"),
      actionButton("run", "Run")
    ),
    mainPanel(
      tags$h4("Output"),
      verbatimTextOutput("result"),
      tags$h4("Quick Reference"),
      uiOutput("quick_reference"),
      tags$h4("Simple R Code"),
      verbatimTextOutput("rcode_used")
    )
  )
)

ai_settings_tab <- tabPanel(
  "AI Settings",
  sidebarLayout(
    sidebarPanel(
      selectInput("ai_provider", "AI provider", choices = ai_provider_choices),
      uiOutput("ai_provider_settings_ui"),
      checkboxInput("ai_store_no_secrets", "Do not store raw API keys in this app", TRUE),
      actionButton("ai_test_settings", "Review Settings")
    ),
    mainPanel(
      tags$h4("Provider Plan"),
      tags$div(
        class = "reference-box",
        tags$p("This settings page configures AI providers without saving raw API keys in project files."),
        tags$p("Use environment variables for online credentials, local CLI sessions for Codex or Claude Code, and the Review Settings button to check provider availability.")
      ),
      tags$h4("Selected Provider Status"),
      verbatimTextOutput("ai_provider_status"),
      tags$h4("Connection Test"),
      verbatimTextOutput("ai_connection_test"),
      tags$h4("Provider Setup Help"),
      uiOutput("ai_provider_help"),
      tags$h4("Implementation Notes"),
      tags$div(
        class = "concept-card",
        tags$ul(
          tags$li("Codex/OpenAI and Claude should use environment variables or a system keyring, not plain text files."),
          tags$li("Ollama should default to a local endpoint such as http://localhost:11434 and clearly show when the model is offline."),
          tags$li("Generic OpenAI-compatible APIs should require an explicit endpoint and model name."),
          tags$li("Online providers should require clear user approval before sending data, code, or output."),
          tags$li("AI-generated R code should be reviewed before running, especially if it reads/writes files or installs packages.")
        )
      )
    )
  )
)

concepts_tab <- tabPanel(
  "Concepts & Definitions",
  sidebarLayout(
    sidebarPanel(
      selectInput("concept_topic", "Choose a topic", choices = concept_topic_choices)
    ),
    mainPanel(
      tags$h4(textOutput("concept_title")),
      uiOutput("concept_list")
    )
  )
)

formulas_tab <- tabPanel(
  "Formulas",
  sidebarLayout(
    sidebarPanel(
      selectInput("formula_topic", "Choose a formula topic", choices = formula_topic_choices)
    ),
    mainPanel(
      tags$h4(textOutput("formula_title")),
      uiOutput("formula_list")
    )
  )
)

search_tab <- tabPanel(
  "Search Helper",
  sidebarLayout(
    sidebarPanel(
      textAreaInput(
        "search_text",
        "Paste or type problem wording",
        "Find the probability that a randomly selected person..."
      ),
      helpText("Search looks for wording clues. Try phrases like given, normally distributed, confidence interval, two groups, equally distributed, or expected count."),
      helpText("Good search phrases: less than 4 stars, Big 12 percent, z-score, nCr, drawing cards without replacement, sample mean, at least one defective.")
    ),
    mainPanel(
      tags$h4("Suggested Problem Types"),
      uiOutput("search_results")
    )
  )
)

examples_tab <- tabPanel(
  "Examples",
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "example_choice",
        "Choose a topic example",
        choices = setNames(seq_len(nrow(example_guides)), paste(example_guides$topic, "-", example_guides$title))
      )
    ),
    mainPanel(
      tags$h4(textOutput("example_title")),
      uiOutput("example_details")
    )
  )
)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { font-size: 15px; }
      .nav-tabs { margin-bottom: 16px; }
      .match-box { border-bottom: 1px solid #ddd; padding: 10px 0; }
      .match-title { font-weight: 700; }
      .match-meta { color: #555; margin-top: 2px; }
      .formula-note { color: #555; margin-bottom: 12px; }
      .reference-box, .formula-card { border: 1px solid #ddd; border-radius: 4px; padding: 10px 12px; margin-bottom: 14px; background: #fafafa; }
      .reference-box p, .formula-card p { margin-bottom: 6px; }
      .reference-box pre, .formula-card pre { background: #fff; border: 1px solid #ddd; padding: 8px; white-space: pre-wrap; }
      .concept-card { border: 1px solid #ddd; border-radius: 4px; padding: 10px 12px; margin-bottom: 12px; background: #fafafa; }
      .concept-card ul { margin-bottom: 0; }
      pre.shiny-text-output { white-space: pre-wrap; max-height: 650px; overflow-y: auto; }
      textarea { min-height: 90px; }
    "))
  ),
  titlePanel("StatGuide R AI"),
  tabsetPanel(
    ask_ai_tab,
    calculator_tab,
    concepts_tab,
    formulas_tab,
    search_tab,
    examples_tab,
    ai_settings_tab
  )
)
