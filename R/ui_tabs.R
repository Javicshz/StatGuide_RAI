# Shiny tab and top-level UI definitions.

ask_ai_tab <- tabPanel(
  "Ask AI",
  sidebarLayout(
    sidebarPanel(
      actionButton("ai_setup_guide_ask", "New to AI? Set up a model step by step", class = "btn-block"),
      tags$br(),
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
      bslib::input_task_button("ai_send", "Ask AI", label_busy = "Waiting for AI...")
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
      uiOutput("ai_response"),
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
      actionButton("ai_setup_guide", "Help me connect an AI (step by step)", class = "btn-block"),
      tags$br(),
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
      uiOutput("search_results"),
      uiOutput("search_selected_details")
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
      :root {
        --charcoal-blue: #2f4858;
        --baltic-blue: #33658a;
        --sky-reflection: #86bbd8;
        --honey-bronze: #f6ae2d;
        --blaze-orange: #f26419;
        --midnight-bg: #101a22;
        --midnight-panel: #172631;
        --midnight-panel-soft: #1d303d;
        --midnight-border: #365466;
        --midnight-text: #e7f1f6;
        --midnight-muted: #b8c9d3;
        --shadow-soft: 0 4px 14px rgba(0, 0, 0, 0.28);
      }
      body { font-size: 15px; color: var(--midnight-text); background: var(--midnight-bg); }
      h2, h4, h5 { color: var(--midnight-text); }
      label, .control-label { color: var(--midnight-text); }
      .help-block { color: var(--midnight-muted); }
      .container-fluid { max-width: 1480px; }
      .nav-tabs { margin-bottom: 18px; border-bottom: 1px solid var(--midnight-border); }
      .nav-tabs > li > a {
        color: var(--sky-reflection);
        border-radius: 999px;
        margin-right: 6px;
        border: 1px solid transparent;
      }
      .nav-tabs > li > a:hover,
      .nav-tabs > li > a:focus {
        color: #fff;
        background: var(--midnight-panel-soft);
        border-color: var(--baltic-blue);
      }
      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:hover,
      .nav-tabs > li.active > a:focus {
        color: var(--midnight-bg);
        background: var(--sky-reflection);
        border-color: var(--sky-reflection);
        box-shadow: var(--shadow-soft);
      }
      .well {
        background: var(--midnight-panel);
        border: 1px solid var(--midnight-border);
        border-radius: 8px;
        box-shadow: var(--shadow-soft);
      }
      .form-control, .selectize-input {
        color: var(--midnight-text);
        background: #0f1a22;
        border-color: var(--midnight-border);
        border-radius: 7px;
      }
      .selectize-control.single .selectize-input,
      .selectize-control.single .selectize-input.full,
      .selectize-control.single .selectize-input.input-active,
      .selectize-input.items {
        color: var(--midnight-text);
        background: #0f1a22;
        border-color: var(--midnight-border);
        box-shadow: none;
      }
      .selectize-input > input {
        color: var(--midnight-text) !important;
      }
      .selectize-input .item {
        color: var(--midnight-text);
      }
      .selectize-control.single .selectize-input:after {
        border-color: var(--midnight-text) transparent transparent transparent;
      }
      .selectize-control.single .selectize-input.dropdown-active:after {
        border-color: transparent transparent var(--midnight-text) transparent;
      }
      .form-control::placeholder { color: #8fa6b3; }
      .form-control:focus, .selectize-input.focus {
        border-color: var(--sky-reflection);
        box-shadow: 0 0 0 3px rgba(134, 187, 216, 0.28);
      }
      .selectize-dropdown, .selectize-dropdown-content {
        color: var(--midnight-text);
        background: var(--midnight-panel);
        border-color: var(--midnight-border);
      }
      .selectize-dropdown .option {
        color: var(--midnight-text);
        background: var(--midnight-panel);
      }
      .selectize-dropdown .active {
        color: #fff;
        background: var(--baltic-blue);
      }
      .btn, .action-button {
        border-radius: 999px;
        border: 1px solid var(--sky-reflection);
        background: var(--baltic-blue);
        color: #fff;
        box-shadow: var(--shadow-soft);
        padding: 7px 16px;
        transition: background 0.15s ease, border-color 0.15s ease, box-shadow 0.15s ease, transform 0.15s ease;
      }
      .btn:hover, .btn:focus, .action-button:hover, .action-button:focus {
        background: var(--sky-reflection);
        border-color: var(--sky-reflection);
        color: var(--midnight-bg);
        box-shadow: 0 5px 16px rgba(0, 0, 0, 0.36);
      }
      .btn-default {
        background: var(--midnight-panel-soft);
        border-color: var(--midnight-border);
        color: #fff;
      }
      .btn:active, .action-button:active { transform: translateY(1px); }
      #ai_send, #run {
        background: var(--blaze-orange);
        border-color: var(--blaze-orange);
      }
      #ai_send:hover, #ai_send:focus, #run:hover, #run:focus {
        background: var(--honey-bronze);
        border-color: var(--honey-bronze);
        color: var(--midnight-bg);
      }
      #ai_refresh_ollama_models, #ai_refresh_online_models {
        margin-bottom: 12px;
      }
      .match-box { border-bottom: 1px solid var(--midnight-border); padding: 10px 0; }
      .match-title { font-weight: 700; }
      .match-link { color: var(--sky-reflection); text-decoration: none; cursor: pointer; }
      .match-link:hover { text-decoration: underline; }
      .match-meta { color: var(--midnight-muted); margin-top: 2px; }
      .formula-note { color: var(--midnight-muted); margin-bottom: 12px; }
      .reference-box, .formula-card { border: 1px solid var(--midnight-border); border-radius: 8px; padding: 10px 12px; margin-bottom: 14px; background: var(--midnight-panel); box-shadow: var(--shadow-soft); }
      .reference-box p, .formula-card p { margin-bottom: 6px; }
      .reference-box pre, .formula-card pre { color: var(--midnight-text); background: #0f1a22; border: 1px solid var(--midnight-border); border-radius: 7px; padding: 8px; white-space: pre-wrap; }
      .concept-card { border: 1px solid var(--midnight-border); border-radius: 8px; padding: 10px 12px; margin-bottom: 12px; background: var(--midnight-panel); box-shadow: var(--shadow-soft); }
      .concept-list-clean p { margin: 0 0 8px 0; line-height: 1.45; }
      .concept-list-clean p:last-child { margin-bottom: 0; }
      .reference-list-clean { display: flex; flex-direction: column; gap: 0; margin-bottom: 0; }
      .reference-row { display: grid; grid-template-columns: minmax(130px, 220px) minmax(0, 1fr); gap: 14px; padding: 8px 0; border-bottom: 1px solid rgba(134, 187, 216, 0.18); line-height: 1.45; }
      .reference-row:last-child { border-bottom: 0; }
      .reference-label { color: var(--honey-bronze); font-weight: 700; }
      .reference-body { color: var(--midnight-text); min-width: 0; }
      .formula-equation-inline { color: var(--sky-reflection); font-family: Menlo, Monaco, Consolas, 'Courier New', monospace; font-size: 14px; white-space: normal; overflow-wrap: anywhere; }
      .formula-note-inline { color: var(--midnight-text); }
      @media (max-width: 700px) {
        .reference-row { grid-template-columns: 1fr; gap: 2px; padding: 9px 0; }
        .btn, .action-button { width: 100%; margin-bottom: 8px; }
        .nav-tabs > li > a { margin-bottom: 6px; }
      }
      .markdown-output { border: 1px solid var(--midnight-border); border-radius: 8px; padding: 10px 12px; background: var(--midnight-panel); box-shadow: var(--shadow-soft); max-height: 650px; overflow-y: auto; }
      .markdown-output p { margin-bottom: 8px; }
      .markdown-output code { background: rgba(246, 174, 45, 0.13); border: 1px solid rgba(246, 174, 45, 0.35); border-radius: 4px; padding: 1px 4px; color: #ffd27a; }
      .markdown-code { background: #0f1a22; border: 1px solid var(--midnight-border); border-radius: 7px; padding: 8px; white-space: pre-wrap; }
      .markdown-bullet { margin: 4px 0 4px 16px; }
      .markdown-bullet:before { content: '• '; margin-left: -12px; }
      .markdown-numbered { margin: 4px 0; }
      pre.shiny-text-output { color: var(--midnight-text); white-space: pre-wrap; max-height: 650px; overflow-y: auto; background: var(--midnight-panel); border-color: var(--midnight-border); border-radius: 8px; box-shadow: var(--shadow-soft); }
      textarea { min-height: 90px; }
    "))
  ),
  titlePanel("StatGuide R AI"),
  tabsetPanel(
    id = "main_tabs",
    ask_ai_tab,
    calculator_tab,
    concepts_tab,
    formulas_tab,
    search_tab,
    ai_settings_tab
  )
)
