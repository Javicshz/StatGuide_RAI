# Server wiring for Ask AI and AI Settings.

register_ai_server <- function(input, output, session) {
  ollama_models <- reactiveVal(character(0))
  ollama_status <- reactiveVal("Click Refresh Ollama Models or Review Settings to load installed local models.")
  online_models <- reactiveVal(character(0))
  online_model_status <- reactiveVal("Click Load Available Models or Review Settings to load account/provider models.")
  # Endpoint the setup guide pre-fills when it switches to a local server.
  guide_endpoint <- reactiveVal("")

  selected_ai_model <- reactive({
    provider <- input$ai_provider %||% "none"
    if (provider == "ollama") {
      dropdown_model <- input$ai_ollama_model %||% ""
      manual_model <- input$ai_ollama_manual_model %||% ""
      if (nzchar(trimws(dropdown_model))) {
        return(dropdown_model)
      }
      return(manual_model)
    }
    if (provider %in% c("openai", "anthropic", "generic_openai")) {
      dropdown_model <- input$ai_online_model %||% ""
      manual_model <- input$ai_model %||% ""
      if (nzchar(trimws(dropdown_model))) {
        return(dropdown_model)
      }
      return(manual_model)
    }
    input$ai_model %||% ""
  })

  selected_ai_endpoint <- reactive({
    provider <- input$ai_provider %||% "none"
    if (provider == "ollama") {
      return(input$ai_ollama_endpoint %||% "")
    }
    input$ai_endpoint %||% ""
  })

  output$ai_provider_settings_ui <- renderUI({
    provider <- input$ai_provider %||% "none"

    if (provider == "ollama") {
      # isolate: re-rendering on refresh would reset the endpoint/model boxes.
      models <- isolate(ollama_models())
      choices <- c("Type model manually" = "", stats::setNames(models, models))
      return(tagList(
        textInput("ai_ollama_endpoint", "Ollama endpoint", "http://localhost:11434"),
        actionButton("ai_refresh_ollama_models", "Refresh Ollama Models"),
        selectInput("ai_ollama_model", "Installed Ollama model", choices = choices),
        textInput("ai_ollama_manual_model", "Manual model name", "", placeholder = "Example: llama3.1"),
        verbatimTextOutput("ai_ollama_model_status"),
        tags$div(class = "formula-note", "Model management should be done directly in Ollama. This app only lists and uses installed models.")
      ))
    }

    if (provider %in% c("openai", "anthropic", "generic_openai")) {
      models <- isolate(online_models())
      choices <- c("Type model manually" = "", stats::setNames(models, models))
      return(tagList(
        textInput("ai_endpoint", "Endpoint URL", isolate(guide_endpoint()), placeholder = if (provider == "anthropic") "Default: https://api.anthropic.com/v1" else "Default: https://api.openai.com/v1"),
        textInput("ai_key_env", "API key environment variable", "", placeholder = if (provider == "anthropic") "ANTHROPIC_API_KEY" else "OPENAI_API_KEY"),
        actionButton("ai_refresh_online_models", "Load Available Models"),
        selectInput("ai_online_model", "Available model", choices = choices),
        textInput("ai_model", "Manual model name", "", placeholder = if (provider == "anthropic") "Example: claude-sonnet-4" else "Example: gpt-4.1"),
        verbatimTextOutput("ai_online_model_status"),
        tags$div(class = "formula-note", "Model lists come from the selected provider account and API key. Use manual entry if a model is not listed.")
      ))
    }

    if (provider == "codex_cli") {
      return(tagList(
        textInput("ai_local_app", "Codex CLI command or path", "", placeholder = "/Applications/Codex.app/Contents/Resources/codex"),
        textInput("ai_model", "Optional model override", "", placeholder = "Leave blank to use Codex CLI default"),
        tags$div(class = "formula-note", "Uses local `codex exec` and your existing Codex CLI login/session. No API key is entered in this app.")
      ))
    }

    if (provider == "claude_code_cli") {
      return(tagList(
        textInput("ai_local_app", "Claude Code CLI command or path", "", placeholder = "claude"),
        textInput("ai_model", "Optional model override", "", placeholder = "Leave blank to use Claude Code default"),
        tags$div(class = "formula-note", "Uses local Claude Code CLI and your existing CLI login/session. No API key is entered in this app.")
      ))
    }

    tags$div(
      class = "reference-box",
      tags$p("Manual prompt-only mode does not need provider settings.")
    )
  })

  observeEvent(input$ai_refresh_ollama_models, {
    result <- ai_ollama_model_names(input$ai_ollama_endpoint %||% "")
    if (isTRUE(result$ok)) {
      ollama_models(result$models %||% character(0))
      ollama_status(result$message)
      current <- selected_ai_model()
      valid_current <- current %in% (result$models %||% character(0))
      updateSelectInput(
        session,
        "ai_ollama_model",
        choices = c("Type model manually" = "", stats::setNames(result$models, result$models)),
        selected = if (valid_current) current else ""
      )
    } else {
      ollama_status(result$message)
    }
  })

  output$ai_ollama_model_status <- renderText({
    ollama_status()
  })

  observeEvent(input$ai_provider, {
    online_models(character(0))
    online_model_status("Click Load Available Models or Review Settings to load account/provider models.")
  }, ignoreInit = TRUE)

  load_online_models <- function(provider) {
    settings <- ai_provider_settings(
      provider,
      selected_ai_model(),
      selected_ai_endpoint(),
      input$ai_key_env %||% "",
      "",
      TRUE
    )

    if (provider %in% c("openai", "generic_openai")) {
      return(ai_openai_connection_summary(settings))
    }

    if (provider == "anthropic") {
      return(ai_anthropic_connection_summary(settings))
    }

    list(
      ok = FALSE,
      status = "provider_not_supported",
      message = "Model loading is available for OpenAI-compatible and Anthropic providers."
    )
  }

  update_online_model_choices <- function(result) {
    models <- result$models %||% character(0)
    online_models(models)
    current <- selected_ai_model()
    valid_current <- current %in% models
    updateSelectInput(
      session,
      "ai_online_model",
      choices = c("Type model manually" = "", stats::setNames(models, models)),
      selected = if (valid_current) current else ""
    )
  }

  observeEvent(input$ai_refresh_online_models, {
    provider <- input$ai_provider %||% "none"
    result <- load_online_models(provider)
    online_model_status(paste(
      paste("Status:", result$status),
      "",
      result$message,
      sep = "\n"
    ))
    if (isTRUE(result$ok)) {
      update_online_model_choices(result)
    }
  })

  output$ai_online_model_status <- renderText({
    online_model_status()
  })

  output$ai_provider_status <- renderText({
    paste(
      ai_provider_summary(
        input$ai_provider %||% "none",
        selected_ai_model(),
        selected_ai_endpoint(),
        input$ai_key_env %||% "",
        input$ai_local_app %||% ""
      ),
      collapse = "\n"
    )
  })

  output$ai_connection_test <- renderText({
    if (is.null(input$ai_test_settings) || input$ai_test_settings == 0) {
      return("Click Review Settings to check the selected provider. Ollama checks the local endpoint and installed model list.")
    }

    provider <- isolate(input$ai_provider %||% "none")
    if (provider == "ollama") {
      result <- isolate(ai_ollama_connection_summary(input$ai_ollama_endpoint %||% ""))
      if (isTRUE(result$ok)) {
        ollama_models(result$models %||% character(0))
        ollama_status(result$message)
        updateSelectInput(
          session,
          "ai_ollama_model",
          choices = c("Type model manually" = "", stats::setNames(result$models, result$models)),
          selected = if ((selected_ai_model() %||% "") %in% (result$models %||% character(0))) selected_ai_model() else ""
        )
      } else {
        ollama_status(result$message)
      }
      return(paste(
        paste("Status:", result$status),
        "",
        result$message,
        sep = "\n"
      ))
    }

    if (provider == "none") {
      return("Status: prompt_only\n\nManual prompt-only mode does not need a connection test.")
    }

    if (provider == "codex_cli") {
      result <- isolate(ai_codex_cli_connection_summary(ai_provider_settings(
        provider,
        selected_ai_model(),
        "",
        "",
        input$ai_local_app %||% "",
        FALSE
      )))
      return(paste(
        paste("Status:", result$status),
        "",
        result$message,
        sep = "\n"
      ))
    }

    if (provider == "claude_code_cli") {
      result <- isolate(ai_claude_code_connection_summary(ai_provider_settings(
        provider,
        selected_ai_model(),
        "",
        "",
        input$ai_local_app %||% "",
        FALSE
      )))
      return(paste(
        paste("Status:", result$status),
        "",
        result$message,
        sep = "\n"
      ))
    }

    if (provider %in% c("openai", "generic_openai")) {
      result <- isolate(load_online_models(provider))
      if (isTRUE(result$ok)) {
        update_online_model_choices(result)
        online_model_status(result$message)
      } else {
        online_model_status(result$message)
      }
      return(paste(
        paste("Status:", result$status),
        "",
        result$message,
        sep = "\n"
      ))
    }

    if (provider == "anthropic") {
      result <- isolate(load_online_models(provider))
      if (isTRUE(result$ok)) {
        update_online_model_choices(result)
        online_model_status(result$message)
      } else {
        online_model_status(result$message)
      }
      return(paste(
        paste("Status:", result$status),
        "",
        result$message,
        sep = "\n"
      ))
    }

    "Status: adapter_not_implemented\n\nConnection tests for online providers and desktop bridges will be added after credential handling and live adapters are implemented."
  })

  output$ai_provider_help <- renderUI({
    provider <- input$ai_provider %||% "none"

    if (provider == "ollama") {
      return(tags$div(
        class = "concept-card",
        tags$ul(
          tags$li("Start Ollama before using this provider."),
          tags$li("Install a model with a terminal command such as `ollama pull llama3.1`."),
          tags$li("Use Refresh Ollama Models to load installed models into the dropdown."),
          tags$li("Prompts stay on the configured local endpoint, usually http://localhost:11434.")
        )
      ))
    }

    if (provider == "openai") {
      return(tags$div(
        class = "concept-card",
        tags$ul(
          tags$li("Set an environment variable named OPENAI_API_KEY outside this app."),
          tags$li("Leave Endpoint blank to use https://api.openai.com/v1."),
          tags$li("Enter the environment variable name, not the raw key."),
          tags$li("Check external-send approval in Ask AI before sending a prompt online.")
        )
      ))
    }

    if (provider == "anthropic") {
      return(tags$div(
        class = "concept-card",
        tags$ul(
          tags$li("Set an environment variable named ANTHROPIC_API_KEY outside this app."),
          tags$li("Leave Endpoint blank to use https://api.anthropic.com/v1."),
          tags$li("Enter the environment variable name, not the raw key."),
          tags$li("Check external-send approval in Ask AI before sending a prompt online.")
        )
      ))
    }

    if (provider == "generic_openai") {
      return(tags$div(
        class = "concept-card",
        tags$ul(
          tags$li("Use this only for providers that support OpenAI-style chat completions."),
          tags$li("Provide the provider endpoint URL, model name, and API key environment variable name."),
          tags$li("Enter the environment variable name, not the raw key."),
          tags$li("Some providers may require custom request fields that are not supported yet.")
        )
      ))
    }

    if (provider == "codex_cli") {
      return(tags$div(
        class = "concept-card",
        tags$ul(
          tags$li("Uses local `codex exec`, not the OpenAI API."),
          tags$li("Leave command blank to use the bundled Codex CLI when available."),
          tags$li("Click Review Settings to confirm the CLI is installed and runnable."),
          tags$li("The app runs Codex in read-only sandbox mode with approvals disabled for non-interactive use.")
        )
      ))
    }

    if (provider == "claude_code_cli") {
      return(tags$div(
        class = "concept-card",
        tags$ul(
          tags$li("Uses local Claude Code CLI, not the Anthropic API."),
          tags$li("Install and log in to Claude Code outside this app first."),
          tags$li("Enter `claude` or the full path to the Claude Code CLI executable."),
          tags$li("Click Review Settings to confirm the CLI is installed and runnable.")
        )
      ))
    }

    tags$div(
      class = "concept-card",
      tags$ul(
        tags$li("Manual mode prepares a prompt but does not send it anywhere."),
        tags$li("Use this mode when you want to copy the prompt into another AI tool yourself."),
        tags$li("No model, endpoint, or API key settings are required.")
      )
    )
  })

  output$ai_prompt_preview <- renderText({
    if (is.null(input$ai_prepare) || input$ai_prepare == 0) {
      return("Enter a question, choose an AI task, then click Prepare AI Prompt.")
    }
    isolate(build_ai_prompt(
      input$ai_task %||% "method",
      input$ai_question %||% "",
      input$ai_context %||% "",
      input$ai_provider %||% "none",
      isTRUE(input$ai_allow_external)
    ))
  })

  # mirai (optional) runs the request in a background R process so the app stays
  # responsive; without it the request runs in this session as before.
  providers_file <- normalizePath(file.path(app_dir, "R", "ai_providers.R"))
  ai_task <- ExtendedTask$new(function(prompt, task, question, context, settings) {
    if (requireNamespace("mirai", quietly = TRUE)) {
      mirai::mirai(
        {
          source(providers_file, local = TRUE)
          send_ai_request(prompt, task, question, context, settings)
        },
        providers_file = providers_file,
        prompt = prompt,
        task = task,
        question = question,
        context = context,
        settings = settings
      )
    } else {
      send_ai_request(prompt, task, question, context, settings)
    }
  })
  bslib::bind_task_button(ai_task, "ai_send")

  observeEvent(input$ai_send, {
    ai_task$invoke(
      build_ai_prompt(
        input$ai_task %||% "method",
        input$ai_question %||% "",
        input$ai_context %||% "",
        input$ai_provider %||% "none",
        isTRUE(input$ai_allow_external)
      ),
      input$ai_task %||% "method",
      input$ai_question %||% "",
      input$ai_context %||% "",
      ai_provider_settings(
        input$ai_provider %||% "none",
        selected_ai_model(),
        selected_ai_endpoint(),
        input$ai_key_env %||% "",
        input$ai_local_app %||% "",
        isTRUE(input$ai_allow_external)
      )
    )
  })

  output$ai_response <- renderUI({
    if (is.null(input$ai_send) || input$ai_send == 0) {
      return(tags$div(
        class = "markdown-output",
        tags$p("Choose an AI provider in AI Settings, enter a request, then click Ask AI."),
        tags$p("Manual prompt-only mode prepares a prompt without sending data anywhere.")
      ))
    }

    result <- ai_task$result()
    tags$div(
      class = "ai-response-wrapper",
      tags$p(tags$strong("Status: "), result$status),
      render_markdownish(result$message)
    )
  })

  output$ai_local_guidance <- renderUI({
    task <- input$ai_task %||% "method"
    provider <- input$ai_provider %||% "none"
    online_provider <- provider %in% c("openai", "anthropic", "generic_openai")
    local_provider <- provider %in% c("ollama", "codex_cli", "claude_code_cli")

    task_tip <- ai_task_tip(task)
    context_tip <- ai_task_context_prompt(task)

    provider_tip <- if (online_provider && !isTRUE(input$ai_allow_external)) {
      "You selected an online provider path, but external-send approval is unchecked. Keep this prompt local until you review sensitive data."
    } else if (provider == "ollama") {
      "Ollama sends prompts to the configured local endpoint only. Make sure Ollama is running and a model name is entered in AI Settings."
    } else if (local_provider) {
      "You selected a local provider path. Later implementation should still show model availability, timeout, and error states."
    } else if (provider == "none") {
      "No provider is configured. You can still copy the prepared prompt into Codex, Claude, ChatGPT, or another tool manually."
    } else {
      "Review the prepared prompt before sending it anywhere."
    }

    tags$div(
      class = "concept-card",
      tags$ul(
        tags$li(tags$strong("Task tip: "), task_tip),
        tags$li(tags$strong("Context to include: "), context_tip),
        tags$li(tags$strong("Provider tip: "), provider_tip),
        tags$li(tags$strong("Safety tip: "), "Do not include private data, secrets, API keys, tokens, or full sensitive datasets."),
        tags$li(tags$strong("Verification tip: "), "Ask AI to show assumptions and R code, then verify the result with the calculators or R output.")
      )
    )
  })

  # Setup guide: a pop-up that walks non-programmers through connecting an AI.
  # Inline tags without the line breaks htmltools adds, so punctuation sits right after them.
  b <- function(...) tags$b(..., .noWS = "outside")
  code <- function(...) tags$code(..., .noWS = "outside")
  kbd <- function(...) tags$kbd(..., .noWS = "outside")
  link <- function(href, text) tags$a(href = href, target = "_blank", text, .noWS = "outside")
  open_terminal_step <- tags$li(
    "Open a terminal. On a Mac press ", kbd("Cmd"), " + ", kbd("Space"),
    ", type ", b("Terminal"), ", and press Enter. On Windows open the Start menu and search for ",
    b("PowerShell"), "."
  )
  type_step <- function(command, after = NULL) {
    tags$li("Type this and press Enter: ", code(command), after)
  }

  guide_steps <- function(choice) {
    switch(choice,
      ollama = tagList(
        tags$p("Ollama runs a free AI model on your own computer. Your questions never leave your machine. You need about 8 GB of memory (RAM) and a few GB of disk space."),
        tags$ol(
          tags$li("Go to ", link("https://ollama.com/download", "ollama.com/download"), ", download Ollama, and install it like any other app. Open it once; it then runs quietly in the background."),
          open_terminal_step,
          type_step("ollama pull llama3.2", " This downloads a small model (about 2 GB). Wait until it says success."),
          tags$li("Click ", b("Check my setup"), " below. When it says Ready, click ", b("Use this"), ".")
        )
      ),
      codex_cli = tagList(
        tags$p("Codex is OpenAI's coding assistant. It uses your ChatGPT plan (Plus, Pro, Business, or Edu), so there is no API key to manage. Your questions are sent to OpenAI."),
        tags$ol(
          tags$li("Install Node.js from ", link("https://nodejs.org", "nodejs.org"), " (click the LTS download) if you have not already."),
          open_terminal_step,
          type_step("npm install -g @openai/codex"),
          type_step("codex login", " A browser window opens. Sign in with your ChatGPT account, then come back."),
          tags$li("Click ", b("Check my setup"), " below. When it says Ready, click ", b("Use this"), ".")
        )
      ),
      claude_code_cli = tagList(
        tags$p("Claude Code is Anthropic's coding assistant. It uses your Claude plan (Pro or Max), so there is no API key to manage. Your questions are sent to Anthropic."),
        tags$ol(
          open_terminal_step,
          tags$li("Install it. On a Mac type ", code("curl -fsSL https://claude.ai/install.sh | bash"),
                  " and press Enter. On Windows (PowerShell) type ", code("irm https://claude.ai/install.ps1 | iex"), " and press Enter."),
          type_step("claude", " Follow the sign-in steps in your browser. When you see the Claude prompt, type /exit and press Enter."),
          tags$li("Close and reopen StatGuide so it can find the new command, then click ", b("Check my setup"), ". When it says Ready, click ", b("Use this"), ".")
        )
      ),
      local_server = tagList(
        tags$p("Use this if you run a model with LM Studio, llama.cpp, vLLM, or a similar app. Everything stays on your computer and no API key is needed."),
        tags$ol(
          tags$li("Easiest option: install ", link("https://lmstudio.ai", "LM Studio"), ", open it, and download a model from its search page."),
          tags$li("In LM Studio open the ", b("Developer"), " tab and turn on ", b("Start server"), ". It listens at ", code("http://localhost:1234/v1"), "."),
          tags$li("Click ", b("Check my setup"), " below. When it says Ready, click ", b("Use this"), ". Then pick your model in AI Settings with ", b("Load Available Models"), "."),
          tags$li("Using a different app? Click ", b("Use this"), " anyway and type that app's address in the Endpoint URL box.")
        )
      ),
      openai = tagList(
        tags$p("An API key lets the app talk to OpenAI directly. API use is billed per question by OpenAI, separately from a ChatGPT plan. Treat the key like a password: never paste it into this app, a chat, or a file you share."),
        tags$ol(
          tags$li("Create a key at ", link("https://platform.openai.com/api-keys", "platform.openai.com/api-keys"), " and copy it."),
          tags$li("In the RStudio Console (or any R window) type ", code('file.edit("~/.Renviron")'), " and press Enter. A file opens."),
          tags$li("Add this line, with your key after the = sign, then save and close the file: ", code("OPENAI_API_KEY=your-key-here")),
          tags$li("Close and reopen StatGuide so it reads the key. This file lives in your home folder, not in the project, so it is never uploaded to GitHub."),
          tags$li("Click ", b("Check my setup"), ". When it says Ready, click ", b("Use this"), " and choose a model in AI Settings with ", b("Load Available Models"), ".")
        )
      ),
      anthropic = tagList(
        tags$p("An API key lets the app talk to Anthropic (Claude) directly. API use is billed per question by Anthropic, separately from a Claude plan. Treat the key like a password: never paste it into this app, a chat, or a file you share."),
        tags$ol(
          tags$li("Create a key at ", link("https://console.anthropic.com/settings/keys", "console.anthropic.com/settings/keys"), " and copy it."),
          tags$li("In the RStudio Console (or any R window) type ", code('file.edit("~/.Renviron")'), " and press Enter. A file opens."),
          tags$li("Add this line, with your key after the = sign, then save and close the file: ", code("ANTHROPIC_API_KEY=your-key-here")),
          tags$li("Close and reopen StatGuide so it reads the key. This file lives in your home folder, not in the project, so it is never uploaded to GitHub."),
          tags$li("Click ", b("Check my setup"), ". When it says Ready, click ", b("Use this"), " and choose a model in AI Settings with ", b("Load Available Models"), ".")
        )
      ),
      tagList(
        tags$p("No setup needed. The app writes a well-structured prompt that you can copy into any AI website, such as ChatGPT or Claude."),
        tags$ol(
          tags$li("Click ", b("Use this"), "."),
          tags$li("In the Ask AI tab, type your question and click ", b("Prepare AI Prompt"), "."),
          tags$li("Copy the prepared prompt into the AI website you use.")
        )
      )
    )
  }

  guide_provider <- c(
    ollama = "ollama", codex_cli = "codex_cli", claude_code_cli = "claude_code_cli",
    local_server = "generic_openai", openai = "openai", anthropic = "anthropic", none = "none"
  )
  local_server_endpoint <- "http://localhost:1234/v1"

  guide_check <- function(choice) {
    key_missing <- function(key_env) {
      list(ok = FALSE, message = paste0(
        "No ", key_env, " was found. Follow steps 2 to 4, then close and reopen StatGuide before checking again."
      ))
    }
    result <- switch(choice,
      ollama = {
        r <- ai_ollama_connection_summary("")
        if (isTRUE(r$ok) && !length(r$models)) {
          r <- list(ok = FALSE, message = "Ollama is running, but no model is installed yet. Do step 3, then check again.")
        }
        r
      },
      codex_cli = ai_codex_cli_connection_summary(list(local_app = "")),
      claude_code_cli = ai_claude_code_connection_summary(list(local_app = "")),
      local_server = ai_openai_connection_summary(ai_provider_settings("generic_openai", "", local_server_endpoint, "", "", FALSE)),
      openai = if (nzchar(Sys.getenv("OPENAI_API_KEY"))) {
        ai_openai_connection_summary(ai_provider_settings("openai", "", "", "OPENAI_API_KEY", "", TRUE))
      } else {
        key_missing("OPENAI_API_KEY")
      },
      anthropic = if (nzchar(Sys.getenv("ANTHROPIC_API_KEY"))) {
        ai_anthropic_connection_summary(ai_provider_settings("anthropic", "", "", "ANTHROPIC_API_KEY", "", TRUE))
      } else {
        key_missing("ANTHROPIC_API_KEY")
      },
      list(ok = TRUE, message = "Nothing to connect.")
    )
    paste0(if (isTRUE(result$ok)) "Ready.\n\n" else "Not ready yet.\n\n", result$message)
  }

  guide_result <- reactiveVal("")

  show_setup_guide <- function() {
    guide_result("")
    showModal(modalDialog(
      title = "Connect an AI model",
      size = "l",
      easyClose = TRUE,
      radioButtons(
        "guide_choice",
        "Which of these fits you?",
        width = "100%",
        choices = c(
          "I want a free AI that runs on my own computer (Ollama)" = "ollama",
          "I pay for ChatGPT and want to use Codex" = "codex_cli",
          "I pay for Claude and want to use Claude Code" = "claude_code_cli",
          "I already run LM Studio or another local model server" = "local_server",
          "I have an OpenAI API key" = "openai",
          "I have an Anthropic (Claude) API key" = "anthropic",
          "No AI, just give me a prompt to copy" = "none"
        )
      ),
      tags$hr(),
      uiOutput("guide_steps"),
      verbatimTextOutput("guide_result"),
      footer = tagList(
        actionButton("guide_check", "Check my setup"),
        actionButton("guide_apply", "Use this"),
        modalButton("Close")
      )
    ))
  }

  observeEvent(input$ai_setup_guide, show_setup_guide())
  observeEvent(input$ai_setup_guide_ask, show_setup_guide())

  output$guide_steps <- renderUI(guide_steps(input$guide_choice %||% "ollama"))
  observeEvent(input$guide_choice, guide_result(""))
  output$guide_result <- renderText({
    if (nzchar(guide_result())) guide_result() else "Follow the steps above, then click Check my setup."
  })

  observeEvent(input$guide_check, {
    guide_result("Checking...")
    guide_result(guide_check(input$guide_choice %||% "ollama"))
  })

  observeEvent(input$guide_apply, {
    choice <- input$guide_choice %||% "ollama"
    guide_endpoint(if (choice == "local_server") local_server_endpoint else "")
    updateSelectInput(session, "ai_provider", selected = guide_provider[[choice]])
    updateTabsetPanel(session, "main_tabs", selected = if (choice == "none") "Ask AI" else "AI Settings")
    removeModal()
  })
}
