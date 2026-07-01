# Console AI helpers for StatGuide R AI.
# These functions are meant for direct RStudio Console use.

.statguide_console_ai_dir <- tryCatch(
  dirname(normalizePath(sys.frames()[[1]]$ofile, mustWork = FALSE)),
  error = function(e) ""
)

statguide_console_ai_project_dir <- function(path) {
  if (!nzchar(path)) {
    return("")
  }
  if (basename(path) == "R") {
    candidate <- dirname(path)
  } else {
    candidate <- path
  }
  if (
    file.exists(file.path(candidate, "app.R")) &&
      file.exists(file.path(candidate, "R", "ai_providers.R"))
  ) {
    return(candidate)
  }
  ""
}

statguide_console_load_ai <- function() {
  required <- c("%||%", "ai_task_choices", "build_ai_prompt", "ai_provider_settings", "send_ai_request")
  if (all(vapply(required, exists, logical(1), mode = "any", inherits = TRUE))) {
    return(invisible(TRUE))
  }

  candidates <- unique(c(
    statguide_console_ai_project_dir(dirname(.statguide_console_ai_dir)),
    statguide_console_ai_project_dir(.statguide_console_ai_dir),
    statguide_console_ai_project_dir(getwd())
  ))
  candidates <- candidates[nzchar(candidates)]
  if (!length(candidates)) {
    stop(
      "Could not find the StatGuide_RAI project folder. Source run_console.R from the folder that contains app.R first.",
      call. = FALSE
    )
  }

  source(file.path(candidates[1], "R", "ai_providers.R"), local = .GlobalEnv)
  invisible(TRUE)
}

statguide_console_prompt <- function(question, task = "generate_r", context = "", provider = "codex_cli", allow_external = TRUE) {
  statguide_console_load_ai()

  question <- trimws(question %||% "")
  context <- trimws(context %||% "")
  task <- trimws(task %||% "generate_r")
  provider <- trimws(provider %||% "codex_cli")

  if (!nzchar(question)) {
    stop("Enter a question for AI.", call. = FALSE)
  }
  if (!task %in% unname(ai_task_choices)) {
    task <- "generate_r"
  }
  if (!provider %in% unname(ai_provider_choices)) {
    provider <- "codex_cli"
  }

  build_ai_prompt(
    task = task,
    question = question,
    context = context,
    provider = provider,
    allow_external = isTRUE(allow_external)
  )
}

statguide_console_print_ai_result <- function(result, provider_label = "AI") {
  if (isTRUE(result$ok)) {
    cat(result$message, "\n", sep = "")
    return(invisible(result$message))
  }

  message <- paste(provider_label, "request failed:", result$message)
  cat(message, "\n", sep = "")
  invisible(result)
}

statguide_ai_choice <- function(prompt, labels, allow_cancel = TRUE) {
  for (i in seq_along(labels)) {
    cat(sprintf("%2d. %s\n", i, labels[i]))
  }
  if (allow_cancel) {
    cat(" 0. Cancel\n")
  }
  repeat {
    raw <- trimws(readline(prompt))
    if (allow_cancel && raw %in% c("", "0", "cancel", "Cancel")) {
      return(0L)
    }
    choice <- suppressWarnings(as.integer(raw))
    if (!is.na(choice) && choice >= 1L && choice <= length(labels)) {
      return(choice)
    }
    cat("Enter a valid option number", if (allow_cancel) ", or 0 to cancel" else "", ".\n", sep = "")
  }
}

statguide_ai_confirm_send <- function(provider_label) {
  cat("\nOnline provider selected:", provider_label, "\n")
  cat("Review the prompt and make sure it does not include private data, secrets, API keys, or sensitive datasets.\n")
  identical(trimws(readline("Type YES to send this prompt to the online provider: ")), "YES")
}

statguide_ai_prepare_settings <- function(
  provider,
  model = "",
  endpoint = "",
  key_env = "",
  command = "",
  allow_external = provider %in% c("codex_cli", "claude_code_cli", "ollama"),
  interactive_setup = base::interactive()
) {
  statguide_console_load_ai()
  provider <- trimws(provider %||% "codex_cli")
  model <- trimws(model %||% "")
  endpoint <- trimws(endpoint %||% "")
  key_env <- trimws(key_env %||% "")
  command <- trimws(command %||% "")

  if (!isTRUE(interactive_setup)) {
    return(ai_provider_settings(provider, model, endpoint, key_env, command, allow_external))
  }

  if (provider == "ollama") {
    if (!nzchar(endpoint)) {
      endpoint <- ai_ollama_endpoint("")
    }
    if (!nzchar(model)) {
      cat("\nOllama model setup\n")
      models_result <- ai_ollama_model_names(endpoint)
      if (isTRUE(models_result$ok) && length(models_result$models %||% character(0))) {
        cat("Installed Ollama models found at ", ai_ollama_endpoint(endpoint), ":\n", sep = "")
        choice <- statguide_ai_choice("Choose model: ", models_result$models)
        if (choice == 0L) {
          return(NULL)
        }
        model <- models_result$models[choice]
      } else {
        cat("No installed Ollama models could be loaded.\n")
        if (!isTRUE(models_result$ok)) {
          cat(models_result$message, "\n")
        }
        choice <- statguide_ai_choice(
          "Choose setup option: ",
          c("Enter an Ollama model name manually", "Show setup help and cancel")
        )
        if (choice == 0L || choice == 2L) {
          ai_setup_help()
          return(NULL)
        }
        model <- trimws(readline("Ollama model name: "))
      }
    }
  } else if (provider == "codex_cli") {
    detected <- ai_command_path(ai_codex_command(command))
    if (!nzchar(detected)) {
      cat("\nCodex CLI was not auto-detected.\n")
      choice <- statguide_ai_choice(
        "Choose setup option: ",
        c("Enter Codex command/path", "Continue with default command name")
      )
      if (choice == 0L) {
        return(NULL)
      }
      if (choice == 1L) {
        command <- trimws(readline("Codex command/path: "))
      }
    }
  } else if (provider == "claude_code_cli") {
    detected <- ai_command_path(ai_claude_code_command(command))
    if (!nzchar(detected)) {
      cat("\nClaude Code CLI was not auto-detected.\n")
      choice <- statguide_ai_choice(
        "Choose setup option: ",
        c("Enter Claude Code command/path", "Continue with default command name")
      )
      if (choice == 0L) {
        return(NULL)
      }
      if (choice == 1L) {
        command <- trimws(readline("Claude Code command/path: "))
      }
    }
  } else if (provider %in% c("openai", "anthropic", "generic_openai")) {
    provider_label <- names(ai_provider_choices)[match(provider, ai_provider_choices)] %||% provider
    if (!nzchar(model)) {
      model <- trimws(readline(paste(provider_label, "model name: ")))
    }
    if (provider == "generic_openai" && !nzchar(endpoint)) {
      endpoint <- trimws(readline("OpenAI-compatible endpoint, such as https://example.com/v1: "))
    } else if (provider == "openai" && !nzchar(endpoint)) {
      endpoint <- trimws(readline("Endpoint [Enter for https://api.openai.com/v1]: "))
    } else if (provider == "anthropic" && !nzchar(endpoint)) {
      endpoint <- trimws(readline("Endpoint [Enter for https://api.anthropic.com/v1]: "))
    }
    if (!nzchar(key_env)) {
      default_key_env <- if (provider == "anthropic") "ANTHROPIC_API_KEY" else "OPENAI_API_KEY"
      entered_key_env <- trimws(readline(paste0("API key environment variable [Enter for ", default_key_env, "]: ")))
      key_env <- if (nzchar(entered_key_env)) entered_key_env else default_key_env
    }
    if (!nzchar(Sys.getenv(key_env, unset = ""))) {
      cat("\nEnvironment variable ", key_env, " is not set in this R session.\n", sep = "")
      cat("Set it outside the app, for example in your shell or .Renviron. Do not save raw keys in project files.\n")
      choice <- statguide_ai_choice(
        "Choose setup option: ",
        c("Enter a different environment variable name", "Continue anyway", "Show setup help and cancel")
      )
      if (choice == 0L || choice == 3L) {
        ai_setup_help()
        return(NULL)
      }
      if (choice == 1L) {
        key_env <- trimws(readline("API key environment variable name: "))
      }
    }
    if (!isTRUE(allow_external)) {
      allow_external <- statguide_ai_confirm_send(provider_label)
    }
  }

  ai_provider_settings(provider, model, endpoint, key_env, command, allow_external)
}

askai <- function(
  question,
  provider = "codex_cli",
  task = "generate_r",
  context = "",
  model = "",
  endpoint = "",
  key_env = "",
  command = "",
  allow_external = provider %in% c("codex_cli", "claude_code_cli", "ollama"),
  setup = base::interactive()
) {
  statguide_console_load_ai()
  provider <- trimws(provider %||% "codex_cli")
  if (!provider %in% unname(ai_provider_choices)) {
    stop("Unknown AI provider. Use one of: ", paste(unname(ai_provider_choices), collapse = ", "), call. = FALSE)
  }

  prompt <- statguide_console_prompt(
    question,
    task = task,
    context = context,
    provider = provider,
    allow_external = allow_external
  )
  settings <- statguide_ai_prepare_settings(
    provider = provider,
    model = model,
    endpoint = endpoint,
    key_env = key_env,
    command = command,
    allow_external = allow_external,
    interactive_setup = setup
  )
  if (is.null(settings)) {
    return(invisible(list(ok = FALSE, status = "setup_cancelled", message = "AI setup was cancelled.")))
  }

  result <- send_ai_request(
    prompt = prompt,
    task = task,
    question = question,
    context = context,
    settings = settings
  )

  provider_label <- names(ai_provider_choices)[match(provider, ai_provider_choices)] %||% provider
  statguide_console_print_ai_result(result, provider_label)
}

codex <- function(question, task = "generate_r", context = "", model = "", command = "") {
  askai(question, provider = "codex_cli", task = task, context = context, model = model, command = command)
}

askcodex <- function(question, task = "generate_r", context = "", model = "", command = "") {
  codex(question, task = task, context = context, model = model, command = command)
}

ollama <- function(question, model = "", task = "generate_r", context = "", endpoint = "") {
  askai(question, provider = "ollama", task = task, context = context, model = model, endpoint = endpoint)
}

askollama <- function(question, model = "", task = "generate_r", context = "", endpoint = "") {
  ollama(question, model = model, task = task, context = context, endpoint = endpoint)
}

claude <- function(question, task = "generate_r", context = "", model = "", command = "") {
  askai(question, provider = "claude_code_cli", task = task, context = context, model = model, command = command)
}

askclaude <- function(question, task = "generate_r", context = "", model = "", command = "") {
  claude(question, task = task, context = context, model = model, command = command)
}

openai <- function(question, model = "", task = "generate_r", context = "", endpoint = "", key_env = "OPENAI_API_KEY", allow_external = FALSE) {
  askai(
    question,
    provider = "openai",
    task = task,
    context = context,
    model = model,
    endpoint = endpoint,
    key_env = key_env,
    allow_external = allow_external
  )
}

anthropic <- function(question, model = "", task = "generate_r", context = "", endpoint = "", key_env = "ANTHROPIC_API_KEY", allow_external = FALSE) {
  askai(
    question,
    provider = "anthropic",
    task = task,
    context = context,
    model = model,
    endpoint = endpoint,
    key_env = key_env,
    allow_external = allow_external
  )
}

openai_compatible <- function(question, model = "", endpoint = "", task = "generate_r", context = "", key_env = "OPENAI_API_KEY", allow_external = FALSE) {
  askai(
    question,
    provider = "generic_openai",
    task = task,
    context = context,
    model = model,
    endpoint = endpoint,
    key_env = key_env,
    allow_external = allow_external
  )
}

ai_setup_help <- function() {
  cat(
    paste(
      "StatGuide R AI console setup:",
      "",
      "Local providers:",
      "- Codex CLI: source('run_ai.R'); codex('your question')",
      "- Ollama: source('run_ai.R'); ollama('your question', model = 'llama3.1')",
      "- Claude Code CLI: source('run_ai.R'); claude('your question')",
      "",
      "Online/API providers:",
      "- Store API keys outside the app as environment variables.",
      "- Example in your shell or .Renviron: OPENAI_API_KEY=your_key_here",
      "- Then call: openai('your question', model = 'model-name', allow_external = TRUE)",
      "- Generic OpenAI-compatible endpoint:",
      "  openai_compatible('your question', model = 'model-name', endpoint = 'https://example.com/v1', key_env = 'OPENAI_API_KEY', allow_external = TRUE)",
      "",
      "This app does not store raw API keys.",
      sep = "\n"
    ),
    "\n"
  )
  invisible(TRUE)
}

Ollama <- ollama
AskOllama <- askollama
Codex <- codex
AskCodex <- askcodex
Claude <- claude
AskClaude <- askclaude
OpenAI <- openai
Anthropic <- anthropic
OpenAICompatible <- openai_compatible
