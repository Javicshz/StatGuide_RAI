# AI provider planning and request helpers.
# Online provider calls remain disabled until credential and privacy behavior
# is implemented. Ollama is supported as a local-only provider.

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

ai_provider_choices <- c(
  "Manual / prompt only" = "none",
  "Local mock provider for testing" = "mock",
  "Codex CLI (local)" = "codex_cli",
  "OpenAI-compatible API" = "openai",
  "Claude / Anthropic API" = "anthropic",
  "Claude Code CLI (local)" = "claude_code_cli",
  "Ollama local model" = "ollama",
  "Generic OpenAI-compatible endpoint" = "generic_openai"
)

ai_task_choices <- c(
  "Choose the right statistical method" = "method",
  "Explain a statistics concept" = "concept",
  "Generate R code" = "generate_r",
  "Debug R code or an error message" = "debug_r",
  "Interpret calculator output" = "interpret",
  "Plan a data analysis workflow" = "workflow",
  "Import, clean, or validate data" = "import_clean",
  "Write tidyverse transformations" = "tidyverse",
  "Build a visualization or EDA plan" = "viz_eda",
  "Work with SQL, databases, or data pipelines" = "data_engineering",
  "Create a Quarto report or notebook" = "quarto",
  "Develop R functions, packages, or tests" = "package_test",
  "Build or debug a Shiny app" = "shiny_help",
  "Plan modeling or machine learning in R" = "modeling",
  "Handle large data or performance issues" = "big_data",
  "Plan a student portfolio or learning path" = "student_path"
)

ai_task_tip <- function(task) {
  switch(task,
    method = "Include the response variable, explanatory variables, number of groups, whether samples are paired, and whether the response is numeric or categorical.",
    concept = "Ask for definitions, intuition, assumptions, formulas, and a small R example.",
    generate_r = "Describe the data frame columns, desired output, packages allowed, and whether you prefer base R or tidyverse.",
    debug_r = "Paste the exact error, the smallest code that reproduces it, and the output of str() for relevant objects.",
    interpret = "Paste the calculator output and ask for a plain-language conclusion with assumptions and limitations.",
    workflow = "Describe the data source, variables, research question, cleaning needs, and final report goal.",
    import_clean = "Include the file type, column names, suspicious values, missing-value rules, and what the cleaned data should look like.",
    tidyverse = "Describe the starting data frame, desired output table, grouping variables, joins, and whether dplyr/tidyr are allowed.",
    viz_eda = "List the variables, their types, the comparison or pattern you want to see, and the audience for the plot.",
    data_engineering = "Include the data source, table relationships, keys, desired query result, size constraints, and whether SQL or R should do the work.",
    quarto = "Describe the report audience, output format, sections needed, code chunks, parameters, and reproducibility requirements.",
    package_test = "Include the repeated code pattern, expected inputs/outputs, edge cases, and what behavior should be tested.",
    shiny_help = "Paste the UI/server code, input/output IDs, exact warning/error, and the interaction that should happen.",
    modeling = "Name the outcome, predictors, prediction/explanation goal, train/test plan, metrics, and assumptions or fairness concerns.",
    big_data = "Include the data size, format, memory symptoms, slow step, and whether a database, Arrow, DuckDB, or sampling is available.",
    student_path = "Describe the learner level, target role, class/trade-school/university context, available time, and portfolio goal.",
    "Include enough context for the AI to identify the statistical task."
  )
}

ai_provider_summary <- function(provider, model, endpoint, key_env, local_app) {
  provider_label <- names(ai_provider_choices)[match(provider, ai_provider_choices)]
  if (is.na(provider_label)) provider_label <- "Unknown provider"

  if (provider == "none") {
    return(c(
      "Status: Manual prompt-only mode.",
      "Mode: The app prepares a structured prompt for use in the AI tool of your choice.",
      "Direct answer: Not available until a provider is selected.",
      "Data sharing: Nothing leaves this app from the AI tab in this mode."
    ))
  }

  if (provider == "mock") {
    return(c(
      "Status: Local mock provider enabled.",
      "Mode: The app returns a deterministic local test response without calling any AI service.",
      "Direct answer: Available for testing UI flow only.",
      "Data sharing: Nothing leaves this app."
    ))
  }

  if (provider == "ollama") {
    return(c(
      paste("Provider:", provider_label),
      paste("Local endpoint:", ifelse(nzchar(endpoint), endpoint, "http://localhost:11434")),
      paste("Model:", ifelse(nzchar(model), model, "not selected")),
      "Current behavior: local Ollama requests are supported through /api/generate.",
      "Data sharing: prompts are sent only to the configured local Ollama endpoint."
    ))
  }

  if (provider == "codex_cli") {
    return(c(
      paste("Provider:", provider_label),
      paste("Command:", ai_codex_command(local_app)),
      paste("Model:", ifelse(nzchar(model), model, "default from Codex config")),
      "Current behavior: runs local `codex exec` using your existing Codex CLI login/session.",
      "Data sharing: prompts are sent to the local Codex CLI process, which may use your configured Codex account/provider."
    ))
  }

  if (provider == "claude_code_cli") {
    return(c(
      paste("Provider:", provider_label),
      paste("Command:", ai_claude_code_command(local_app)),
      paste("Model:", ifelse(nzchar(model), model, "default from Claude Code config")),
      "Current behavior: runs local Claude Code CLI using your existing Claude Code login/session.",
      "Data sharing: prompts are sent to the local Claude Code CLI process, which may use your configured Claude account/provider."
    ))
  }

  if (provider == "anthropic") {
    return(c(
      paste("Provider:", provider_label),
      paste("Model:", ifelse(nzchar(model), model, "not selected")),
      paste("Endpoint:", ifelse(nzchar(endpoint), endpoint, "https://api.anthropic.com/v1")),
      paste("API key environment variable:", ifelse(nzchar(key_env), key_env, "ANTHROPIC_API_KEY")),
      "Current behavior: Anthropic Messages API requests are supported after explicit send approval."
    ))
  }

  c(
    paste("Provider:", provider_label),
    paste("Model:", ifelse(nzchar(model), model, "not selected")),
    paste("Endpoint:", ifelse(nzchar(endpoint), endpoint, ai_openai_endpoint(endpoint, provider))),
    paste("API key environment variable:", ifelse(nzchar(key_env), key_env, "not specified")),
    if (provider %in% c("openai", "generic_openai")) {
      "Current behavior: OpenAI-compatible chat completion requests are supported after explicit send approval."
    } else {
      "Current behavior: request interface exists, but no Anthropic online API call is made yet."
    }
  )
}

build_ai_prompt <- function(task, question, context, provider, allow_external) {
  task_label <- names(ai_task_choices)[match(task, ai_task_choices)]
  if (is.na(task_label)) task_label <- "General statistics/R help"

  data_policy <- if (isTRUE(allow_external)) {
    "The user has indicated this prompt may be sent to the selected provider after real provider support is implemented."
  } else {
    "Do not send this prompt to an online provider. Keep it local or copy it manually after reviewing sensitive data."
  }

  provider_label <- names(ai_provider_choices)[match(provider, ai_provider_choices)] %||% "Unknown"

  paste(
    "You are helping with statistics, data analysis, and R code inside an RStudio workflow.",
    "",
    paste("Task:", task_label),
    paste("Provider mode:", provider_label),
    paste("Data policy:", data_policy),
    "",
    "User question:",
    ifelse(nzchar(trimws(question)), trimws(question), "[No question entered yet]"),
    "",
    "Optional context, data summary, code, or error message:",
    ifelse(nzchar(trimws(context)), trimws(context), "[No extra context provided]"),
    "",
    "Answer requirements:",
    "- State the recommended method or R approach.",
    "- Explain why it fits and which assumptions must be checked.",
    "- Provide reproducible R code when code is helpful.",
    "- Do not invent data or hidden assumptions.",
    "- If information is missing, ask concise follow-up questions.",
    "- If interpreting results, include practical context and limitations.",
    sep = "\n"
  )
}

ai_provider_settings <- function(provider, model, endpoint, key_env, local_app, allow_external) {
  list(
    provider = provider %||% "none",
    model = model %||% "",
    endpoint = endpoint %||% "",
    key_env = key_env %||% "",
    local_app = local_app %||% "",
    allow_external = isTRUE(allow_external)
  )
}

ai_mock_answer <- function(task, question, context, prompt) {
  question_text <- trimws(question)
  if (!nzchar(question_text)) {
    question_text <- "No question was provided."
  }

  task_label <- names(ai_task_choices)[match(task, ai_task_choices)]
  if (is.na(task_label)) task_label <- "General statistics/R help"

  paste(
    "Mock AI response (local test mode)",
    "",
    paste("Task:", task_label),
    "",
    "What I would do next:",
    paste("- Clarify the goal:", question_text),
    "- Identify the data type, response variable, explanatory variables, sample size, and grouping structure.",
    "- Choose the R workflow or statistical method based on those details.",
    "- Provide reproducible R code and ask you to verify assumptions before trusting the result.",
    "",
    "Suggested next prompt improvement:",
    ai_task_tip(task),
    "",
    "Safety note:",
    "This is a deterministic local test response. It proves the app can route and display direct AI-style responses, but it is not a real model answer yet.",
    sep = "\n"
  )
}

ai_ollama_endpoint <- function(endpoint) {
  endpoint <- trimws(endpoint %||% "")
  if (!nzchar(endpoint)) {
    endpoint <- "http://localhost:11434"
  }
  sub("/+$", "", endpoint)
}

ai_json_response <- function(response) {
  text <- rawToChar(response$content)
  if (!nzchar(text)) {
    return(list())
  }
  jsonlite::fromJSON(text, simplifyVector = FALSE)
}

ai_ollama_request <- function(path, method = "GET", body = NULL, endpoint = "", timeout = 60) {
  if (!requireNamespace("curl", quietly = TRUE)) {
    return(list(ok = FALSE, status = "missing_dependency", message = "The curl R package is required for Ollama support."))
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    return(list(ok = FALSE, status = "missing_dependency", message = "The jsonlite R package is required for Ollama support."))
  }

  url <- paste0(ai_ollama_endpoint(endpoint), path)
  handle <- curl::new_handle()
  curl::handle_setopt(handle, timeout = timeout, connecttimeout = 5)

  if (identical(method, "POST")) {
    curl::handle_setopt(
      handle,
      customrequest = "POST",
      postfields = jsonlite::toJSON(body, auto_unbox = TRUE, null = "null")
    )
    curl::handle_setheaders(handle, "Content-Type" = "application/json")
  }

  response <- tryCatch(
    curl::curl_fetch_memory(url, handle = handle),
    error = function(e) e
  )

  if (inherits(response, "error")) {
    return(list(
      ok = FALSE,
      status = "ollama_unreachable",
      message = paste(
        "Could not reach Ollama at",
        ai_ollama_endpoint(endpoint),
        "\nStart Ollama, confirm the endpoint, and try again.\n\nDetails:",
        conditionMessage(response)
      )
    ))
  }

  parsed <- tryCatch(ai_json_response(response), error = function(e) e)
  if (inherits(parsed, "error")) {
    return(list(
      ok = FALSE,
      status = "ollama_bad_json",
      message = paste("Ollama returned a response that could not be parsed as JSON.", conditionMessage(parsed))
    ))
  }

  if (response$status_code < 200 || response$status_code >= 300) {
    ollama_error <- parsed$error %||% rawToChar(response$content)
    return(list(
      ok = FALSE,
      status = "ollama_http_error",
      message = paste("Ollama returned HTTP", response$status_code, "-", ollama_error)
    ))
  }

  list(ok = TRUE, status = "ok", data = parsed)
}

ai_ollama_connection_summary <- function(endpoint = "") {
  version_result <- ai_ollama_request("/api/version", endpoint = endpoint, timeout = 10)
  if (!isTRUE(version_result$ok)) {
    return(version_result)
  }

  tags_result <- ai_ollama_request("/api/tags", endpoint = endpoint, timeout = 10)
  if (!isTRUE(tags_result$ok)) {
    return(tags_result)
  }

  models <- tags_result$data$models %||% list()
  model_names <- vapply(models, function(model) model$name %||% "", character(1))
  model_names <- model_names[nzchar(model_names)]

  message <- c(
    paste("Connected to Ollama at", ai_ollama_endpoint(endpoint)),
    paste("Version:", version_result$data$version %||% "unknown"),
    if (length(model_names)) {
      paste("Installed models:", paste(model_names, collapse = ", "))
    } else {
      "Installed models: none found. Run `ollama pull <model>` before asking the app to use Ollama."
    }
  )

  list(ok = TRUE, status = "ollama_connected", message = paste(message, collapse = "\n"), models = model_names)
}

ai_ollama_model_names <- function(endpoint = "") {
  result <- ai_ollama_connection_summary(endpoint)
  if (!isTRUE(result$ok)) {
    return(result)
  }
  list(
    ok = TRUE,
    status = "ollama_models_loaded",
    message = result$message,
    models = result$models %||% character(0)
  )
}

ai_ollama_delete_model <- function(model, endpoint = "") {
  model <- trimws(model %||% "")
  if (!nzchar(model)) {
    return(list(
      ok = FALSE,
      status = "ollama_delete_missing_model",
      message = "Choose an installed Ollama model before deleting."
    ))
  }

  result <- ai_ollama_request(
    "/api/delete",
    method = "POST",
    body = list(model = model),
    endpoint = endpoint,
    timeout = 60
  )

  if (!isTRUE(result$ok)) {
    return(result)
  }

  list(
    ok = TRUE,
    status = "ollama_model_deleted",
    message = paste("Deleted Ollama model:", model)
  )
}

ai_ollama_generate <- function(prompt, settings) {
  model <- trimws(settings$model %||% "")
  if (!nzchar(model)) {
    return(list(
      ok = FALSE,
      status = "ollama_missing_model",
      message = "Choose or type an Ollama model name first. Example: llama3.1, mistral, qwen2.5-coder, or any model shown by `ollama list`."
    ))
  }

  body <- list(
    model = model,
    prompt = prompt,
    stream = FALSE
  )

  result <- ai_ollama_request("/api/generate", method = "POST", body = body, endpoint = settings$endpoint, timeout = 120)
  if (!isTRUE(result$ok)) {
    return(result)
  }

  response_text <- result$data$response %||% ""
  if (!nzchar(trimws(response_text))) {
    return(list(
      ok = FALSE,
      status = "ollama_empty_response",
      message = "Ollama responded successfully, but the response text was empty."
    ))
  }

  list(
    ok = TRUE,
    status = "ollama_response",
    message = response_text
  )
}

ai_openai_endpoint <- function(endpoint, provider = "openai") {
  endpoint <- trimws(endpoint %||% "")
  if (!nzchar(endpoint)) {
    if (provider == "generic_openai") {
      return("")
    }
    endpoint <- "https://api.openai.com/v1"
  }
  sub("/+$", "", endpoint)
}

ai_openai_key_env <- function(key_env, provider = "openai") {
  key_env <- trimws(key_env %||% "")
  if (nzchar(key_env)) {
    return(key_env)
  }
  if (provider %in% c("openai", "generic_openai")) {
    return("OPENAI_API_KEY")
  }
  key_env
}

ai_openai_request <- function(path, method = "GET", body = NULL, settings, timeout = 90) {
  if (!requireNamespace("curl", quietly = TRUE)) {
    return(list(ok = FALSE, status = "missing_dependency", message = "The curl R package is required for OpenAI-compatible API support."))
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    return(list(ok = FALSE, status = "missing_dependency", message = "The jsonlite R package is required for OpenAI-compatible API support."))
  }

  endpoint <- ai_openai_endpoint(settings$endpoint, settings$provider)
  if (!nzchar(endpoint)) {
    return(list(
      ok = FALSE,
      status = "missing_endpoint",
      message = "Enter an OpenAI-compatible endpoint URL for this provider. Example: https://api.openai.com/v1"
    ))
  }

  key_env <- ai_openai_key_env(settings$key_env, settings$provider)
  api_key <- Sys.getenv(key_env, unset = "")
  if (!nzchar(api_key)) {
    return(list(
      ok = FALSE,
      status = "missing_credentials",
      message = paste("Missing API key. Set environment variable", key_env, "before using this provider.")
    ))
  }

  url <- paste0(endpoint, path)
  handle <- curl::new_handle()
  curl::handle_setopt(handle, timeout = timeout, connecttimeout = 10)
  curl::handle_setheaders(
    handle,
    "Authorization" = paste("Bearer", api_key),
    "Content-Type" = "application/json"
  )

  if (identical(method, "POST")) {
    curl::handle_setopt(
      handle,
      customrequest = "POST",
      postfields = jsonlite::toJSON(body, auto_unbox = TRUE, null = "null")
    )
  }

  response <- tryCatch(
    curl::curl_fetch_memory(url, handle = handle),
    error = function(e) e
  )

  if (inherits(response, "error")) {
    return(list(
      ok = FALSE,
      status = "online_provider_unreachable",
      message = paste("Could not reach", endpoint, "\n\nDetails:", conditionMessage(response))
    ))
  }

  parsed <- tryCatch(ai_json_response(response), error = function(e) e)
  if (inherits(parsed, "error")) {
    return(list(
      ok = FALSE,
      status = "online_provider_bad_json",
      message = paste("Provider returned a response that could not be parsed as JSON.", conditionMessage(parsed))
    ))
  }

  if (response$status_code < 200 || response$status_code >= 300) {
    provider_error <- parsed$error$message %||% parsed$error %||% rawToChar(response$content)
    return(list(
      ok = FALSE,
      status = "online_provider_http_error",
      message = paste("Provider returned HTTP", response$status_code, "-", provider_error)
    ))
  }

  list(ok = TRUE, status = "ok", data = parsed)
}

ai_openai_connection_summary <- function(settings) {
  result <- ai_openai_request("/models", method = "GET", settings = settings, timeout = 30)
  if (!isTRUE(result$ok)) {
    return(result)
  }

  models <- result$data$data %||% list()
  model_ids <- vapply(models, function(model) model$id %||% "", character(1))
  model_ids <- model_ids[nzchar(model_ids)]

  list(
    ok = TRUE,
    status = "online_provider_connected",
    message = paste(
      c(
        paste("Connected to", ai_openai_endpoint(settings$endpoint, settings$provider)),
        paste("API key environment variable:", ai_openai_key_env(settings$key_env, settings$provider)),
        if (length(model_ids)) {
          paste("Available model IDs returned:", paste(utils::head(model_ids, 10), collapse = ", "))
        } else {
          "The provider responded, but no model IDs were returned."
        }
      ),
      collapse = "\n"
    ),
    models = model_ids
  )
}

ai_openai_generate <- function(prompt, settings) {
  model <- trimws(settings$model %||% "")
  if (!nzchar(model)) {
    return(list(
      ok = FALSE,
      status = "missing_model",
      message = "Enter a model name before asking this provider."
    ))
  }

  body <- list(
    model = model,
    messages = list(
      list(role = "system", content = "You are a careful statistics, R, data science, and data engineering assistant inside an RStudio workflow. Be accurate, explicit about assumptions, and provide reproducible R code when useful."),
      list(role = "user", content = prompt)
    ),
    temperature = 0.2
  )

  result <- ai_openai_request("/chat/completions", method = "POST", body = body, settings = settings, timeout = 120)
  if (!isTRUE(result$ok)) {
    return(result)
  }

  choices <- result$data$choices %||% list()
  response_text <- ""
  if (length(choices) >= 1) {
    response_text <- choices[[1]]$message$content %||% ""
  }

  if (!nzchar(trimws(response_text))) {
    return(list(
      ok = FALSE,
      status = "online_provider_empty_response",
      message = "The provider responded successfully, but no answer text was returned."
    ))
  }

  list(
    ok = TRUE,
    status = "online_provider_response",
    message = response_text
  )
}

ai_anthropic_endpoint <- function(endpoint) {
  endpoint <- trimws(endpoint %||% "")
  if (!nzchar(endpoint)) {
    endpoint <- "https://api.anthropic.com/v1"
  }
  sub("/+$", "", endpoint)
}

ai_anthropic_key_env <- function(key_env) {
  key_env <- trimws(key_env %||% "")
  if (nzchar(key_env)) {
    return(key_env)
  }
  "ANTHROPIC_API_KEY"
}

ai_anthropic_request <- function(path, method = "GET", body = NULL, settings, timeout = 90) {
  if (!requireNamespace("curl", quietly = TRUE)) {
    return(list(ok = FALSE, status = "missing_dependency", message = "The curl R package is required for Anthropic API support."))
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    return(list(ok = FALSE, status = "missing_dependency", message = "The jsonlite R package is required for Anthropic API support."))
  }

  endpoint <- ai_anthropic_endpoint(settings$endpoint)
  key_env <- ai_anthropic_key_env(settings$key_env)
  api_key <- Sys.getenv(key_env, unset = "")
  if (!nzchar(api_key)) {
    return(list(
      ok = FALSE,
      status = "missing_credentials",
      message = paste("Missing API key. Set environment variable", key_env, "before using Anthropic.")
    ))
  }

  url <- paste0(endpoint, path)
  handle <- curl::new_handle()
  curl::handle_setopt(handle, timeout = timeout, connecttimeout = 10)
  curl::handle_setheaders(
    handle,
    "x-api-key" = api_key,
    "anthropic-version" = "2023-06-01",
    "Content-Type" = "application/json"
  )

  if (identical(method, "POST")) {
    curl::handle_setopt(
      handle,
      customrequest = "POST",
      postfields = jsonlite::toJSON(body, auto_unbox = TRUE, null = "null")
    )
  }

  response <- tryCatch(
    curl::curl_fetch_memory(url, handle = handle),
    error = function(e) e
  )

  if (inherits(response, "error")) {
    return(list(
      ok = FALSE,
      status = "anthropic_unreachable",
      message = paste("Could not reach", endpoint, "\n\nDetails:", conditionMessage(response))
    ))
  }

  parsed <- tryCatch(ai_json_response(response), error = function(e) e)
  if (inherits(parsed, "error")) {
    return(list(
      ok = FALSE,
      status = "anthropic_bad_json",
      message = paste("Anthropic returned a response that could not be parsed as JSON.", conditionMessage(parsed))
    ))
  }

  if (response$status_code < 200 || response$status_code >= 300) {
    provider_error <- parsed$error$message %||% parsed$error %||% rawToChar(response$content)
    return(list(
      ok = FALSE,
      status = "anthropic_http_error",
      message = paste("Anthropic returned HTTP", response$status_code, "-", provider_error)
    ))
  }

  list(ok = TRUE, status = "ok", data = parsed)
}

ai_anthropic_connection_summary <- function(settings) {
  result <- ai_anthropic_request("/models", method = "GET", settings = settings, timeout = 30)
  if (!isTRUE(result$ok)) {
    return(result)
  }

  models <- result$data$data %||% list()
  model_ids <- vapply(models, function(model) model$id %||% "", character(1))
  model_ids <- model_ids[nzchar(model_ids)]

  list(
    ok = TRUE,
    status = "anthropic_connected",
    message = paste(
      c(
        paste("Connected to", ai_anthropic_endpoint(settings$endpoint)),
        paste("API key environment variable:", ai_anthropic_key_env(settings$key_env)),
        if (length(model_ids)) {
          paste("Available model IDs returned:", paste(utils::head(model_ids, 10), collapse = ", "))
        } else {
          "Anthropic responded, but no model IDs were returned."
        }
      ),
      collapse = "\n"
    ),
    models = model_ids
  )
}

ai_anthropic_generate <- function(prompt, settings) {
  model <- trimws(settings$model %||% "")
  if (!nzchar(model)) {
    return(list(
      ok = FALSE,
      status = "missing_model",
      message = "Enter a Claude/Anthropic model name before asking this provider."
    ))
  }

  body <- list(
    model = model,
    max_tokens = 2000,
    system = "You are a careful statistics, R, data science, and data engineering assistant inside an RStudio workflow. Be accurate, explicit about assumptions, and provide reproducible R code when useful.",
    messages = list(
      list(role = "user", content = prompt)
    )
  )

  result <- ai_anthropic_request("/messages", method = "POST", body = body, settings = settings, timeout = 120)
  if (!isTRUE(result$ok)) {
    return(result)
  }

  blocks <- result$data$content %||% list()
  text_blocks <- vapply(blocks, function(block) {
    if (identical(block$type %||% "", "text")) {
      return(block$text %||% "")
    }
    ""
  }, character(1))
  response_text <- paste(text_blocks[nzchar(text_blocks)], collapse = "\n\n")

  if (!nzchar(trimws(response_text))) {
    return(list(
      ok = FALSE,
      status = "anthropic_empty_response",
      message = "Anthropic responded successfully, but no answer text was returned."
    ))
  }

  list(
    ok = TRUE,
    status = "anthropic_response",
    message = response_text
  )
}

ai_command_path <- function(command) {
  command <- trimws(command %||% "")
  if (!nzchar(command)) {
    return("")
  }
  if (file.exists(command)) {
    return(command)
  }
  found <- Sys.which(command)
  if (nzchar(found)) {
    return(unname(found))
  }
  ""
}

ai_codex_command <- function(command) {
  command <- trimws(command %||% "")
  if (nzchar(command)) {
    return(command)
  }
  bundled <- "/Applications/Codex.app/Contents/Resources/codex"
  if (file.exists(bundled)) {
    return(bundled)
  }
  "codex"
}

ai_claude_code_command <- function(command) {
  command <- trimws(command %||% "")
  if (nzchar(command)) {
    return(command)
  }
  "claude"
}

ai_cli_check <- function(command, label) {
  resolved <- ai_command_path(command)
  if (!nzchar(resolved)) {
    return(list(
      ok = FALSE,
      status = "cli_not_found",
      message = paste(label, "command was not found. Enter the executable name or full path in AI Settings.")
    ))
  }

  version <- tryCatch(
    system2(resolved, "--version", stdout = TRUE, stderr = TRUE, timeout = 15),
    error = function(e) e
  )

  if (inherits(version, "error")) {
    return(list(
      ok = FALSE,
      status = "cli_check_failed",
      message = paste(label, "was found at", resolved, "but could not run --version.\n\nDetails:", conditionMessage(version))
    ))
  }

  list(
    ok = TRUE,
    status = "cli_ready",
    message = paste(c(
      paste(label, "command:", resolved),
      paste("Version/check output:", paste(version, collapse = "\n"))
    ), collapse = "\n")
  )
}

ai_cli_run <- function(command, args, input = NULL, label = "CLI", timeout = 120) {
  resolved <- ai_command_path(command)
  if (!nzchar(resolved)) {
    return(list(
      ok = FALSE,
      status = "cli_not_found",
      message = paste(label, "command was not found. Enter the executable name or full path in AI Settings.")
    ))
  }

  output <- tryCatch(
    system2(resolved, args = args, input = input, stdout = TRUE, stderr = TRUE, timeout = timeout),
    warning = function(w) w,
    error = function(e) e
  )

  status <- attr(output, "status")
  if (inherits(output, "warning") || inherits(output, "error") || (!is.null(status) && !identical(status, 0L))) {
    detail <- if (inherits(output, "condition")) conditionMessage(output) else paste(output, collapse = "\n")
    return(list(
      ok = FALSE,
      status = "cli_run_failed",
      message = paste(label, "command failed.\n\nDetails:", detail)
    ))
  }

  text <- paste(output, collapse = "\n")
  list(ok = TRUE, status = "cli_response", message = text)
}

ai_codex_cli_connection_summary <- function(settings) {
  ai_cli_check(ai_codex_command(settings$local_app), "Codex CLI")
}

ai_claude_code_connection_summary <- function(settings) {
  ai_cli_check(ai_claude_code_command(settings$local_app), "Claude Code CLI")
}

ai_codex_cli_generate <- function(prompt, settings) {
  command <- ai_codex_command(settings$local_app)
  output_file <- tempfile("codex-last-message-", fileext = ".txt")
  on.exit(unlink(output_file), add = TRUE)

  args <- c(
    "exec",
    "--sandbox", "read-only",
    "-c", "approval_policy=\"never\"",
    "--skip-git-repo-check",
    "--color", "never",
    "--output-last-message", output_file
  )
  model <- trimws(settings$model %||% "")
  if (nzchar(model)) {
    args <- c(args, "--model", model)
  }
  args <- c(args, "-")

  result <- ai_cli_run(command, args, input = prompt, label = "Codex CLI", timeout = 180)
  if (!isTRUE(result$ok)) {
    return(result)
  }

  response_text <- ""
  if (file.exists(output_file)) {
    response_text <- paste(readLines(output_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  }
  if (!nzchar(trimws(response_text))) {
    response_text <- result$message
  }
  if (!nzchar(trimws(response_text))) {
    return(list(ok = FALSE, status = "cli_empty_response", message = "Codex CLI completed but no response text was captured."))
  }

  list(ok = TRUE, status = "codex_cli_response", message = response_text)
}

ai_claude_code_generate <- function(prompt, settings) {
  command <- ai_claude_code_command(settings$local_app)
  args <- c("-p", prompt, "--output-format", "text")
  model <- trimws(settings$model %||% "")
  if (nzchar(model)) {
    args <- c(args, "--model", model)
  }

  result <- ai_cli_run(command, args, label = "Claude Code CLI", timeout = 180)
  if (!isTRUE(result$ok)) {
    return(result)
  }
  if (!nzchar(trimws(result$message))) {
    return(list(ok = FALSE, status = "cli_empty_response", message = "Claude Code CLI completed but no response text was captured."))
  }

  list(ok = TRUE, status = "claude_code_cli_response", message = result$message)
}

send_ai_request <- function(prompt, task, question, context, settings) {
  provider <- settings$provider %||% "none"

  if (!nzchar(trimws(question)) && !nzchar(trimws(context))) {
    return(list(
      ok = FALSE,
      status = "missing_input",
      message = "Enter a question, code snippet, data summary, or error message before asking AI."
    ))
  }

  if (provider == "none") {
    return(list(
      ok = FALSE,
      status = "prompt_only",
      message = paste(
        "Manual prompt-only mode is selected.",
        "The app prepared the prompt, but no direct AI provider is configured yet.",
        "Choose the local mock provider to test direct-answer UI flow, or configure a real provider after provider adapters are implemented.",
        sep = "\n"
      )
    ))
  }

  if (provider == "mock") {
    return(list(
      ok = TRUE,
      status = "mock_response",
      message = ai_mock_answer(task, question, context, prompt)
    ))
  }

  if (provider == "ollama") {
    return(ai_ollama_generate(prompt, settings))
  }

  if (provider == "codex_cli") {
    return(ai_codex_cli_generate(prompt, settings))
  }

  if (provider == "claude_code_cli") {
    return(ai_claude_code_generate(prompt, settings))
  }

  if (provider %in% c("openai", "anthropic", "generic_openai") && !isTRUE(settings$allow_external)) {
    return(list(
      ok = FALSE,
      status = "blocked_external_send",
      message = "Online provider selected, but external-send approval is unchecked. Review the prompt for private data, then explicitly approve sending before a live adapter can use it."
    ))
  }

  if (provider %in% c("openai", "generic_openai")) {
    return(ai_openai_generate(prompt, settings))
  }

  if (provider == "anthropic") {
    return(ai_anthropic_generate(prompt, settings))
  }

  list(
    ok = FALSE,
    status = "adapter_not_implemented",
    message = paste(
      "The provider request reached the new AI interface, but the live adapter is not implemented in this phase.",
      "This is expected for future provider-specific endpoints until connector behavior, privacy notices, timeouts, and tests are complete.",
      sep = "\n"
    )
  )
}
