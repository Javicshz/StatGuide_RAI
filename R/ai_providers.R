# AI provider planning and request helpers.
# Local providers (Ollama, Codex CLI, Claude Code CLI, localhost OpenAI-compatible
# servers) run without send approval. Online APIs require explicit approval.

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

ai_provider_choices <- c(
  "Manual / prompt only" = "none",
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
  "Create a Quarto report or notebook" = "quarto",
  "Develop R functions, packages, or tests" = "package_test",
  "Build or debug a Shiny app" = "shiny_help",
  "Plan modeling or machine learning in R" = "modeling",
  "Handle large data or performance issues" = "big_data"
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
    quarto = "Describe the report audience, output format, sections needed, code chunks, parameters, and reproducibility requirements.",
    package_test = "Include the repeated code pattern, expected inputs/outputs, edge cases, and what behavior should be tested.",
    shiny_help = "Paste the UI/server code, input/output IDs, exact warning/error, and the interaction that should happen.",
    modeling = "Name the outcome, predictors, prediction/explanation goal, train/test plan, metrics, and assumptions or fairness concerns.",
    big_data = "Include the data size, format, memory symptoms, slow step, and whether a database, Arrow, DuckDB, or sampling is available.",
    "Include enough context for the AI to identify the statistical task."
  )
}

ai_task_answer_requirements <- function(task) {
  common <- c(
    "- Do not invent data, variables, package output, or hidden assumptions.",
    "- If information is missing, ask concise follow-up questions before giving a final recommendation.",
    "- When giving R code, keep it reproducible and explain what the user should replace."
  )

  task_specific <- switch(task,
    method = c(
      "- Identify the response variable and explanatory variable, if they are provided.",
      "- Classify each variable as numeric or categorical.",
      "- Decide whether the design is one-sample, two independent samples, paired samples, or a table/count problem.",
      "- Recommend the most likely calculator/formula/R function and explain why.",
      "- List the assumptions or conditions to check before trusting the method."
    ),
    concept = c(
      "- Start with a plain-language definition.",
      "- Explain the intuition and why the concept works.",
      "- Include the important formula, if one applies.",
      "- Give a small example and connect it to R when useful.",
      "- Mention common mistakes or wording traps."
    ),
    generate_r = c(
      "- Provide runnable R code first, then briefly explain it.",
      "- Prefer base R unless the user asks for tidyverse or another package.",
      "- Include placeholder data frame and column names only when the user did not provide names.",
      "- Avoid installing packages unless the user asks for package setup.",
      "- Include checks such as str(), summary(), names(), or head() when helpful."
    ),
    debug_r = c(
      "- Identify the most likely cause of the error.",
      "- Show the smallest corrected code example.",
      "- Explain how to inspect the object, column names, package loading, and data types.",
      "- Give a checklist the user can run in RStudio.",
      "- Avoid rewriting the whole project unless the issue requires it."
    ),
    interpret = c(
      "- Translate the output into plain English.",
      "- State the decision rule when a P-value, alpha, or confidence interval is involved.",
      "- Include the parameter, population/context, direction of evidence, and practical limitation.",
      "- Avoid saying that a non-significant result proves no effect.",
      "- Mention assumptions that could weaken the interpretation."
    ),
    workflow = c(
      "- Break the analysis into steps: import, inspect, clean, explore, model/test, verify, report.",
      "- Name the R packages or base R functions that fit each step.",
      "- Include checkpoints for missing values, data types, duplicates, and outliers.",
      "- Suggest a simple deliverable such as a table, chart, report, or app output.",
      "- Keep the plan realistic for an RStudio workflow."
    ),
    import_clean = c(
      "- Start by inspecting names, dimensions, types, missing values, and duplicate rows.",
      "- Provide R code for import and cleaning.",
      "- Explain how to handle text numbers, dates, inconsistent labels, and impossible values.",
      "- Preserve raw data and create a cleaned object rather than overwriting immediately.",
      "- Include validation checks after cleaning."
    ),
    tidyverse = c(
      "- Use clear dplyr/tidyr code with pipes.",
      "- Explain grouping, summarizing, filtering, joining, and reshaping steps.",
      "- Include row-count checks for joins.",
      "- Avoid many-to-many joins unless they are intentional.",
      "- Show the expected shape of the output."
    ),
    viz_eda = c(
      "- Choose plot types based on variable types.",
      "- Suggest quick exploratory summaries before final charts.",
      "- Provide ggplot2 code when helpful.",
      "- Explain what pattern, comparison, or outlier the chart is meant to reveal.",
      "- Mention chart limitations or possible misleading choices."
    ),
    quarto = c(
      "- Suggest a report outline with sections.",
      "- Include a minimal Quarto chunk structure.",
      "- Separate narrative, code, output, and limitations.",
      "- Mention reproducibility steps such as relative paths and session/package notes.",
      "- Keep the report readable for the intended audience."
    ),
    package_test = c(
      "- Turn repeated script logic into a small function.",
      "- Define inputs, outputs, and edge cases.",
      "- Include testthat examples when useful.",
      "- Keep functions small and predictable.",
      "- Avoid unnecessary package structure unless the user asks for a package."
    ),
    shiny_help = c(
      "- Identify whether the issue is UI, server, reactive flow, input ID mismatch, or output rendering.",
      "- Give a minimal corrected Shiny example.",
      "- Explain reactive dependencies and when to use render*, observeEvent, reactive, or req.",
      "- Suggest validation messages for bad inputs.",
      "- Keep calculation logic separate from UI where practical."
    ),
    modeling = c(
      "- Clarify whether the goal is prediction, explanation, or inference.",
      "- Identify outcome type and predictor types.",
      "- Suggest train/test split, resampling, metrics, and leakage checks.",
      "- Include baseline model and interpretation cautions.",
      "- Avoid claiming model performance without test results."
    ),
    big_data = c(
      "- Diagnose whether the bottleneck is file size, memory, computation, or joins.",
      "- Suggest sampling, column selection, chunking, database queries, DuckDB, Arrow, or parquet when relevant.",
      "- Avoid collecting full data into memory unless necessary.",
      "- Include performance checks and a smaller reproducible test.",
      "- Mention tradeoffs clearly."
    ),
    c(
      "- State the recommended method or R approach.",
      "- Explain why it fits and which assumptions must be checked.",
      "- Provide reproducible R code when code is helpful.",
      "- If interpreting results, include practical context and limitations."
    )
  )

  unique(c(task_specific, common))
}

ai_task_context_prompt <- function(task) {
  switch(task,
    method = "Helpful details: response variable, explanatory variable, variable types, number of groups, paired vs independent samples, sample size, and exact problem wording.",
    concept = "Helpful details: topic name, formula or vocabulary causing confusion, class/work context, and whether an R example would help.",
    generate_r = "Helpful details: data frame name, column names, desired output, preferred packages, and a tiny example of the data shape.",
    debug_r = "Helpful details: exact error text, code that caused it, object names, package calls, str() output, and what you expected to happen.",
    interpret = "Helpful details: calculator/R output, alpha or confidence level, original claim/question, variable meanings, and study context.",
    workflow = "Helpful details: data source, variables, question, cleaning needs, analysis goal, audience, and final deliverable.",
    import_clean = "Helpful details: file type, column names, bad values, missing-value rules, duplicate rules, and what clean data should look like.",
    tidyverse = "Helpful details: starting table, desired table, grouping variables, join keys, row count expectations, and package preference.",
    viz_eda = "Helpful details: variable names and types, comparison goal, chart audience, and whether the plot is exploratory or final.",
    quarto = "Helpful details: report audience, output format, sections, figures/tables, code chunks, and reproducibility needs.",
    package_test = "Helpful details: repeated code, desired function behavior, valid inputs, invalid inputs, and expected outputs.",
    shiny_help = "Helpful details: UI code, server code, input/output IDs, exact warning/error, and the interaction that should happen.",
    modeling = "Helpful details: outcome, predictors, row count, goal, train/test plan, metrics, leakage risks, and fairness concerns.",
    big_data = "Helpful details: data size, file format, slow step, memory error, available tools, and whether sampling is acceptable.",
    "Helpful details: problem wording, data summary, current code, output, and what you want to happen next."
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
    if (ai_is_local_endpoint(endpoint)) {
      "Current behavior: local OpenAI-compatible server. No API key or send approval needed."
    } else {
      "Current behavior: OpenAI-compatible chat completion requests are supported after explicit send approval."
    }
  )
}

build_ai_prompt <- function(task, question, context, provider, allow_external) {
  task_label <- names(ai_task_choices)[match(task, ai_task_choices)]
  if (is.na(task_label)) task_label <- "General statistics/R help"

  data_policy <- if (isTRUE(allow_external) && provider %in% c("codex_cli", "claude_code_cli", "ollama")) {
    "The user has indicated this prompt may be sent to the selected local provider or CLI tool."
  } else if (isTRUE(allow_external)) {
    "The user has indicated this prompt may be sent to the selected provider."
  } else {
    "Do not send this prompt to an online provider. Keep it local or copy it manually after reviewing sensitive data."
  }

  provider_label <- names(ai_provider_choices)[match(provider, ai_provider_choices)] %||% "Unknown"

  paste(
    "You are helping with statistics, data analysis, and R code inside an RStudio workflow.",
    "The app is primarily a statistics calculator, guide, and R helper. AI support is optional and should support that workflow.",
    "",
    paste("Task:", task_label),
    paste("Provider mode:", provider_label),
    paste("Data policy:", data_policy),
    paste("Context checklist:", ai_task_context_prompt(task)),
    "",
    "User question:",
    ifelse(nzchar(trimws(question)), trimws(question), "[No question entered yet]"),
    "",
    "Optional context, data summary, code, or error message:",
    ifelse(nzchar(trimws(context)), trimws(context), "[No extra context provided]"),
    "",
    "Answer requirements:",
    paste(ai_task_answer_requirements(task), collapse = "\n"),
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

# Shared JSON-over-HTTP call for Ollama, OpenAI-compatible, and Anthropic APIs.
# status_prefix names the statuses (e.g. "ollama" -> "ollama_unreachable"); who
# names the server in error messages.
ai_http_json <- function(url, method = "GET", body = NULL, headers = list(), timeout = 90,
                         connect_timeout = 10, feature, status_prefix, who, unreachable_message) {
  for (pkg in c("curl", "jsonlite")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      return(list(ok = FALSE, status = "missing_dependency", message = paste("The", pkg, "R package is required for", feature, "support.")))
    }
  }

  handle <- curl::new_handle()
  curl::handle_setopt(handle, timeout = timeout, connecttimeout = connect_timeout)
  if (identical(method, "POST")) {
    curl::handle_setopt(
      handle,
      customrequest = "POST",
      postfields = jsonlite::toJSON(body, auto_unbox = TRUE, null = "null")
    )
    headers[["Content-Type"]] <- "application/json"
  }
  if (length(headers)) {
    do.call(curl::handle_setheaders, c(list(handle), headers))
  }

  response <- tryCatch(
    curl::curl_fetch_memory(url, handle = handle),
    error = function(e) e
  )

  if (inherits(response, "error")) {
    return(list(
      ok = FALSE,
      status = paste0(status_prefix, "_unreachable"),
      message = paste(unreachable_message, "\n\nDetails:", conditionMessage(response))
    ))
  }

  parsed <- tryCatch(ai_json_response(response), error = function(e) e)
  if (inherits(parsed, "error")) {
    return(list(
      ok = FALSE,
      status = paste0(status_prefix, "_bad_json"),
      message = paste(who, "returned a response that could not be parsed as JSON.", conditionMessage(parsed))
    ))
  }

  if (response$status_code < 200 || response$status_code >= 300) {
    server_error <- parsed$error
    if (is.list(server_error)) {
      server_error <- server_error$message
    }
    if (is.null(server_error)) {
      server_error <- rawToChar(response$content)
    }
    return(list(
      ok = FALSE,
      status = paste0(status_prefix, "_http_error"),
      message = paste(who, "returned HTTP", response$status_code, "-", server_error)
    ))
  }

  list(ok = TRUE, status = "ok", data = parsed)
}

ai_ollama_request <- function(path, method = "GET", body = NULL, endpoint = "", timeout = 60) {
  ai_http_json(
    paste0(ai_ollama_endpoint(endpoint), path),
    method = method,
    body = body,
    timeout = timeout,
    connect_timeout = 5,
    feature = "Ollama",
    status_prefix = "ollama",
    who = "Ollama",
    unreachable_message = paste(
      "Could not reach Ollama at",
      ai_ollama_endpoint(endpoint),
      "\nStart Ollama, confirm the endpoint, and try again."
    )
  )
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

# LM Studio, llama.cpp, vLLM, and Ollama's /v1 serve OpenAI-style APIs on localhost.
ai_is_local_endpoint <- function(endpoint) {
  endpoint <- trimws(endpoint %||% "")
  authority <- sub("^https?://([^/?#]*).*$", "\\1", endpoint, ignore.case = TRUE)
  # Not a URL, or user info such as "localhost:1@other.host" that points elsewhere.
  if (identical(authority, endpoint) || grepl("@", authority, fixed = TRUE)) {
    return(FALSE)
  }
  tolower(sub(":[0-9]*$", "", authority)) %in% c("localhost", "127.0.0.1", "[::1]")
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
  if (!nzchar(api_key) && !ai_is_local_endpoint(endpoint)) {
    return(list(
      ok = FALSE,
      status = "missing_credentials",
      message = paste("Missing API key. Set environment variable", key_env, "before using this provider.")
    ))
  }

  ai_http_json(
    paste0(endpoint, path),
    method = method,
    body = body,
    headers = if (nzchar(api_key)) list("Authorization" = paste("Bearer", api_key)) else list(),
    timeout = timeout,
    feature = "OpenAI-compatible API",
    status_prefix = "online_provider",
    who = "Provider",
    unreachable_message = paste("Could not reach", endpoint)
  )
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
      list(role = "system", content = "You are a careful statistics, data analysis, and R code assistant inside an RStudio workflow. Be accurate, explicit about assumptions, and provide reproducible R code when useful."),
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

  ai_http_json(
    paste0(endpoint, path),
    method = method,
    body = body,
    headers = list("x-api-key" = api_key, "anthropic-version" = "2023-06-01"),
    timeout = timeout,
    feature = "Anthropic API",
    status_prefix = "anthropic",
    who = "Anthropic",
    unreachable_message = paste("Could not reach", endpoint)
  )
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
    system = "You are a careful statistics, data analysis, and R code assistant inside an RStudio workflow. Be accurate, explicit about assumptions, and provide reproducible R code when useful.",
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
    return(normalizePath(path.expand(command)))
  }
  found <- Sys.which(command)
  if (nzchar(found)) {
    return(unname(found))
  }
  # R started from RStudio or a launcher often lacks the shell PATH, so check
  # the usual install folders for Claude Code, Codex, Homebrew, and npm.
  candidates <- c(
    file.path(path.expand("~"), ".local", "bin", command),
    file.path("/opt/homebrew/bin", command),
    file.path("/usr/local/bin", command),
    file.path(path.expand("~"), ".npm-global", "bin", command),
    file.path(Sys.getenv("APPDATA"), "npm", paste0(command, ".cmd")),
    file.path(Sys.getenv("USERPROFILE"), ".local", "bin", paste0(command, ".exe"))
  )
  found <- candidates[file.exists(candidates)]
  if (length(found)) {
    return(found[1])
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

# Windows cannot start npm's .cmd shims directly, so run them through cmd.exe.
# ponytail: untested on Windows; /s /c with outer quotes is cmd's documented quoting form.
ai_cli_invocation <- function(resolved, args = character(0)) {
  if (.Platform$OS.type == "windows" && grepl("\\.(cmd|bat)$", resolved, ignore.case = TRUE)) {
    inner <- paste(c(shQuote(resolved), args), collapse = " ")
    return(list(command = Sys.getenv("COMSPEC", "cmd.exe"), args = c("/s", "/c", paste0('"', inner, '"'))))
  }
  list(command = resolved, args = args)
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

  call <- ai_cli_invocation(resolved, "--version")
  version <- tryCatch(
    suppressWarnings(system2(call$command, call$args, stdout = TRUE, stderr = TRUE, timeout = 15)),
    error = function(e) e
  )
  status <- attr(version, "status")
  if (!inherits(version, "error") && !is.null(status) && !identical(status, 0L)) {
    version <- simpleError(paste0("exit status ", status, ": ", paste(version, collapse = "\n")))
  }

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

  # system2 quotes the command but not args, and runs through the shell.
  call <- ai_cli_invocation(resolved, shQuote(args))
  output <- tryCatch(
    suppressWarnings(system2(call$command, args = call$args, input = input, stdout = TRUE, stderr = TRUE, timeout = timeout)),
    error = function(e) e
  )

  status <- attr(output, "status")
  if (inherits(output, "error") || (!is.null(status) && !identical(status, 0L))) {
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
  args <- c("-p", "--output-format", "text")
  model <- trimws(settings$model %||% "")
  if (nzchar(model)) {
    args <- c(args, "--model", model)
  }

  result <- ai_cli_run(command, args, input = prompt, label = "Claude Code CLI", timeout = 180)
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
        "Choose a configured local or online provider to request a direct answer.",
        sep = "\n"
      )
    ))
  }

  blocked <- list(
    ok = FALSE,
    status = "blocked_external_send",
    message = "This provider sends your prompt to another computer, but external-send approval is unchecked. Review the prompt for private data, then explicitly approve sending."
  )

  if (provider == "ollama") {
    if (!ai_is_local_endpoint(ai_ollama_endpoint(settings$endpoint)) && !isTRUE(settings$allow_external)) {
      return(blocked)
    }
    return(ai_ollama_generate(prompt, settings))
  }

  if (provider == "codex_cli") {
    return(ai_codex_cli_generate(prompt, settings))
  }

  if (provider == "claude_code_cli") {
    return(ai_claude_code_generate(prompt, settings))
  }

  local_openai <- provider %in% c("openai", "generic_openai") && ai_is_local_endpoint(settings$endpoint)
  if (provider %in% c("openai", "anthropic", "generic_openai") && !local_openai && !isTRUE(settings$allow_external)) {
    return(blocked)
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
